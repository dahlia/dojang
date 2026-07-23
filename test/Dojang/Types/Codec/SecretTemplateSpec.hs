{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Codec.SecretTemplateSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad.State.Strict (State, get, put, runState)
import Data.ByteString qualified as ByteString
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty
  ( expectationFailure
  , shouldBe
  , shouldNotContain
  )
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.Types.Codec.Evaluate
  ( CodecEvaluationRequest (CodecEvaluationRequest)
  , CodecRuntime (CodecRuntime)
  , EvaluatedCodec (..)
  , EvaluationMode (NormalEvaluation)
  , ExternalInput (ExternalInput)
  , ExternalInputFailure (OpaqueExternalInputFailure)
  , ExternalInputRequest (BackendInputRequest)
  , codecRegistry
  , evaluateCodec
  , formatCodecError
  , opaqueBytes
  , reflectCodec
  , revealBytes
  )
import Dojang.Types.Codec.SecretTemplate
  ( secretTemplateCodecImplementation
  , secretTemplateCodecSpec
  )
import Dojang.Types.Codec.Template (secretReferenceKey)
import Dojang.Types.CodecBackend.Protocol
  ( BackendOperation (Lookup)
  )


spec :: Spec
spec = describe "secret-template codec" $ do
  specify "looks up only the secret selected by an arbitrary branch" $ hedgehog $ do
    selected <- forAll Gen.bool
    secret <- forAll $ Gen.text (Range.linear 0 2048) Gen.unicode
    let selectedItem = if selected then "first" else "second"
        source =
          "{{ secret(\"vault\", \"first\") if facts.selected == \"true\" else secret(\"vault\", \"second\") }}"
        request =
          CodecEvaluationRequest
            "route"
            secretTemplateCodecSpec
            (opaqueBytes source)
            (Map.singleton "selected" $ if selected then "true" else "false")
            Map.empty
        (result, requests) =
          runState (evaluateCodec (runtime secret) request Nothing) []
    (revealBytes . (.renderedBytes) <$> result) === Right (encodeUtf8 secret)
    requests
      === [BackendInputRequest "vault" (Lookup selectedItem) $ opaqueBytes ""]

  specify "reuses one lookup within an evaluation" $ hedgehog $ do
    secret <- forAll $ Gen.text (Range.linear 0 2048) Gen.unicode
    let source =
          "{{ secret(\"vault\", \"item\") }}{{ secret(\"vault\", \"item\") }}"
        request =
          CodecEvaluationRequest
            "route"
            secretTemplateCodecSpec
            (opaqueBytes source)
            Map.empty
            Map.empty
        (result, requests) =
          runState (evaluateCodec (runtime secret) request Nothing) []
    (revealBytes . (.renderedBytes) <$> result)
      === Right (encodeUtf8 secret <> encodeUtf8 secret)
    requests === [BackendInputRequest "vault" (Lookup "item") $ opaqueBytes ""]

  specify "distinguishes NUL bytes across secret reference components" $ do
    let selectedRequest =
          BackendInputRequest "a" (Lookup "\NULb") $ opaqueBytes ""
        selectedSecret = "selected-secret"
        source =
          "{{ secret(\"a\", \"\NULb\") if facts.selected == \"true\" else secret(\"a\NUL\", \"b\") }}"
        request =
          CodecEvaluationRequest
            "route"
            secretTemplateCodecSpec
            (opaqueBytes source)
            (Map.singleton "selected" "true")
            Map.empty
        codecRuntime =
          CodecRuntime
            (codecRegistry [secretTemplateCodecImplementation])
            NormalEvaluation
            ( \externalRequest ->
                pure $
                  Right $
                    ExternalInput
                      ( opaqueBytes $
                          if externalRequest == selectedRequest
                            then selectedSecret
                            else "unselected-secret"
                      )
                      "backend-v1"
            )
    completed <- timeout 1000000 $ evaluateCodec codecRuntime request Nothing
    case completed of
      Nothing -> expectationFailure "secret-template evaluation did not terminate"
      Just result ->
        (revealBytes . (.renderedBytes) <$> result)
          `shouldBe` Right selectedSecret

  specify "does not treat a missing variable as a secret request" $ do
    let marker = secretReferenceKey "vault" "item"
        source =
          "{{ vars[\""
            <> marker
            <> "\"] if facts.selected == \"true\" else secret(\"vault\", \"item\") }}"
        request =
          CodecEvaluationRequest
            "route"
            secretTemplateCodecSpec
            (opaqueBytes $ encodeUtf8 source)
            (Map.singleton "selected" "true")
            Map.empty
        (result, requests) =
          runState (evaluateCodec (runtime "secret") request Nothing) []
    completed <-
      timeout 1000000 $
        evaluate $
          either formatCodecError (const "unexpected success") result
    case completed of
      Nothing -> expectationFailure "secret-template evaluation did not terminate"
      Just message -> do
        message
          `shouldBe` ( "Route route codec secret-template is missing template input vars."
                         <> marker
                         <> " at line 1, column 8."
                     )
        requests `shouldBe` []

  specify "rejects dynamic secret references before lookup" $ do
    let request =
          CodecEvaluationRequest
            "route"
            secretTemplateCodecSpec
            (opaqueBytes "{{ secret(vars.BACKEND, \"item\") }}")
            Map.empty
            (Map.singleton "BACKEND" "vault")
        (result, requests) =
          runState (evaluateCodec (runtime "secret") request Nothing) []
    either formatCodecError (const "unexpected success") result
      `shouldBe` "Route route codec secret-template uses unsupported source syntax at line 1, column 10."
    requests `shouldBe` []

  specify "rejects reflection without looking up a secret" $ do
    let request =
          CodecEvaluationRequest
            "route"
            secretTemplateCodecSpec
            (opaqueBytes "{{ secret(\"vault\", \"item\") }}")
            Map.empty
            Map.empty
        (result, requests) =
          runState
            (reflectCodec (runtime "secret") request $ opaqueBytes "changed")
            []
    either formatCodecError (const "unexpected success") result
      `shouldBe` "Route route codec secret-template rejects reflection."
    requests `shouldBe` []

  specify "redacts secret values from failures" $ do
    let sentinel :: Text
        sentinel = "do-not-print-secret"
        request =
          CodecEvaluationRequest
            "route"
            secretTemplateCodecSpec
            (opaqueBytes "{{ secret(\"vault\", \"item\") }}")
            Map.empty
            Map.empty
        brokenRuntime =
          CodecRuntime
            (codecRegistry [secretTemplateCodecImplementation])
            NormalEvaluation
            (const $ pure $ Left $ OpaqueExternalInputFailure sentinel)
        result = fst $ runState (evaluateCodec brokenRuntime request Nothing) []
    show result `shouldNotContain` "do-not-print-secret"
    show (BackendInputRequest "vault" (Lookup sentinel) $ opaqueBytes "")
      `shouldNotContain` "do-not-print-secret"

  specify "rejects a bare secret function reference" $ do
    let request =
          CodecEvaluationRequest
            "route"
            secretTemplateCodecSpec
            (opaqueBytes "{{ secret }}")
            Map.empty
            Map.empty
        (result, requests) =
          runState (evaluateCodec (runtime "secret") request Nothing) []
    either formatCodecError (const "unexpected success") result
      `shouldBe` "Route route codec secret-template uses unsupported source syntax at line 1, column 4."
    requests `shouldBe` []


runtime :: Text -> CodecRuntime (State [ExternalInputRequest])
runtime secret =
  CodecRuntime
    (codecRegistry [secretTemplateCodecImplementation])
    NormalEvaluation
    resolve
 where
  resolve
    :: ExternalInputRequest
    -> State [ExternalInputRequest] (Either ExternalInputFailure ExternalInput)
  resolve request@(BackendInputRequest "vault" (Lookup _) payload)
    | ByteString.null $ revealBytes payload = do
        previous <- get
        put $ previous <> [request]
        pure $ Right $ ExternalInput (opaqueBytes $ encodeUtf8 secret) "backend-v1"
  resolve _ = pure $ Left "unexpected request"
