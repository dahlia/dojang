{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Codec.EncryptedSpec (spec) where

import Data.ByteString qualified as ByteString
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map.Strict qualified as Map
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldNotContain)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.Types.Codec
  ( CodecConfiguration (CodecConfiguration)
  , CodecName
  , CodecSpec (CodecSpec)
  , CodecValue (CodecString)
  , ReflectPolicy (ReflectReject)
  )
import Dojang.Types.Codec.Encrypted
  ( encryptedCodecImplementation
  , encryptedCodecName
  , encryptedReAddCodecImplementation
  , encryptedReAddCodecName
  )
import Dojang.Types.Codec.Evaluate
  ( CodecEvaluationRequest (CodecEvaluationRequest)
  , CodecRuntime (CodecRuntime)
  , EvaluatedCodec (..)
  , EvaluationMode (NormalEvaluation)
  , ExternalInput (ExternalInput)
  , ExternalInputFailure (CodecBackendReportedFailure)
  , ExternalInputRequest (BackendInputRequest)
  , codecReflectPolicy
  , codecRegistry
  , evaluateCodec
  , formatCodecError
  , opaqueBytes
  , reflectCodec
  , revealBytes
  )
import Dojang.Types.CodecBackend.Protocol
  ( BackendFailure (BackendPermissionDenied)
  , BackendOperation (Decrypt, Encrypt)
  )


spec :: Spec
spec = describe "encrypted codecs" $ do
  specify "decrypts arbitrary binary source through the selected backend" $
    hedgehog $ do
      plaintext <- forAll $ Gen.bytes $ Range.linear 0 4096
      let ciphertext = "cipher:" <> plaintext
          request = evaluationRequest encryptedCodecName ciphertext
          result = runIdentity $ evaluateCodec runtime request Nothing
      (revealBytes . (.renderedBytes) <$> result) === Right plaintext

  specify "rejects reflection for the default encrypted codec" $ do
    codecReflectPolicy runtime "route" (codecSpec encryptedCodecName)
      `shouldBe` Right ReflectReject

  specify "re-encrypts and validates arbitrary deployed bytes" $ hedgehog $ do
    deployed <- forAll $ Gen.bytes $ Range.linear 0 4096
    let request = evaluationRequest encryptedReAddCodecName "cipher:old"
        result = runIdentity $ reflectCodec runtime request $ opaqueBytes deployed
    (revealBytes <$> result) === Right ("cipher:" <> deployed)

  specify "rejects re-encryption whose decrypted bytes do not round trip" $ do
    let brokenRuntime =
          CodecRuntime
            (codecRegistry [encryptedReAddCodecImplementation])
            NormalEvaluation
            ( \backendRequest -> pure $ case backendRequest of
                BackendInputRequest "vault" Encrypt _ ->
                  Right $ ExternalInput (opaqueBytes "ciphertext") "backend-v1"
                BackendInputRequest "vault" Decrypt _ ->
                  Right $ ExternalInput (opaqueBytes "different") "backend-v1"
                _ -> Left "unexpected request"
            )
        request = evaluationRequest encryptedReAddCodecName "old"
        result =
          runIdentity $ reflectCodec brokenRuntime request $ opaqueBytes "deployed"
    either formatCodecError (const "unexpected success") result
      `shouldBe` "Route route codec encrypted-re-add failed reverse validation."

  specify "redacts backend payloads from requests and failures" $ do
    let sentinel = "do-not-print-secret"
        request = BackendInputRequest "vault" Decrypt $ opaqueBytes sentinel
        brokenRuntime =
          CodecRuntime
            (codecRegistry [encryptedCodecImplementation])
            NormalEvaluation
            (const $ pure $ Left "do-not-print-secret")
        result =
          runIdentity $
            evaluateCodec
              brokenRuntime
              (evaluationRequest encryptedCodecName sentinel)
              Nothing
    show request `shouldNotContain` "do-not-print-secret"
    show result `shouldNotContain` "do-not-print-secret"

  specify "preserves sanitized backend failure categories" $ do
    let brokenRuntime =
          CodecRuntime
            (codecRegistry [encryptedCodecImplementation])
            NormalEvaluation
            ( const $
                pure $
                  Left $
                    CodecBackendReportedFailure BackendPermissionDenied
            )
        result =
          runIdentity $
            evaluateCodec
              brokenRuntime
              (evaluationRequest encryptedCodecName "ciphertext")
              Nothing
    either formatCodecError (const "unexpected success") result
      `shouldBe` "Route route codec encrypted could not resolve declared input backend:vault:decrypt. The backend denied access to the request."

  specify "rejects unknown encrypted codec configuration" $ do
    let spec' =
          CodecSpec encryptedCodecName $
            CodecConfiguration $
              Map.fromList
                [ ("backend", CodecString "vault")
                , ("secret", CodecString "must-not-appear")
                ]
        result =
          runIdentity $
            evaluateCodec runtime (evaluationRequestFor spec' "cipher:value") Nothing
    either formatCodecError (const "unexpected success") result
      `shouldBe` "Route route codec encrypted has invalid configuration."


runtime :: CodecRuntime Identity
runtime =
  CodecRuntime
    (codecRegistry [encryptedCodecImplementation, encryptedReAddCodecImplementation])
    NormalEvaluation
    resolve
 where
  resolve (BackendInputRequest "vault" Decrypt payload) =
    pure $ case ByteString.stripPrefix "cipher:" $ revealBytes payload of
      Nothing -> Left "invalid ciphertext"
      Just plaintext -> Right $ ExternalInput (opaqueBytes plaintext) "backend-v1"
  resolve (BackendInputRequest "vault" Encrypt payload) =
    pure $
      Right $
        ExternalInput
          (opaqueBytes $ "cipher:" <> revealBytes payload)
          "backend-v1"
  resolve _ = pure $ Left "unexpected backend request"


evaluationRequest
  :: CodecName -> ByteString.ByteString -> CodecEvaluationRequest
evaluationRequest name = evaluationRequestFor $ codecSpec name


evaluationRequestFor
  :: CodecSpec -> ByteString.ByteString -> CodecEvaluationRequest
evaluationRequestFor spec' source =
  CodecEvaluationRequest "route" spec' (opaqueBytes source) Map.empty Map.empty


codecSpec :: CodecName -> CodecSpec
codecSpec name =
  CodecSpec name $
    CodecConfiguration $
      Map.singleton "backend" $
        CodecString "vault"
