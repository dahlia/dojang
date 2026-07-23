{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.CodecBackendSpec (spec) where

import Data.ByteString qualified as ByteString
import Data.Map.Strict qualified as Map
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Info (os)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (evalIO, forAll, hedgehog, (===))

import Dojang.CommandEffect
  ( BinaryProcessRequest (BinaryProcessRequest)
  , BinaryProcessResult (..)
  , CommandEffect (BinaryProcessRun, HostPlatformRead)
  , CommandEffectKind (CodecBackendExecution)
  , CommandEffectResponse (..)
  , redactedProcessBytes
  , runCommandEffectTest
  )
import Dojang.Types.Codec (CodecValue (CodecString))
import Dojang.Types.Codec.BackendRuntime
  ( executeCodecBackendProcess
  , resolveCodecBackendInput
  )
import Dojang.Types.Codec.Evaluate
  ( ExternalInput (..)
  , ExternalInputFailure (..)
  , ExternalInputRequest (BackendInputRequest)
  , opaqueBytes
  , revealBytes
  )
import Dojang.Types.CodecBackend
  ( CodecBackend (..)
  , CodecBackendOptions (CodecBackendOptions)
  )
import Dojang.Types.CodecBackend.Protocol
  ( BackendFailure (..)
  , BackendOperation (..)
  , BackendProtocolRequest (..)
  , decodeBackendFailure
  , encodeBackendStandardInput
  , formatBackendFailure
  )
import Dojang.Types.FilePathExpression
  ( FilePathExpression (BareComponent, PathSeparator, Root, Substitution)
  )
import Dojang.Types.FilePathExpression.Expansion
  ( ExpansionWarning (UndefinedEnvironmentVariable)
  , VariableLookup (VariableLookup)
  )


spec :: Spec
spec = describe "codec backend protocol" $ do
  it "preserves every arbitrary binary payload after the header" $ hedgehog $ do
    payload <- forAll $ Gen.bytes (Range.linear 0 16384)
    let request =
          BackendProtocolRequest
            "vault"
            "2026-07"
            Decrypt
            mempty
        encoded = encodeBackendStandardInput request payload
        (_, framedPayload) = ByteString.break (== 10) encoded
    ByteString.drop 1 framedPayload === payload

  it "encodes lookup item identities in the JSON header" $ do
    let request =
          BackendProtocolRequest
            "vault"
            "1"
            (Lookup "prod/token")
            mempty
        (header, _) = ByteString.break (== 10) $ encodeBackendStandardInput request ""
    header
      `shouldBe` "{\"backend\":\"vault\",\"item\":\"prod/token\",\"operation\":\"lookup\",\"options\":{},\"protocol\":\"dojang-codec-backend-v1\",\"version\":\"1\"}"

  it "accepts only allowlisted backend diagnostic codes" $ do
    decodeBackendFailure "{\"code\":\"missing-item\",\"detail\":\"secret\"}"
      `shouldBe` BackendMissingItem
    decodeBackendFailure "{\"code\":\"secret-value\"}"
      `shouldBe` BackendProtocolFailure
    decodeBackendFailure "not JSON" `shouldBe` BackendProtocolFailure

  it "formats failures without retaining backend diagnostics" $ do
    formatBackendFailure BackendInvalidInput
      `shouldBe` "The backend rejected the supplied input."

  it "executes arbitrary binary payloads without arguments or environment" $
    hedgehog $ do
      payload <- forAll $ Gen.bytes (Range.linear 0 16384)
      output <- forAll $ Gen.bytes (Range.linear 0 16384)
      let backend =
            CodecBackend
              (BareComponent "/configured/backend")
              "backend-v1"
              17
              (CodecBackendOptions $ Map.singleton "profile" $ CodecString "work")
          protocolRequest =
            BackendProtocolRequest
              "vault"
              "backend-v1"
              Decrypt
              (Map.singleton "profile" $ CodecString "work")
          processRequest =
            BinaryProcessRequest
              "/expanded/backend"
              (Just "/repository")
              []
              (redactedProcessBytes $ encodeBackendStandardInput protocolRequest payload)
              17
          response =
            BinaryProcessValue $
              BinaryProcessCompleted
                ExitSuccess
                (redactedProcessBytes output)
                (redactedProcessBytes "")
      result <-
        evalIO $
          runCommandEffectTest [response] $
            executeCodecBackendProcess
              "vault"
              backend
              "/expanded/backend"
              "/repository"
              Decrypt
              (opaqueBytes payload)
      case result of
        Left err -> fail $ show err
        Right (Left err, _) -> fail $ show err
        Right (Right input, effects) -> do
          revealBytes (input.value) === output
          effects === [BinaryProcessRun processRequest]

  it "resolves an absolute backend into the expected process request" $ do
    let windows = os == "mingw32"
        configuredRoot = Root $ if windows then Just 'C' else Nothing
        configuredBackend =
          if windows then "C:\\configured-backend" else "/configured-backend"
        repositoryPath =
          if windows then "C:\\repository" else "/repository"
        backend =
          CodecBackend
            (PathSeparator configuredRoot $ BareComponent "configured-backend")
            "backend-v1"
            17
            (CodecBackendOptions $ Map.singleton "profile" $ CodecString "work")
        protocolRequest =
          BackendProtocolRequest
            "vault"
            "backend-v1"
            Decrypt
            (Map.singleton "profile" $ CodecString "work")
        processRequest =
          BinaryProcessRequest
            configuredBackend
            (Just repositoryPath)
            []
            (redactedProcessBytes $ encodeBackendStandardInput protocolRequest "ciphertext")
            17
        responses =
          [ PlatformValue "linux"
          , BinaryProcessValue $
              BinaryProcessCompleted
                ExitSuccess
                (redactedProcessBytes "plaintext")
                (redactedProcessBytes "")
          ]
        variableGetter _ = pure $ VariableLookup Nothing [] Map.empty
    result <-
      runCommandEffectTest responses $
        resolveCodecBackendInput
          (Map.singleton "vault" backend)
          variableGetter
          (unsafeEncodeUtf repositoryPath)
          (BackendInputRequest "vault" Decrypt $ opaqueBytes "ciphertext")
    case result of
      Left err -> fail $ show err
      Right (Left err, _) -> fail $ show err
      Right (Right input, effects) -> do
        revealBytes input.value `shouldBe` "plaintext"
        effects
          `shouldBe` [HostPlatformRead, BinaryProcessRun processRequest]

  it "maps backend process failures to redacted diagnostics" $ do
    let backend =
          CodecBackend
            (BareComponent "/configured/backend")
            "backend-v1"
            17
            (CodecBackendOptions Map.empty)
        execute response =
          runCommandEffectTest [BinaryProcessValue response] $
            executeCodecBackendProcess
              "vault"
              backend
              "/expanded/backend"
              "/repository"
              Decrypt
              (opaqueBytes "secret")
        failures =
          [
            ( BinaryProcessCompleted
                (ExitFailure 2)
                (redactedProcessBytes "leaked output")
                (redactedProcessBytes "{\"code\":\"permission-denied\",\"detail\":\"secret\"}")
            , CodecBackendReportedFailure BackendPermissionDenied
            )
          , (BinaryProcessTimedOut, CodecBackendTimeout)
          ,
            ( BinaryProcessStartFailed "secret"
            , CodecBackendStartFailure
            )
          ,
            ( BinaryProcessUnavailable CodecBackendExecution
            , CodecBackendExecutionUnavailable
            )
          ]
    mapM_
      ( \(response, expected) -> do
          result <- execute response
          case result of
            Right (Left message, _) -> message `shouldBe` expected
            other -> fail $ show other
      )
      failures

  it "changes the input fingerprint when the declared version changes" $ do
    let backend version =
          CodecBackend
            (BareComponent "/configured/backend")
            version
            17
            (CodecBackendOptions Map.empty)
        execute version =
          runCommandEffectTest
            [ BinaryProcessValue $
                BinaryProcessCompleted
                  ExitSuccess
                  (redactedProcessBytes "plaintext")
                  (redactedProcessBytes "")
            ]
            $ executeCodecBackendProcess
              "vault"
              (backend version)
              "/expanded/backend"
              "/repository"
              Decrypt
              (opaqueBytes "ciphertext")
    Right (Right first, _) <- execute "backend-v1"
    Right (Right second, _) <- execute "backend-v2"
    revealBytes first.value `shouldBe` revealBytes second.value
    first.fingerprint == second.fingerprint `shouldBe` False

  it
    "rejects Windows, undeclared, and unexpandable backend commands before execution"
    $ do
      let sourcePath = unsafeEncodeUtf "/repository"
          variableGetter _ = pure $ VariableLookup Nothing [] Map.empty
          payload = opaqueBytes "secret"
          request = BackendInputRequest "vault" Decrypt payload
          relativeBackend =
            CodecBackend
              (BareComponent "relative-backend")
              "1"
              10
              (CodecBackendOptions Map.empty)
          absoluteBackend =
            relativeBackend
              { command = PathSeparator (Root Nothing) (BareComponent "backend")
              }
          warningBackend = relativeBackend{command = Substitution "MISSING"}
          resolveWith getter platform backends =
            runCommandEffectTest [PlatformValue platform] $
              resolveCodecBackendInput
                backends
                getter
                sourcePath
                request
          resolve = resolveWith variableGetter
          warningGetter variable =
            pure $
              VariableLookup
                Nothing
                [UndefinedEnvironmentVariable variable]
                Map.empty
      windows <- resolve "mingw32" $ Map.singleton "vault" absoluteBackend
      undeclared <- resolve "linux" Map.empty
      relative <- resolve "linux" $ Map.singleton "vault" relativeBackend
      unexpandable <-
        resolveWith warningGetter "linux" $ Map.singleton "vault" warningBackend
      fmap fst windows
        `shouldBe` Right (Left CodecBackendUnsupportedPlatform)
      fmap fst undeclared
        `shouldBe` Right (Left CodecBackendNotDeclared)
      fmap fst relative
        `shouldBe` Right (Left CodecBackendCommandNotAbsolute)
      fmap fst unexpandable
        `shouldBe` Right (Left CodecBackendCommandExpansionFailure)
