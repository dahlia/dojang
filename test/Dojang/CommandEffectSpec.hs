{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.CommandEffectSpec (spec) where

import Control.Monad (void, when)
import Control.Monad.Except (catchError, throwError)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List (isInfixOf)
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Exit (ExitCode (..))
import System.Info qualified
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (evalIO, forAll, hedgehog, (===))

import Dojang.CommandEffect
  ( BinaryProcessRequest (..)
  , BinaryProcessResult (..)
  , CommandEffect (..)
  , CommandEffectError (..)
  , CommandEffectKind (..)
  , CommandEffectResponse (..)
  , MonadCommandEffect (..)
  , MonadProcessControl (startProcess)
  , OutputStream (..)
  , ProcessRequest (..)
  , ProcessResult (..)
  , PromptRequest (..)
  , PromptResult (..)
  , awaitProcess
  , cancelStartedProcess
  , emptyProcessRequest
  , redactedProcessBytes
  , revealProcessBytes
  , runBinaryProcessIO
  , runCommandEffectTest
  )
import Dojang.MonadFileSystem (dryRunIO)


spec :: Spec
spec = do
  describe "runCommandEffectTest" $ do
    it "replays arbitrary environment lookups in order" $ hedgehog $ do
      names <-
        forAll $
          Gen.list
            (Range.linear 0 100)
            (Gen.string (Range.linear 0 30) Gen.unicode)
      let script = EnvironmentValue . Just <$> names
          action = mapM lookupEnvironmentVariable names
          expectedEvents = EnvironmentLookup <$> names
      result <- evalIO $ runCommandEffectTest script action
      result === Right (map Just names, expectedEvents)

    it "replays arbitrary host platform identifiers" $ hedgehog $ do
      platform <-
        forAll $ Gen.string (Range.linear 0 30) Gen.unicode
      result <-
        evalIO $
          runCommandEffectTest [PlatformValue platform] hostPlatform
      result === Right (platform, [HostPlatformRead])

    it "fails on an unexpected effect" $ do
      result <- runCommandEffectTest [] currentTime
      result `shouldBe` Left (UnexpectedEffect CurrentTimeRead)

    it "fails when a scripted response has the wrong type" $ do
      result <- runCommandEffectTest [TerminalValue True] currentTime
      result
        `shouldBe` Left
          (UnexpectedResponse CurrentTimeRead (TerminalValue True))

    it "fails when scripted responses remain unused" $ do
      let response = EnvironmentValue $ Just "value"
      result <- runCommandEffectTest [response] $ return ()
      result `shouldBe` Left (UnusedResponses [response])

    it "fails when a started process is leaked" $ do
      let request = emptyProcessRequest{executable = "hook"}
      result <- runCommandEffectTest [ProcessStartedValue] $ do
        void $ startProcess request
      result `shouldBe` Left (UnfinishedProcesses [0])

    it "preserves effects and responses across caught I/O errors" $ do
      let Just uuid = UUID.fromString "00112233-4455-6677-8899-aabbccddeeff"
          time = read "2026-07-22 00:00:00 UTC" :: UTCTime
      result <-
        runCommandEffectTest [CurrentTimeValue time, UUIDValue uuid] $
          catchError
            (currentTime >> throwError (userError "failure"))
            (const newUUID)
      result `shouldBe` Right (uuid, [CurrentTimeRead, UUIDGenerated])

    it "preserves process accounting across caught I/O errors" $ do
      let request = emptyProcessRequest{executable = "hook"}
      result <-
        runCommandEffectTest [ProcessStartedValue] $
          catchError
            (startProcess request >> throwError (userError "failure"))
            (const $ return ())
      result `shouldBe` Left (UnfinishedProcesses [0])

    it "stops at an arbitrary command exit" $ hedgehog $ do
      code <- forAll $ Gen.int (Range.linear 1 255)
      result <-
        evalIO $
          runCommandEffectTest [] $ do
            _ <- abortCommand $ ExitFailure code
            writeStream OutputError "unreachable"
      result === Left (CommandAborted $ ExitFailure code)

    it "reports arbitrary command exits with leaked processes" $ hedgehog $ do
      code <- forAll $ Gen.int (Range.linear 1 255)
      let request = emptyProcessRequest{executable = "hook"}
      result <-
        evalIO $
          runCommandEffectTest [ProcessStartedValue] $ do
            void $ startProcess request
            void $ abortCommand $ ExitFailure code
      result
        === Left
          (CommandAbortedWithUnfinishedProcesses (ExitFailure code) [0])

  describe "dry-run command effects" $ do
    it "does not execute processes" $ do
      result <- runCommandEffectTest [ExecutionRequired ProcessExecution] $ do
        runProcess emptyProcessRequest{executable = "editor"}
      result
        `shouldBe` Right
          ( ProcessUnavailable ProcessExecution
          , [ProcessRun emptyProcessRequest{executable = "editor"}]
          )

    it "refuses real process execution in DryRunIO" $ do
      result <-
        dryRunIO $
          runProcess emptyProcessRequest{executable = "does-not-exist"}
      result `shouldBe` ProcessUnavailable ProcessExecution

    it "refuses codec backend execution in DryRunIO" $ do
      let request =
            BinaryProcessRequest
              "does-not-exist"
              Nothing
              []
              (redactedProcessBytes "payload")
              30
      result <- dryRunIO $ runBinaryProcess request
      result `shouldBe` BinaryProcessUnavailable CodecBackendExecution

    it "records output without requiring execution" $ do
      result <- runCommandEffectTest [] $ writeStream OutputError "warning\n"
      result `shouldBe` Right ((), [StreamWrite OutputError "warning\n"])

    it "does not consume prompts" $ do
      let request = ConfirmPrompt "Continue?"
      result <- runCommandEffectTest [ExecutionRequired PromptInteraction] $ do
        prompt request
      result `shouldBe` Right (PromptUnavailable, [Prompted request])

  describe "effect value round trips" $ do
    it "preserves arbitrary binary process input and output" $ hedgehog $ do
      input <- forAll $ Gen.bytes (Range.linear 0 4096)
      output <- forAll $ Gen.bytes (Range.linear 0 4096)
      let request =
            BinaryProcessRequest
              "/backend"
              (Just "/repository")
              []
              (redactedProcessBytes input)
              30
          completed =
            BinaryProcessCompleted
              ExitSuccess
              (redactedProcessBytes output)
              (redactedProcessBytes ByteString.empty)
      result <-
        evalIO $
          runCommandEffectTest [BinaryProcessValue completed] $
            runBinaryProcess request
      result === Right (completed, [BinaryProcessRun request])

    it "redacts binary process payloads from Show" $ do
      let sentinel = "DOJANG-SENTINEL-SECRET" :: ByteString.ByteString
          request =
            BinaryProcessRequest
              "/backend"
              Nothing
              []
              (redactedProcessBytes sentinel)
              30
      show request
        `shouldSatisfy` (not . isInfixOf (ByteString.Char8.unpack sentinel))

    it "streams large binary payloads without pipe deadlock" $ do
      when (System.Info.os /= "mingw32") $ do
        let payload = ByteString.replicate (1024 * 1024) 97
            request =
              BinaryProcessRequest
                "/bin/cat"
                Nothing
                []
                (redactedProcessBytes payload)
                5
        result <- runBinaryProcessIO request
        case result of
          BinaryProcessCompleted ExitSuccess output errors -> do
            revealProcessBytes output `shouldBe` payload
            revealProcessBytes errors `shouldBe` ByteString.empty
          _ ->
            result
              `shouldBe` BinaryProcessCompleted
                ExitSuccess
                (redactedProcessBytes payload)
                (redactedProcessBytes ByteString.empty)

    it "retains an early backend exit when stdin closes" $ do
      when (System.Info.os /= "mingw32") $ do
        let request =
              BinaryProcessRequest
                "/bin/false"
                Nothing
                []
                (redactedProcessBytes $ ByteString.replicate (1024 * 1024) 97)
                5
        result <- runBinaryProcessIO request
        result
          `shouldBe` BinaryProcessCompleted
            (ExitFailure 1)
            (redactedProcessBytes ByteString.empty)
            (redactedProcessBytes ByteString.empty)

    it "terminates a backend at its hard timeout" $ do
      when (System.Info.os /= "mingw32") $ do
        let request =
              BinaryProcessRequest
                "/usr/bin/yes"
                Nothing
                []
                (redactedProcessBytes ByteString.empty)
                1
        result <- runBinaryProcessIO request
        result `shouldBe` BinaryProcessTimedOut

    it "rejects non-positive binary process timeouts" $ do
      let request =
            BinaryProcessRequest
              "does-not-exist"
              Nothing
              []
              (redactedProcessBytes ByteString.empty)
              0
      result <- runBinaryProcessIO request
      result `shouldBe` BinaryProcessIOFailed "Backend timeout must be positive."

    it "preserves arbitrary process arguments and environments" $ hedgehog $ do
      executable <- forAll $ Gen.string (Range.linear 0 30) Gen.unicode
      arguments <-
        forAll $
          Gen.list (Range.linear 0 30) $
            Gen.string (Range.linear 0 30) Gen.unicode
      environment <-
        forAll $
          Gen.list (Range.linear 0 30) $
            (,)
              <$> Gen.string (Range.linear 0 20) Gen.unicode
              <*> Gen.string (Range.linear 0 30) Gen.unicode
      let request =
            emptyProcessRequest
              { executable = executable
              , arguments = arguments
              , environment = Just environment
              , captureOutput = True
              }
          completed = ProcessCompleted ExitSuccess "stdout" "stderr"
      result <-
        evalIO $
          runCommandEffectTest [ProcessValue completed] $
            runProcess request
      result === Right (completed, [ProcessRun request])

    it "separates arbitrary process starts from waits" $ hedgehog $ do
      code <- forAll $ Gen.int (Range.linear 1 255)
      let request = emptyProcessRequest{executable = "hook", arguments = [show code]}
          completed = ProcessCompleted (ExitFailure code) "" ""
      result <-
        evalIO $
          runCommandEffectTest [ProcessStartedValue, ProcessValue completed] $ do
            started <- startProcess request
            case started of
              Left failure -> return failure
              Right process -> awaitProcess process
      result
        === Right
          (completed, [ProcessStart request, ProcessAwait 0])

    it "records arbitrary process cancellation" $ hedgehog $ do
      argument <- forAll $ Gen.string (Range.linear 0 30) Gen.unicode
      let request = emptyProcessRequest{executable = "hook", arguments = [argument]}
      result <-
        evalIO
          $ runCommandEffectTest
            [ProcessStartedValue, ProcessCancellationConfirmed]
          $ do
            started <- startProcess request
            case started of
              Left _ -> return ()
              Right process -> cancelStartedProcess process
      result
        === Right
          ((), [ProcessStart request, ProcessCancel 0])

    it "preserves UUIDs, times, and process exit codes" $ do
      let Just uuid = UUID.fromString "00112233-4455-6677-8899-aabbccddeeff"
          time = read "2026-07-22 00:00:00 UTC" :: UTCTime
          request = emptyProcessRequest{executable = "tool", arguments = ["a b"]}
          script =
            [ UUIDValue uuid
            , CurrentTimeValue time
            , ProcessValue $ ProcessCompleted (ExitFailure 7) "out" "err"
            ]
      result <- runCommandEffectTest script $ do
        uuid' <- newUUID
        time' <- currentTime
        process <- runProcess request
        return (uuid', time', process)
      result
        `shouldBe` Right
          ( (uuid, time, ProcessCompleted (ExitFailure 7) "out" "err")
          , [UUIDGenerated, CurrentTimeRead, ProcessRun request]
          )
