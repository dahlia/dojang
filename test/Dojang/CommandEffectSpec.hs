{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.CommandEffectSpec (spec) where

import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Exit (ExitCode (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (evalIO, forAll, hedgehog, (===))

import Dojang.CommandEffect
  ( CommandEffect (..)
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
  , StartedProcess (..)
  , emptyProcessRequest
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

    it "stops at an arbitrary command exit" $ hedgehog $ do
      code <- forAll $ Gen.int (Range.linear 1 255)
      result <-
        evalIO $
          runCommandEffectTest [] $ do
            _ <- abortCommand $ ExitFailure code
            writeStream OutputError "unreachable"
      result === Left (CommandAborted $ ExitFailure code)

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

    it "records output without requiring execution" $ do
      result <- runCommandEffectTest [] $ writeStream OutputError "warning\n"
      result `shouldBe` Right ((), [StreamWrite OutputError "warning\n"])

    it "does not consume prompts" $ do
      let request = ConfirmPrompt "Continue?"
      result <- runCommandEffectTest [ExecutionRequired PromptInteraction] $ do
        prompt request
      result `shouldBe` Right (PromptUnavailable, [Prompted request])

  describe "effect value round trips" $ do
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
      result <- evalIO $ runCommandEffectTest [ProcessValue completed] $ do
        started <- startProcess request
        case started of
          Left failure -> return failure
          Right (StartedProcess await _) -> await
      result === Right (completed, [ProcessStart request])

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
