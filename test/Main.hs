module Main where

import qualified Spec

import Control.Exception (bracket, onException)
import Control.Monad (when)

import qualified Data.ByteString as ByteString
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO
  ( Handle
  , SeekMode (AbsoluteSeek)
  , hClose
  , hFlush
  , hSeek
  , stderr
  )
import System.IO.Temp (withSystemTempFile)
import Test.Hspec.Api.Formatters.V1 (specdoc, useFormatter)
import Test.Hspec.JUnit.Formatter ()
import Test.Hspec.Runner
  ( defaultConfig
  , evaluateSummary
  , hspecWithResult
  , summaryFailures
  )


main :: IO ()
main = do
  (standardError, summary) <-
    captureStderr $
      hspecWithResult (useFormatter ("specdoc", specdoc) defaultConfig) Spec.spec
  when (summaryFailures summary > 0) $ ByteString.hPut stderr standardError
  evaluateSummary summary


-- | Captures standard error while an action runs, restoring the original
-- handle even if the action raises an exception.
--
-- Command tests deliberately exercise diagnostics and failure paths.  Keeping
-- their output until the test result is known avoids making successful test
-- runs look like failures while preserving the diagnostics for actual test
-- failures.
captureStderr :: IO a -> IO (ByteString.ByteString, a)
captureStderr action =
  withSystemTempFile "dojang-spec-stderr" $ \_ captureHandle ->
    bracket (hDuplicate stderr) restoreStderr $ \originalHandle -> do
      hDuplicateTo captureHandle stderr
      result <-
        action `onException` replayCapturedStderr captureHandle originalHandle
      captured <- readCapturedStderr captureHandle
      pure (captured, result)
 where
  readCapturedStderr :: Handle -> IO ByteString.ByteString
  readCapturedStderr captureHandle = do
    hFlush stderr
    hSeek captureHandle AbsoluteSeek 0
    ByteString.hGetContents captureHandle

  replayCapturedStderr :: Handle -> Handle -> IO ()
  replayCapturedStderr captureHandle originalHandle = do
    captured <- readCapturedStderr captureHandle
    ByteString.hPut originalHandle captured
    hFlush originalHandle

  restoreStderr :: Handle -> IO ()
  restoreStderr originalHandle = do
    hFlush stderr
    hDuplicateTo originalHandle stderr
    hClose originalHandle

-- cSpell:ignore specdoc
