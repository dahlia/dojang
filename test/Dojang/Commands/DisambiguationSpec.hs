{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.DisambiguationSpec (spec) where

import Prelude hiding (readFile, writeFile)

import Control.Exception (bracket_)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.OsPath (encodeFS)
import Test.Hspec (Spec, describe, it, runIO, sequential)
import Test.Hspec.Expectations.Pretty (shouldBe)

import Dojang.Commands.Disambiguation
  ( AutoSelectMode (..)
  , getAutoSelectMode
  )
import Dojang.MonadFileSystem qualified (FileType (..))
import Dojang.Types.Context (CandidateRoute (..))
import Dojang.Types.Repository (RouteResult (..))


-- | Helper to run a test with a specific environment variable setting.
withEnvVar :: String -> Maybe String -> IO a -> IO a
withEnvVar name value action = do
  oldValue <- lookupEnv name
  bracket_
    (setOrUnset value)
    (setOrUnset oldValue)
    action
 where
  setOrUnset :: Maybe String -> IO ()
  setOrUnset (Just v) = setEnv name v
  setOrUnset Nothing = unsetEnv name


spec :: Spec
spec = do
  foo <- runIO $ encodeFS "foo"
  dst <- runIO $ encodeFS "dst"

  -- These tests must run sequentially because they modify global env vars.
  sequential $ describe "getAutoSelectMode" $ do
    it "returns Interactive when DOJANG_AUTO_SELECT is unset" $ do
      withEnvVar "DOJANG_AUTO_SELECT" Nothing $ do
        mode <- getAutoSelectMode
        mode `shouldBe` Interactive

    it "returns AutoSelectFirst when DOJANG_AUTO_SELECT is 'first'" $ do
      withEnvVar "DOJANG_AUTO_SELECT" (Just "first") $ do
        mode <- getAutoSelectMode
        mode `shouldBe` AutoSelectFirst

    it "returns ErrorOnAmbiguity when DOJANG_AUTO_SELECT is 'error'" $ do
      withEnvVar "DOJANG_AUTO_SELECT" (Just "error") $ do
        mode <- getAutoSelectMode
        mode `shouldBe` ErrorOnAmbiguity

    it "returns Interactive for unknown values" $ do
      withEnvVar "DOJANG_AUTO_SELECT" (Just "unknown") $ do
        mode <- getAutoSelectMode
        mode `shouldBe` Interactive

  describe "CandidateRoute" $ do
    it "has expected fields" $ do
      let route =
            RouteResult
              { sourcePath = foo
              , routeName = foo
              , destinationPath = dst
              , fileType = Dojang.MonadFileSystem.Directory
              }
      let candidate = CandidateRoute route 1 foo True
      candidate.specificity `shouldBe` 1
      candidate.sourceFilePath `shouldBe` foo
      candidate.sourceExists `shouldBe` True
      candidate.route.routeName `shouldBe` foo
