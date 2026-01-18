{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.EditSpec (spec) where

import Control.Exception (bracket_)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Info (os)

import Test.Hspec (Spec, describe, it, sequential)
import Test.Hspec.Expectations.Pretty (shouldBe)

import Dojang.Commands.Edit (defaultEditor, getEditor)


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


-- | Helper to run a test with multiple environment variables set.
withEnvVars :: [(String, Maybe String)] -> IO a -> IO a
withEnvVars [] action = action
withEnvVars ((name, value) : rest) action =
  withEnvVar name value (withEnvVars rest action)


spec :: Spec
spec = do
  describe "defaultEditor" $ do
    it "is vi on POSIX" $ do
      if os == "mingw32"
        then defaultEditor `shouldBe` "notepad"
        else defaultEditor `shouldBe` "vi"

  -- These tests must run sequentially because they modify global env vars.
  sequential $ describe "getEditor" $ do
    it "returns --editor option when provided" $ do
      withEnvVars [("VISUAL", Just "emacs"), ("EDITOR", Just "nano")] $ do
        editor <- getEditor (Just "vim")
        editor `shouldBe` Just "vim"

    it "returns VISUAL when --editor not provided" $ do
      withEnvVars [("VISUAL", Just "emacs"), ("EDITOR", Just "nano")] $ do
        editor <- getEditor Nothing
        editor `shouldBe` Just "emacs"

    it "returns EDITOR when VISUAL not set" $ do
      withEnvVars [("VISUAL", Nothing), ("EDITOR", Just "nano")] $ do
        editor <- getEditor Nothing
        editor `shouldBe` Just "nano"

    it "returns Nothing when no env vars set and no option" $ do
      withEnvVars [("VISUAL", Nothing), ("EDITOR", Nothing)] $ do
        editor <- getEditor Nothing
        editor `shouldBe` Nothing
