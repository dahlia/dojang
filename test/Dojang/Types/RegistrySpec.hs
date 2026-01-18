{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.RegistrySpec (spec) where

import System.Info (os)

import System.OsPath (encodeFS, (</>))
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Expectations.Pretty (shouldBe)

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Registry
  ( Registry (..)
  , readRegistry
  , registryFilename
  , writeRegistry
  )


win :: Bool
win = os == "mingw32"


spec :: Spec
spec = do
  dotDojang <- runIO $ encodeFS ".dojang"
  repoPath <- runIO $ encodeFS $ if win then "C:\\dotfiles" else "/home/test/dotfiles"

  describe "registryFilename" $ do
    it "is .dojang" $ do
      registryFilename `shouldBe` dotDojang

  describe "readRegistry" $ do
    it "returns Nothing if file does not exist" $ withTempDir $ \tmpDir _ -> do
      result <- readRegistry (tmpDir </> dotDojang)
      result `shouldBe` Nothing

    it "parses valid registry file" $ withTempDir $ \tmpDir _ -> do
      let registryPath = tmpDir </> dotDojang
      Dojang.MonadFileSystem.writeFile registryPath $
        "repository = \""
          <> (if win then "C:\\\\dotfiles" else "/home/test/dotfiles")
          <> "\"\n"
      result <- readRegistry registryPath
      result `shouldBe` Just (Registry repoPath)

    it "returns Nothing for invalid TOML" $ withTempDir $ \tmpDir _ -> do
      let registryPath = tmpDir </> dotDojang
      Dojang.MonadFileSystem.writeFile registryPath "invalid toml content"
      result <- readRegistry registryPath
      result `shouldBe` Nothing

  describe "writeRegistry" $ do
    it "creates registry file with repository path" $ withTempDir $ \tmpDir _ -> do
      let registryPath = tmpDir </> dotDojang
      writeRegistry registryPath (Registry repoPath)
      result <- readRegistry registryPath
      result `shouldBe` Just (Registry repoPath)

    it "overwrites existing registry file" $ withTempDir $ \tmpDir _ -> do
      let registryPath = tmpDir </> dotDojang
      oldPath <- encodeFS $ if win then "C:\\old" else "/old"
      newPath <- encodeFS $ if win then "C:\\new" else "/new"
      writeRegistry registryPath (Registry oldPath)
      writeRegistry registryPath (Registry newPath)
      result <- readRegistry registryPath
      result `shouldBe` Just (Registry newPath)
