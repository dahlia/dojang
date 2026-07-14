{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.RegistrySpec (spec) where

import qualified Control.Exception as Exception
import Data.Either (isLeft)
import qualified System.Directory.OsPath as Directory
import System.Info (os)

import System.OsPath (encodeFS, (</>))
import Test.Hspec (Spec, describe, it, runIO, xit)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Registry
  ( Registry (..)
  , readRegistry
  , readRegistryStrict
  , registryFilename
  , writeRegistry
  )


win :: Bool
win = os == "mingw32"


spec :: Spec
spec = do
  dotDojang <- runIO $ encodeFS ".dojang"
  symlinkAvailable <- runIO $ withTempDir $ \tmpDir _ -> do
    target <- encodeFS "missing"
    link <- encodeFS "link"
    (Directory.createFileLink target (tmpDir </> link) >> return True)
      `catchIO` const (return False)
  let symlinkIt = if symlinkAvailable then it else xit
  repoPath <-
    runIO $ encodeFS $ if win then "C:\\dotfiles" else "/home/test/dotfiles"

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

  describe "readRegistryStrict" $ do
    it "distinguishes invalid TOML from an absent registry" $
      withTempDir $ \tmpDir _ -> do
        let registryPath = tmpDir </> dotDojang
        Dojang.MonadFileSystem.writeFile registryPath "invalid toml content"
        result <- readRegistryStrict registryPath
        result `shouldSatisfy` isLeft

    it "rejects invalid UTF-8" $ withTempDir $ \tmpDir _ -> do
      let registryPath = tmpDir </> dotDojang
      Dojang.MonadFileSystem.writeFile registryPath "\xff"
      result <- readRegistryStrict registryPath
      result `shouldSatisfy` isLeft

    it "rejects a directory instead of trying to read it" $
      withTempDir $ \tmpDir _ -> do
        let registryPath = tmpDir </> dotDojang
        createDirectories registryPath
        result <- readRegistryStrict registryPath
        result `shouldSatisfy` isLeft

    symlinkIt "rejects a dangling symbolic link instead of treating it as absent" $
      withTempDir $ \tmpDir _ -> do
        missing <- encodeFS "missing"
        let registryPath = tmpDir </> dotDojang
        Directory.createFileLink missing registryPath
        result <- readRegistryStrict registryPath
        result `shouldSatisfy` isLeft

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


catchIO :: IO a -> (IOError -> IO a) -> IO a
catchIO = Exception.catch
