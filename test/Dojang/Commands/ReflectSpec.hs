{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.ReflectSpec (spec) where

import Control.Exception (bracket_)
import Data.HashMap.Strict (singleton)
import qualified Data.Map.Strict as Map
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath (OsPath, encodeFS, (</>))
import Test.Hspec (Spec, describe, it, sequential)
import Test.Hspec.Expectations.Pretty (shouldReturn, shouldThrow)
import Prelude hiding (readFile, writeFile)

import Dojang.App (AppEnv (..), runAppWithoutLogging)
import Dojang.Commands.Reflect (reflect)
import Dojang.ExitCodes (ignoredFileError)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression (FilePathExpression (Substitution))
import Dojang.Types.Manifest (manifest)
import Dojang.Types.MonikerName (parseMonikerName)


spec :: Spec
spec = sequential $ do
  describe "reflect" $ do
    it "reflects a destination-side deletion" $
      withDeletedDestination $
        \appEnv source intermediate _ -> do
          runAppWithoutLogging appEnv (reflect False True False Nothing [])
            `shouldReturn` ExitSuccess
          exists source `shouldReturn` False
          exists intermediate `shouldReturn` False

    it "reflects an explicitly named deleted destination" $
      withDeletedDestination $ \appEnv source intermediate destination -> do
        runAppWithoutLogging
          appEnv
          (reflect False True False Nothing [destination])
          `shouldReturn` ExitSuccess
        exists source `shouldReturn` False
        exists intermediate `shouldReturn` False

    it "accepts a source-only change" $
      withSourceOnlyChange $ \appEnv source intermediate destination -> do
        runAppWithoutLogging appEnv (reflect False True False Nothing [])
          `shouldReturn` ExitSuccess
        readFile source `shouldReturn` "managed contents"
        readFile intermediate `shouldReturn` "managed contents"
        readFile destination `shouldReturn` "managed contents"

    it "reflects an explicitly forced ignored file" $
      withIgnoredDestination $ \appEnv source intermediate destination -> do
        runAppWithoutLogging
          appEnv
          (reflect False True False Nothing [destination])
          `shouldThrow` (== ignoredFileError)
        runAppWithoutLogging
          appEnv
          (reflect True True False Nothing [destination])
          `shouldReturn` ExitSuccess
        readFile source `shouldReturn` "ignored contents"
        readFile intermediate `shouldReturn` "ignored contents"


withDeletedDestination :: (AppEnv -> OsPath -> OsPath -> OsPath -> IO a) -> IO a
withDeletedDestination action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  routeName <- encodeFS "managed-file"
  destination <- encodeFS "destination"
  let repository = tmpDir </> sourceDir
  let intermediate = repository </> intermediateDir </> routeName
  let source = repository </> routeName
  let destinationPath = tmpDir </> destination
  let Right always = parseMonikerName "always"
  let manifest' =
        manifest
          (singleton always Always)
          (Map.singleton routeName [(always, Just $ Substitution "DEST")])
          mempty
          mempty
          mempty
  let appEnv =
        AppEnv
          repository
          intermediateDir
          manifestFilename
          envFilename
          False
          False

  createDirectories $ repository </> intermediateDir
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile source "managed contents"
  writeFile intermediate "managed contents"

  withEnvVar "DEST" (Just destinationPath) $
    action appEnv source intermediate destinationPath


withSourceOnlyChange :: (AppEnv -> OsPath -> OsPath -> OsPath -> IO a) -> IO a
withSourceOnlyChange action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  routeName <- encodeFS "managed-file"
  destination <- encodeFS "destination"
  let repository = tmpDir </> sourceDir
  let intermediate = repository </> intermediateDir </> routeName
  let source = repository </> routeName
  let destinationPath = tmpDir </> destination
  let Right always = parseMonikerName "always"
  let manifest' =
        manifest
          (singleton always Always)
          (Map.singleton routeName [(always, Just $ Substitution "DEST")])
          mempty
          mempty
          mempty
  let appEnv =
        AppEnv
          repository
          intermediateDir
          manifestFilename
          envFilename
          False
          False

  createDirectories $ repository </> intermediateDir
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile source "source contents"
  writeFile intermediate "managed contents"
  writeFile destinationPath "managed contents"

  withEnvVar "DEST" (Just destinationPath) $
    action appEnv source intermediate destinationPath


withIgnoredDestination
  :: (AppEnv -> OsPath -> OsPath -> OsPath -> IO a)
  -> IO a
withIgnoredDestination action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  routeName <- encodeFS "managed-directory"
  filename <- encodeFS "ignored-file"
  destinationDir <- encodeFS "destination"
  let repository = tmpDir </> sourceDir
  let intermediate = repository </> intermediateDir </> routeName </> filename
  let source = repository </> routeName </> filename
  let destinationRoot = tmpDir </> destinationDir
  let destination = destinationRoot </> filename
  let Right always = parseMonikerName "always"
  let manifest' =
        manifest
          (singleton always Always)
          mempty
          ( Map.singleton
              routeName
              [(always, Just $ Substitution "DEST_DIR")]
          )
          (Map.singleton routeName ["*"])
          mempty
  let appEnv =
        AppEnv
          repository
          intermediateDir
          manifestFilename
          envFilename
          False
          False

  createDirectories $ repository </> intermediateDir
  createDirectories destinationRoot
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile destination "ignored contents"

  withEnvVar "DEST_DIR" (Just destinationRoot) $
    action appEnv source intermediate destination


withEnvVar :: String -> Maybe OsPath -> IO a -> IO a
withEnvVar name value action = do
  oldValue <- lookupEnv name
  bracket_
    (setOrUnset value)
    (maybe (unsetEnv name) (setEnv name) oldValue)
    action
 where
  setOrUnset Nothing = unsetEnv name
  setOrUnset (Just value') = decodePath value' >>= setEnv name
