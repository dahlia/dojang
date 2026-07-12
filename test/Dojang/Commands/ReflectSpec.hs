{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.ReflectSpec (spec) where

import Control.Exception (bracket_)
import Data.ByteString (ByteString)
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
withDeletedDestination =
  withManagedFile "managed contents" "managed contents" Nothing


withSourceOnlyChange :: (AppEnv -> OsPath -> OsPath -> OsPath -> IO a) -> IO a
withSourceOnlyChange =
  withManagedFile
    "source contents"
    "managed contents"
    (Just "managed contents")


withManagedFile
  :: ByteString
  -> ByteString
  -> Maybe ByteString
  -> (AppEnv -> OsPath -> OsPath -> OsPath -> IO a)
  -> IO a
withManagedFile sourceContents intermediateContents destinationContents action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "managed-file"
    destination <- encodeFS "destination"
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

    writeManifestFile manifest' manifestPath
    writeFile source sourceContents
    writeFile intermediate intermediateContents
    maybe (return ()) (writeFile destinationPath) destinationContents

    withEnvVar "DEST" (Just destinationPath) $
      action appEnv source intermediate destinationPath


withIgnoredDestination
  :: (AppEnv -> OsPath -> OsPath -> OsPath -> IO a)
  -> IO a
withIgnoredDestination action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "managed-directory"
    filename <- encodeFS "ignored-file"
    destinationDir <- encodeFS "destination"
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

    createDirectories destinationRoot
    writeManifestFile manifest' manifestPath
    writeFile destination "ignored contents"

    withEnvVar "DEST_DIR" (Just destinationRoot) $
      action appEnv source intermediate destination


withReflectRepository
  :: (OsPath -> OsPath -> OsPath -> OsPath -> AppEnv -> IO a)
  -> IO a
withReflectRepository action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  let repository = tmpDir </> sourceDir
  let manifestPath = repository </> manifestFilename
  let appEnv =
        AppEnv
          repository
          intermediateDir
          manifestFilename
          envFilename
          False
          False

  createDirectories $ repository </> intermediateDir
  action tmpDir repository intermediateDir manifestPath appEnv


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
