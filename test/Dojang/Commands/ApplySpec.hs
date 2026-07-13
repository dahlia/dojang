{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.ApplySpec (spec) where

import Control.Exception (bracket_)
import Data.HashMap.Strict (singleton)
import qualified Data.Map.Strict as Map
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath (OsPath, encodeFS, (</>))
import Test.Hspec (Spec, describe, it, sequential)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Prelude hiding (readFile, writeFile)

import Dojang.App (AppEnv (..), runAppWithoutLogging)
import Dojang.Commands.Apply (apply)
import Dojang.MonadFileSystem (MonadFileSystem (..), dryRunIO)
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression (FilePathExpression (Substitution))
import Dojang.Types.Manifest (manifest)
import Dojang.Types.MonikerName (parseMonikerName)


spec :: Spec
spec = sequential $ do
  describe "apply" $ do
    it "does not update destinations outside the selected paths" $
      withTwoManagedFiles $ \appEnv sourceA destinationA destinationB -> do
        (result, destinationAContents, destinationBContents) <- dryRunIO $ do
          result <- runAppWithoutLogging appEnv (apply True [sourceA])
          destinationAContents <- readFile destinationA
          destinationBContents <- readFile destinationB
          return (result, destinationAContents, destinationBContents)
        result `shouldBe` ExitSuccess
        destinationAContents `shouldBe` "source a"
        destinationBContents `shouldBe` "destination b"

    it "keeps tracked files managed when they match an ignore pattern" $
      withTrackedIgnoredFile $ \appEnv intermediate destination -> do
        (result, intermediateContents, destinationContents) <- dryRunIO $ do
          result <- runAppWithoutLogging appEnv (apply False [])
          intermediateContents <- readFile intermediate
          destinationContents <- readFile destination
          return (result, intermediateContents, destinationContents)
        result `shouldBe` ExitSuccess
        intermediateContents `shouldBe` "source contents"
        destinationContents `shouldBe` "source contents"


withTwoManagedFiles
  :: (AppEnv -> OsPath -> OsPath -> OsPath -> IO a)
  -> IO a
withTwoManagedFiles action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  routeA <- encodeFS "managed-a"
  routeB <- encodeFS "managed-b"
  destinationAName <- encodeFS "destination-a"
  destinationBName <- encodeFS "destination-b"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let intermediateA = repository </> intermediateDir </> routeA
  let intermediateB = repository </> intermediateDir </> routeB
  let sourceA = repository </> routeA
  let sourceB = repository </> routeB
  let destinationA = tmpDir </> destinationAName
  let destinationB = tmpDir </> destinationBName
  let home = tmpDir </> homeName
  let Right always = parseMonikerName "always"
  let manifest' =
        manifest
          (singleton always Always)
          ( Map.fromList
              [ (routeA, [(always, Just $ Substitution "DEST_A")])
              , (routeB, [(always, Just $ Substitution "DEST_B")])
              ]
          )
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
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile sourceA "source a"
  writeFile intermediateA "base a"
  writeFile destinationA "base a"
  writeFile sourceB "base b"
  writeFile intermediateB "base b"
  writeFile destinationB "destination b"

  withEnvVars
    [ ("DEST_A", Just destinationA)
    , ("DEST_B", Just destinationB)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv sourceA destinationA destinationB


withTrackedIgnoredFile
  :: (AppEnv -> OsPath -> OsPath -> IO a)
  -> IO a
withTrackedIgnoredFile action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  routeName <- encodeFS "managed-directory"
  filename <- encodeFS "tracked-file"
  destinationDir <- encodeFS "destination"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let sourceRoot = repository </> routeName
  let intermediateRoot = repository </> intermediateDir </> routeName
  let destinationRoot = tmpDir </> destinationDir
  let source = sourceRoot </> filename
  let intermediate = intermediateRoot </> filename
  let destination = destinationRoot </> filename
  let home = tmpDir </> homeName
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

  createDirectories sourceRoot
  createDirectories intermediateRoot
  createDirectories destinationRoot
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile source "source contents"
  writeFile intermediate "base contents"
  writeFile destination "base contents"

  withEnvVars
    [ ("DEST_DIR", Just destinationRoot)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv intermediate destination


withEnvVars :: [(String, Maybe OsPath)] -> IO a -> IO a
withEnvVars [] action = action
withEnvVars ((name, value) : rest) action = do
  oldValue <- lookupEnv name
  bracket_
    (setOrUnset value)
    (maybe (unsetEnv name) (setEnv name) oldValue)
    (withEnvVars rest action)
 where
  setOrUnset Nothing = unsetEnv name
  setOrUnset (Just value') = decodePath value' >>= setEnv name
