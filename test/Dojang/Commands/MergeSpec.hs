{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.MergeSpec (spec) where

import Control.Exception (bracket_)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (singleton)
import Data.Map.Strict qualified as Map
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.OsPath (OsPath, encodeFS, (</>))
import Test.Hspec (Spec, describe, it, sequential)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldReturn
  , shouldThrow
  )
import Prelude hiding (readFile, writeFile)

import Hedgehog.Gen qualified as Gen
import Test.Hspec.Hedgehog (evalIO, forAll, hedgehog, (===))

import Dojang.App (App, AppEnv (..), runAppWithoutLogging)
import Dojang.CommandEffect
  ( ProcessRequest (..)
  , ProcessResult (..)
  )
import Dojang.Commands.Merge
  ( defaultMergeDriverConfigPath
  , makeMergeDriverProcessRequest
  , mergeWithDriverRunner
  )
import Dojang.ExitCodes (conflictError)
import Dojang.MonadFileSystem
  ( MonadFileSystem (..)
  , dryRunIO
  )
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression (FilePathExpression (Substitution))
import Dojang.Types.MachineState
  ( MachineState (targetRecords)
  , readMachineId
  , readRepositoryState
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (updatedBy)
  , SynchronizationCommand (Merged)
  )
import Dojang.Types.Manifest (Manifest (repositoryId), manifest)
import Dojang.Types.MergeDriver (makeMergeDriverSpec)
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.RepositoryId (RepositoryId, parseRepositoryId)


data Fixture = Fixture
  { fixtureEnv :: AppEnv
  , fixtureRepositoryId :: RepositoryId
  , fixtureConfigPath :: OsPath
  , sourcePath :: OsPath
  , basePath :: OsPath
  , destinationPath :: OsPath
  }


spec :: Spec
spec = sequential $ do
  describe "mergeWithDriverRunner" $ do
    it "commits a resolved result and records a merged target" $
      withFixture $ \fixture -> do
        let merged = "merged result"
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath merged
              return $ ProcessCompleted ExitSuccess "" ""
        mergeWith fixture runner `shouldReturn` ExitSuccess
        readReplicas fixture `shouldReturn` replicate 3 merged
        Right (Just machineId) <-
          readMachineId fixture.fixtureEnv.stateDirectory
        Right (Just state) <-
          readRepositoryState
            fixture.fixtureEnv.stateDirectory
            fixture.fixtureRepositoryId
            machineId
        ((.updatedBy) <$> Map.elems state.targetRecords)
          `shouldBe` [Merged]

    it "does not run a driver or mutate replicas in dry-run mode" $
      withFixture $ \fixture -> do
        let dryRunEnv = fixture.fixtureEnv{dryRun = True}
            runner _ = error "dry-run started a merge driver"
        result <-
          dryRunIO $
            runAppWithoutLogging dryRunEnv $
              mergeWithDriverRunner
                runner
                Nothing
                (Just fixture.fixtureConfigPath)
                []
        result `shouldBe` ExitSuccess
        readReplicas fixture
          `shouldReturn` ["source", "base", "destination"]
        readMachineId fixture.fixtureEnv.stateDirectory
          `shouldReturn` Right Nothing

    it "selects either authoritative endpoint for arbitrary conflicts" $
      hedgehog $ do
        selectSource <- forAll Gen.bool
        outcome <- evalIO $ withFixture $ \fixture -> do
          let selected =
                if selectSource
                  then fixture.sourcePath
                  else fixture.destinationPath
              runner :: ProcessRequest -> App IO ProcessResult
              runner request = do
                resultPath <- encodePath $ last request.arguments
                writeFile resultPath "merged"
                return $ ProcessCompleted ExitSuccess "" ""
          result <-
            runAppWithoutLogging fixture.fixtureEnv $
              mergeWithDriverRunner
                runner
                Nothing
                (Just fixture.fixtureConfigPath)
                [selected]
          replicas <- readReplicas fixture
          return (result, replicas)
        outcome === (ExitSuccess, replicate 3 "merged")

    it "preserves authoritative replicas when the driver is unresolved" $
      withFixture $ \fixture -> do
        let runner _ =
              return $ ProcessCompleted (ExitFailure 1) "" ""
        mergeWith fixture runner `shouldThrow` (== conflictError)
        readReplicas fixture
          `shouldReturn` ["source", "base", "destination"]

  describe "makeMergeDriverProcessRequest" $
    it "expands arguments and exposes only the configured environment" $ do
      let Right driver =
            makeMergeDriverSpec
              [ "driver"
              , "{source}"
              , "{base}"
              , "{destination}"
              , "{result}"
              ]
              ["PATH"]
              (Map.singleton "TOKEN" "fixed")
              [1]
              [2]
          request =
            makeMergeDriverProcessRequest
              "linux"
              [("PATH", "/bin"), ("TOKEN", "parent"), ("SECRET", "hidden")]
              driver
              "/workspace"
              "/workspace/source"
              "/workspace/base"
              "/workspace/destination"
              "/workspace/result"
      request.executable `shouldBe` "driver"
      request.arguments
        `shouldBe` [ "/workspace/source"
                   , "/workspace/base"
                   , "/workspace/destination"
                   , "/workspace/result"
                   ]
      request.workingDirectory `shouldBe` Just "/workspace"
      request.environment
        `shouldBe` Just [("PATH", "/bin"), ("TOKEN", "fixed")]

  describe "defaultMergeDriverConfigPath" $
    it "uses the absolute XDG configuration root on POSIX" $
      withFixture $ \fixture -> do
        configRootName <- encodeFS "xdg"
        dojangName <- encodeFS "dojang"
        fileName <- encodeFS "merge-drivers.toml"
        let configRoot =
              fixture.fixtureEnv.stateDirectory </> configRootName
        withEnvVars [("XDG_CONFIG_HOME", Just configRoot)] $
          ( runAppWithoutLogging fixture.fixtureEnv $
              defaultMergeDriverConfigPath "linux"
          )
            `shouldReturn` configRoot </> dojangName </> fileName


mergeWith
  :: Fixture
  -> (ProcessRequest -> App IO ProcessResult)
  -> IO ExitCode
mergeWith fixture runner =
  runAppWithoutLogging fixture.fixtureEnv $
    mergeWithDriverRunner
      runner
      Nothing
      (Just fixture.fixtureConfigPath)
      []


withFixture :: (Fixture -> IO a) -> IO a
withFixture action = withTempDir $ \root _ -> do
  sourceDirectoryName <- encodeFS "repository"
  intermediateDirectoryName <- encodeFS ".dojang"
  manifestName <- encodeFS "dojang.toml"
  environmentName <- encodeFS "dojang-env.toml"
  stateDirectoryName <- encodeFS "state"
  routeName <- encodeFS "managed-file"
  destinationName <- encodeFS "destination"
  configName <- encodeFS "merge-drivers.toml"
  homeName <- encodeFS "home"
  let repository = root </> sourceDirectoryName
      source = repository </> routeName
      base = repository </> intermediateDirectoryName </> routeName
      destination = root </> destinationName
      configPath = root </> configName
      home = root </> homeName
      Right always = parseMonikerName "always"
      Right repositoryId =
        parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      manifest' =
        ( manifest
            (singleton always Always)
            (Map.singleton routeName [(always, Just $ Substitution "DEST")])
            mempty
            mempty
            mempty
        )
          { repositoryId = Just repositoryId
          }
      appEnv =
        AppEnv
          repository
          False
          (Just intermediateDirectoryName)
          (root </> stateDirectoryName)
          manifestName
          environmentName
          False
          False
      fixture =
        Fixture appEnv repositoryId configPath source base destination
  createDirectories $ repository </> intermediateDirectoryName
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestName
  writeFile configPath mergeDriverConfig
  writeFile source "source"
  writeFile base "base"
  writeFile destination "destination"
  withEnvVars
    [ ("DEST", Just destination)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action fixture


readReplicas :: Fixture -> IO [ByteString]
readReplicas fixture =
  mapM
    readFile
    [fixture.sourcePath, fixture.basePath, fixture.destinationPath]


mergeDriverConfig :: ByteString
mergeDriverConfig =
  "default-driver = \"test\"\n"
    <> "\n"
    <> "[merge-drivers.test]\n"
    <> "command = [\"test-driver\", \"{source}\", \"{base}\", "
    <> "\"{destination}\", \"{result}\"]\n"
    <> "unresolved-exit-codes = [1]\n"
    <> "canceled-exit-codes = [2]\n"


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
