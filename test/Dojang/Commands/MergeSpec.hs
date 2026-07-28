{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.MergeSpec (spec) where

import Control.Exception (bracket_)
import Control.Monad.Except (throwError)
import Data.ByteString (ByteString)
import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.HashMap.Strict (singleton)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as Text
import System.Directory.OsPath qualified as OsDirectory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Info (os)
import System.OsPath
  ( OsPath
  , decodeFS
  , encodeFS
  , joinPath
  , takeDirectory
  , takeFileName
  , (</>)
  )
import Test.Hspec (Spec, describe, it, sequential)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldReturn
  , shouldThrow
  )
import Prelude hiding (readFile, writeFile)

import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec.Hedgehog (evalIO, forAll, hedgehog, (===))

import Dojang.App
  ( App
  , AppEnv (..)
  , liftApp
  , prepareMachineState
  , runAppWithoutLogging
  )
import Dojang.CommandEffect
  ( MonadCommandEffect (abortCommand)
  , ProcessRequest (..)
  , ProcessResult (..)
  )
import Dojang.Commands.Merge
  ( defaultMergeDriverConfigPath
  , makeMergeDriverProcessRequest
  , mergeWithDriverRunner
  , mergeWithDriverRunnerAndPublisher
  , mergeWithDriverRunnerAndPublisherAndPreparer
  , persistMergedTarget
  )
import Dojang.ExitCodes
  ( conflictError
  , externalProgramNonZeroExit
  , fileNotRoutedError
  , fileWriteError
  , machineStateError
  )
import Dojang.MonadFileSystem
  ( MonadFileSystem (..)
  , dryRunIO
  )
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression (FilePathExpression (Substitution))
import Dojang.Types.FileRoute
  ( FileRoute (..)
  , RouteKind (SymlinkRoute)
  , RouteMode (DefaultMode, ReadOnly)
  , RouteTarget (..)
  )
import Dojang.Types.MachineState
  ( MachineState (targetRecords)
  , readMachineId
  , readRepositoryState
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (updatedBy)
  , SynchronizationCommand (Merged)
  )
import Dojang.Types.Manifest
  ( Manifest (fileRoutes, repositoryId)
  , manifest
  )
import Dojang.Types.Merge (mergeWorkspaceRepositoryRoot)
import Dojang.Types.MergeDriver (makeMergeDriverSpec)
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.RepositoryId (RepositoryId, parseRepositoryId)
import Dojang.Types.RouteMetadata (PortableMode (writable))


data Fixture = Fixture
  { fixtureEnv :: AppEnv
  , fixtureRepositoryId :: RepositoryId
  , fixtureManifest :: Manifest
  , fixtureRetargetedManifest :: Manifest
  , fixtureReadOnlyManifest :: Manifest
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

    it "retries a merge whose authoritative replicas were already committed" $
      hedgehog $ do
        mergedText <-
          forAll $ Gen.text (Range.linear 1 80) Gen.alphaNum
        let merged = Text.encodeUtf8 mergedText
        replicas <- evalIO $ withFixture $ \fixture -> do
          writeFile fixture.sourcePath merged
          writeFile fixture.destinationPath merged
          mergeWith fixture (error "partial recovery ran a driver")
            `shouldReturn` ExitSuccess
          readReplicas fixture
        replicas === replicate 3 merged

    it "rejects a route retargeted while the driver is running" $
      withFixture $ \fixture -> do
        retargetedName <- encodeFS "retargeted-destination"
        let retargeted =
              takeDirectory fixture.destinationPath
                </> retargetedName
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath "merged"
              writeManifestFile
                fixture.fixtureRetargetedManifest
                ( fixture.fixtureEnv.sourceDirectory
                    </> fixture.fixtureEnv.manifestFile
                )
              return $ ProcessCompleted ExitSuccess "" ""
        writeFile retargeted "destination"
        withEnvVars [("RETARGET", Just retargeted)] $
          mergeWith fixture runner `shouldThrow` (== conflictError)
        readReplicas fixture
          `shouldReturn` ["source", "base", "destination"]

    it "rejects a repository identity changed while the driver is running" $
      withFixture $ \fixture -> do
        let Right changedRepositoryId =
              parseRepositoryId "123e4567-e89b-42d3-a456-426614174001"
            changedManifest =
              fixture.fixtureManifest
                { repositoryId = Just changedRepositoryId
                }
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath "merged"
              writeManifestFile
                changedManifest
                ( fixture.fixtureEnv.sourceDirectory
                    </> fixture.fixtureEnv.manifestFile
                )
              return $ ProcessCompleted ExitSuccess "" ""
        mergeWith fixture runner `shouldThrow` (== conflictError)
        readReplicas fixture
          `shouldReturn` ["source", "base", "destination"]
        Right (Just machineId) <-
          readMachineId fixture.fixtureEnv.stateDirectory
        readRepositoryState
          fixture.fixtureEnv.stateDirectory
          changedRepositoryId
          machineId
          `shouldReturn` Right Nothing

    it "retries a merge whose baseline mode update was interrupted" $
      withFixture $ \fixture -> do
        let merged = "merged"
            manifestPath =
              fixture.fixtureEnv.sourceDirectory
                </> fixture.fixtureEnv.manifestFile
        writeManifestFile fixture.fixtureReadOnlyManifest manifestPath
        writeFile fixture.sourcePath merged
        writeFile fixture.basePath merged
        writeFile fixture.destinationPath merged
        setPortableMode fixture.destinationPath 0o444
        setPortableMode fixture.basePath 0o644
        mergeWith fixture (error "mode recovery ran a driver")
          `shouldReturn` ExitSuccess
        mode <- getPortableMode fixture.basePath
        mode.writable `shouldBe` False

    it "restores destination mode while recovering stale baseline content" $
      withFixture $ \fixture -> do
        let merged = "merged"
            manifestPath =
              fixture.fixtureEnv.sourceDirectory
                </> fixture.fixtureEnv.manifestFile
        writeManifestFile fixture.fixtureReadOnlyManifest manifestPath
        writeFile fixture.sourcePath merged
        writeFile fixture.destinationPath merged
        setPortableMode fixture.destinationPath 0o644
        setPortableMode fixture.basePath 0o644
        mergeWith fixture (error "mode recovery ran a driver")
          `shouldReturn` ExitSuccess
        destinationMode <- getPortableMode fixture.destinationPath
        baseMode <- getPortableMode fixture.basePath
        (destinationMode.writable, baseMode.writable)
          `shouldBe` (False, False)

    it "skips unsupported recovery candidates when merging other conflicts" $
      withFixture $ \fixture -> do
        treeName <- encodeFS "tree"
        childName <- encodeFS "child"
        treeDestinationName <- encodeFS "tree-destination"
        intermediateName <- encodeFS ".dojang"
        let treeDestination =
              takeDirectory fixture.destinationPath
                </> treeDestinationName
            sourceChild =
              fixture.fixtureEnv.sourceDirectory
                </> treeName
                </> childName
            baseChild =
              fixture.fixtureEnv.sourceDirectory
                </> intermediateName
                </> treeName
                </> childName
            destinationChild = treeDestination </> childName
            Right always = parseMonikerName "always"
            manifest' =
              ( manifest
                  (singleton always Always)
                  ( Map.singleton
                      (takeFileName fixture.sourcePath)
                      [(always, Just $ Substitution "DEST")]
                  )
                  (Map.singleton treeName [(always, Just $ Substitution "TREE_DEST")])
                  mempty
                  mempty
              )
                { repositoryId = Just fixture.fixtureRepositoryId
                }
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath "merged"
              return $ ProcessCompleted ExitSuccess "" ""
        createDirectories $ takeDirectory sourceChild
        createDirectories $ takeDirectory baseChild
        createDirectories $ takeDirectory destinationChild
        writeFile sourceChild "same"
        writeFile baseChild "old"
        writeFile destinationChild "same"
        writeManifestFile
          manifest'
          ( fixture.fixtureEnv.sourceDirectory
              </> fixture.fixtureEnv.manifestFile
          )
        withEnvVars [("TREE_DEST", Just treeDestination)] $
          mergeWith fixture runner `shouldReturn` ExitSuccess
        readReplicas fixture
          `shouldReturn` replicate 3 "merged"
        readFile baseChild `shouldReturn` "old"

    it "retries target publication after replicas have converged" $
      withFixture $ \fixture -> do
        let merged = "merged"
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath merged
              return $ ProcessCompleted ExitSuccess "" ""
            failPublication _ _ _ = abortCommand machineStateError
        ( runAppWithoutLogging fixture.fixtureEnv $
            mergeWithDriverRunnerAndPublisher
              failPublication
              runner
              Nothing
              (Just fixture.fixtureConfigPath)
              []
          )
          `shouldThrow` (== machineStateError)
        readReplicas fixture `shouldReturn` replicate 3 merged
        removeFile fixture.fixtureConfigPath
        mergeWith fixture (error "publication retry ran a driver")
          `shouldReturn` ExitSuccess
        Right (Just machineId) <-
          readMachineId fixture.fixtureEnv.stateDirectory
        Right (Just state) <-
          readRepositoryState
            fixture.fixtureEnv.stateDirectory
            fixture.fixtureRepositoryId
            machineId
        ((.updatedBy) <$> Map.elems state.targetRecords)
          `shouldBe` [Merged]
        ( runAppWithoutLogging fixture.fixtureEnv $
            mergeWithDriverRunnerAndPublisher
              (\_ _ _ -> error "cleaned publication retried")
              (error "cleaned publication ran a driver")
              Nothing
              (Just fixture.fixtureConfigPath)
              []
          )
          `shouldReturn` ExitSuccess

    it "repairs destination mode before retrying target publication" $
      withFixture $ \fixture -> do
        let merged = "base"
            manifestPath =
              fixture.fixtureEnv.sourceDirectory
                </> fixture.fixtureEnv.manifestFile
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath merged
              return $ ProcessCompleted ExitSuccess "" ""
            failPublication _ _ _ = abortCommand machineStateError
        writeManifestFile fixture.fixtureReadOnlyManifest manifestPath
        ( runAppWithoutLogging fixture.fixtureEnv $
            mergeWithDriverRunnerAndPublisher
              failPublication
              runner
              Nothing
              (Just fixture.fixtureConfigPath)
              []
          )
          `shouldThrow` (== machineStateError)
        readReplicas fixture `shouldReturn` replicate 3 merged
        setPortableMode fixture.destinationPath 0o644
        removeFile fixture.fixtureConfigPath
        mergeWith fixture (error "publication recovery ran a driver")
          `shouldReturn` ExitSuccess
        destinationMode <- getPortableMode fixture.destinationPath
        baseMode <- getPortableMode fixture.basePath
        (destinationMode.writable, baseMode.writable)
          `shouldBe` (False, False)
        pendingPublicationCount fixture `shouldReturn` 0

    it "retains publication when any replica loses convergence" $
      hedgehog $ do
        changedReplica <- forAll $ Gen.int $ Range.linear 0 2
        concurrentText <-
          forAll $
            Gen.filter
              (/= "merged")
              (Gen.text (Range.linear 1 80) Gen.alphaNum)
        let merged = "merged"
            concurrent = Text.encodeUtf8 concurrentText
        evalIO $ withFixture $ \fixture -> do
          let replicaPaths =
                [ fixture.sourcePath
                , fixture.basePath
                , fixture.destinationPath
                ]
              changedPath = replicaPaths !! changedReplica
              runner :: ProcessRequest -> App IO ProcessResult
              runner request = do
                resultPath <- encodePath $ last request.arguments
                writeFile resultPath merged
                return $ ProcessCompleted ExitSuccess "" ""
              publishAfterDivergence ctx machineState managed = do
                writeFile changedPath concurrent
                persistMergedTarget ctx machineState managed
              expectedReplicas =
                take changedReplica (replicate 3 merged)
                  <> [concurrent]
                  <> drop (changedReplica + 1) (replicate 3 merged)
          ( runAppWithoutLogging fixture.fixtureEnv $
              mergeWithDriverRunnerAndPublisher
                publishAfterDivergence
                runner
                Nothing
                (Just fixture.fixtureConfigPath)
                []
            )
            `shouldThrow` (== conflictError)
          readReplicas fixture `shouldReturn` expectedReplicas
          pendingPublicationCount fixture `shouldReturn` 1

    it "maps replica write failures to the file-write exit code" $
      if os == "mingw32"
        then return ()
        else withFixture $ \fixture -> do
          let runner :: ProcessRequest -> App IO ProcessResult
              runner request = do
                resultPath <- encodePath $ last request.arguments
                writeFile resultPath "merged"
                setPortableMode
                  fixture.fixtureEnv.sourceDirectory
                  0o500
                return $ ProcessCompleted ExitSuccess "" ""
              restore =
                setPortableMode fixture.fixtureEnv.sourceDirectory 0o700
          bracket_
            (return ())
            restore
            (mergeWith fixture runner `shouldThrow` (== fileWriteError))

    it "maps invocation-root failures to the file-write exit code" $
      if os == "mingw32"
        then return ()
        else withFixture $ \fixture -> do
          workspaceName <- encodeFS "merge-workspaces"
          let workspaceRoot =
                fixture.fixtureEnv.stateDirectory </> workspaceName
              restore = setPortableMode workspaceRoot 0o700
          _ <-
            runAppWithoutLogging fixture.fixtureEnv $
              prepareMachineState fixture.fixtureManifest
          createDirectories workspaceRoot
          setPortableMode workspaceRoot 0o500
          bracket_
            (return ())
            restore
            ( mergeWith fixture (error "invocation failure ran a driver")
                `shouldThrow` (== fileWriteError)
            )

    it "maps workspace preparation failures to the file-write exit code" $
      withFixture $ \fixture -> do
        let failPreparation root _ _ _ = do
              createPrivateDirectory root
              throwError $ userError "injected workspace preparation failure"
        ( runAppWithoutLogging fixture.fixtureEnv $
            mergeWithDriverRunnerAndPublisherAndPreparer
              persistMergedTarget
              failPreparation
              (error "preparation failure ran a driver")
              Nothing
              (Just fixture.fixtureConfigPath)
              []
          )
          `shouldThrow` (== fileWriteError)

    it "maps unreadable inputs to the conflict exit code" $
      if os == "mingw32"
        then return ()
        else withFixture $ \fixture -> do
          expectedReplicas <- readReplicas fixture
          bracket_
            (setPortableMode fixture.sourcePath 0o000)
            (setPortableMode fixture.sourcePath 0o600)
            ( mergeWith fixture (error "input rejection ran a driver")
                `shouldThrow` (== conflictError)
            )
          readReplicas fixture `shouldReturn` expectedReplicas

    it "maps final invocation cleanup failures to the file-write exit code" $
      if os == "mingw32"
        then return ()
        else withFixture $ \fixture -> do
          invocationRootRef <- newIORef Nothing
          let runner :: ProcessRequest -> App IO ProcessResult
              runner request = do
                let Just workspace = request.workingDirectory
                workspacePath <- encodePath workspace
                liftApp $
                  writeIORef invocationRootRef $
                    Just $
                      takeDirectory workspacePath
                resultPath <- encodePath $ last request.arguments
                writeFile resultPath "merged"
                return $ ProcessCompleted ExitSuccess "" ""
              publisher context machineState managed = do
                persistMergedTarget context machineState managed
                invocationRoot <-
                  liftApp $
                    readIORef invocationRootRef
                      >>= maybe (fail "driver did not run") return
                setPortableMode (takeDirectory invocationRoot) 0o500
              restore = do
                Just invocationRoot <- readIORef invocationRootRef
                setPortableMode (takeDirectory invocationRoot) 0o700
          bracket_
            (return ())
            restore
            ( ( runAppWithoutLogging fixture.fixtureEnv $
                  mergeWithDriverRunnerAndPublisher
                    publisher
                    runner
                    Nothing
                    (Just fixture.fixtureConfigPath)
                    []
              )
                `shouldThrow` (== fileWriteError)
            )
          readReplicas fixture `shouldReturn` replicate 3 "merged"

    it "does not traverse driver-created retained workspace subtrees" $
      if os == "mingw32"
        then return ()
        else withFixture $ \fixture -> do
          privateName <- encodeFS "driver-private"
          privatePathRef <- newIORef Nothing
          let unresolvedRunner :: ProcessRequest -> App IO ProcessResult
              unresolvedRunner request = do
                let Just workspace = request.workingDirectory
                workspacePath <- encodePath workspace
                let privatePath = workspacePath </> privateName
                createPrivateDirectory privatePath
                setPortableMode privatePath 0o000
                liftApp $ writeIORef privatePathRef $ Just privatePath
                return $ ProcessCompleted (ExitFailure 1) "" ""
              resolvedRunner :: ProcessRequest -> App IO ProcessResult
              resolvedRunner request = do
                resultPath <- encodePath $ last request.arguments
                writeFile resultPath "merged"
                return $ ProcessCompleted ExitSuccess "" ""
          mergeWith fixture unresolvedRunner
            `shouldThrow` (== conflictError)
          Just privatePath <- readIORef privatePathRef
          bracket_
            (return ())
            (setPortableMode privatePath 0o700)
            (mergeWith fixture resolvedRunner `shouldReturn` ExitSuccess)
          readReplicas fixture `shouldReturn` replicate 3 "merged"

    it "maps an unreadable successful result to the driver exit code" $
      if os == "mingw32"
        then return ()
        else withFixture $ \fixture -> do
          let runner :: ProcessRequest -> App IO ProcessResult
              runner request = do
                resultPath <- encodePath $ last request.arguments
                writeFile resultPath "merged"
                setPortableMode resultPath 0o000
                return $ ProcessCompleted ExitSuccess "" ""
          mergeWith fixture runner
            `shouldThrow` (== externalProgramNonZeroExit)
          readReplicas fixture
            `shouldReturn` ["source", "base", "destination"]

    it "maps inaccessible result metadata to the driver exit code" $
      if os == "mingw32"
        then return ()
        else withFixture $ \fixture -> do
          workspaceRef <- newIORef Nothing
          let runner :: ProcessRequest -> App IO ProcessResult
              runner request = do
                let Just workspace = request.workingDirectory
                workspacePath <- encodePath workspace
                liftApp $ writeIORef workspaceRef $ Just workspacePath
                resultPath <- encodePath $ last request.arguments
                writeFile resultPath "merged"
                setPortableMode workspacePath 0o000
                return $ ProcessCompleted ExitSuccess "" ""
              restore = do
                Just workspace <- readIORef workspaceRef
                setPortableMode workspace 0o700
          bracket_
            (return ())
            restore
            ( mergeWith fixture runner
                `shouldThrow` (== externalProgramNonZeroExit)
            )
          readReplicas fixture
            `shouldReturn` ["source", "base", "destination"]

    it "skips a stale publication marker after route policy changes" $
      withFixture $ \fixture -> do
        let runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath "merged"
              return $ ProcessCompleted ExitSuccess "" ""
            failPublication _ _ _ = abortCommand machineStateError
            changedManifest =
              fixture.fixtureReadOnlyManifest
                { fileRoutes =
                    fmap
                      ( \route ->
                          route
                            { predicates =
                                fmap
                                  ( fmap $
                                      fmap
                                        ( \target ->
                                            target
                                              { kind = SymlinkRoute
                                              , mode = DefaultMode
                                              }
                                        )
                                  )
                                  route.predicates
                            }
                      )
                      fixture.fixtureReadOnlyManifest.fileRoutes
                }
        ( runAppWithoutLogging fixture.fixtureEnv $
            mergeWithDriverRunnerAndPublisher
              failPublication
              runner
              Nothing
              (Just fixture.fixtureConfigPath)
              []
          )
          `shouldThrow` (== machineStateError)
        writeManifestFile
          changedManifest
          ( fixture.fixtureEnv.sourceDirectory
              </> fixture.fixtureEnv.manifestFile
          )
        removeFile fixture.fixtureConfigPath
        mergeWith fixture (error "stale publication ran a driver")
          `shouldReturn` ExitSuccess

    it "matches explicit selectors using native path identity" $
      withFixture $ \fixture -> do
        rendered <- decodePath fixture.sourcePath
        selected <- encodeFS $ fmap swapCase rendered
        let runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath "merged"
              return $ ProcessCompleted ExitSuccess "" ""
            runSelected =
              runAppWithoutLogging fixture.fixtureEnv $
                mergeWithDriverRunner
                  runner
                  Nothing
                  (Just fixture.fixtureConfigPath)
                  [selected]
        if os == "mingw32"
          then runSelected `shouldReturn` ExitSuccess
          else runSelected `shouldThrow` (== fileNotRoutedError)

    it "matches relative source selectors for a relative repository" $
      hedgehog $ do
        dotDepth <- forAll $ Gen.int (Range.linear 0 4)
        outcome <- evalIO $ withFixture $ \fixture -> do
          dot <- encodeFS "."
          let repository = takeDirectory fixture.sourcePath
              selected =
                joinPath $
                  replicate dotDepth dot
                    <> [takeFileName fixture.sourcePath]
              relativeEnv =
                fixture.fixtureEnv{sourceDirectory = dot}
              runner :: ProcessRequest -> App IO ProcessResult
              runner request = do
                resultPath <- encodePath $ last request.arguments
                writeFile resultPath "merged"
                return $ ProcessCompleted ExitSuccess "" ""
          result <-
            OsDirectory.withCurrentDirectory repository $
              runAppWithoutLogging relativeEnv $
                mergeWithDriverRunner
                  runner
                  Nothing
                  (Just fixture.fixtureConfigPath)
                  [selected]
          replicas <- readReplicas fixture
          return (result, replicas)
        outcome === (ExitSuccess, replicate 3 "merged")

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
      retargetedManifest =
        ( manifest
            (singleton always Always)
            (Map.singleton routeName [(always, Just $ Substitution "RETARGET")])
            mempty
            mempty
            mempty
        )
          { repositoryId = Just repositoryId
          }
      readOnlyManifest =
        manifest'
          { fileRoutes =
              Map.adjust
                ( \route ->
                    route
                      { predicates =
                          fmap
                            (fmap $ fmap (\target -> target{mode = ReadOnly}))
                            route.predicates
                      }
                )
                routeName
                manifest'.fileRoutes
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
        Fixture
          appEnv
          repositoryId
          manifest'
          retargetedManifest
          readOnlyManifest
          configPath
          source
          base
          destination
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


swapCase :: Char -> Char
swapCase character
  | isLower character = toUpper character
  | isUpper character = toLower character
  | otherwise = character


readReplicas :: Fixture -> IO [ByteString]
readReplicas fixture =
  mapM
    readFile
    [fixture.sourcePath, fixture.basePath, fixture.destinationPath]


pendingPublicationCount :: Fixture -> IO Int
pendingPublicationCount fixture = do
  root <-
    mergeWorkspaceRepositoryRoot
      fixture.fixtureEnv.stateDirectory
      fixture.fixtureRepositoryId
  rootExists <- isDirectory root
  if not rootExists
    then return 0
    else do
      entries <- listDirectoryRecursively root []
      names <- mapM (decodeFS . takeFileName . snd) entries
      return $ length $ filter ("pending-" `isPrefixOf`) names


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
