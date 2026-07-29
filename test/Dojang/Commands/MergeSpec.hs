{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.MergeSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket_)
import Control.Exception qualified as Exception
import Control.Monad (filterM, forM_, when)
import Control.Monad.Except (catchError, throwError)
import Data.ByteString (ByteString)
import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.HashMap.Strict (singleton)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Text (pack)
import Data.Text.Encoding qualified as Text
import System.Directory.OsPath qualified as OsDirectory
import System.Environment
  ( getExecutablePath
  , lookupEnv
  , setEnv
  , unsetEnv
  )
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
import Test.Hspec (Spec, describe, it, runIO, sequential, xit)
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
  , mergeWithDriverRunnerAndFinalizationBarrier
  , mergeWithDriverRunnerAndInvocationBarrier
  , mergeWithDriverRunnerAndPublisher
  , mergeWithDriverRunnerAndPublisherAndPreparer
  , mergeWithDriverRunnerAndReplicaBarrier
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
  ( FileType (Directory)
  , MonadFileSystem (..)
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
import Dojang.Types.Hook
  ( Hook (..)
  , HookPolicy (HookAlways)
  , HookType (PostMerge)
  )
import Dojang.Types.MachineState
  ( MachineState (targetRecords)
  , forgetRepositoryStateWith
  , markRepositoryForgetInProgress
  , readMachineId
  , readRepositoryState
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (updatedBy)
  , SynchronizationCommand (Merged)
  )
import Dojang.Types.Manifest
  ( Manifest (fileRoutes, hooks, repositoryId)
  , manifest
  )
import Dojang.Types.Merge
  ( MergeCommitReplica (SourceCommitReplica)
  , mergeWorkspaceRepositoryRoot
  )
import Dojang.Types.MergeDriver (makeMergeDriverSpec)
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.RepositoryId (RepositoryId, parseRepositoryId)
import Dojang.Types.RouteMetadata (PortableMode (writable))

import System.Timeout (timeout)


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
  symlinkAvailable <- runIO $ withTempDir $ \root _ -> do
    targetName <- encodeFS "symlink-target"
    linkName <- encodeFS "symlink"
    let target = root </> targetName
        link = root </> linkName
    ( createDirectory target
        >> createSymbolicLink target link Directory
        >> return True
      )
      `catchError` const (return False)
  let symlinkIt = if symlinkAvailable then it else xit

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

    concurrentForgetSpec
    posixLinkedWorkspaceCreationSpec

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

    it "rejects a route retargeted at the commit boundary" $
      withFixture $ \fixture -> do
        retargetedName <- encodeFS "retargeted-destination"
        let retargeted =
              takeDirectory fixture.destinationPath
                </> retargetedName
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath "merged"
              return $ ProcessCompleted ExitSuccess "" ""
            beforeFinalization =
              writeManifestFile
                fixture.fixtureRetargetedManifest
                ( fixture.fixtureEnv.sourceDirectory
                    </> fixture.fixtureEnv.manifestFile
                )
        writeFile retargeted "destination"
        withEnvVars [("RETARGET", Just retargeted)] $
          ( runAppWithoutLogging fixture.fixtureEnv $
              mergeWithDriverRunnerAndFinalizationBarrier
                beforeFinalization
                runner
                Nothing
                (Just fixture.fixtureConfigPath)
                []
          )
            `shouldThrow` (== conflictError)
        readReplicas fixture
          `shouldReturn` ["source", "base", "destination"]

    it "rejects a route retargeted between replica writes" $
      withFixture $ \fixture -> do
        retargetedName <- encodeFS "retargeted-destination"
        let retargeted =
              takeDirectory fixture.destinationPath
                </> retargetedName
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath "merged"
              return $ ProcessCompleted ExitSuccess "" ""
            afterReplica replica =
              when (replica == SourceCommitReplica) $
                writeManifestFile
                  fixture.fixtureRetargetedManifest
                  ( fixture.fixtureEnv.sourceDirectory
                      </> fixture.fixtureEnv.manifestFile
                  )
        writeFile retargeted "destination"
        withEnvVars [("RETARGET", Just retargeted)] $
          ( runAppWithoutLogging fixture.fixtureEnv $
              mergeWithDriverRunnerAndReplicaBarrier
                afterReplica
                runner
                Nothing
                (Just fixture.fixtureConfigPath)
                []
          )
            `shouldThrow` (== conflictError)
        readReplicas fixture
          `shouldReturn` ["merged", "base", "destination"]
        pendingPublicationCount fixture `shouldReturn` 1

    it "rejects a route retargeted at the recovery boundary" $
      withFixture $ \fixture -> do
        retargetedName <- encodeFS "retargeted-destination"
        let retargeted =
              takeDirectory fixture.destinationPath
                </> retargetedName
            merged = "merged"
            beforeFinalization =
              writeManifestFile
                fixture.fixtureRetargetedManifest
                ( fixture.fixtureEnv.sourceDirectory
                    </> fixture.fixtureEnv.manifestFile
                )
        writeFile fixture.sourcePath merged
        writeFile fixture.destinationPath merged
        writeFile retargeted "destination"
        withEnvVars [("RETARGET", Just retargeted)] $
          ( runAppWithoutLogging fixture.fixtureEnv $
              mergeWithDriverRunnerAndFinalizationBarrier
                beforeFinalization
                (error "partial recovery ran a driver")
                Nothing
                (Just fixture.fixtureConfigPath)
                []
          )
            `shouldThrow` (== conflictError)
        readReplicas fixture
          `shouldReturn` [merged, "base", merged]
        pendingPublicationCount fixture `shouldReturn` 1

    it "maps unreadable policy refresh inputs to the conflict exit code" $
      if os == "mingw32"
        then return ()
        else hedgehog $ do
          width <- forAll $ Gen.int $ Range.linear 1 40
          sourceText <-
            forAll $ Gen.text (Range.singleton width) Gen.alphaNum
          baseText <-
            forAll $
              Gen.filter
                (/= sourceText)
                (Gen.text (Range.singleton width) Gen.alphaNum)
          destinationText <-
            forAll $
              Gen.filter
                ( \value ->
                    value /= sourceText
                      && value /= baseText
                )
                (Gen.text (Range.singleton width) Gen.alphaNum)
          let inputs =
                Text.encodeUtf8
                  <$> [sourceText, baseText, destinationText]
          evalIO $ withFixture $ \fixture -> do
            sequence_ $
              zipWith
                writeFile
                [ fixture.sourcePath
                , fixture.basePath
                , fixture.destinationPath
                ]
                inputs
            let runner :: ProcessRequest -> App IO ProcessResult
                runner request = do
                  resultPath <- encodePath $ last request.arguments
                  writeFile resultPath "merged"
                  setPortableMode fixture.sourcePath 0o000
                  return $ ProcessCompleted ExitSuccess "" ""
            bracket_
              (return ())
              (setPortableMode fixture.sourcePath 0o600)
              (mergeWith fixture runner `shouldThrow` (== conflictError))
            readReplicas fixture `shouldReturn` inputs

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

    it "does not commit replicas after forget is approved" $
      withFixture $ \fixture -> do
        let runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath "merged"
              marked <-
                markRepositoryForgetInProgress
                  fixture.fixtureEnv.stateDirectory
                  fixture.fixtureRepositoryId
              case marked of
                Left err -> throwError $ userError $ show err
                Right () -> return ()
              return $ ProcessCompleted ExitSuccess "" ""
        mergeWith fixture runner `shouldThrow` (== machineStateError)
        readReplicas fixture
          `shouldReturn` ["source", "base", "destination"]
        pendingPublicationCount fixture `shouldReturn` 1

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

    it "rejects stale policy inside pending target publication" $
      withFixture $ \fixture -> do
        retargetedName <- encodeFS "retargeted-destination"
        let retargeted =
              takeDirectory fixture.destinationPath
                </> retargetedName
            merged = "merged"
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath merged
              return $ ProcessCompleted ExitSuccess "" ""
            failPublication _ _ _ = abortCommand machineStateError
            retargetBeforePublication ctx state managed = do
              writeManifestFile
                fixture.fixtureRetargetedManifest
                ( fixture.fixtureEnv.sourceDirectory
                    </> fixture.fixtureEnv.manifestFile
                )
              persistMergedTarget ctx state managed
        writeFile retargeted "destination"
        ( runAppWithoutLogging fixture.fixtureEnv $
            mergeWithDriverRunnerAndPublisher
              failPublication
              runner
              Nothing
              (Just fixture.fixtureConfigPath)
              []
          )
          `shouldThrow` (== machineStateError)
        withEnvVars [("RETARGET", Just retargeted)] $
          ( runAppWithoutLogging fixture.fixtureEnv $
              mergeWithDriverRunnerAndPublisher
                retargetBeforePublication
                (error "publication retry ran a driver")
                Nothing
                (Just fixture.fixtureConfigPath)
                []
          )
            `shouldThrow` (== conflictError)
        readReplicas fixture `shouldReturn` replicate 3 merged
        pendingPublicationCount fixture `shouldReturn` 2
        Right (Just machineId) <-
          readMachineId fixture.fixtureEnv.stateDirectory
        Right (Just state) <-
          readRepositoryState
            fixture.fixtureEnv.stateDirectory
            fixture.fixtureRepositoryId
            machineId
        state.targetRecords `shouldBe` Map.empty

    it "keeps a sibling recovery workspace at its invocation path" $
      withFixture $ \fixture -> do
        unrelatedName <- encodeFS "unrelated-workspace"
        sentinelName <- encodeFS "retained"
        let runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath "merged"
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
        [pendingMarker] <- pendingPublicationPaths fixture
        let workspace = takeDirectory pendingMarker
            invocation = takeDirectory workspace
            repositoryRoot = takeDirectory invocation
            unrelated = invocation </> unrelatedName
            sentinel = unrelated </> sentinelName
        createDirectory unrelated
        writeFile sentinel "retained"
        removeFile fixture.fixtureConfigPath
        mergeWith fixture (error "publication retry ran a driver")
          `shouldReturn` ExitSuccess
        readFile sentinel `shouldReturn` "retained"
        listDirectory invocation `shouldReturn` [unrelatedName]
        rootEntries <- listDirectory repositoryRoot
        decoded <- mapM decodeFS rootEntries
        filter (".dojang-empty-cleanup-" `isPrefixOf`) decoded
          `shouldBe` []

    symlinkIt "preserves pending markers beneath a replaced invocation ancestor" $
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
        [pendingMarker] <- pendingPublicationPaths fixture
        parkedName <- encodeFS "parked-invocation"
        externalName <- encodeFS "external-invocation"
        let workspace = takeDirectory pendingMarker
            invocation = takeDirectory workspace
            repositoryRoot = takeDirectory invocation
            parked = repositoryRoot </> parkedName
            external = repositoryRoot </> externalName
            externalWorkspace = external </> takeFileName workspace
            externalMarker = externalWorkspace </> takeFileName pendingMarker
            publisher context machineState managed = do
              persistMergedTarget context machineState managed
              liftApp $ do
                createDirectories externalWorkspace
                writeFile externalMarker "external"
                renameDirectory invocation parked
                createSymbolicLink external invocation Directory
            restore = do
              linked <- isSymlink invocation
              when linked $ OsDirectory.removeDirectoryLink invocation
              parkedExists <- isDirectory parked
              when parkedExists $ renameDirectory parked invocation
        bracket_
          (return ())
          restore
          ( do
              ( runAppWithoutLogging fixture.fixtureEnv $
                  mergeWithDriverRunnerAndPublisher
                    publisher
                    (error "publication retry ran a driver")
                    Nothing
                    (Just fixture.fixtureConfigPath)
                    []
                )
                `shouldReturn` ExitSuccess
              readFile externalMarker `shouldReturn` "external"
          )

    it "retains publication after a guarded baseline abort" $
      withFixture $ \fixture -> do
        let merged = "merged"
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath merged
              writeFile fixture.basePath merged
              return $ ProcessCompleted ExitSuccess "" ""
        mergeWith fixture runner `shouldThrow` (== conflictError)
        readReplicas fixture
          `shouldReturn` ["source", merged, "destination"]
        pendingPublicationCount fixture `shouldReturn` 1
        writeFile fixture.sourcePath merged
        writeFile fixture.destinationPath merged
        removeFile fixture.fixtureConfigPath
        mergeWith fixture (error "publication retry ran a driver")
          `shouldReturn` ExitSuccess
        pendingPublicationCount fixture `shouldReturn` 0
        Right (Just machineId) <-
          readMachineId fixture.fixtureEnv.stateDirectory
        Right (Just state) <-
          readRepositoryState
            fixture.fixtureEnv.stateDirectory
            fixture.fixtureRepositoryId
            machineId
        ((.updatedBy) <$> Map.elems state.targetRecords)
          `shouldBe` [Merged]

    it "does not publish a target into a recreated generation" $
      withFixture $ \fixture -> do
        let merged = "merged"
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath merged
              return $ ProcessCompleted ExitSuccess "" ""
            publishAfterRecreation ctx stale managed = do
              machineResult <-
                readMachineId fixture.fixtureEnv.stateDirectory
              machineId <- case machineResult of
                Right (Just identifier) -> return identifier
                result ->
                  throwError $
                    userError $
                      "Unexpected machine identity: " <> show result
              forgotten <-
                forgetRepositoryStateWith
                  fixture.fixtureEnv.stateDirectory
                  fixture.fixtureRepositoryId
                  machineId
                  (const $ removeDirectoryRecursively $ takeDirectory fixture.basePath)
              case forgotten of
                Left err -> throwError $ userError $ show err
                Right Nothing ->
                  throwError $ userError "repository state disappeared"
                Right (Just ()) -> return ()
              _ <- prepareMachineState fixture.fixtureManifest
              persistMergedTarget ctx stale managed
        ( runAppWithoutLogging fixture.fixtureEnv $
            mergeWithDriverRunnerAndPublisher
              publishAfterRecreation
              runner
              Nothing
              (Just fixture.fixtureConfigPath)
              []
          )
          `shouldThrow` (== machineStateError)
        Right (Just machineId) <-
          readMachineId fixture.fixtureEnv.stateDirectory
        Right (Just state) <-
          readRepositoryState
            fixture.fixtureEnv.stateDirectory
            fixture.fixtureRepositoryId
            machineId
        state.targetRecords `shouldBe` Map.empty
        pendingPublicationCount fixture `shouldReturn` 1

    it "does not recreate state for post-merge hooks after forget" $
      withFixture $ \fixture -> do
        let merged = "merged"
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath merged
              return $ ProcessCompleted ExitSuccess "" ""
            publishThenForget ctx stale managed = do
              persistMergedTarget ctx stale managed
              machineResult <-
                readMachineId fixture.fixtureEnv.stateDirectory
              machineId <- case machineResult of
                Right (Just identifier) -> return identifier
                result ->
                  throwError $
                    userError $
                      "Unexpected machine identity: " <> show result
              workspaceRoot <-
                mergeWorkspaceRepositoryRoot
                  fixture.fixtureEnv.stateDirectory
                  fixture.fixtureRepositoryId
              forgotten <-
                forgetRepositoryStateWith
                  fixture.fixtureEnv.stateDirectory
                  fixture.fixtureRepositoryId
                  machineId
                  ( const $ do
                      removeDirectoryRecursively $
                        takeDirectory fixture.basePath
                      removeDirectoryRecursively workspaceRoot
                  )
              case forgotten of
                Left err -> throwError $ userError $ show err
                Right Nothing ->
                  throwError $ userError "repository state disappeared"
                Right (Just ()) -> return ()
        ( runAppWithoutLogging fixture.fixtureEnv $
            mergeWithDriverRunnerAndPublisher
              publishThenForget
              runner
              Nothing
              (Just fixture.fixtureConfigPath)
              []
          )
          `shouldThrow` (== machineStateError)
        Right (Just machineId) <-
          readMachineId fixture.fixtureEnv.stateDirectory
        readRepositoryState
          fixture.fixtureEnv.stateDirectory
          fixture.fixtureRepositoryId
          machineId
          `shouldReturn` Right Nothing

    it "reloads removed post-merge hooks after the driver exits" $
      withFixture $ \fixture -> do
        command <- getExecutablePath >>= encodeFS
        markerName <- encodeFS "stale-post-merge-hook-ran"
        let marker = fixture.fixtureEnv.sourceDirectory </> markerName
            manifestPath =
              fixture.fixtureEnv.sourceDirectory
                </> fixture.fixtureEnv.manifestFile
            hook =
              Hook
                { hookId = Nothing
                , policy = HookAlways
                , changeKey = Nothing
                , command = command
                , args =
                    [ "--match"
                    , pack postMergeHookProbePattern
                    , "--seed"
                    , pack postMergeHookProbeSeed
                    ]
                , condition = Always
                , workingDirectory = Nothing
                , ignoreFailure = False
                }
            initialManifest =
              fixture.fixtureManifest
                { hooks = Map.singleton PostMerge [hook]
                }
            replacementManifest =
              fixture.fixtureManifest
                { hooks = Map.empty
                }
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath "merged"
              writeManifestFile replacementManifest manifestPath
              return $ ProcessCompleted ExitSuccess "" ""
        writeManifestFile initialManifest manifestPath
        mergeWith fixture runner `shouldReturn` ExitSuccess
        isFile marker `shouldReturn` False

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

    it "maps final replica read failures to the conflict exit code" $
      if os == "mingw32"
        then return ()
        else withFixture $ \fixture -> do
          let merged = "merged"
              runner :: ProcessRequest -> App IO ProcessResult
              runner request = do
                resultPath <- encodePath $ last request.arguments
                writeFile resultPath merged
                return $ ProcessCompleted ExitSuccess "" ""
              publishWithUnreadableReplica ctx machineState managed = do
                setPortableMode fixture.sourcePath 0o000
                persistMergedTarget ctx machineState managed
          bracket_
            (return ())
            (setPortableMode fixture.sourcePath 0o600)
            ( ( runAppWithoutLogging fixture.fixtureEnv $
                  mergeWithDriverRunnerAndPublisher
                    publishWithUnreadableReplica
                    runner
                    Nothing
                    (Just fixture.fixtureConfigPath)
                    []
              )
                `shouldThrow` (== conflictError)
            )
          readReplicas fixture `shouldReturn` replicate 3 merged
          pendingPublicationCount fixture `shouldReturn` 1

    it "retains publication when a declared mode drifts" $
      hedgehog $ do
        driftDestination <- forAll Gen.bool
        driftMode <-
          forAll $ Gen.element ([0o600, 0o644, 0o700, 0o755] :: [Word])
        evalIO $ withFixture $ \fixture -> do
          let merged = "merged"
              manifestPath =
                fixture.fixtureEnv.sourceDirectory
                  </> fixture.fixtureEnv.manifestFile
              changedPath =
                if driftDestination
                  then fixture.destinationPath
                  else fixture.basePath
              runner :: ProcessRequest -> App IO ProcessResult
              runner request = do
                resultPath <- encodePath $ last request.arguments
                writeFile resultPath merged
                return $ ProcessCompleted ExitSuccess "" ""
              publishAfterModeDrift ctx machineState managed = do
                setPortableMode changedPath driftMode
                persistMergedTarget ctx machineState managed
          writeManifestFile fixture.fixtureReadOnlyManifest manifestPath
          ( runAppWithoutLogging fixture.fixtureEnv $
              mergeWithDriverRunnerAndPublisher
                publishAfterModeDrift
                runner
                Nothing
                (Just fixture.fixtureConfigPath)
                []
            )
            `shouldThrow` (== conflictError)
          pendingPublicationCount fixture `shouldReturn` 1
          Right (Just machineId) <-
            readMachineId fixture.fixtureEnv.stateDirectory
          Right (Just state) <-
            readRepositoryState
              fixture.fixtureEnv.stateDirectory
              fixture.fixtureRepositoryId
              machineId
          state.targetRecords `shouldBe` Map.empty

    it "publishes a converged deletion without checking modes" $
      withFixture $ \fixture -> do
        let merged = "merged"
            manifestPath =
              fixture.fixtureEnv.sourceDirectory
                </> fixture.fixtureEnv.manifestFile
            runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath merged
              return $ ProcessCompleted ExitSuccess "" ""
            publishAfterDeletion ctx machineState managed = do
              forM_
                ( [ fixture.sourcePath
                  , fixture.basePath
                  , fixture.destinationPath
                  ]
                    :: [OsPath]
                )
                ( \path -> do
                    setPortableMode path 0o644
                    removeFile path
                )
              persistMergedTarget ctx machineState managed
        writeManifestFile fixture.fixtureReadOnlyManifest manifestPath
        ( runAppWithoutLogging fixture.fixtureEnv $
            mergeWithDriverRunnerAndPublisher
              publishAfterDeletion
              runner
              Nothing
              (Just fixture.fixtureConfigPath)
              []
          )
          `shouldReturn` ExitSuccess
        pendingPublicationCount fixture `shouldReturn` 0
        Right (Just machineId) <-
          readMachineId fixture.fixtureEnv.stateDirectory
        Right (Just state) <-
          readRepositoryState
            fixture.fixtureEnv.stateDirectory
            fixture.fixtureRepositoryId
            machineId
        state.targetRecords `shouldBe` Map.empty

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

    it "recovers every journal-authenticated commit subset" $
      if os == "mingw32"
        then return ()
        else hedgehog $ do
          width <- forAll $ Gen.int $ Range.linear 1 40
          sourceText <-
            forAll $ Gen.text (Range.singleton width) Gen.alphaNum
          baseText <-
            forAll $
              Gen.filter
                (/= sourceText)
                (Gen.text (Range.singleton width) Gen.alphaNum)
          destinationText <-
            forAll $
              Gen.filter
                ( \value ->
                    value /= sourceText
                      && value /= baseText
                )
                (Gen.text (Range.singleton width) Gen.alphaNum)
          resultPrefix <-
            forAll $ Gen.text (Range.linear 0 40) Gen.alphaNum
          resultSuffix <-
            forAll $ Gen.text (Range.linear 0 40) Gen.alphaNum
          sourceCommitted <- forAll Gen.bool
          baseCommitted <- forAll Gen.bool
          destinationCommitted <- forAll Gen.bool
          let resultText = resultPrefix <> "\n" <> resultSuffix
          let source = Text.encodeUtf8 sourceText
              base = Text.encodeUtf8 baseText
              destination = Text.encodeUtf8 destinationText
              result = Text.encodeUtf8 resultText
          evalIO $ withFixture $ \fixture -> do
            writeFile fixture.sourcePath source
            writeFile fixture.basePath base
            writeFile fixture.destinationPath destination
            let destinationParent = takeDirectory fixture.destinationPath
                runner :: ProcessRequest -> App IO ProcessResult
                runner request = do
                  resultPath <- encodePath $ last request.arguments
                  writeFile resultPath result
                  setPortableMode destinationParent 0o500
                  return $ ProcessCompleted ExitSuccess "" ""
                restore = setPortableMode destinationParent 0o700
            bracket_
              (return ())
              restore
              (mergeWith fixture runner `shouldThrow` (== fileWriteError))
            readReplicas fixture
              `shouldReturn` [result, base, destination]
            pendingPublicationCount fixture `shouldReturn` 1
            writeFile
              fixture.sourcePath
              (if sourceCommitted then result else source)
            writeFile
              fixture.basePath
              (if baseCommitted then result else base)
            writeFile
              fixture.destinationPath
              (if destinationCommitted then result else destination)
            removeFile fixture.fixtureConfigPath
            mergeWith fixture (error "journal recovery ran a driver")
              `shouldReturn` ExitSuccess
            readReplicas fixture `shouldReturn` replicate 3 result
            pendingPublicationCount fixture `shouldReturn` 0

    it "reruns the driver when an accepted-result journal is corrupt" $
      if os == "mingw32"
        then return ()
        else withFixture $ \fixture -> do
          let accepted = "accepted\nresult"
              replacement = "replacement\nresult"
              destinationParent = takeDirectory fixture.destinationPath
              interruptedRunner :: ProcessRequest -> App IO ProcessResult
              interruptedRunner request = do
                resultPath <- encodePath $ last request.arguments
                writeFile resultPath accepted
                setPortableMode destinationParent 0o500
                return $ ProcessCompleted ExitSuccess "" ""
              retryRunner :: ProcessRequest -> App IO ProcessResult
              retryRunner request = do
                resultPath <- encodePath $ last request.arguments
                writeFile resultPath replacement
                return $ ProcessCompleted ExitSuccess "" ""
              restore = setPortableMode destinationParent 0o700
          bracket_
            (return ())
            restore
            ( mergeWith fixture interruptedRunner
                `shouldThrow` (== fileWriteError)
            )
          [pendingMarker] <- pendingPublicationPaths fixture
          writeFile pendingMarker "corrupt"
          mergeWith fixture retryRunner `shouldReturn` ExitSuccess
          readReplicas fixture `shouldReturn` replicate 3 replacement
          pendingPublicationCount fixture `shouldReturn` 0

    it "maps invocation-root failures to the file-write exit code" $
      if os == "mingw32"
        then return ()
        else withFixture $ \fixture -> do
          repositoryWorkspaceRoot <-
            mergeWorkspaceRepositoryRoot
              fixture.fixtureEnv.stateDirectory
              fixture.fixtureRepositoryId
          let workspaceRoot = takeDirectory repositoryWorkspaceRoot
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

    it "maps unreadable correspondence reads to the conflict exit code" $
      if os == "mingw32"
        then return ()
        else withFixture $ \fixture -> do
          writeFile fixture.basePath "parent"
          expectedReplicas <- readReplicas fixture
          bracket_
            (setPortableMode fixture.sourcePath 0o000)
            (setPortableMode fixture.sourcePath 0o600)
            ( mergeWith fixture (error "input rejection ran a driver")
                `shouldThrow` (== conflictError)
            )
          readReplicas fixture `shouldReturn` expectedReplicas

    it "maps unreadable reconciliation reads to the conflict exit code" $
      if os == "mingw32"
        then return ()
        else withFixture $ \fixture -> do
          writeFile fixture.destinationPath "target"
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

    it "retains a workspace replaced before cleanup" $
      withFixture $ \fixture -> do
        workspaceRef <- newIORef Nothing
        sentinelName <- encodeFS "replacement"
        let runner :: ProcessRequest -> App IO ProcessResult
            runner request = do
              let Just workspace = request.workingDirectory
              workspacePath <- encodePath workspace
              liftApp $ writeIORef workspaceRef $ Just workspacePath
              resultPath <- encodePath $ last request.arguments
              writeFile resultPath "merged"
              return $ ProcessCompleted ExitSuccess "" ""
            publisher context machineState managed = do
              persistMergedTarget context machineState managed
              workspace <-
                liftApp $
                  readIORef workspaceRef
                    >>= maybe (fail "driver did not run") return
              removeDirectoryRecursively workspace
              createPrivateDirectory workspace
              writeFile (workspace </> sentinelName) "replacement"
        ( runAppWithoutLogging fixture.fixtureEnv $
            mergeWithDriverRunnerAndPublisher
              publisher
              runner
              Nothing
              (Just fixture.fixtureConfigPath)
              []
          )
          `shouldReturn` ExitSuccess
        workspace <-
          readIORef workspaceRef
            >>= maybe (fail "driver did not run") return
        isFile (workspace </> sentinelName) `shouldReturn` True

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

    it "removed post-merge hook probe" $ do
      event <- lookupEnv "DOJANG_HOOK_EVENT"
      when (event == Just "post-merge") $ do
        repository <- lookupEnv "DOJANG_REPOSITORY"
        case repository of
          Nothing -> fail "DOJANG_REPOSITORY is missing"
          Just path -> do
            repositoryPath <- encodeFS path
            markerName <- encodeFS "stale-post-merge-hook-ran"
            writeFile (repositoryPath </> markerName) ""

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

#ifdef mingw32_HOST_OS
posixLinkedWorkspaceCreationSpec :: Spec
posixLinkedWorkspaceCreationSpec = return ()
#endif


concurrentForgetSpec :: Spec
concurrentForgetSpec =
  it "does not recreate a workspace after concurrent forget" $
    withFixture $ \fixture -> do
      _ <-
        runAppWithoutLogging fixture.fixtureEnv $
          prepareMachineState fixture.fixtureManifest
      Right (Just machineId) <-
        readMachineId fixture.fixtureEnv.stateDirectory
      workspaceRoot <-
        mergeWorkspaceRepositoryRoot
          fixture.fixtureEnv.stateDirectory
          fixture.fixtureRepositoryId
      outcome <- newEmptyMVar
      invocationReady <- newEmptyMVar
      resumeInvocation <- newEmptyMVar
      let runner :: ProcessRequest -> App IO ProcessResult
          runner request = do
            resultPath <- encodePath $ last request.arguments
            writeFile resultPath "merged"
            return $ ProcessCompleted ExitSuccess "" ""
          beforeInvocation =
            liftApp $ putMVar invocationReady () >> takeMVar resumeInvocation
      Exception.bracket_
        (return ())
        (putMVar resumeInvocation ())
        $ do
          _ <-
            forkIO $ do
              result <-
                ( Exception.try $
                    runAppWithoutLogging fixture.fixtureEnv $
                      mergeWithDriverRunnerAndInvocationBarrier
                        beforeInvocation
                        runner
                        Nothing
                        (Just fixture.fixtureConfigPath)
                        []
                )
                  :: IO (Either Exception.SomeException ExitCode)
              putMVar outcome result
          ready <- timeout 30000000 $ takeMVar invocationReady
          ready `shouldBe` Just ()
          let removeTree path = do
                present <- exists path
                when present $ removeDirectoryRecursively path
          forgotten <-
            forgetRepositoryStateWith
              fixture.fixtureEnv.stateDirectory
              fixture.fixtureRepositoryId
              machineId
              (const $ removeTree workspaceRoot)
          forgotten `shouldBe` Right (Just ())
      completed <- timeout 30000000 $ takeMVar outcome
      case completed of
        Just (Left err) ->
          Exception.fromException err `shouldBe` Just machineStateError
        Just (Right result) ->
          fail $ "Merge unexpectedly returned " <> show result <> "."
        Nothing -> fail "Merge did not finish after concurrent forget."
      exists workspaceRoot `shouldReturn` False

#ifndef mingw32_HOST_OS
posixLinkedWorkspaceCreationSpec :: Spec
posixLinkedWorkspaceCreationSpec =
  it "rejects a linked merge-workspace ancestor before creation" $
    withFixture $ \fixture -> do
      _ <-
        runAppWithoutLogging fixture.fixtureEnv $
          prepareMachineState fixture.fixtureManifest
      workspaceRoot <-
        mergeWorkspaceRepositoryRoot
          fixture.fixtureEnv.stateDirectory
          fixture.fixtureRepositoryId
      externalName <- encodeFS "external-merge-workspaces"
      let workspaceStore = takeDirectory workspaceRoot
          externalStore =
            takeDirectory fixture.fixtureEnv.sourceDirectory </> externalName
      createDirectories externalStore
      Exception.bracket
        (OsDirectory.createDirectoryLink externalStore workspaceStore)
        (const $ OsDirectory.removeDirectoryLink workspaceStore)
        $ \_ -> do
          mergeWith fixture (error "linked workspace ran a driver")
            `shouldThrow` (== fileWriteError)
          listDirectory externalStore `shouldReturn` []
#endif


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
  length <$> pendingPublicationPaths fixture


pendingPublicationPaths :: Fixture -> IO [OsPath]
pendingPublicationPaths fixture = do
  root <-
    mergeWorkspaceRepositoryRoot
      fixture.fixtureEnv.stateDirectory
      fixture.fixtureRepositoryId
  rootExists <- isDirectory root
  if not rootExists
    then return []
    else do
      entries <- listDirectoryRecursively root []
      filterM
        ( \entry -> do
            name <- decodeFS $ takeFileName entry
            return $ "pending-" `isPrefixOf` name
        )
        [root </> relative | (_, relative) <- entries]


mergeDriverConfig :: ByteString
mergeDriverConfig =
  "default-driver = \"test\"\n"
    <> "\n"
    <> "[merge-drivers.test]\n"
    <> "command = [\"test-driver\", \"{source}\", \"{base}\", "
    <> "\"{destination}\", \"{result}\"]\n"
    <> "unresolved-exit-codes = [1]\n"
    <> "canceled-exit-codes = [2]\n"


postMergeHookProbePattern :: String
postMergeHookProbePattern =
  "/Dojang.Commands.Merge/mergeWithDriverRunner/removed post-merge hook probe/"


postMergeHookProbeSeed :: String
postMergeHookProbeSeed = "250027"


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
