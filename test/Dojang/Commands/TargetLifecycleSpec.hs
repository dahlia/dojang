{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.TargetLifecycleSpec (spec) where

import Control.Exception (bracket_)
import Control.Monad.Except (MonadError, catchError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (ReaderT), ask, runReaderT)
import Data.HashMap.Strict (singleton)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Text.Encoding (encodeUtf8)
import System.Directory.OsPath qualified
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath (OsPath, encodeFS, takeDirectory, takeFileName, (</>))
import Test.Hspec (Spec, describe, it, runIO, sequential, xit)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldContain
  , shouldNotBe
  , shouldReturn
  , shouldThrow
  )
import Prelude hiding (readFile, writeFile)

import Dojang.App (AppEnv (..), runAppWithoutLogging)
import Dojang.Commands.Apply (apply)
import Dojang.Commands.Status (defaultStatusOptions, status)
import Dojang.Commands.TargetLifecycle (forget, unmanage)
import Dojang.ExitCodes
  ( accidentalDeletionWarning
  , lifecycleSelectionError
  , machineStateError
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.MonadFileSystem qualified as FileSystem
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression
  ( FilePathExpression (BareComponent, Substitution, SubstitutionWithDefault)
  )
import Dojang.Types.MachineState
  ( MachineState (..)
  , encodeMachineState
  , listRepositoryStates
  , migrationMarkerPath
  , readMachineId
  , readRepositoryState
  , repositoryStateDirectory
  , repositoryStatePath
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (..)
  , OrphanStatus (OrphanUnchanged)
  )
import Dojang.Types.Manifest (Manifest (..), manifest)
import Dojang.Types.Manifest qualified as Manifest
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.Registry
  ( Registry (Registry)
  , registryFilename
  , writeRegistry
  )
import Dojang.Types.RepositoryId (RepositoryId, parseRepositoryId)
import Dojang.Types.TargetTracking (observeOrphanStatus)


spec :: Spec
spec = do
  symlinkAvailable <- runIO $ withTempDir $ \root _ -> do
    targetName <- encodeFS "symlink-target"
    linkName <- encodeFS "symlink-link"
    let target = root </> targetName
    createDirectories target
    ( System.Directory.OsPath.createDirectoryLink target (root </> linkName)
        >> pure True
      )
      `catchError` const (pure False)
  let symlinkIt = if symlinkAvailable then it else xit
  sequential $ do
    describe "unmanage" $ do
      it "rejects active records and leaves all state untouched" $
        withManagedTarget $ \fixture -> do
          runAppWithoutLogging
            fixture.appEnv
            (unmanage (Just fixture.routeName) [] False)
            `shouldThrow` (== lifecycleSelectionError)
          state <- loadState fixture
          Map.size state.targetRecords `shouldBe` 1
          readFile fixture.destination `shouldReturn` "managed"

      it "rejects an active record with a relative current destination" $
        withRelativeManagedTarget $ \fixture -> do
          runAppWithoutLogging
            fixture.appEnv
            (unmanage (Just fixture.routeName) [] False)
            `shouldThrow` (== lifecycleSelectionError)
          state <- loadState fixture
          Map.size state.targetRecords `shouldBe` 1

      it "rejects an active record whose route name needs normalization" $
        withManagedTarget $ \fixture -> do
          rawRouteName <- encodeFS "./config"
          writeFile
            fixture.manifestPath
            "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n[dirs]\n[files]\n\"./config\" = [{ when = \"always\", path = \"$DEST\" }]\n[ignores]\n[monikers]\n"
          runAppWithoutLogging
            fixture.appEnv
            (unmanage (Just rawRouteName) [] False)
            `shouldThrow` (== lifecycleSelectionError)
          state <- loadState fixture
          Map.size state.targetRecords `shouldBe` 1

      it "requires force for modified orphans and never deletes destinations" $
        withManagedTarget $ \fixture -> do
          writeManifestFile fixture.orphanManifest fixture.manifestPath
          writeFile fixture.destination "locally modified"
          runAppWithoutLogging
            fixture.appEnv
            (unmanage (Just fixture.routeName) [] False)
            `shouldThrow` (== accidentalDeletionWarning)
          runAppWithoutLogging
            fixture.appEnv
            (unmanage (Just fixture.routeName) [] True)
            `shouldReturn` ExitSuccess
          state <- loadState fixture
          state.targetRecords `shouldBe` Map.empty
          readFile fixture.destination `shouldReturn` "locally modified"

      it "unmanages a missing orphan selected by destination" $
        withManagedTarget $ \fixture -> do
          writeManifestFile fixture.orphanManifest fixture.manifestPath
          removeFile fixture.destination
          before <- loadState fixture
          target <- case Map.elems before.targetRecords of
            [one] -> return one
            records -> fail $ "Unexpected managed targets: " <> show records
          runAppWithoutLogging
            fixture.appEnv
            (unmanage Nothing [fixture.destination] False)
            `shouldReturn` ExitSuccess
          state <- loadState fixture
          state.targetRecords `shouldBe` Map.empty
          exists fixture.destination `shouldReturn` False
          exists target.snapshotPath `shouldReturn` False
          exists fixture.intermediate `shouldReturn` False

      it "cleans the baseline after a successful managed deletion" $
        withManagedTarget $ \fixture -> do
          before <- loadState fixture
          target <- case Map.elems before.targetRecords of
            [one] -> return one
            records -> fail $ "Unexpected managed targets: " <> show records
          removeFile fixture.source
          runAppWithoutLogging fixture.appEnv (apply True [])
            `shouldReturn` ExitSuccess
          after <- loadState fixture
          after.targetRecords `shouldBe` Map.empty
          exists target.snapshotPath `shouldReturn` False

      it "preserves an orphan baseline when a route destination changes" $
        withManagedTarget $ \fixture -> do
          replacementName <- encodeFS "replacement-destination"
          let replacement = takeDirectory fixture.destination </> replacementName
          writeFile fixture.source "new generation"
          withEnvVars [("DEST", replacement)] $
            runAppWithoutLogging fixture.appEnv (apply True [])
              `shouldReturn` ExitSuccess
          state <- loadState fixture
          let records = Map.elems state.targetRecords
          old <- case find ((== fixture.destination) . (.destinationPath)) records of
            Nothing -> fail "The old destination record was not retained."
            Just target -> return target
          new <- case find ((== replacement) . (.destinationPath)) records of
            Nothing -> fail "The replacement destination record was not created."
            Just target -> return target
          old.snapshotPath `shouldNotBe` new.snapshotPath
          observeOrphanStatus old `shouldReturn` OrphanUnchanged

      it "unmanages a stale generation beside its active replacement" $
        withManagedTarget $ \fixture -> do
          let Right always = parseMonikerName "always"
          let changedManifest =
                ( manifest
                    (singleton always Always)
                    ( Map.singleton
                        fixture.routeName
                        [
                          ( always
                          , Just $
                              SubstitutionWithDefault
                                "DEST"
                                (BareComponent "fallback")
                          )
                        ]
                    )
                    mempty
                    mempty
                    mempty
                )
                  { Manifest.repositoryId = Just fixture.repositoryId
                  }
          writeManifestFile changedManifest fixture.manifestPath
          runAppWithoutLogging fixture.appEnv (apply True [])
            `shouldReturn` ExitSuccess
          before <- loadState fixture
          Map.size before.targetRecords `shouldBe` 2
          runAppWithoutLogging
            fixture.appEnv
            (unmanage (Just fixture.routeName) [] False)
            `shouldReturn` ExitSuccess
          after <- loadState fixture
          case Map.elems after.targetRecords of
            [active] ->
              active.routeDefinition
                `shouldBe` "${DEST:-fallback}"
            records -> fail $ "Unexpected managed targets: " <> show records
          readFile fixture.destination `shouldReturn` "managed"

      it "preserves the intermediate entry used by a current route" $
        withManagedTarget $ \fixture -> do
          replacementName <- encodeFS "replacement-destination"
          let replacement = takeDirectory fixture.destination </> replacementName
          withEnvVars [("DEST", replacement)] $ do
            runAppWithoutLogging
              fixture.appEnv
              (unmanage (Just fixture.routeName) [] False)
              `shouldReturn` ExitSuccess
            exists fixture.intermediate `shouldReturn` True

      symlinkIt "rejects a retargeted intermediate ancestor during cleanup" $
        withManagedTarget $ \fixture -> do
          state <- loadState fixture
          writeManifestFile fixture.orphanManifest fixture.manifestPath
          originalName <- encodeFS "original-intermediate-parent"
          retargetName <- encodeFS "retargeted-intermediate-parent"
          aliasName <- encodeFS "intermediate-parent-alias"
          snapshotName <- encodeFS "snapshot"
          sentinelName <- encodeFS "preserve-me"
          let root = takeDirectory fixture.appEnv.sourceDirectory
          let original = root </> originalName
          let retargeted = root </> retargetName
          let alias = root </> aliasName
          let persistedSnapshot = alias </> snapshotName
          let sentinel =
                retargeted
                  </> snapshotName
                  </> fixture.routeName
                  </> sentinelName
          createDirectories $ original </> snapshotName
          createDirectories $ takeDirectory sentinel
          writeFile sentinel "unrelated data"
          System.Directory.OsPath.createDirectoryLink original alias
          let corrupted = state{intermediatePath = persistedSnapshot}
          writeFile
            ( repositoryStatePath
                fixture.appEnv.stateDirectory
                fixture.repositoryId
            )
            (encodeUtf8 $ encodeMachineState corrupted)
          System.Directory.OsPath.removeDirectoryLink alias
          System.Directory.OsPath.createDirectoryLink retargeted alias
          runAppWithoutLogging
            fixture.appEnv
            (unmanage (Just fixture.routeName) [] True)
            `shouldThrow` (== machineStateError)
          readFile sentinel `shouldReturn` "unrelated data"
          System.Directory.OsPath.removeDirectoryLink alias

      it "unmanages an entry no longer produced by an active directory route" $
        withTempDir $ \root _ -> do
          repositoryName <- encodeFS "repository"
          stateName <- encodeFS "state"
          intermediateName <- encodeFS "intermediate"
          manifestName <- encodeFS "dojang.toml"
          envName <- encodeFS "dojang-env.toml"
          routeName <- encodeFS "config"
          entryName <- encodeFS "ignored.toml"
          destinationName <- encodeFS "destination"
          homeName <- encodeFS "home"
          let repository = root </> repositoryName
          let sourceRoot = repository </> routeName
          let source = sourceRoot </> entryName
          let intermediate =
                repository </> intermediateName </> routeName </> entryName
          let destinationRoot = root </> destinationName
          let destination = destinationRoot </> entryName
          let manifestPath = repository </> manifestName
          let home = root </> homeName
          let Right always = parseMonikerName "always"
          let Right repositoryId =
                parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
          let activeManifest =
                ( manifest
                    (singleton always Always)
                    mempty
                    ( Map.singleton
                        routeName
                        [(always, Just $ Substitution "DEST")]
                    )
                    mempty
                    mempty
                )
                  { Manifest.repositoryId = Just repositoryId
                  }
          let ignoredManifest =
                activeManifest
                  { Manifest.ignorePatterns =
                      Map.singleton routeName ["ignored.toml"]
                  }
          let appEnv =
                AppEnv
                  repository
                  False
                  (Just intermediateName)
                  (root </> stateName)
                  manifestName
                  envName
                  False
                  False
          createDirectories sourceRoot
          createDirectories home
          writeManifestFile activeManifest manifestPath
          writeFile source "managed"
          withEnvVars [("DEST", destinationRoot), ("HOME", home)] $ do
            runAppWithoutLogging appEnv (apply False [])
              `shouldReturn` ExitSuccess
            writeManifestFile ignoredManifest manifestPath
            removeFile source
            exists intermediate `shouldReturn` True
            readFile destination `shouldReturn` "managed"
            before <- loadRepositoryState appEnv repositoryId
            Map.size before.targetRecords `shouldBe` 1
            runAppWithoutLogging appEnv (unmanage (Just routeName) [] False)
              `shouldReturn` ExitSuccess
            after <- loadRepositoryState appEnv repositoryId
            after.targetRecords `shouldBe` Map.empty
            exists intermediate `shouldReturn` False
            readFile destination `shouldReturn` "managed"

    describe "forget" $ do
      it "does not create state or migrate a legacy snapshot when state is absent" $
        withTempDir $ \root _ -> do
          repositoryName <- encodeFS "repository"
          stateName <- encodeFS "state"
          manifestName <- encodeFS "dojang.toml"
          envName <- encodeFS "dojang-env.toml"
          legacyName <- encodeFS ".dojang"
          baselineName <- encodeFS "baseline"
          let repository = root </> repositoryName
          let stateRoot = root </> stateName
          let legacy = repository </> legacyName
          let manifestPath = repository </> manifestName
          let Right repositoryId =
                parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
          let repositoryManifest =
                (manifest mempty mempty mempty mempty mempty)
                  { Manifest.repositoryId = Just repositoryId
                  }
          let appEnv =
                AppEnv
                  repository
                  False
                  Nothing
                  stateRoot
                  manifestName
                  envName
                  False
                  False
          createDirectories legacy
          writeFile (legacy </> baselineName) "preserve me"
          writeManifestFile repositoryManifest manifestPath
          runAppWithoutLogging appEnv (forget False)
            `shouldReturn` ExitSuccess
          exists stateRoot `shouldReturn` False
          readFile (legacy </> baselineName) `shouldReturn` "preserve me"

      it "removes only machine-local state and preserves source and destination" $
        withManagedTarget $ \fixture -> do
          state <- loadState fixture
          runAppWithoutLogging fixture.appEnv (forget False)
            `shouldReturn` ExitSuccess
          readFile fixture.source `shouldReturn` "managed"
          readFile fixture.destination `shouldReturn` "managed"
          exists fixture.intermediate `shouldReturn` False
          Right (Just machineId) <- readMachineId fixture.appEnv.stateDirectory
          readRepositoryState
            fixture.appEnv.stateDirectory
            fixture.repositoryId
            machineId
            `shouldReturn` Right Nothing
          listRepositoryStates fixture.appEnv.stateDirectory machineId
            `shouldReturn` Right []
          exists (takeDirectory state.targetSnapshotRoot) `shouldReturn` False

      it "removes an interrupted migration journal when forgetting" $
        withManagedTarget $ \fixture -> do
          let marker =
                migrationMarkerPath
                  fixture.appEnv.stateDirectory
                  fixture.repositoryId
          writeFile marker "interrupted migration"
          runAppWithoutLogging fixture.appEnv (forget False)
            `shouldReturn` ExitSuccess
          exists marker `shouldReturn` False
          Right (Just machineId) <- readMachineId fixture.appEnv.stateDirectory
          listRepositoryStates fixture.appEnv.stateDirectory machineId
            `shouldReturn` Right []

      it "clears matching legacy first-apply history when forgetting" $
        withManagedTarget $ \fixture -> do
          decoyName <- encodeFS "process-home"
          let decoyHome =
                takeDirectory fixture.appEnv.sourceDirectory </> decoyName
          let legacyRegistry = fixture.home </> registryFilename
          createDirectories decoyHome
          writeRegistry
            legacyRegistry
            (Registry fixture.appEnv.sourceDirectory)
          withEnvVars
            [("HOME", decoyHome), ("USERPROFILE", decoyHome)]
            $ do
              before <- loadState fixture
              before.firstApplied `shouldBe` True
              runIsolatedHomeIO
                fixture.home
                (runAppWithoutLogging fixture.appEnv $ forget False)
                `shouldReturn` ExitSuccess
              exists legacyRegistry `shouldReturn` False
              runIsolatedHomeIO
                fixture.home
                (runAppWithoutLogging fixture.appEnv $ status defaultStatusOptions)
                `shouldReturn` ExitSuccess
              after <- loadState fixture
              after.firstApplied `shouldBe` False

      it "rejects forget from a duplicate live checkout" $
        withManagedTarget $ \fixture -> do
          duplicateName <- encodeFS "duplicate-repository"
          let duplicate = takeDirectory fixture.appEnv.sourceDirectory </> duplicateName
          let duplicateManifest = duplicate </> fixture.appEnv.manifestFile
          createDirectories duplicate
          copyFile fixture.manifestPath duplicateManifest
          let duplicateEnv = fixture.appEnv{sourceDirectory = duplicate}
          runAppWithoutLogging duplicateEnv (forget True)
            `shouldThrow` (== machineStateError)
          state <- loadState fixture
          Map.size state.targetRecords `shouldBe` 1

      it "rejects an intermediate root that contains the checkout" $
        withManagedTarget $ \fixture -> do
          state <- loadState fixture
          let corrupted =
                state{intermediatePath = fixture.appEnv.sourceDirectory}
          writeFile
            ( repositoryStatePath
                fixture.appEnv.stateDirectory
                fixture.repositoryId
            )
            (encodeUtf8 $ encodeMachineState corrupted)
          runAppWithoutLogging fixture.appEnv (forget True)
            `shouldThrow` (== machineStateError)
          readFile fixture.source `shouldReturn` "managed"
          exists fixture.manifestPath `shouldReturn` True

      symlinkIt "rejects a retargeted external snapshot ancestor" $
        withManagedTarget $ \fixture -> do
          state <- loadState fixture
          originalName <- encodeFS "original-snapshot-parent"
          retargetName <- encodeFS "retargeted-snapshot-parent"
          aliasName <- encodeFS "snapshot-parent-alias"
          snapshotName <- encodeFS "snapshot"
          sentinelName <- encodeFS "preserve-me"
          let root = takeDirectory fixture.appEnv.sourceDirectory
          let original = root </> originalName
          let retargeted = root </> retargetName
          let alias = root </> aliasName
          let persistedSnapshot = alias </> snapshotName
          let sentinel = retargeted </> snapshotName </> sentinelName
          createDirectories $ original </> snapshotName
          createDirectories $ retargeted </> snapshotName
          writeFile sentinel "unrelated data"
          System.Directory.OsPath.createDirectoryLink original alias
          let corrupted = state{intermediatePath = persistedSnapshot}
          writeFile
            ( repositoryStatePath
                fixture.appEnv.stateDirectory
                fixture.repositoryId
            )
            (encodeUtf8 $ encodeMachineState corrupted)
          System.Directory.OsPath.removeDirectoryLink alias
          System.Directory.OsPath.createDirectoryLink retargeted alias
          runAppWithoutLogging fixture.appEnv (forget True)
            `shouldThrow` (== machineStateError)
          readFile sentinel `shouldReturn` "unrelated data"
          System.Directory.OsPath.removeDirectoryLink alias

      symlinkIt "retries published snapshot cleanup during the next command" $
        withManagedTarget $ \fixture -> do
          writeManifestFile fixture.orphanManifest fixture.manifestPath
          before <- loadState fixture
          target <- case Map.elems before.targetRecords of
            [one] -> return one
            records -> fail $ "Unexpected managed targets: " <> show records
          blockerName <- encodeFS "snapshot-cleanup-blocker"
          let snapshotAncestor = takeDirectory target.snapshotPath
          let blocker = takeDirectory snapshotAncestor </> blockerName
          sentinelName <- encodeFS "preserve-me"
          let escapedSnapshot = blocker </> takeFileName target.snapshotPath
          let sentinel = escapedSnapshot </> sentinelName
          removeDirectoryRecursively snapshotAncestor
          createDirectories escapedSnapshot
          writeFile sentinel "outside snapshot"
          System.Directory.OsPath.createDirectoryLink blocker snapshotAncestor
          runAppWithoutLogging
            fixture.appEnv
            (unmanage (Just fixture.routeName) [] True)
            `shouldThrow` (== machineStateError)
          readFile sentinel `shouldReturn` "outside snapshot"
          afterFailure <- loadState fixture
          afterFailure.targetRecords `shouldBe` Map.empty
          afterFailure.pendingCleanupPaths `shouldContain` [target.snapshotPath]
          System.Directory.OsPath.removeDirectoryLink snapshotAncestor
          createDirectories target.snapshotPath
          runAppWithoutLogging fixture.appEnv (apply True [])
            `shouldReturn` ExitSuccess
          exists target.snapshotPath `shouldReturn` False
          afterRetry <- loadState fixture
          afterRetry.pendingCleanupPaths `shouldBe` []

      symlinkIt "keeps forget retryable after snapshot cleanup fails" $
        withManagedTarget $ \fixture -> do
          state <- loadState fixture
          blockerName <- encodeFS "snapshot-cleanup-blocker"
          let blocker = takeDirectory state.targetSnapshotRoot </> blockerName
          removeDirectoryRecursively state.targetSnapshotRoot
          createDirectories blocker
          System.Directory.OsPath.createDirectoryLink blocker state.targetSnapshotRoot
          runAppWithoutLogging fixture.appEnv (forget True)
            `shouldThrow` (== machineStateError)
          exists state.intermediatePath `shouldReturn` True
          System.Directory.OsPath.removeDirectoryLink state.targetSnapshotRoot
          createDirectories state.targetSnapshotRoot
          runAppWithoutLogging fixture.appEnv (forget True)
            `shouldReturn` ExitSuccess

      it "retries an approved partial forget without force" $
        withManagedTarget $ \fixture -> do
          state <- loadState fixture
          markerName <- encodeFS "forget-in-progress"
          let marker =
                repositoryStateDirectory
                  fixture.appEnv.stateDirectory
                  fixture.repositoryId
                  </> markerName
          removeDirectoryRecursively state.targetSnapshotRoot
          writeFile marker "approved"
          runAppWithoutLogging fixture.appEnv (forget False)
            `shouldReturn` ExitSuccess
          exists marker `shouldReturn` False

      it "clears an approval marker after state deletion completed" $
        withManagedTarget $ \fixture -> do
          state <- loadState fixture
          markerName <- encodeFS "forget-in-progress"
          let marker =
                repositoryStateDirectory
                  fixture.appEnv.stateDirectory
                  fixture.repositoryId
                  </> markerName
          writeFile marker "approved"
          removeDirectoryRecursively state.targetSnapshotRoot
          removeDirectory $ takeDirectory state.targetSnapshotRoot
          removeDirectoryRecursively state.intermediatePath
          removeFile $
            repositoryStatePath
              fixture.appEnv.stateDirectory
              fixture.repositoryId
          runAppWithoutLogging fixture.appEnv (forget False)
            `shouldReturn` ExitSuccess
          exists marker `shouldReturn` False
          runAppWithoutLogging fixture.appEnv (forget False)
            `shouldReturn` ExitSuccess

      it "blocks synchronization while forget approval is pending" $
        withManagedTarget $ \fixture -> do
          markerName <- encodeFS "forget-in-progress"
          let marker =
                repositoryStateDirectory
                  fixture.appEnv.stateDirectory
                  fixture.repositoryId
                  </> markerName
          writeFile marker "approved"
          writeFile fixture.source "fresh"
          runAppWithoutLogging fixture.appEnv (apply False [])
            `shouldThrow` (== machineStateError)
          readFile fixture.destination `shouldReturn` "managed"
          state <- loadState fixture
          Map.size state.targetRecords `shouldBe` 1

      it "does not approve a fresh forget of a modified destination" $
        withManagedTarget $ \fixture -> do
          markerName <- encodeFS "forget-in-progress"
          let marker =
                repositoryStateDirectory
                  fixture.appEnv.stateDirectory
                  fixture.repositoryId
                  </> markerName
          writeFile fixture.destination "modified"
          runAppWithoutLogging fixture.appEnv (forget False)
            `shouldThrow` (== accidentalDeletionWarning)
          exists marker `shouldReturn` False


data Fixture = Fixture
  { appEnv :: AppEnv
  , repositoryId :: RepositoryId
  , routeName :: OsPath
  , source :: OsPath
  , intermediate :: OsPath
  , destination :: OsPath
  , manifestPath :: OsPath
  , home :: OsPath
  , orphanManifest :: Manifest
  }


withManagedTarget :: (Fixture -> IO a) -> IO a
withManagedTarget = withManagedTargetDestination False


withRelativeManagedTarget :: (Fixture -> IO a) -> IO a
withRelativeManagedTarget = withManagedTargetDestination True


withManagedTargetDestination :: Bool -> (Fixture -> IO a) -> IO a
withManagedTargetDestination relativeDestination action =
  withTempDir $ \root _ -> do
    repositoryName <- encodeFS "repository"
    stateName <- encodeFS "state"
    intermediateName <- encodeFS "intermediate"
    manifestName <- encodeFS "dojang.toml"
    envName <- encodeFS "dojang-env.toml"
    routeName <- encodeFS "config"
    destinationName <- encodeFS "destination"
    homeName <- encodeFS "home"
    let repository = root </> repositoryName
    let source = repository </> routeName
    let intermediate = repository </> intermediateName </> routeName
    let destination = root </> destinationName
    let manifestPath = repository </> manifestName
    let home = root </> homeName
    let Right always = parseMonikerName "always"
    let Right repositoryId =
          parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
    let baseManifest =
          (manifest (singleton always Always) mempty mempty mempty mempty)
            { Manifest.repositoryId = Just repositoryId
            }
    let activeManifest =
          ( manifest
              (singleton always Always)
              (Map.singleton routeName [(always, Just $ Substitution "DEST")])
              mempty
              mempty
              mempty
          )
            { Manifest.repositoryId = Just repositoryId
            }
    let appEnv =
          AppEnv
            repository
            False
            (Just intermediateName)
            (root </> stateName)
            manifestName
            envName
            False
            False
    createDirectories repository
    createDirectories home
    writeManifestFile activeManifest manifestPath
    writeFile source "managed"
    let configuredDestination =
          if relativeDestination then destinationName else destination
    System.Directory.OsPath.withCurrentDirectory root
      $ withEnvVars
        [ ("DEST", configuredDestination)
        , ("HOME", home)
        ]
      $ do
        runAppWithoutLogging appEnv (apply False []) `shouldReturn` ExitSuccess
        action $
          Fixture
            appEnv
            repositoryId
            routeName
            source
            intermediate
            destination
            manifestPath
            home
            baseManifest


loadState :: Fixture -> IO MachineState
loadState fixture =
  loadRepositoryState fixture.appEnv fixture.repositoryId


loadRepositoryState :: AppEnv -> RepositoryId -> IO MachineState
loadRepositoryState appEnv repositoryId = do
  Right (Just machineId) <- readMachineId appEnv.stateDirectory
  Right (Just state) <-
    readRepositoryState
      appEnv.stateDirectory
      repositoryId
      machineId
  return state


withEnvVars :: [(String, OsPath)] -> IO a -> IO a
withEnvVars variables action = do
  originals <- traverse (lookupEnv . fst) variables
  let restore = sequence_ $ zipWith restoreOne variables originals
  bracket_
    (mapM_ (\(name, value) -> decodePath value >>= setEnv name) variables)
    restore
    action
 where
  restoreOne (name, _) Nothing = unsetEnv name
  restoreOne (name, _) (Just value) = setEnv name value


newtype IsolatedHomeIO a = IsolatedHomeIO
  { unIsolatedHomeIO :: ReaderT OsPath IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError IOError)


runIsolatedHomeIO :: OsPath -> IsolatedHomeIO a -> IO a
runIsolatedHomeIO home action = runReaderT action.unIsolatedHomeIO home


instance MonadFileSystem IsolatedHomeIO where
  encodePath = liftIO . FileSystem.encodePath
  decodePath = liftIO . FileSystem.decodePath
  getCurrentDirectory = liftIO FileSystem.getCurrentDirectory
  getHomeDirectory = IsolatedHomeIO ask
  exists = liftIO . FileSystem.exists
  isFile = liftIO . FileSystem.isFile
  isRegularFile = liftIO . FileSystem.isRegularFile
  isDirectory = liftIO . FileSystem.isDirectory
  isSymlink = liftIO . FileSystem.isSymlink
  readFile = liftIO . FileSystem.readFile
  writeFile path = liftIO . FileSystem.writeFile path
  replaceFile source = liftIO . FileSystem.replaceFile source
  writeTemporaryFile directory template =
    liftIO . FileSystem.writeTemporaryFile directory template
  withFileLock lockPath action = IsolatedHomeIO $ ReaderT $ \home ->
    FileSystem.withFileLock
      lockPath
      (runReaderT action.unIsolatedHomeIO home)
  canonicalizePath = liftIO . FileSystem.canonicalizePath
  readSymlinkTarget = liftIO . FileSystem.readSymlinkTarget
  copyFile source = liftIO . FileSystem.copyFile source
  copyFileWithMetadata source =
    liftIO . FileSystem.copyFileWithMetadata source
  copyFilePermissions source =
    liftIO . FileSystem.copyFilePermissions source
  createDirectory = liftIO . FileSystem.createDirectory
  removeFile = liftIO . FileSystem.removeFile
  removeDirectory = liftIO . FileSystem.removeDirectory
  listDirectory = liftIO . FileSystem.listDirectory
  getFileSize = liftIO . FileSystem.getFileSize
