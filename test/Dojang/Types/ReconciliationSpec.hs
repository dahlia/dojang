{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.ReconciliationSpec (spec) where

import System.IO.Error (ioeGetFileName)
import System.OsPath (OsPath, encodeFS, takeDirectory, (</>))
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Expectations.Pretty (shouldBe)

import Control.Monad.Except (tryError)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (linear)
import Test.Hspec.Expectations.Pretty
  ( shouldReturn
  , shouldSatisfy
  , shouldThrow
  )
import Test.Hspec.Hedgehog (MonadGen, forAll, hedgehog, (===))

import Dojang.MonadFileSystem qualified as FileSystem
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Codec (identityCodecSpec)
import Dojang.Types.Codec.Evaluate (opaqueBytes)
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , ManagedCorrespondence (..)
  , RouteState (..)
  )
import Dojang.Types.Environment.Current (MonadEnvironment (currentEnvironment))
import Dojang.Types.FilePathExpression.Expansion (simpleVariableGetter)
import Dojang.Types.FileRoute (RouteKind (CopyRoute, SymlinkRoute))
import Dojang.Types.Manifest qualified as Manifest
import Dojang.Types.Reconciliation
  ( ConflictPolicy (..)
  , ContentGuard (ContentGuard)
  , PlannedSyncOp (..)
  , ReconciliationDirection (..)
  , ReconciliationInput (..)
  , ReconciliationItem (..)
  , ReconciliationOutcome (..)
  , ReconciliationPlan (..)
  , ReconciliationSkipReason (..)
  , Replica (..)
  , ReplicaComparison (..)
  , SyncOp (..)
  , destructiveOperations
  , executeReconciliationPlan
  , executeReconciliationPlanGuarded
  , executeReconciliationPlanWith
  , executeSyncOp
  , observeModeDrift
  , observeReconciliationInput
  , planReconciliation
  )
import Dojang.Types.Repository (Repository (..), RouteResult (..))
import Dojang.Types.RouteMetadata
  ( PortableMode (..)
  , RouteMode (DefaultMode, Private, ReadOnly)
  , portableModeFromBits
  )


data Paths = Paths
  { source :: OsPath
  , intermediate :: OsPath
  , destination :: OsPath
  }
  deriving (Eq, Show)


data ModelState
  = ModelMissing
  | ModelDirectory
  | ModelFile Int
  deriving (Eq, Show)


modelState :: (MonadGen m) => m ModelState
modelState =
  Gen.choice
    [ return ModelMissing
    , return ModelDirectory
    , ModelFile <$> Gen.int (linear 0 8)
    ]


modelStat :: ModelState -> FileStat
modelStat ModelMissing = Missing
modelStat ModelDirectory = Directory
modelStat (ModelFile _) = File 1


modelDelta :: ModelState -> ModelState -> FileDeltaKind
modelDelta ModelMissing ModelMissing = Unchanged
modelDelta ModelMissing _ = Added
modelDelta _ ModelMissing = Removed
modelDelta ModelDirectory ModelDirectory = Unchanged
modelDelta (ModelFile a) (ModelFile b)
  | a == b = Unchanged
  | otherwise = Modified
modelDelta _ _ = Modified


modelComparison :: ModelState -> ModelState -> ReplicaComparison
modelComparison a b
  | a == b = ReplicasEquivalent
  | otherwise = ReplicasDifferent


makeModelInput
  :: Paths
  -> RouteState
  -> ModelState
  -> ModelState
  -> ModelState
  -> ReconciliationInput
makeModelInput paths routeState sourceState intermediateState destinationState =
  makeInput
    paths
    (modelStat sourceState)
    (modelStat intermediateState)
    (modelStat destinationState)
    (modelDelta intermediateState sourceState)
    (modelDelta intermediateState destinationState)
    (modelComparison sourceState destinationState)
    routeState


applyModelOperations
  :: Map OsPath ModelState
  -> [PlannedSyncOp]
  -> Map OsPath ModelState
applyModelOperations = foldl' applyOperation
 where
  applyOperation state (PlannedSyncOp _ operation) = case operation of
    RemoveDirs path -> Map.insert path ModelMissing state
    RemoveFile path -> Map.insert path ModelMissing state
    RemoveLink path -> Map.insert path ModelMissing state
    CopyFile sourcePath destinationPath ->
      Map.insert destinationPath (state Map.! sourcePath) state
    CreateDir path -> Map.insert path ModelDirectory state
    CreateDirs path -> Map.insert path ModelDirectory state
    SetEntryMode _ _ _ -> state
    CreateSymlink _ _ _ -> state
    RemoveDirsExcept path _ -> Map.insert path ModelMissing state
    WriteContent _ destinationPath -> Map.insert destinationPath (ModelFile 0) state
    WriteContentGuarded _ _ destinationPath ->
      Map.insert destinationPath (ModelFile 0) state


swapReplica :: Replica -> Replica
swapReplica SourceReplica = DestinationReplica
swapReplica IntermediateReplica = IntermediateReplica
swapReplica DestinationReplica = SourceReplica


swapPlannedReplica :: PlannedSyncOp -> PlannedSyncOp
swapPlannedReplica (PlannedSyncOp replica operation) =
  PlannedSyncOp (swapReplica replica) operation


swapInput :: ReconciliationInput -> ReconciliationInput
swapInput input =
  ReconciliationInput
    { correspondence =
        FileCorrespondence
          { source = input.correspondence.destination
          , sourceDelta = input.correspondence.destinationDelta
          , intermediate = input.correspondence.intermediate
          , destination = input.correspondence.source
          , destinationDelta = input.correspondence.sourceDelta
          }
    , sourceDestinationComparison = input.sourceDestinationComparison
    , destinationRouteState = input.destinationRouteState
    , declaredDestinationMode = input.declaredDestinationMode
    , observedDestinationMode = input.observedDestinationMode
    , destinationKind = input.destinationKind
    , routeFileType = input.routeFileType
    , protectedDestinations = input.protectedDestinations
    , renderedSource = Nothing
    , renderedSourceGuard = Nothing
    }


makeInput
  :: Paths
  -> FileStat
  -> FileStat
  -> FileStat
  -> FileDeltaKind
  -> FileDeltaKind
  -> ReplicaComparison
  -> RouteState
  -> ReconciliationInput
makeInput paths sourceStat intermediateStat destinationStat sourceDelta destinationDelta comparison routeState =
  ReconciliationInput
    { correspondence =
        FileCorrespondence
          { source = FileEntry paths.source sourceStat
          , sourceDelta = sourceDelta
          , intermediate = FileEntry paths.intermediate intermediateStat
          , destination = FileEntry paths.destination destinationStat
          , destinationDelta = destinationDelta
          }
    , sourceDestinationComparison = comparison
    , destinationRouteState = routeState
    , declaredDestinationMode = DefaultMode
    , observedDestinationMode = Nothing
    , destinationKind = CopyRoute
    , routeFileType = FileSystem.File
    , protectedDestinations = []
    , renderedSource = Nothing
    , renderedSourceGuard = Nothing
    }


spec :: Spec
spec = do
  sourceRoot <- runIO $ encodeFS "source"
  intermediateRoot <- runIO $ encodeFS "intermediate"
  destinationRoot <- runIO $ encodeFS "destination"
  file <- runIO $ encodeFS "file"
  otherFile <- runIO $ encodeFS "other-file"
  route <- runIO $ encodeFS "route"
  target <- runIO $ encodeFS "target"
  let paths =
        Paths
          { source = sourceRoot </> file
          , intermediate = intermediateRoot </> file
          , destination = destinationRoot </> file
          }
  let routed = Routed route

  describe "planReconciliation" $ do
    it "produces no operations for an already reconciled entry" $ do
      let input =
            makeInput
              paths
              (File 3)
              (File 3)
              (File 3)
              Unchanged
              Unchanged
              ReplicasEquivalent
              routed
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations `shouldBe` []
      plan.conflicts `shouldBe` []
      fmap (.outcome) plan.items `shouldBe` [NoChange]
      fmap (.desiredEntry) plan.items
        `shouldBe` [input.correspondence.source]

    it "plans a source-only addition through the intermediate replica" $ do
      let input =
            makeInput
              paths
              (File 3)
              Missing
              Missing
              Added
              Unchanged
              ReplicasDifferent
              routed
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations
        `shouldBe` [ PlannedSyncOp
                       IntermediateReplica
                       (CreateDirs $ takeDirectory paths.intermediate)
                   , PlannedSyncOp
                       IntermediateReplica
                       (CopyFile paths.source paths.intermediate)
                   , PlannedSyncOp
                       DestinationReplica
                       (CreateDirs $ takeDirectory paths.destination)
                   , PlannedSyncOp
                       DestinationReplica
                       (CopyFile paths.intermediate paths.destination)
                   ]
      fmap (.outcome) plan.items `shouldBe` [WillReconcile]

    it "plans rendered bytes without exposing them" $ do
      let rendered = opaqueBytes "rendered-secret"
          guard = ContentGuard paths.source $ opaqueBytes "raw-secret"
          input =
            ( makeInput
                paths
                (File 15)
                Missing
                Missing
                Added
                Unchanged
                ReplicasDifferent
                routed
            )
              { renderedSource = Just rendered
              , renderedSourceGuard = Just guard
              }
          plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations
        `shouldBe` [ PlannedSyncOp
                       IntermediateReplica
                       (CreateDirs $ takeDirectory paths.intermediate)
                   , PlannedSyncOp
                       IntermediateReplica
                       (WriteContentGuarded rendered guard paths.intermediate)
                   , PlannedSyncOp
                       DestinationReplica
                       (CreateDirs $ takeDirectory paths.destination)
                   , PlannedSyncOp
                       DestinationReplica
                       (CopyFile paths.intermediate paths.destination)
                   ]
      show plan `shouldSatisfy` not . ("rendered-secret" `isInfixOf`)
      show plan `shouldSatisfy` not . ("raw-secret" `isInfixOf`)

    it "reports an incompatible two-sided change without planning it" $ do
      let input =
            makeInput
              paths
              (File 3)
              (File 3)
              (File 3)
              Modified
              Modified
              ReplicasDifferent
              routed
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations `shouldBe` []
      length plan.conflicts `shouldBe` 1
      fmap (.outcome) plan.items `shouldBe` [ConflictDetected]

      let forced =
            planReconciliation SourceToDestination PreferAuthoritative [input]
      length forced.conflicts `shouldBe` 1
      fmap (.outcome) forced.items `shouldBe` [WillReconcile]

    it "accepts equal independent additions and only records the base" $ do
      let input =
            makeInput
              paths
              (File 3)
              Missing
              (File 3)
              Added
              Added
              ReplicasEquivalent
              routed
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations
        `shouldBe` [ PlannedSyncOp
                       IntermediateReplica
                       (CreateDirs $ takeDirectory paths.intermediate)
                   , PlannedSyncOp
                       IntermediateReplica
                       (CopyFile paths.source paths.intermediate)
                   ]
      plan.conflicts `shouldBe` []

    it "accepts simultaneous removals and removes only the base" $ do
      let input =
            makeInput
              paths
              Missing
              (File 3)
              Missing
              Removed
              Removed
              ReplicasEquivalent
              routed
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations
        `shouldBe` [ PlannedSyncOp
                       IntermediateReplica
                       (RemoveFile paths.intermediate)
                   ]
      plan.conflicts `shouldBe` []

    it "replaces regular files with directories" $ do
      let input =
            makeInput
              paths
              Directory
              (File 3)
              (File 3)
              Modified
              Unchanged
              ReplicasDifferent
              routed
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations
        `shouldBe` [ PlannedSyncOp
                       IntermediateReplica
                       (RemoveFile paths.intermediate)
                   , PlannedSyncOp
                       IntermediateReplica
                       (CreateDirs paths.intermediate)
                   , PlannedSyncOp
                       DestinationReplica
                       (RemoveFile paths.destination)
                   , PlannedSyncOp
                       DestinationReplica
                       (CreateDirs paths.destination)
                   ]

    it "removes symbolic links before replacing them with regular files" $ do
      let input =
            makeInput
              paths
              (File 3)
              (Symlink target)
              (Symlink target)
              Modified
              Unchanged
              ReplicasDifferent
              routed
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations
        `shouldBe` [ PlannedSyncOp
                       IntermediateReplica
                       (RemoveFile paths.intermediate)
                   , PlannedSyncOp
                       IntermediateReplica
                       (CreateDirs $ takeDirectory paths.intermediate)
                   , PlannedSyncOp
                       IntermediateReplica
                       (CopyFile paths.source paths.intermediate)
                   , PlannedSyncOp
                       DestinationReplica
                       (RemoveFile paths.destination)
                   , PlannedSyncOp
                       DestinationReplica
                       (CreateDirs $ takeDirectory paths.destination)
                   , PlannedSyncOp
                       DestinationReplica
                       (CopyFile paths.intermediate paths.destination)
                   ]

    it "removes the overwritten replica before the recovery copy" $ do
      let input =
            makeInput
              paths
              Missing
              (File 3)
              (File 3)
              Removed
              Unchanged
              ReplicasDifferent
              routed
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations
        `shouldBe` [ PlannedSyncOp DestinationReplica (RemoveFile paths.destination)
                   , PlannedSyncOp IntermediateReplica (RemoveFile paths.intermediate)
                   ]
      destructiveOperations plan `shouldBe` plan.operations

    it "skips a symbolic link that would have to be created" $ do
      let input =
            makeInput
              paths
              (Symlink target)
              Missing
              Missing
              Added
              Unchanged
              ReplicasDifferent
              routed
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations `shouldBe` []
      fmap (.outcome) plan.items
        `shouldBe` [Skipped UnsupportedSymlink]

    it "preserves an ignored destination-only addition" $ do
      let input =
            makeInput
              paths
              Missing
              Missing
              (File 3)
              Unchanged
              Added
              ReplicasDifferent
              (Ignored route "ignored")
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations `shouldBe` []
      fmap (.outcome) plan.items
        `shouldBe` [Skipped $ IgnoredDestination route "ignored"]

    it "updates the intermediate while preserving an ignored destination" $ do
      let input =
            makeInput
              paths
              (File 3)
              (File 4)
              (File 4)
              Modified
              Unchanged
              ReplicasDifferent
              (Ignored route "ignored")
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations
        `shouldBe` [ PlannedSyncOp
                       IntermediateReplica
                       (CopyFile paths.source paths.intermediate)
                   ]
      fmap (.outcome) plan.items
        `shouldBe` [Skipped $ IgnoredDestination route "ignored"]

    it "does not reflect an ignored destination" $ do
      let input =
            makeInput
              paths
              Missing
              Missing
              (File 3)
              Unchanged
              Added
              ReplicasDifferent
              (Ignored route "ignored")
      let plan =
            planReconciliation DestinationToSource RefuseConflicts [input]
      plan.operations `shouldBe` []
      fmap (.outcome) plan.items
        `shouldBe` [Skipped $ IgnoredDestination route "ignored"]

    it "collapses descendant work under a recursive replacement" $ do
      tree <- encodeFS "tree"
      child <- encodeFS "child"
      let parentPaths =
            Paths
              { source = sourceRoot </> tree
              , intermediate = intermediateRoot </> tree
              , destination = destinationRoot </> tree
              }
      let childPaths =
            Paths
              { source = parentPaths.source </> child
              , intermediate = parentPaths.intermediate </> child
              , destination = parentPaths.destination </> child
              }
      let parentInput =
            makeInput
              parentPaths
              (File 3)
              Directory
              Directory
              Modified
              Unchanged
              ReplicasDifferent
              routed
      let childInput =
            makeInput
              childPaths
              Missing
              (File 3)
              (File 3)
              Removed
              Unchanged
              ReplicasDifferent
              routed
      let plan =
            planReconciliation
              SourceToDestination
              RefuseConflicts
              [childInput, parentInput]
      plan.operations
        `shouldBe` [ PlannedSyncOp
                       IntermediateReplica
                       (RemoveDirs parentPaths.intermediate)
                   , PlannedSyncOp
                       IntermediateReplica
                       (CreateDirs $ takeDirectory parentPaths.intermediate)
                   , PlannedSyncOp
                       IntermediateReplica
                       (CopyFile parentPaths.source parentPaths.intermediate)
                   , PlannedSyncOp
                       DestinationReplica
                       (RemoveDirs parentPaths.destination)
                   , PlannedSyncOp
                       DestinationReplica
                       (CreateDirs $ takeDirectory parentPaths.destination)
                   , PlannedSyncOp
                       DestinationReplica
                       (CopyFile parentPaths.intermediate parentPaths.destination)
                   ]

    it "is deterministic regardless of input order" $ hedgehog $ do
      sourceA <- forAll modelState
      intermediateA <- forAll modelState
      destinationA <- forAll modelState
      sourceB <- forAll modelState
      intermediateB <- forAll modelState
      destinationB <- forAll modelState
      let paths' =
            Paths
              { source = sourceRoot </> otherFile
              , intermediate = intermediateRoot </> otherFile
              , destination = destinationRoot </> otherFile
              }
      let inputs =
            [ makeModelInput paths routed sourceA intermediateA destinationA
            , makeModelInput paths' routed sourceB intermediateB destinationB
            ]
      planReconciliation SourceToDestination PreferAuthoritative inputs
        === planReconciliation
          SourceToDestination
          PreferAuthoritative
          (reverse inputs)

    it "is symmetric when replicas and direction are swapped" $ hedgehog $ do
      sourceState <- forAll modelState
      intermediateState <- forAll modelState
      destinationState <- forAll modelState
      policy <- forAll $ Gen.element [RefuseConflicts, PreferAuthoritative]
      let input =
            makeModelInput
              paths
              routed
              sourceState
              intermediateState
              destinationState
      let forward =
            planReconciliation SourceToDestination policy [input]
      let backward =
            planReconciliation DestinationToSource policy [swapInput input]
      fmap swapPlannedReplica forward.operations === backward.operations
      fmap (.outcome) forward.items === fmap (.outcome) backward.items
      length forward.conflicts === length backward.conflicts

    it "converges supported states and is then idempotent" $ hedgehog $ do
      sourceState <- forAll modelState
      intermediateState <- forAll modelState
      destinationState <- forAll modelState
      direction <-
        forAll $ Gen.element [SourceToDestination, DestinationToSource]
      let input =
            makeModelInput
              paths
              routed
              sourceState
              intermediateState
              destinationState
      let plan = planReconciliation direction PreferAuthoritative [input]
      let initial =
            Map.fromList
              [ (paths.source, sourceState)
              , (paths.intermediate, intermediateState)
              , (paths.destination, destinationState)
              ]
      let final = applyModelOperations initial plan.operations
      let authoritative = case direction of
            SourceToDestination -> sourceState
            DestinationToSource -> destinationState
      final Map.! paths.source === authoritative
      final Map.! paths.intermediate === authoritative
      final Map.! paths.destination === authoritative
      let reconciled =
            makeModelInput paths routed authoritative authoritative authoritative
      let secondPlan =
            planReconciliation direction RefuseConflicts [reconciled]
      secondPlan.operations === []
      fmap (.outcome) secondPlan.items === [NoChange]

  describe "RemoveDirsExcept" $ do
    it "preserves protected subtrees during removal" $
      withTempDir $ \tmpDir _ -> do
        rootName <- encodeFS "except-root"
        goneName <- encodeFS "gone"
        keepName <- encodeFS "keep"
        fileName' <- encodeFS "file"
        let rootPath = tmpDir </> rootName
        FileSystem.createDirectories $ rootPath </> goneName
        FileSystem.writeFile (rootPath </> goneName </> fileName') "gone"
        FileSystem.writeFile (rootPath </> fileName') "gone"
        FileSystem.createDirectories $ rootPath </> keepName
        FileSystem.writeFile (rootPath </> keepName </> fileName') "kept"
        executeSyncOp $
          RemoveDirsExcept rootPath [rootPath </> keepName]
        FileSystem.exists (rootPath </> goneName) `shouldReturn` False
        FileSystem.exists (rootPath </> fileName') `shouldReturn` False
        FileSystem.readFile (rootPath </> keepName </> fileName')
          `shouldReturn` "kept"

    it "preserves deeply nested protected roots" $
      withTempDir $ \tmpDir _ -> do
        rootName <- encodeFS "except-root"
        midName <- encodeFS "mid"
        keepName <- encodeFS "keep"
        fileName' <- encodeFS "file"
        let rootPath = tmpDir </> rootName
        let keepPath = rootPath </> midName </> keepName
        FileSystem.createDirectories keepPath
        FileSystem.writeFile (keepPath </> fileName') "kept"
        FileSystem.writeFile (rootPath </> midName </> fileName') "gone"
        executeSyncOp $ RemoveDirsExcept rootPath [keepPath]
        FileSystem.exists (rootPath </> midName </> fileName')
          `shouldReturn` False
        FileSystem.readFile (keepPath </> fileName') `shouldReturn` "kept"

    it "removes everything when nothing is protected" $
      withTempDir $ \tmpDir _ -> do
        rootName <- encodeFS "except-root"
        fileName' <- encodeFS "file"
        let rootPath = tmpDir </> rootName
        FileSystem.createDirectories rootPath
        FileSystem.writeFile (rootPath </> fileName') "gone"
        executeSyncOp $ RemoveDirsExcept rootPath []
        FileSystem.exists rootPath `shouldReturn` False

    it "converts ancestor removals to protected removals" $ do
      let removal =
            ( makeInput
                paths
                Missing
                Directory
                Directory
                Removed
                Unchanged
                ReplicasDifferent
                (Routed route)
            )
              { protectedDestinations = [paths.destination </> paths.source]
              }
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [removal]
      [op.syncOp | op <- plan.operations, op.replica == DestinationReplica]
        `shouldBe` [ RemoveDirsExcept
                       paths.destination
                       [paths.destination </> paths.source]
                   ]
      -- The intermediate is unaffected by destination protection:
      [op.syncOp | op <- plan.operations, op.replica == IntermediateReplica]
        `shouldBe` [RemoveDirs paths.intermediate]

    it "skips replacing a directory that contains protected entries" $ do
      -- The broad route now wants a file where its destination currently
      -- holds a directory, but a nested route owns a subtree inside that
      -- directory.  A protected removal would keep the directory in
      -- place and the following copy would fail after unprotected
      -- siblings were already deleted, so the replacement must be
      -- skipped as a whole:
      let replacement =
            ( makeInput
                paths
                (File 4)
                Directory
                Directory
                Modified
                Unchanged
                ReplicasDifferent
                (Routed route)
            )
              { protectedDestinations = [paths.destination </> paths.source]
              }
      let plan =
            planReconciliation SourceToDestination PreferAuthoritative [replacement]
      fmap (.outcome) plan.items
        `shouldBe` [Skipped ProtectedSubtreeReplacement]
      [op.syncOp | op <- plan.operations, op.replica == DestinationReplica]
        `shouldBe` []

  describe "deployment links" $ do
    let linkInput destinationStat =
          ( makeInput
              paths
              (File 4)
              Missing
              destinationStat
              Unchanged
              Unchanged
              ReplicasDifferent
              (Routed route)
          )
            { destinationKind = SymlinkRoute
            , routeFileType = FileSystem.File
            }

    it "creates a missing deployment link" $ do
      let plan =
            planReconciliation
              SourceToDestination
              RefuseConflicts
              [linkInput Missing]
      ((.outcome) <$> plan.items) `shouldBe` [WillReconcile]
      plan.operations
        `shouldBe` [ PlannedSyncOp DestinationReplica $
                       CreateDirs (takeDirectory paths.destination)
                   , PlannedSyncOp DestinationReplica $
                       CreateSymlink
                         paths.source
                         paths.destination
                         FileSystem.File
                   ]

    it "accepts a correctly targeted link even when broken" $ do
      let plan =
            planReconciliation
              SourceToDestination
              RefuseConflicts
              [linkInput $ Symlink paths.source]
      ((.outcome) <$> plan.items) `shouldBe` [NoChange]
      plan.operations `shouldBe` []

    it "replaces a link with the wrong target" $ do
      let plan =
            planReconciliation
              SourceToDestination
              RefuseConflicts
              [linkInput $ Symlink paths.intermediate]
      ((.outcome) <$> plan.items) `shouldBe` [WillReconcile]
      plan.operations
        `shouldBe` [ PlannedSyncOp DestinationReplica $
                       RemoveLink paths.destination
                   , PlannedSyncOp DestinationReplica $
                       CreateSymlink
                         paths.source
                         paths.destination
                         FileSystem.File
                   ]
      -- Repairing a drifted link destroys no content, so the plan must
      -- not require forcing past the accidental-deletion guard:
      destructiveOperations plan `shouldBe` []

    it "refuses to replace an existing entry without force" $ do
      let plan =
            planReconciliation
              SourceToDestination
              RefuseConflicts
              [linkInput $ File 4]
      ((.outcome) <$> plan.items) `shouldBe` [ConflictDetected]
      length plan.conflicts `shouldBe` 1
      plan.operations `shouldBe` []

    it "replaces an existing entry when preferred" $ do
      let plan =
            planReconciliation
              SourceToDestination
              PreferAuthoritative
              [linkInput $ File 4]
      ((.outcome) <$> plan.items) `shouldBe` [WillReconcile]
      plan.operations
        `shouldBe` [ PlannedSyncOp DestinationReplica $
                       RemoveFile paths.destination
                   , PlannedSyncOp DestinationReplica $
                       CreateSymlink
                         paths.source
                         paths.destination
                         FileSystem.File
                   ]

    it "replaces an existing directory when preferred" $ do
      let input =
            (linkInput Directory){routeFileType = FileSystem.Directory}
      let plan =
            planReconciliation SourceToDestination PreferAuthoritative [input]
      plan.operations
        `shouldBe` [ PlannedSyncOp DestinationReplica $
                       RemoveDirs paths.destination
                   , PlannedSyncOp DestinationReplica $
                       CreateSymlink
                         paths.source
                         paths.destination
                         FileSystem.Directory
                   ]

    it "never reflects a deployment link" $ do
      let missingPlan =
            planReconciliation
              DestinationToSource
              RefuseConflicts
              [linkInput Missing]
      ((.outcome) <$> missingPlan.items)
        `shouldBe` [Skipped DeploymentLinkNotReflectable]
      missingPlan.operations `shouldBe` []
      -- Even with the authoritative-preference policy (--force):
      let forcedPlan =
            planReconciliation
              DestinationToSource
              PreferAuthoritative
              [linkInput $ File 4]
      ((.outcome) <$> forcedPlan.items)
        `shouldBe` [Skipped DeploymentLinkNotReflectable]
      forcedPlan.operations `shouldBe` []

  describe "declared destination modes" $ do
    let withMode declared observed input =
          input
            { declaredDestinationMode = declared
            , observedDestinationMode = observed
            }
    let unchangedFile =
          makeInput
            paths
            (File 1)
            (File 1)
            (File 1)
            Unchanged
            Unchanged
            ReplicasEquivalent
            (Routed route)
    let observed644 = Just $ portableModeFromBits 0o644
    let observed600 = Just $ portableModeFromBits 0o600

    it "plans a mode change for metadata-only drift" $ do
      let input = withMode Private observed644 unchangedFile
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      ((.outcome) <$> plan.items) `shouldBe` [WillReconcile]
      plan.operations
        `shouldBe` [ PlannedSyncOp DestinationReplica $
                       SetEntryMode paths.destination Private FileSystem.File
                   ]

    it "plans nothing when the observed mode satisfies the declared one" $ do
      let input = withMode Private observed600 unchangedFile
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      ((.outcome) <$> plan.items) `shouldBe` [NoChange]
      plan.operations `shouldBe` []

    it "plans nothing for the default mode" $ do
      let input = withMode DefaultMode observed644 unchangedFile
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      ((.outcome) <$> plan.items) `shouldBe` [NoChange]
      plan.operations `shouldBe` []

    it "applies the declared mode after content operations" $ do
      let changedFile =
            makeInput
              paths
              (File 2)
              (File 1)
              (File 1)
              Modified
              Unchanged
              ReplicasDifferent
              (Routed route)
      let input = withMode Private observed600 changedFile
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      let destinationOps =
            [op.syncOp | op <- plan.operations, op.replica == DestinationReplica]
      case reverse destinationOps of
        SetEntryMode path mode fileType : _ -> do
          path `shouldBe` paths.destination
          mode `shouldBe` Private
          fileType `shouldBe` FileSystem.File
        other -> fail $ "Unexpected destination ops: " <> show other
      -- Content copies precede the mode change:
      destinationOps
        `shouldSatisfy` any
          ( \case
              CopyFile _ _ -> True
              _ -> False
          )

    it "targets the destination even when reflecting" $ do
      let input = withMode ReadOnly observed644 unchangedFile
      let plan =
            planReconciliation DestinationToSource RefuseConflicts [input]
      plan.operations
        `shouldBe` [ PlannedSyncOp DestinationReplica $
                       SetEntryMode paths.destination ReadOnly FileSystem.File
                   ]

    it "plans directory modes with directory semantics" $ do
      let unchangedDir =
            makeInput
              paths
              Directory
              Directory
              Directory
              Unchanged
              Unchanged
              ReplicasEquivalent
              (Routed route)
      let input = withMode Private observed644 unchangedDir
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations
        `shouldBe` [ PlannedSyncOp DestinationReplica $
                       SetEntryMode
                         paths.destination
                         Private
                         FileSystem.Directory
                   ]

    it "plans no mode change for removals" $ do
      let removal =
            makeInput
              paths
              Missing
              (File 1)
              (File 1)
              Removed
              Unchanged
              ReplicasDifferent
              (Routed route)
      let input = withMode Private observed600 removal
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      plan.operations
        `shouldSatisfy` all
          ( \op -> case op.syncOp of
              SetEntryMode{} -> False
              _ -> True
          )

    it "suppresses mode changes on ignored destinations" $ do
      let input =
            withMode Private observed644 $
              makeInput
                paths
                (File 1)
                (File 1)
                (File 1)
                Unchanged
                Unchanged
                ReplicasEquivalent
                (Ignored route "pattern")
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      ((.outcome) <$> plan.items) `shouldBe` [NoChange]
      plan.operations `shouldBe` []

    it "plans a mode change for a destination it creates" $ do
      let creation =
            makeInput
              paths
              (File 1)
              Missing
              Missing
              Added
              Unchanged
              ReplicasDifferent
              (Routed route)
      let input = withMode Private Nothing creation
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      let destinationOps =
            [op.syncOp | op <- plan.operations, op.replica == DestinationReplica]
      case reverse destinationOps of
        SetEntryMode path mode fileType : _ -> do
          path `shouldBe` paths.destination
          mode `shouldBe` Private
          fileType `shouldBe` FileSystem.File
        other -> fail $ "Unexpected destination ops: " <> show other

  windowsCaseProtectionSpec paths route

  describe "executeReconciliationPlanGuarded" $ do
    restoreFailureReportSpec
    let manualPlan ops =
          ReconciliationPlan
            { direction = SourceToDestination
            , conflictPolicy = RefuseConflicts
            , items = []
            , conflicts = []
            , operations = PlannedSyncOp DestinationReplica <$> ops
            }

    it "widens read-only entries and reapplies declared modes" $
      withTempDir $ \tmpDir _ -> do
        sourceName <- encodeFS "guard-source"
        destinationName <- encodeFS "guard-destination"
        let sourcePath = tmpDir </> sourceName
        let destinationPath = tmpDir </> destinationName
        FileSystem.writeFile sourcePath "new contents"
        FileSystem.writeFile destinationPath "old contents"
        FileSystem.setPortableWritable destinationPath False
        result <-
          executeReconciliationPlanGuarded (const $ return ()) (const $ return ()) $
            manualPlan
              [ CopyFile sourcePath destinationPath
              , SetEntryMode destinationPath ReadOnly FileSystem.File
              ]
        result `shouldBe` Right ()
        FileSystem.readFile destinationPath `shouldReturn` "new contents"
        finalMode <- FileSystem.getPortableMode destinationPath
        finalMode.writable `shouldBe` False
        FileSystem.setPortableWritable destinationPath True

    it "rejects stale buffered content before any mutation" $ hedgehog $ do
      expected <- forAll $ Gen.bytes $ linear 0 256
      rendered <- forAll $ Gen.bytes $ linear 0 256
      victimContents <- forAll $ Gen.bytes $ linear 0 256
      destinationContents <- forAll $ Gen.bytes $ linear 0 256
      observed <- liftIO $ withTempDir $ \tmpDir _ -> do
        authoritativeName <- encodeFS "authoritative"
        victimName <- encodeFS "victim"
        destinationName <- encodeFS "destination"
        let authoritative = tmpDir </> authoritativeName
            victim = tmpDir </> victimName
            destinationPath = tmpDir </> destinationName
            staleGuard =
              ContentGuard authoritative $ opaqueBytes expected
        FileSystem.writeFile authoritative $ expected <> "\0"
        FileSystem.writeFile victim victimContents
        FileSystem.writeFile destinationPath destinationContents
        result <-
          tryError $
            executeReconciliationPlanGuarded
              (const $ return ())
              (const $ return ())
              ( manualPlan
                  [ RemoveFile victim
                  , WriteContentGuarded
                      (opaqueBytes rendered)
                      staleGuard
                      destinationPath
                  ]
              )
        victimAfter <- FileSystem.readFile victim
        destinationAfter <- FileSystem.readFile destinationPath
        return
          ( case result of Left _ -> True; Right _ -> False
          , victimAfter
          , destinationAfter
          )
      observed === (True, victimContents, destinationContents)

    it "rechecks buffered content immediately before writing" $
      withTempDir $ \tmpDir _ -> do
        authoritativeName <- encodeFS "authoritative"
        destinationName <- encodeFS "destination"
        let authoritative = tmpDir </> authoritativeName
            destinationPath = tmpDir </> destinationName
            guard = ContentGuard authoritative $ opaqueBytes "evaluated"
            operation =
              WriteContentGuarded
                (opaqueBytes "rendered")
                guard
                destinationPath
        FileSystem.writeFile authoritative "evaluated"
        FileSystem.writeFile destinationPath "preserved"
        result <-
          tryError $
            executeReconciliationPlanGuarded
              (\_ -> FileSystem.writeFile authoritative "changed")
              (const $ return ())
              (manualPlan [operation])
        result `shouldSatisfy` \case
          Left _ -> True
          Right _ -> False
        FileSystem.readFile destinationPath `shouldReturn` "preserved"

    it "leaves recreated entries writable without a declared mode" $
      withTempDir $ \tmpDir _ -> do
        sourceName <- encodeFS "guard-source"
        destinationName <- encodeFS "guard-destination"
        let sourcePath = tmpDir </> sourceName
        let destinationPath = tmpDir </> destinationName
        FileSystem.writeFile sourcePath "new contents"
        FileSystem.writeFile destinationPath "old contents"
        FileSystem.setPortableWritable destinationPath False
        result <-
          executeReconciliationPlanGuarded (const $ return ()) (const $ return ()) $
            manualPlan [CopyFile sourcePath destinationPath]
        result `shouldBe` Right ()
        FileSystem.readFile destinationPath `shouldReturn` "new contents"
        -- The widened entry is narrowed back to its prior state, since no
        -- declared mode superseded it:
        finalMode <- FileSystem.getPortableMode destinationPath
        finalMode.writable `shouldBe` False
        FileSystem.setPortableWritable destinationPath True

    it "removes read-only trees" $
      withTempDir $ \tmpDir _ -> do
        treeName <- encodeFS "guard-tree"
        childName <- encodeFS "child"
        fileName' <- encodeFS "file"
        let treePath = tmpDir </> treeName
        let childPath = treePath </> childName
        FileSystem.createDirectories childPath
        FileSystem.writeFile (childPath </> fileName') "contents"
        FileSystem.setPortableWritable (childPath </> fileName') False
        FileSystem.setPortableWritable childPath False
        FileSystem.setPortableWritable treePath False
        result <-
          executeReconciliationPlanGuarded (const $ return ()) (const $ return ()) $
            manualPlan [RemoveDirs treePath]
        result `shouldBe` Right ()
        FileSystem.exists treePath `shouldReturn` False

    it "narrows preserved containers after protected removal" $
      withTempDir $ \tmpDir _ -> do
        rootName <- encodeFS "guard-tree"
        midName <- encodeFS "mid"
        keepName <- encodeFS "keep"
        goneName <- encodeFS "gone"
        fileName' <- encodeFS "file"
        let rootPath = tmpDir </> rootName
        let keepPath = rootPath </> midName </> keepName
        FileSystem.createDirectories keepPath
        FileSystem.writeFile (keepPath </> fileName') "kept"
        FileSystem.createDirectories $ rootPath </> goneName
        FileSystem.setPortableWritable (rootPath </> midName) False
        FileSystem.setPortableWritable rootPath False
        result <-
          executeReconciliationPlanGuarded (const $ return ()) (const $ return ()) $
            manualPlan [RemoveDirsExcept rootPath [keepPath]]
        result `shouldBe` Right ()
        FileSystem.exists (rootPath </> goneName) `shouldReturn` False
        FileSystem.readFile (keepPath </> fileName') `shouldReturn` "kept"
        -- The surviving root and container are narrowed back:
        rootMode <- FileSystem.getPortableMode rootPath
        rootMode.writable `shouldBe` False
        midMode <- FileSystem.getPortableMode (rootPath </> midName)
        midMode.writable `shouldBe` False
        FileSystem.setPortableWritable rootPath True
        FileSystem.setPortableWritable (rootPath </> midName) True

    it "restores prior modes when execution fails" $
      withTempDir $ \tmpDir _ -> do
        sourceName <- encodeFS "guard-source"
        destinationName <- encodeFS "guard-destination"
        missingName <- encodeFS "guard-missing"
        otherName <- encodeFS "guard-other"
        let sourcePath = tmpDir </> sourceName
        let destinationPath = tmpDir </> destinationName
        FileSystem.writeFile sourcePath "new contents"
        FileSystem.writeFile destinationPath "old contents"
        FileSystem.setPortableWritable destinationPath False
        result <-
          tryError $
            executeReconciliationPlanGuarded (const $ return ()) (const $ return ()) $
              manualPlan
                [ CopyFile sourcePath destinationPath
                , CopyFile (tmpDir </> missingName) (tmpDir </> otherName)
                , SetEntryMode destinationPath ReadOnly FileSystem.File
                ]
        result `shouldSatisfy` \case
          Left _ -> True
          Right _ -> False
        -- The widened destination is restored to read-only despite the
        -- failure happening before the declared mode was applied:
        finalMode <- FileSystem.getPortableMode destinationPath
        finalMode.writable `shouldBe` False
        FileSystem.setPortableWritable destinationPath True

  describe "observeModeDrift" $ do
    it "reports destinations that violate their declared mode" $
      withTempDir $ \tmpDir _ -> do
        sourceName <- encodeFS "mode-source"
        destinationName <- encodeFS "mode-destination"
        intermediateName <- encodeFS "mode-intermediate"
        routeName <- encodeFS "mode-route"
        let destinationPath = tmpDir </> destinationName
        FileSystem.writeFile destinationPath "contents"
        FileSystem.setPortableWritable destinationPath True
        let managedWith mode' =
              ManagedCorrespondence
                ( RouteResult
                    (tmpDir </> sourceName)
                    routeName
                    destinationPath
                    FileSystem.File
                    mode'
                    CopyRoute
                    ""
                    mempty
                    identityCodecSpec
                )
                mempty
                FileCorrespondence
                  { source = FileEntry (tmpDir </> sourceName) (File 8)
                  , sourceDelta = Unchanged
                  , intermediate =
                      FileEntry (tmpDir </> intermediateName) (File 8)
                  , destination = FileEntry destinationPath (File 8)
                  , destinationDelta = Unchanged
                  }
        drifted <- observeModeDrift [managedWith ReadOnly]
        [m.route.routeName | (m, _) <- drifted] `shouldBe` [routeName]
        -- A satisfied declaration reports nothing:
        FileSystem.setPortableWritable destinationPath False
        satisfied <- observeModeDrift [managedWith ReadOnly]
        FileSystem.setPortableWritable destinationPath True
        [m.route.routeName | (m, _) <- satisfied] `shouldBe` []
        -- The default mode is never reported:
        defaulted <- observeModeDrift [managedWith DefaultMode]
        [m.route.routeName | (m, _) <- defaulted] `shouldBe` []

  describe "observeReconciliationInput" $ do
    it "compares equal-size files by content" $ withTempDir $ \tmpDir _ -> do
      sourceName <- encodeFS "source-file"
      destinationName <- encodeFS "destination-file"
      intermediateName <- encodeFS "intermediate-file"
      intermediateDir <- encodeFS ".dojang"
      let sourcePath = tmpDir </> sourceName
      let destinationPath = tmpDir </> destinationName
      FileSystem.writeFile sourcePath "same"
      FileSystem.writeFile destinationPath "same"
      environment <- currentEnvironment
      let repository =
            Repository
              tmpDir
              (tmpDir </> intermediateDir)
              (Manifest.manifest mempty mempty mempty mempty mempty)
      let context =
            Context repository environment $
              simpleVariableGetter $
                const $
                  return Nothing
      let correspondence =
            FileCorrespondence
              { source = FileEntry sourcePath (File 4)
              , sourceDelta = Modified
              , intermediate = FileEntry (tmpDir </> intermediateName) (File 4)
              , destination = FileEntry destinationPath (File 4)
              , destinationDelta = Modified
              }
      equalInput <- observeReconciliationInput context DefaultMode correspondence
      equalInput.sourceDestinationComparison `shouldBe` ReplicasEquivalent
      equalInput.destinationRouteState `shouldBe` NotRouted
      equalInput.declaredDestinationMode `shouldBe` DefaultMode
      ((.writable) <$> equalInput.observedDestinationMode) `shouldBe` Just True

      FileSystem.writeFile destinationPath "else"
      differentInput <-
        observeReconciliationInput context Private correspondence
      differentInput.sourceDestinationComparison `shouldBe` ReplicasDifferent
      differentInput.declaredDestinationMode `shouldBe` Private

  describe "execution" $ do
    it "writes arbitrary rendered bytes exactly" $ hedgehog $ do
      bytes <- forAll $ Gen.bytes $ linear 0 4096
      observed <- liftIO $ withTempDir $ \tmpDir _ -> do
        renderedName <- encodeFS "rendered"
        let renderedPath = tmpDir </> renderedName
        executeSyncOp $ WriteContent (opaqueBytes bytes) renderedPath
        FileSystem.readFile renderedPath
      observed === bytes

    it "interprets every synchronization operation" $ withTempDir $ \tmpDir _ -> do
      sourceName <- encodeFS "source-file"
      parentName <- encodeFS "parent"
      nestedName <- encodeFS "nested"
      copiedName <- encodeFS "copied"
      emptyName <- encodeFS "empty"
      let sourcePath = tmpDir </> sourceName
      let nestedPath = tmpDir </> parentName </> nestedName
      let copiedPath = nestedPath </> copiedName
      let emptyPath = nestedPath </> emptyName
      FileSystem.writeFile sourcePath "contents"
      executeSyncOp $ CreateDirs nestedPath
      executeSyncOp $ CopyFile sourcePath copiedPath
      FileSystem.readFile copiedPath `shouldReturn` "contents"
      executeSyncOp $ RemoveFile copiedPath
      FileSystem.exists copiedPath `shouldReturn` False
      executeSyncOp $ CreateDir emptyPath
      FileSystem.isDirectory emptyPath `shouldReturn` True
      executeSyncOp $ RemoveDirs nestedPath
      FileSystem.exists nestedPath `shouldReturn` False

    it "executes a ready plan to convergence" $ withTempDir $ \tmpDir _ -> do
      sourceName <- encodeFS "source-file"
      intermediateName <- encodeFS "intermediate-file"
      destinationName <- encodeFS "destination-file"
      let executionPaths =
            Paths
              { source = tmpDir </> sourceName
              , intermediate = tmpDir </> intermediateName
              , destination = tmpDir </> destinationName
              }
      FileSystem.writeFile executionPaths.source "contents"
      let input =
            makeInput
              executionPaths
              (File 8)
              Missing
              Missing
              Added
              Unchanged
              ReplicasDifferent
              routed
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [input]
      executeReconciliationPlan plan `shouldReturn` Right ()
      FileSystem.readFile executionPaths.intermediate `shouldReturn` "contents"
      FileSystem.readFile executionPaths.destination `shouldReturn` "contents"

    it "observes each operation immediately before execution" $
      withTempDir $ \tmpDir _ -> do
        sourceName <- encodeFS "source-file"
        intermediateName <- encodeFS "intermediate-file"
        destinationName <- encodeFS "destination-file"
        let executionPaths =
              Paths
                { source = tmpDir </> sourceName
                , intermediate = tmpDir </> intermediateName
                , destination = tmpDir </> destinationName
                }
        FileSystem.writeFile executionPaths.source "contents"
        let input =
              makeInput
                executionPaths
                (File 8)
                Missing
                Missing
                Added
                Unchanged
                ReplicasDifferent
                routed
        let plan =
              planReconciliation SourceToDestination RefuseConflicts [input]
        observed <- newIORef []
        executeReconciliationPlanWith
          (modifyIORef' observed . (:))
          plan
          `shouldReturn` Right ()
        reverse <$> readIORef observed `shouldReturn` plan.operations

    it "refuses the whole plan before any mutation" $ withTempDir $ \tmpDir _ -> do
      sourceName <- encodeFS "source-file"
      intermediateName <- encodeFS "intermediate-file"
      destinationName <- encodeFS "destination-file"
      conflictName <- encodeFS "conflict"
      let additionPaths =
            Paths
              { source = tmpDir </> sourceName
              , intermediate = tmpDir </> intermediateName
              , destination = tmpDir </> destinationName
              }
      let conflictPaths =
            Paths
              { source = tmpDir </> conflictName
              , intermediate = tmpDir </> conflictName
              , destination = tmpDir </> conflictName
              }
      FileSystem.writeFile additionPaths.source "contents"
      let addition =
            makeInput
              additionPaths
              (File 8)
              Missing
              Missing
              Added
              Unchanged
              ReplicasDifferent
              routed
      let conflict =
            makeInput
              conflictPaths
              (File 3)
              (File 3)
              (File 3)
              Modified
              Modified
              ReplicasDifferent
              routed
      let plan =
            planReconciliation
              SourceToDestination
              RefuseConflicts
              [addition, conflict]
      observed <- newIORef []
      result <-
        executeReconciliationPlanWith
          (modifyIORef' observed . (:))
          plan
      result `shouldSatisfy` \case
        Left _ -> True
        Right _ -> False
      readIORef observed `shouldReturn` []
      FileSystem.exists additionPaths.intermediate `shouldReturn` False
      FileSystem.exists additionPaths.destination `shouldReturn` False

    it "keeps the recovery copy when the overwritten replica cannot be removed" $
      withTempDir $ \tmpDir _ -> do
        sourceName <- encodeFS "source"
        intermediateName <- encodeFS "intermediate"
        destinationName <- encodeFS "destination"
        let failurePaths =
              Paths
                { source = tmpDir </> sourceName
                , intermediate = tmpDir </> intermediateName
                , destination = tmpDir </> destinationName
                }
        FileSystem.createDirectories failurePaths.source
        FileSystem.writeFile failurePaths.intermediate "recovery"
        let input =
              makeInput
                failurePaths
                (File 8)
                (File 8)
                Missing
                Unchanged
                Removed
                ReplicasDifferent
                routed
        let plan =
              planReconciliation
                DestinationToSource
                RefuseConflicts
                [input]
        sourcePath <- FileSystem.decodePath failurePaths.source
        executeReconciliationPlan plan
          `shouldThrow` (\e -> ioeGetFileName e == Just sourcePath)
        FileSystem.readFile failurePaths.intermediate
          `shouldReturn` "recovery"

-- Making the container unreadable is only expressible on POSIX, where a
-- directory stripped of its execute bit blocks the restoration of the
-- read-only entry inside it.
#ifndef mingw32_HOST_OS
restoreFailureReportSpec :: Spec
restoreFailureReportSpec =
  it "reports restoration failures while unwinding" $
    withTempDir $ \tmpDir _ -> do
      boxName <- encodeFS "guard-box"
      sourceName <- encodeFS "guard-source"
      roName <- encodeFS "ro-file"
      missingName <- encodeFS "guard-missing"
      otherName <- encodeFS "guard-other"
      let boxPath = tmpDir </> boxName
      let roPath = boxPath </> roName
      FileSystem.createDirectories boxPath
      FileSystem.writeFile (tmpDir </> sourceName) "new contents"
      FileSystem.writeFile roPath "old contents"
      FileSystem.setPortableWritable roPath False
      reported <- newIORef ([] :: [IOError])
      result <-
        tryError $
          executeReconciliationPlanGuarded
            (const $ return ())
            (\err -> modifyIORef' reported (err :))
            ReconciliationPlan
              { direction = SourceToDestination
              , conflictPolicy = RefuseConflicts
              , items = []
              , conflicts = []
              , operations =
                  PlannedSyncOp DestinationReplica
                    <$> [ CopyFile (tmpDir </> sourceName) roPath
                        , SetEntryMode boxPath ReadOnly FileSystem.File
                        , CopyFile
                            (tmpDir </> missingName)
                            (tmpDir </> otherName)
                        ]
              }
      FileSystem.setPortableMode boxPath 0o755
      FileSystem.setPortableWritable roPath True
      -- The original operation error still propagates:
      result `shouldSatisfy` \case
        Left _ -> True
        _ -> False
      -- The failed restoration is reported rather than swallowed:
      failures <- readIORef reported
      failures `shouldSatisfy` (not . null)
#else
restoreFailureReportSpec :: Spec
restoreFailureReportSpec = pure ()
#endif


-- Case-variant containment only differs on Windows, where ownership
-- selection accepts nesting whose spelling differs from the enumerated
-- destination paths.
#ifdef mingw32_HOST_OS
windowsCaseProtectionSpec :: Paths -> OsPath -> Spec
windowsCaseProtectionSpec paths route =
  describe "case-variant protection (Windows)" $ do
    it "protects case-variant nested destinations in plans" $ do
      upper <- encodeFS "FILE"
      nested <- encodeFS "nested"
      destinationUpper <- encodeFS "DESTINATION"
      let upperNested = destinationUpper </> upper </> nested
      let removal =
            ( makeInput
                paths
                Missing
                Directory
                Directory
                Removed
                Unchanged
                ReplicasDifferent
                (Routed route)
            )
              { protectedDestinations = [upperNested]
              }
      let plan =
            planReconciliation SourceToDestination RefuseConflicts [removal]
      [op.syncOp | op <- plan.operations, op.replica == DestinationReplica]
        `shouldBe` [RemoveDirsExcept paths.destination [upperNested]]

    it "preserves case-variant protected subtrees during removal" $
      withTempDir $ \tmpDir _ -> do
        rootName <- encodeFS "except-root"
        goneName <- encodeFS "gone"
        keepName <- encodeFS "keep"
        keepUpper <- encodeFS "KEEP"
        fileName' <- encodeFS "file"
        let rootPath = tmpDir </> rootName
        FileSystem.createDirectories $ rootPath </> goneName
        FileSystem.writeFile (rootPath </> goneName </> fileName') "gone"
        FileSystem.createDirectories $ rootPath </> keepName
        FileSystem.writeFile (rootPath </> keepName </> fileName') "kept"
        executeSyncOp $
          RemoveDirsExcept rootPath [rootPath </> keepUpper]
        FileSystem.exists (rootPath </> goneName) `shouldReturn` False
        FileSystem.readFile (rootPath </> keepName </> fileName')
          `shouldReturn` "kept"
#else
windowsCaseProtectionSpec :: Paths -> OsPath -> Spec
windowsCaseProtectionSpec _ _ = pure ()
#endif
