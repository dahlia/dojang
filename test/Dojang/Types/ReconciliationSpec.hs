{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.ReconciliationSpec (spec) where

import System.OsPath (OsPath, encodeFS, takeDirectory, (</>))
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Expectations.Pretty (shouldBe)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (linear)
import Test.Hspec.Expectations.Pretty (shouldReturn, shouldSatisfy)
import Test.Hspec.Hedgehog (MonadGen, forAll, hedgehog, (===))

import Dojang.MonadFileSystem qualified as FileSystem
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , RouteState (..)
  )
import Dojang.Types.Environment.Current (MonadEnvironment (currentEnvironment))
import Dojang.Types.Manifest qualified as Manifest
import Dojang.Types.Reconciliation
  ( ConflictPolicy (..)
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
  , executeSyncOp
  , observeReconciliationInput
  , planReconciliation
  )
import Dojang.Types.Repository (Repository (..))


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
    CopyFile sourcePath destinationPath ->
      Map.insert destinationPath (state Map.! sourcePath) state
    CreateDir path -> Map.insert path ModelDirectory state
    CreateDirs path -> Map.insert path ModelDirectory state


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
      let context = Context repository environment (const $ return Nothing)
      let correspondence =
            FileCorrespondence
              { source = FileEntry sourcePath (File 4)
              , sourceDelta = Modified
              , intermediate = FileEntry (tmpDir </> intermediateName) (File 4)
              , destination = FileEntry destinationPath (File 4)
              , destinationDelta = Modified
              }
      equalInput <- observeReconciliationInput context correspondence
      equalInput.sourceDestinationComparison `shouldBe` ReplicasEquivalent
      equalInput.destinationRouteState `shouldBe` NotRouted

      FileSystem.writeFile destinationPath "else"
      differentInput <- observeReconciliationInput context correspondence
      differentInput.sourceDestinationComparison `shouldBe` ReplicasDifferent

  describe "execution" $ do
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
      result <- executeReconciliationPlan plan
      result `shouldSatisfy` \case
        Left _ -> True
        Right _ -> False
      FileSystem.exists additionPaths.intermediate `shouldReturn` False
      FileSystem.exists additionPaths.destination `shouldReturn` False
