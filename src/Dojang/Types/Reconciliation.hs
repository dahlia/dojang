{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure reconciliation planning and filesystem-plan execution.
module Dojang.Types.Reconciliation
  ( ConflictPolicy (..)
  , PlannedSyncOp (..)
  , ReconciliationConflict (..)
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
  , executeReconciliationPlanWith
  , executeSyncOp
  , isDestructiveSyncOp
  , observeReconciliationInput
  , planReconciliation
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.List (isPrefixOf, nub, sortBy, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import System.FilePattern (FilePattern)
import System.OsPath
  ( OsPath
  , normalise
  , splitDirectories
  , takeDirectory
  )

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.MonadFileSystem qualified as FileSystem (FileType (..))
import Dojang.Types.Context
  ( Context
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , RouteState (..)
  , calculateFileDelta
  , getRouteState
  )
import Dojang.Types.RouteMetadata
  ( PortableMode
  , RouteMode (DefaultMode)
  , portableModeFromBits
  , posixDirectoryModeBits
  , posixFileModeBits
  , satisfiesPortableMode
  )


-- | The direction in which reconciliation makes replicas converge.
data ReconciliationDirection
  = -- | Treat the repository source as authoritative.
    SourceToDestination
  | -- | Treat the machine destination as authoritative.
    DestinationToSource
  deriving (Eq, Ord, Show)


-- | The policy to apply to incompatible changes on both sides.
data ConflictPolicy
  = -- | Report conflicts and refuse to execute the whole plan.
    RefuseConflicts
  | -- | Resolve conflicts in favor of the direction's authoritative side.
    PreferAuthoritative
  deriving (Eq, Ord, Show)


-- | One of the three replicas participating in reconciliation.
data Replica
  = -- | The source file in the repository.
    SourceReplica
  | -- | The common-ancestor snapshot.
    IntermediateReplica
  | -- | The destination file on the current machine.
    DestinationReplica
  deriving (Eq, Ord, Show)


-- | Whether the source and destination contain equivalent states.
data ReplicaComparison
  = -- | Source and destination have equivalent states and contents.
    ReplicasEquivalent
  | -- | Source and destination differ in state or contents.
    ReplicasDifferent
  deriving (Eq, Ord, Show)


-- | A correspondence plus the observations needed by the pure planner.
data ReconciliationInput = ReconciliationInput
  { correspondence :: FileCorrespondence
  -- ^ The observed source, intermediate, and destination entries.
  , sourceDestinationComparison :: ReplicaComparison
  -- ^ Whether source and destination are equivalent to each other.
  , destinationRouteState :: RouteState
  -- ^ The routing state observed for the destination path.
  , declaredDestinationMode :: RouteMode
  -- ^ The portable mode the owning route declares for the destination.
  -- The manifest is authoritative for modes, so a drift from this value
  -- is always reconciled toward it and never treated as a conflict.
  , observedDestinationMode :: Maybe PortableMode
  -- ^ The observed permission state of the destination entry, or
  -- 'Nothing' when the destination is missing or a symbolic link.
  }
  deriving (Eq, Ord, Show)


-- | A two-sided change that cannot be reconciled without a preference.
newtype ReconciliationConflict = ReconciliationConflict
  { correspondence :: FileCorrespondence
  -- ^ The conflicting correspondence.
  }
  deriving (Eq, Ord, Show)


-- | Why a correspondence could not be fully reconciled.
data ReconciliationSkipReason
  = -- | The destination is protected by an ignore pattern.
    IgnoredDestination OsPath FilePattern
  | -- | Reconciliation would require creating a symbolic link, which the
    -- current filesystem operation vocabulary cannot express.
    UnsupportedSymlink
  deriving (Eq, Ord, Show)


-- | The planning result for one correspondence.
data ReconciliationOutcome
  = -- | Every replica already has the desired state.
    NoChange
  | -- | At least one operation will reconcile the correspondence.
    WillReconcile
  | -- | An incompatible two-sided change was refused.
    ConflictDetected
  | -- | Some or all work was skipped for the given reason.
    Skipped ReconciliationSkipReason
  deriving (Eq, Ord, Show)


-- | The result associated with one reconciliation input.
data ReconciliationItem = ReconciliationItem
  { correspondence :: FileCorrespondence
  -- ^ The correspondence being described.
  , desiredEntry :: FileEntry
  -- ^ The authoritative entry whose state and contents are desired for all
  -- replicas.  Its path remains the authoritative replica's path.
  , outcome :: ReconciliationOutcome
  -- ^ The result chosen by the planner.
  }
  deriving (Eq, Ord, Show)


-- | A filesystem synchronization operation.
data SyncOp
  = -- | Remove a directory and all of its children.
    RemoveDirs OsPath
  | -- | Remove a regular file or symbolic link.
    RemoveFile OsPath
  | -- | Copy a regular file from the first path to the second path.
    CopyFile OsPath OsPath
  | -- | Create one directory whose parent already exists.
    CreateDir OsPath
  | -- | Create a directory and any missing ancestors.
    CreateDirs OsPath
  | -- | Apply a declared portable mode to an existing entry.  The
    -- 'FileSystem.FileType' selects whether directory or file permission
    -- bits apply; the default mode is a no-op.
    SetEntryMode OsPath RouteMode FileSystem.FileType
  deriving (Eq, Show)


syncOpOrdKey :: SyncOp -> (OsPath, Int, OsPath)
syncOpOrdKey (RemoveFile path) = (takeDirectory path, 1, path)
syncOpOrdKey (RemoveDirs path) = (path, 2, path)
syncOpOrdKey (CreateDir path) = (path, 3, path)
syncOpOrdKey (CreateDirs path) = (path, 4, path)
syncOpOrdKey (CopyFile _ destination) =
  (takeDirectory destination, 5, destination)
syncOpOrdKey (SetEntryMode path _ _) = (takeDirectory path, 6, path)


instance Ord SyncOp where
  compare a b = compare (syncOpOrdKey a) (syncOpOrdKey b)


-- | A synchronization operation annotated with the replica it mutates.
data PlannedSyncOp = PlannedSyncOp
  { replica :: Replica
  -- ^ The replica changed by the operation.
  , syncOp :: SyncOp
  -- ^ The filesystem operation to execute.
  }
  deriving (Eq, Ord, Show)


-- | A complete, inspectable reconciliation plan.
data ReconciliationPlan = ReconciliationPlan
  { direction :: ReconciliationDirection
  -- ^ The authoritative direction used to build the plan.
  , conflictPolicy :: ConflictPolicy
  -- ^ The conflict policy used to build the plan.
  , items :: [ReconciliationItem]
  -- ^ Per-correspondence planning results in stable order.
  , conflicts :: [ReconciliationConflict]
  -- ^ Incompatible two-sided changes, including overridden conflicts.
  , operations :: [PlannedSyncOp]
  -- ^ Filesystem operations in execution order.
  }
  deriving (Eq, Show)


-- | Observes the facts needed to turn a correspondence into pure planner
-- input.  File contents are compared only when both sides changed and file
-- metadata cannot decide equivalence.
observeReconciliationInput
  :: (MonadFileSystem m, MonadIO m)
  => Context m
  -- ^ The repository context used to observe destination routing.
  -> FileCorrespondence
  -- ^ The correspondence whose remaining facts should be observed.
  -> m ReconciliationInput
  -- ^ Pure planner input containing the correspondence and extra observations.
observeReconciliationInput context correspondence = do
  comparison <- observeComparison correspondence
  (routeState, _) <- getRouteState context correspondence.destination.path
  return
    ReconciliationInput
      { correspondence = correspondence
      , sourceDestinationComparison = comparison
      , destinationRouteState = routeState
      , declaredDestinationMode = DefaultMode
      , observedDestinationMode = Nothing
      }
 where
  observeComparison
    :: (MonadFileSystem m) => FileCorrespondence -> m ReplicaComparison
  observeComparison value =
    case (value.sourceDelta, value.destinationDelta) of
      (Unchanged, Unchanged) -> return ReplicasEquivalent
      (Unchanged, _) -> return ReplicasDifferent
      (_, Unchanged) -> return ReplicasDifferent
      (Removed, Removed) -> return ReplicasEquivalent
      _ -> do
        delta <- calculateFileDelta value.source value.destination
        return $
          if delta == Unchanged
            then ReplicasEquivalent
            else ReplicasDifferent


-- | Produces a pure reconciliation plan from fully observed inputs.
planReconciliation
  :: ReconciliationDirection
  -- ^ The authoritative direction.
  -> ConflictPolicy
  -- ^ The policy for incompatible two-sided changes.
  -> [ReconciliationInput]
  -- ^ Fully observed correspondences to reconcile.
  -> ReconciliationPlan
  -- ^ A deterministic plan containing results, conflicts, and ordered effects.
planReconciliation direction policy inputs =
  ReconciliationPlan
    { direction = direction
    , conflictPolicy = policy
    , items = fmap (\(item, _, _) -> item) planned
    , conflicts = concatMap (\(_, conflicts, _) -> conflicts) planned
    , operations = normalizeOperations $ concatMap (\(_, _, ops) -> ops) planned
    }
 where
  planned =
    fmap (planInput direction policy) $
      sortOn inputOrderKey inputs


inputOrderKey :: ReconciliationInput -> (OsPath, OsPath, OsPath)
inputOrderKey input =
  ( input.correspondence.intermediate.path
  , input.correspondence.source.path
  , input.correspondence.destination.path
  )


data OperationPhase = FirstPhase | SecondPhase | ThirdPhase
  deriving (Eq, Ord, Show)


data TaggedOperation = TaggedOperation
  { phase :: OperationPhase
  , plannedOperation :: PlannedSyncOp
  }
  deriving (Eq, Show)


planInput
  :: ReconciliationDirection
  -> ConflictPolicy
  -> ReconciliationInput
  -> (ReconciliationItem, [ReconciliationConflict], [TaggedOperation])
planInput direction policy input
  | conflict && policy == RefuseConflicts =
      ( ReconciliationItem correspondence authoritative ConflictDetected
      , conflictList
      , []
      )
  | otherwise =
      case planSupportedInput direction input of
        (outcome, operations) ->
          ( ReconciliationItem correspondence authoritative outcome
          , conflictList
          , operations
          )
 where
  correspondence = input.correspondence
  authoritative = case direction of
    SourceToDestination -> correspondence.source
    DestinationToSource -> correspondence.destination
  conflict = isConflict input
  conflictList =
    [ReconciliationConflict correspondence | conflict]


isConflict :: ReconciliationInput -> Bool
isConflict input =
  sourceChanged
    && destinationChanged
    && not bothRemoved
    && input.sourceDestinationComparison == ReplicasDifferent
 where
  correspondence = input.correspondence
  sourceChanged = correspondence.sourceDelta /= Unchanged
  destinationChanged = correspondence.destinationDelta /= Unchanged
  bothRemoved =
    correspondence.sourceDelta == Removed
      && correspondence.destinationDelta == Removed


planSupportedInput
  :: ReconciliationDirection
  -> ReconciliationInput
  -> (ReconciliationOutcome, [TaggedOperation])
planSupportedInput direction input
  | Just reason <- ignoredAuthoritative = (Skipped reason, [])
  | isSymlinkStat authoritative.stat
      && (authoritativeDelta /= Unchanged || targetNeedsUpdate) =
      (Skipped UnsupportedSymlink, [])
  | authoritative.stat == Missing = planRemoval
  | otherwise = planExisting
 where
  correspondence = input.correspondence
  (authoritative, authoritativeDelta, overwritten, overwrittenReplica) =
    case direction of
      SourceToDestination ->
        ( correspondence.source
        , correspondence.sourceDelta
        , correspondence.destination
        , DestinationReplica
        )
      DestinationToSource ->
        ( correspondence.destination
        , correspondence.destinationDelta
        , correspondence.source
        , SourceReplica
        )
  targetNeedsUpdate =
    input.sourceDestinationComparison == ReplicasDifferent
  ignoredAuthoritative =
    case (direction, input.destinationRouteState) of
      (DestinationToSource, Ignored route pattern)
        | authoritativeDelta /= Unchanged || targetNeedsUpdate ->
            Just $ IgnoredDestination route pattern
      _ -> Nothing
  planRemoval :: (ReconciliationOutcome, [TaggedOperation])
  planRemoval =
    let ignored = case (direction, input.destinationRouteState) of
          (SourceToDestination, Ignored route pattern)
            | overwritten.stat /= Missing ->
                Just $ IgnoredDestination route pattern
          _ -> Nothing
        targetOperations =
          case ignored of
            Just _ -> []
            Nothing ->
              tagOperations FirstPhase overwrittenReplica $
                removeEntry overwritten
        intermediateOperations =
          tagOperations SecondPhase IntermediateReplica $
            removeEntry correspondence.intermediate
        operations = targetOperations ++ intermediateOperations
        outcome = case ignored of
          Just reason -> Skipped reason
          Nothing
            | null operations -> NoChange
            | otherwise -> WillReconcile
    in (outcome, operations)

  planExisting :: (ReconciliationOutcome, [TaggedOperation])
  planExisting =
    let ignored = case (direction, input.destinationRouteState) of
          (SourceToDestination, Ignored route pattern)
            | targetNeedsUpdate ->
                Just $ IgnoredDestination route pattern
          _ -> Nothing
        intermediateOperations =
          if authoritativeDelta == Unchanged
            then []
            else
              tagOperations FirstPhase IntermediateReplica $
                replaceEntry authoritative correspondence.intermediate
        stagedIntermediate =
          FileEntry correspondence.intermediate.path authoritative.stat
        targetOperations =
          case ignored of
            Just _ -> []
            Nothing
              | not targetNeedsUpdate -> []
              | otherwise ->
                  tagOperations SecondPhase overwrittenReplica $
                    replaceEntry stagedIntermediate overwritten
        modeOperations =
          tagOperations ThirdPhase DestinationReplica $
            planModeChange input authoritative $
              not $
                null targetOperations
        operations =
          intermediateOperations ++ targetOperations ++ modeOperations
        outcome = case ignored of
          Just reason -> Skipped reason
          Nothing
            | null operations -> NoChange
            | otherwise -> WillReconcile
    in (outcome, operations)


-- | Plans the mode change reconciling the destination toward the mode
-- declared by its owning route.  The manifest is authoritative: an
-- unsatisfied observed mode is reconciled regardless of direction, and
-- a destination that content operations recreate always has its declared
-- mode reapplied.  Ignored destinations and symbolic links are left
-- untouched, and the default mode plans nothing.
planModeChange :: ReconciliationInput -> FileEntry -> Bool -> [SyncOp]
planModeChange input finalEntry destinationTouched
  | input.declaredDestinationMode == DefaultMode = []
  | ignoredDestination = []
  | otherwise = case entryType of
      Nothing -> []
      Just fileType -> case declaredBits fileType of
        Nothing -> []
        Just bits
          | destinationTouched
              || not (satisfied $ portableModeFromBits bits) ->
              [ SetEntryMode
                  input.correspondence.destination.path
                  input.declaredDestinationMode
                  fileType
              ]
          | otherwise -> []
 where
  ignoredDestination = case input.destinationRouteState of
    Ignored _ _ -> True
    _ -> False
  entryType = case finalEntry.stat of
    Directory -> Just FileSystem.Directory
    File _ -> Just FileSystem.File
    _ -> Nothing
  declaredBits FileSystem.Directory =
    posixDirectoryModeBits input.declaredDestinationMode
  declaredBits _ = posixFileModeBits input.declaredDestinationMode
  satisfied declared = case input.observedDestinationMode of
    Just observed -> observed `satisfiesPortableMode` declared
    Nothing -> False


isSymlinkStat :: FileStat -> Bool
isSymlinkStat (Symlink _) = True
isSymlinkStat _ = False


tagOperations :: OperationPhase -> Replica -> [SyncOp] -> [TaggedOperation]
tagOperations phase replica =
  fmap $ TaggedOperation phase . PlannedSyncOp replica


removeEntry :: FileEntry -> [SyncOp]
removeEntry (FileEntry _ Missing) = []
removeEntry (FileEntry path Directory) = [RemoveDirs path]
removeEntry (FileEntry path _) = [RemoveFile path]


replaceEntry :: FileEntry -> FileEntry -> [SyncOp]
replaceEntry from to = case from.stat of
  Missing -> removeEntry to
  Directory -> case to.stat of
    Missing -> [CreateDirs to.path]
    Directory -> []
    _ -> [RemoveFile to.path, CreateDirs to.path]
  File _ -> replaceWithFile
  Symlink _ -> []
 where
  replaceWithFile = case to.stat of
    Missing -> createAndCopy
    File _ -> [CopyFile from.path to.path]
    Symlink _ -> RemoveFile to.path : createAndCopy
    Directory -> RemoveDirs to.path : createAndCopy
  createAndCopy =
    [ CreateDirs $ takeDirectory to.path
    , CopyFile from.path to.path
    ]


normalizeOperations :: [TaggedOperation] -> [PlannedSyncOp]
normalizeOperations operations =
  fmap (.plannedOperation) $
    nub $
      sortBy compareTaggedOperation $
        filter (not . shadowedByRecursiveRemoval operations) operations


shadowedByRecursiveRemoval :: [TaggedOperation] -> TaggedOperation -> Bool
shadowedByRecursiveRemoval operations operation =
  any shadows operations
 where
  candidate = operation.plannedOperation
  candidatePath = syncOpTarget candidate.syncOp
  shadows :: TaggedOperation -> Bool
  shadows other =
    other.plannedOperation.replica == candidate.replica
      && case other.plannedOperation.syncOp of
        RemoveDirs path -> path `strictlyContains` candidatePath
        _ -> False


strictlyContains :: OsPath -> OsPath -> Bool
strictlyContains parent child =
  let parentParts = splitDirectories $ normalise parent
      childParts = splitDirectories $ normalise child
  in length parentParts < length childParts
       && parentParts `isPrefixOf` childParts


compareTaggedOperation :: TaggedOperation -> TaggedOperation -> Ordering
compareTaggedOperation a b = compare (operationKey a) (operationKey b)


operationKey
  :: TaggedOperation
  -> (OperationPhase, Int, Int, Replica, OsPath, SyncOp)
operationKey operation =
  ( operation.phase
  , category
  , depthKey
  , operation.plannedOperation.replica
  , path
  , operation.plannedOperation.syncOp
  )
 where
  syncOperation = operation.plannedOperation.syncOp
  path = syncOpTarget syncOperation
  depth = length $ splitDirectories $ normalise path
  (category, depthKey) = case syncOperation of
    RemoveDirs _ -> (0, negate depth)
    RemoveFile _ -> (0, negate depth)
    CreateDir _ -> (1, depth)
    CreateDirs _ -> (1, depth)
    CopyFile _ _ -> (2, depth)
    SetEntryMode{} -> (3, depth)


syncOpTarget :: SyncOp -> OsPath
syncOpTarget (RemoveDirs path) = path
syncOpTarget (RemoveFile path) = path
syncOpTarget (CopyFile _ destination) = destination
syncOpTarget (CreateDir path) = path
syncOpTarget (CreateDirs path) = path
syncOpTarget (SetEntryMode path _ _) = path


-- | Whether an operation removes an existing filesystem entry.
isDestructiveSyncOp
  :: SyncOp
  -- ^ The operation to classify.
  -> Bool
  -- ^ Whether it removes a file or directory.
isDestructiveSyncOp (RemoveDirs _) = True
isDestructiveSyncOp (RemoveFile _) = True
isDestructiveSyncOp _ = False


-- | Returns the destructive subset of a plan without changing its order.
destructiveOperations
  :: ReconciliationPlan
  -- ^ The plan to inspect.
  -> [PlannedSyncOp]
  -- ^ The destructive operations in their original execution order.
destructiveOperations plan =
  filter (isDestructiveSyncOp . (.syncOp)) plan.operations


-- | Interprets one synchronization operation through 'MonadFileSystem'.
executeSyncOp
  :: (MonadFileSystem m)
  => SyncOp
  -- ^ The operation to execute.
  -> m ()
  -- ^ The interpreted filesystem effect.
executeSyncOp (RemoveDirs path) = removeDirectoryRecursively path
executeSyncOp (RemoveFile path) = removeFile path
executeSyncOp (CopyFile source destination) = copyFile source destination
executeSyncOp (CreateDir path) = createDirectory path
executeSyncOp (CreateDirs path) = createDirectories path
executeSyncOp (SetEntryMode path mode fileType) =
  case bits of
    Nothing -> return ()
    Just bits' -> setPortableMode path bits'
 where
  bits = case fileType of
    FileSystem.Directory -> posixDirectoryModeBits mode
    _ -> posixFileModeBits mode


-- | Executes a plan in order.  A refused conflict returns before the first
-- filesystem mutation.
executeReconciliationPlan
  :: (MonadFileSystem m)
  => ReconciliationPlan
  -- ^ The plan to execute.
  -> m (Either (NonEmpty ReconciliationConflict) ())
  -- ^ Refused conflicts, or success after all ordered operations complete.
executeReconciliationPlan = executeReconciliationPlanWith $ const $ return ()


-- | Executes a plan in order, invoking an observer immediately before each
-- filesystem operation.  A refused conflict returns before the observer or
-- filesystem is touched.
executeReconciliationPlanWith
  :: (MonadFileSystem m)
  => (PlannedSyncOp -> m ())
  -- ^ An action invoked immediately before each operation is executed.
  -> ReconciliationPlan
  -- ^ The plan to execute.
  -> m (Either (NonEmpty ReconciliationConflict) ())
  -- ^ Refused conflicts, or success after all ordered operations complete.
executeReconciliationPlanWith observe plan =
  case (plan.conflictPolicy, NE.nonEmpty plan.conflicts) of
    (RefuseConflicts, Just conflicts) -> return $ Left conflicts
    _ -> do
      forM_ plan.operations $ \operation -> do
        observe operation
        executeSyncOp operation.syncOp
      return $ Right ()
