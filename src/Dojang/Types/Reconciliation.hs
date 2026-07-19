{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , executeReconciliationPlanGuarded
  , executeReconciliationPlanWith
  , executeSyncOp
  , isDestructiveSyncOp
  , observeModeDrift
  , observeReconciliationInput
  , planReconciliation
  ) where

import Control.Monad (forM, forM_)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.List (isPrefixOf, nub, sortBy, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import System.FilePattern (FilePattern)
import System.OsPath
  ( OsPath
  , isAbsolute
  , normalise
  , splitDirectories
  , takeDirectory
  , (</>)
  )

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.MonadFileSystem qualified as FileSystem (FileType (..))
import Dojang.Types.Context
  ( Context
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , ManagedCorrespondence (..)
  , RouteState (..)
  , calculateFileDelta
  , getRouteState
  )
import Dojang.Types.Repository (RouteResult (..))
import Dojang.Types.RouteMetadata
  ( PortableMode (..)
  , RouteKind (CopyRoute, SymlinkRoute)
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
  , destinationKind :: RouteKind
  -- ^ Whether the owning route deploys a copied entry or a symbolic
  -- link.  Deployment links are one-way projections of the repository
  -- source: they are created and repaired by apply and never reflected.
  , routeFileType :: FileSystem.FileType
  -- ^ The file type the owning route declares, selecting the intrinsic
  -- link type for deployment links.
  , protectedDestinations :: [OsPath]
  -- ^ Absolute destination roots owned by routes nested inside this
  -- correspondence's route.  Recursive removals must preserve them.
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
  | -- | The destination is a deployment link: a one-way projection of the
    -- repository source that is never reflected back, regardless of
    -- forcing or path selection.
    DeploymentLinkNotReflectable
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
  | -- | Remove a directory's contents except the given protected subtree
    -- roots (and the containers leading to them).  The directory itself is
    -- kept when protected entries remain under it.  Symbolic links are
    -- removed as entries and never followed.
    RemoveDirsExcept OsPath [OsPath]
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
  | -- | Create a symbolic link at the second path pointing at the first.
    -- The 'FileSystem.FileType' selects the intrinsic link type on
    -- Windows.
    CreateSymlink OsPath OsPath FileSystem.FileType
  deriving (Eq, Show)


syncOpOrdKey :: SyncOp -> (OsPath, Int, OsPath)
syncOpOrdKey (RemoveFile path) = (takeDirectory path, 1, path)
syncOpOrdKey (RemoveDirs path) = (path, 2, path)
syncOpOrdKey (RemoveDirsExcept path _) = (path, 2, path)
syncOpOrdKey (CreateDir path) = (path, 3, path)
syncOpOrdKey (CreateDirs path) = (path, 4, path)
syncOpOrdKey (CopyFile _ destination) =
  (takeDirectory destination, 5, destination)
syncOpOrdKey (SetEntryMode path _ _) = (takeDirectory path, 6, path)
syncOpOrdKey (CreateSymlink _ link _) = (takeDirectory link, 5, link)


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
  -> RouteMode
  -- ^ The portable mode declared by the route owning the destination.
  -> FileCorrespondence
  -- ^ The correspondence whose remaining facts should be observed.
  -> m ReconciliationInput
  -- ^ Pure planner input containing the correspondence and extra observations.
observeReconciliationInput context declaredMode correspondence = do
  comparison <- observeComparison correspondence
  (routeState, _) <- getRouteState context correspondence.destination.path
  observedMode <- case correspondence.destination.stat of
    File _ -> observeDestinationMode
    Directory -> observeDestinationMode
    _ -> return Nothing
  return
    ReconciliationInput
      { correspondence = correspondence
      , sourceDestinationComparison = comparison
      , destinationRouteState = routeState
      , declaredDestinationMode = declaredMode
      , observedDestinationMode = observedMode
      , destinationKind = CopyRoute
      , routeFileType = FileSystem.File
      , protectedDestinations = []
      }
 where
  observeDestinationMode =
    (Just <$> getPortableMode correspondence.destination.path)
      `catchError` const (return Nothing)
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


-- | Observes destinations whose permission state does not satisfy the
-- portable mode declared by their owning route.  Destinations that are
-- missing, symbolic links, or covered by an unobservable platform field
-- are not reported.
observeModeDrift
  :: (MonadFileSystem m)
  => [ManagedCorrespondence]
  -- ^ The managed correspondences to inspect.
  -> m [(ManagedCorrespondence, PortableMode)]
  -- ^ Drifted correspondences paired with their observed modes.
observeModeDrift managed =
  fmap (concat :: [[a]] -> [a]) $ forM managed $ \m -> do
    let declared = m.route.mode
    let path = m.correspondence.destination.path
    let entryType = case m.correspondence.destination.stat of
          Directory -> Just FileSystem.Directory
          File _ -> Just FileSystem.File
          _ -> Nothing
    let declaredBits = case entryType of
          Just FileSystem.Directory -> posixDirectoryModeBits declared
          Just _ -> posixFileModeBits declared
          Nothing -> Nothing
    case declaredBits of
      Nothing -> return []
      Just bits -> do
        observed <-
          (Just <$> getPortableMode path)
            `catchError` const (return Nothing)
        return $ case observed of
          Just observed'
            | not $
                observed' `satisfiesPortableMode` portableModeFromBits bits ->
                [(m, observed')]
          _ -> []


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
  | input.destinationKind == SymlinkRoute =
      planDeploymentLink direction policy input
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


-- | Plans a deployment-link route: the destination is a one-way
-- projection of the repository source.  Apply creates or repairs the
-- link; reflection is refused categorically, including under forcing.
-- A broken link whose stored target is already correct is converged:
-- the missing source is a source-side problem, not a missing
-- destination.
planDeploymentLink
  :: ReconciliationDirection
  -> ConflictPolicy
  -> ReconciliationInput
  -> (ReconciliationItem, [ReconciliationConflict], [TaggedOperation])
planDeploymentLink DestinationToSource _ input =
  ( ReconciliationItem
      input.correspondence
      input.correspondence.source
      (Skipped DeploymentLinkNotReflectable)
  , []
  , []
  )
planDeploymentLink SourceToDestination policy input =
  case destination.stat of
    Symlink target
      | resolveFrom destination.path target
          == resolveFrom destination.path desiredTarget ->
          converged
    Missing ->
      reconciled [CreateDirs $ takeDirectory destination.path, createLink]
    Symlink _ -> reconciled [RemoveFile destination.path, createLink]
    _
      | policy == PreferAuthoritative ->
          reconciled $ removeEntry destination ++ [createLink]
      | otherwise ->
          ( ReconciliationItem correspondence desired ConflictDetected
          , [ReconciliationConflict correspondence]
          , []
          )
 where
  correspondence = input.correspondence
  destination = correspondence.destination
  desiredTarget = correspondence.source.path
  desired = FileEntry destination.path $ Symlink desiredTarget
  createLink =
    CreateSymlink desiredTarget destination.path input.routeFileType
  converged =
    (ReconciliationItem correspondence desired NoChange, [], [])
  reconciled operations =
    ( ReconciliationItem correspondence desired WillReconcile
    , []
    , tagOperations SecondPhase DestinationReplica operations
    )


-- | Resolves a symbolic-link target from the directory containing the
-- link, so semantically correct relative targets compare equal to the
-- absolute path they denote.
resolveFrom :: OsPath -> OsPath -> OsPath
resolveFrom link target
  | isAbsolute target = normalise target
  | otherwise = normalise $ takeDirectory link </> target


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
                protectRemovals input overwrittenReplica $
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
                    protectRemovals input overwrittenReplica $
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


-- | Converts recursive destination removals into protected removals when
-- routes nested inside this route own subtrees under the removed path.
-- Only destination-replica removals are converted: the repository and the
-- intermediate contain no nested-owned entries.
protectRemovals :: ReconciliationInput -> Replica -> [SyncOp] -> [SyncOp]
protectRemovals input replica operations
  | replica /= DestinationReplica = operations
  | null input.protectedDestinations = operations
  | otherwise = fmap protect operations
 where
  protect :: SyncOp -> SyncOp
  protect (RemoveDirs path) =
    case protectedUnder path of
      [] -> RemoveDirs path
      protected -> RemoveDirsExcept path protected
  protect operation = operation
  protectedUnder :: OsPath -> [OsPath]
  protectedUnder path =
    [ protected
    | protected <- input.protectedDestinations
    , splitDirectories (normalise path)
        `isPrefixOf` splitDirectories (normalise protected)
    ]


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
    RemoveDirsExcept _ _ -> (0, negate depth)
    RemoveFile _ -> (0, negate depth)
    CreateDir _ -> (1, depth)
    CreateDirs _ -> (1, depth)
    CopyFile _ _ -> (2, depth)
    CreateSymlink{} -> (2, depth)
    SetEntryMode{} -> (3, depth)


syncOpTarget :: SyncOp -> OsPath
syncOpTarget (RemoveDirs path) = path
syncOpTarget (RemoveDirsExcept path _) = path
syncOpTarget (RemoveFile path) = path
syncOpTarget (CopyFile _ destination) = destination
syncOpTarget (CreateDir path) = path
syncOpTarget (CreateDirs path) = path
syncOpTarget (SetEntryMode path _ _) = path
syncOpTarget (CreateSymlink _ link _) = link


-- | Whether an operation removes an existing filesystem entry.
isDestructiveSyncOp
  :: SyncOp
  -- ^ The operation to classify.
  -> Bool
  -- ^ Whether it removes a file or directory.
isDestructiveSyncOp (RemoveDirs _) = True
isDestructiveSyncOp (RemoveDirsExcept _ _) = True
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
executeSyncOp (RemoveDirsExcept path protected) =
  removeDirsExcept path protected
executeSyncOp (RemoveFile path) = removeFile path
executeSyncOp (CopyFile source destination) = copyFile source destination
executeSyncOp (CreateDir path) = createDirectory path
executeSyncOp (CreateDirs path) = createDirectories path
executeSyncOp (CreateSymlink target link fileType) =
  createSymbolicLink target link fileType
executeSyncOp (SetEntryMode path mode fileType) =
  case bits of
    Nothing -> return ()
    Just bits' -> setPortableMode path bits'
 where
  bits = case fileType of
    FileSystem.Directory -> posixDirectoryModeBits mode
    _ -> posixFileModeBits mode


-- | Removes a directory's contents while preserving the protected
-- subtree roots and every container on the way to them.  Symbolic links
-- are removed as entries and never followed.
removeDirsExcept
  :: forall m
   . (MonadFileSystem m)
  => OsPath
  -- ^ The directory whose contents should be removed.
  -> [OsPath]
  -- ^ The protected subtree roots.
  -> m ()
removeDirsExcept root protected = go root
 where
  protected' = fmap normalise protected
  isProtectedRoot :: OsPath -> Bool
  isProtectedRoot path = normalise path `elem` protected'
  containsProtected :: OsPath -> Bool
  containsProtected path =
    any (\p -> path' `isPrefixOf'` p) protected'
   where
    path' = normalise path
  isPrefixOf' :: OsPath -> OsPath -> Bool
  isPrefixOf' ancestor path =
    splitDirectories ancestor `isPrefixOf` splitDirectories path
  go :: OsPath -> m ()
  go path
    | isProtectedRoot path = return ()
    | otherwise = do
        isLink <- isSymlink path
        if isLink
          then
            -- A symbolic link on the way to a protected root must not be
            -- traversed: removing it would lose the protected entries
            -- reachable through it, and recursing would delete through it.
            if containsProtected path
              then return ()
              else removeFile path
          else
            if not $ containsProtected path
              then do
                isDir <- isDirectory path
                if isDir
                  then removeDirectoryRecursively path
                  else removeFile path
              else do
                entries <- listDirectory path
                forM_ entries $ \entry -> go $ path </> entry


-- | Executes a plan in order.  A refused conflict returns before the first
-- filesystem mutation.
executeReconciliationPlan
  :: (MonadFileSystem m)
  => ReconciliationPlan
  -- ^ The plan to execute.
  -> m (Either (NonEmpty ReconciliationConflict) ())
  -- ^ Refused conflicts, or success after all ordered operations complete.
executeReconciliationPlan = executeReconciliationPlanWith $ const $ return ()


-- | Executes a plan like 'executeReconciliationPlanWith', but treats
-- read-only permissions as a desired final state rather than a
-- precondition: entries (and their parents) that an operation must mutate
-- are temporarily made writable first.  After the plan succeeds, widened
-- entries whose declared mode was not reapplied by a 'SetEntryMode'
-- operation are narrowed back; if any operation fails, every widened entry
-- that still exists is restored in reverse order before the error is
-- rethrown.
executeReconciliationPlanGuarded
  :: forall m
   . (MonadFileSystem m)
  => (PlannedSyncOp -> m ())
  -- ^ An action invoked immediately before each operation is executed.
  -> ReconciliationPlan
  -- ^ The plan to execute.
  -> m (Either (NonEmpty ReconciliationConflict) ())
  -- ^ Refused conflicts, or success after all ordered operations complete.
executeReconciliationPlanGuarded observe plan =
  case (plan.conflictPolicy, NE.nonEmpty plan.conflicts) of
    (RefuseConflicts, Just conflicts) -> return $ Left conflicts
    _ -> do
      widened <- go [] plan.operations
      restoreAll widened
      return $ Right ()
 where
  go :: [OsPath] -> [PlannedSyncOp] -> m [OsPath]
  go widened [] = return widened
  go widened (operation : rest) = do
    newlyWidened <-
      widenForSyncOp operation.syncOp
        `catchError` \err -> restoreAll widened >> throwError err
    let widened' = newlyWidened ++ widened
    ( do
        observe operation
        executeSyncOp operation.syncOp
      )
      `catchError` \err -> restoreAll widened' >> throwError err
    -- A reapplied declared mode supersedes the widening, and a removed
    -- entry no longer has a prior mode to restore (a later entry at the
    -- same path, such as a deployment link, is a different entry):
    let widened'' = case operation.syncOp of
          SetEntryMode path _ _ -> filter (/= normalise path) widened'
          RemoveFile path -> dropUnder path widened'
          RemoveDirs path -> dropUnder path widened'
          RemoveDirsExcept path _ -> dropUnder path widened'
          _ -> widened'
    go widened'' rest
  dropUnder :: OsPath -> [OsPath] -> [OsPath]
  dropUnder root =
    filter $ \path ->
      not $
        splitDirectories (normalise root)
          `isPrefixOf` splitDirectories path
  restoreAll :: [OsPath] -> m ()
  restoreAll paths =
    forM_ paths $ \path ->
      ( do
          isLink <- isSymlink path
          stillExists <- exists path
          if stillExists && not isLink
            then setPortableWritable path False
            else return ()
      )
        `catchError` const (return ())
  widenForSyncOp :: SyncOp -> m [OsPath]
  widenForSyncOp (RemoveFile path) = do
    entry <- widenIfReadOnly path
    parent <- widenIfReadOnly $ takeDirectory path
    return $ entry ++ parent
  widenForSyncOp (RemoveDirs path) = do
    parent <- widenIfReadOnly $ takeDirectory path
    tree <- widenTree (const False) path
    return $ tree ++ parent
  widenForSyncOp (RemoveDirsExcept path protected) = do
    parent <- widenIfReadOnly $ takeDirectory path
    let protected' = fmap normalise protected
    tree <- widenTree ((`elem` protected') . normalise) path
    return $ tree ++ parent
  widenForSyncOp (CopyFile _ destination) = do
    entry <- widenIfReadOnly destination
    parent <- widenIfReadOnly $ takeDirectory destination
    return $ entry ++ parent
  widenForSyncOp (CreateDir path) =
    widenIfReadOnly $ takeDirectory path
  widenForSyncOp (CreateDirs path) =
    widenNearestExistingAncestor $ takeDirectory path
  widenForSyncOp (SetEntryMode _ _ _) = return []
  widenForSyncOp (CreateSymlink _ link _) =
    widenIfReadOnly $ takeDirectory link
  widenIfReadOnly :: OsPath -> m [OsPath]
  widenIfReadOnly path = do
    present <- exists path
    if not present
      then return []
      else do
        mode <-
          (Just <$> getPortableMode path)
            `catchError` const (return Nothing)
        case mode of
          Just mode' | not mode'.writable -> do
            setPortableWritable path True
            return [normalise path]
          _ -> return []
  widenNearestExistingAncestor :: OsPath -> m [OsPath]
  widenNearestExistingAncestor path = do
    present <- exists path
    if present
      then widenIfReadOnly path
      else do
        let parent = takeDirectory path
        if parent == path
          then return []
          else widenNearestExistingAncestor parent
  widenTree :: (OsPath -> Bool) -> OsPath -> m [OsPath]
  widenTree isProtected path
    | isProtected path = return []
    | otherwise = do
        isDir <- isDirectory path
        isLink <- isSymlink path
        root <- widenIfReadOnly path
        if isDir && not isLink
          then do
            entries <- listDirectory path
            nested <- mapM (widenTree isProtected . (path </>)) entries
            return $ concat nested ++ root
          else return root


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
