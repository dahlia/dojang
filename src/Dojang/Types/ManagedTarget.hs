{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Machine-local records describing destinations managed by Dojang.
module Dojang.Types.ManagedTarget
  ( CurrentEntry (..)
  , CurrentRoute (..)
  , ManagedTarget (..)
  , OrphanReason (..)
  , OrphanStatus (..)
  , SynchronizationCommand (..)
  , TargetFingerprint (..)
  , classifyOrphan
  , destinationPathIdentity
  , equalDestinationPath
  , isSafeManagedRelativePath
  , makeCurrentEntries
  , makeCurrentRouteAbsolute
  , makeCurrentRoutes
  , mergeConvergedTargets
  , selectOrphanRecords
  , unreachableSnapshots
  ) where

import Data.Char (ord, toLower)
import Data.List (isPrefixOf, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (Down))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word32)
import System.IO.Unsafe (unsafePerformIO)
import System.Info (os)
import System.OsPath
  ( OsPath
  , encodeFS
  , isAbsolute
  , makeRelative
  , normalise
  , splitDirectories
  , toChar
  , unpack
  , (</>)
  )

import Dojang.MonadFileSystem qualified as FileSystem
import Dojang.Types.Context
  ( FileCorrespondence (..)
  , FileEntry (..)
  , FileStat (Missing)
  , ManagedCorrespondence (..)
  )
import Dojang.Types.Repository (RouteResult (..))


-- | The content identity recorded after a successful synchronization.
data TargetFingerprint
  = -- | The destination was a regular file with the given size and SHA-256.
    FileFingerprint Integer Text
  | -- | The destination was a directory.
    DirectoryFingerprint
  deriving (Eq, Ord, Show)


-- | The command that most recently synchronized a destination.
data SynchronizationCommand = Applied | Reflected
  deriving (Eq, Ord, Show)


-- | One successfully synchronized destination entry.
data ManagedTarget = ManagedTarget
  { targetId :: Text
  -- ^ Stable identifier for this route entry.
  , routeName :: OsPath
  -- ^ Manifest route name relative to the repository root.
  , sourcePath :: OsPath
  -- ^ Source entry path relative to the repository root.
  , routeType :: FileSystem.FileType
  -- ^ Whether the producing route managed one file or a directory tree.
  , destinationPath :: OsPath
  -- ^ Fully expanded destination path.
  , snapshotPath :: OsPath
  -- ^ Immutable baseline used to detect later divergence.
  , routeDefinition :: Text
  -- ^ Canonical selected route definition.
  , routeProvenance :: Map Text Text
  -- ^ Environment facts and privacy-preserving input fingerprints.
  , fingerprint :: TargetFingerprint
  -- ^ Destination identity after the synchronization.
  , updatedBy :: SynchronizationCommand
  -- ^ Command that most recently updated this record.
  , updatedTime :: UTCTime
  -- ^ Time at which this record was updated.
  }
  deriving (Eq, Ord, Show)


-- | The part of a currently applicable route needed for orphan detection.
data CurrentRoute = CurrentRoute
  { routeName :: OsPath
  -- ^ Manifest route name relative to the repository root.
  , destinationPath :: OsPath
  -- ^ Currently expanded destination path.
  , routeDefinition :: Text
  -- ^ Canonical selected route definition.
  , fileType :: FileSystem.FileType
  -- ^ Whether this route manages one file or a directory tree.
  }
  deriving (Eq, Ord, Show)


-- | One entry currently produced by an applicable route.
data CurrentEntry = CurrentEntry
  { routeName :: OsPath
  -- ^ Normalized manifest route name.
  , sourcePath :: OsPath
  -- ^ Normalized source entry relative to the repository root.
  }
  deriving (Eq, Ord, Show)


-- | Why a previously managed destination is no longer active.
data OrphanReason
  = -- | The manifest no longer contains an applicable route with this name.
    RouteRemoved
  | -- | The route still exists, but its selected definition changed.
    RouteChanged
  | -- | The same route now expands to another destination.
    DestinationChanged
  | -- | The route remains applicable but no longer produces this entry.
    EntryRemoved
  deriving (Eq, Ord, Show)


-- | How an orphan destination differs from its last snapshot.
data OrphanStatus = OrphanUnchanged | OrphanModified | OrphanMissing
  deriving (Eq, Ord, Show)


-- | Produces the normalized code units used for native destination identity.
--
-- Windows path identity is case-insensitive.  POSIX path identity preserves
-- case and every surrogate-escaped filesystem byte.
destinationPathIdentity :: OsPath -> [Word32]
destinationPathIdentity = fmap canonicalUnit . unpack . normalise
 where
  canonicalUnit value =
    fromIntegral $
      ord $
        if os == "mingw32"
          then toLower $ toChar value
          else toChar value


-- | Compares two destination paths using the host platform's native semantics.
equalDestinationPath :: OsPath -> OsPath -> Bool
equalDestinationPath left right =
  destinationPathIdentity left == destinationPathIdentity right


-- | Determines whether a managed record is orphaned by the current routes.
classifyOrphan
  :: Map OsPath CurrentRoute
  -- ^ Currently applicable routes indexed by route name.
  -> Set CurrentEntry
  -- ^ Entries currently produced by the applicable routes.
  -> ManagedTarget
  -- ^ Persisted record to classify.
  -> Maybe OrphanReason
  -- ^ The reason it is orphaned, or 'Nothing' while it remains active.
classifyOrphan routes entries target = case Map.lookup target.routeName routes of
  Nothing -> Just RouteRemoved
  Just route
    | route.fileType /= target.routeType -> Just RouteChanged
    | route.routeDefinition /= target.routeDefinition -> Just RouteChanged
    | not $
        equalDestinationPath
          (expectedDestination route target)
          target.destinationPath ->
        Just DestinationChanged
    | currentEntry target `Set.notMember` entries -> Just EntryRemoved
    | otherwise -> Nothing


-- | Builds the normalized entry index used for orphan detection.
--
-- File routes remain present while their route is applicable, including when
-- they currently represent a deletion.  Directory routes contribute only
-- entries that still exist in the source tree; intermediate-only paths are
-- synchronization history rather than entries produced by the current route.
makeCurrentEntries :: [ManagedCorrespondence] -> Set CurrentEntry
makeCurrentEntries =
  Set.fromList . fmap fromCorrespondence . filter isCurrentlyProduced
 where
  isCurrentlyProduced :: ManagedCorrespondence -> Bool
  isCurrentlyProduced managed =
    managed.route.fileType /= FileSystem.Directory
      || managed.correspondence.source.stat /= Missing

  fromCorrespondence :: ManagedCorrespondence -> CurrentEntry
  fromCorrespondence managed =
    CurrentEntry
      (normalise managed.route.routeName)
      ( normalise $
          managed.route.routeName </> managed.relativePath
      )


currentEntry :: ManagedTarget -> CurrentEntry
currentEntry target =
  CurrentEntry
    (normalise target.routeName)
    (normalise target.sourcePath)


-- | Resolves a current route's destination root before orphan comparison.
--
-- Persisted target destinations are absolute, so callers must apply this to
-- current routes whose path expressions may expand to relative paths.
makeCurrentRouteAbsolute
  :: (FileSystem.MonadFileSystem m)
  => CurrentRoute
  -> m CurrentRoute
makeCurrentRouteAbsolute route = do
  destination <- FileSystem.makeAbsolute route.destinationPath
  return $
    CurrentRoute
      route.routeName
      (normalise destination)
      route.routeDefinition
      route.fileType


-- | Builds the normalized current-route index used for orphan detection.
--
-- Both the map keys and the embedded route names are normalized so equivalent
-- manifest spellings identify the same persisted target generation.
makeCurrentRoutes
  :: (FileSystem.MonadFileSystem m)
  => [RouteResult]
  -> m (Map OsPath CurrentRoute)
makeCurrentRoutes routes = Map.fromList <$> mapM makeCurrentRoute routes
 where
  makeCurrentRoute
    :: (FileSystem.MonadFileSystem m)
    => RouteResult
    -> m (OsPath, CurrentRoute)
  makeCurrentRoute route = do
    let routeName = normalise route.routeName
    absolute <-
      makeCurrentRouteAbsolute $
        CurrentRoute
          routeName
          route.destinationPath
          route.routeDefinition
          route.fileType
    return (routeName, absolute)


expectedDestination :: CurrentRoute -> ManagedTarget -> OsPath
expectedDestination route target = case route.fileType of
  FileSystem.Directory ->
    normalise $
      route.destinationPath
        </> makeRelative target.routeName target.sourcePath
  _ -> normalise route.destinationPath


-- | Applies observations for operations known to have converged.
--
-- An absent observation means that the operation did not complete and is not
-- passed to this function.  A present 'Nothing' removes a record after a
-- successful deletion.  Replaced and removed records are returned so their
-- baselines can be reclaimed after the new state is published.
mergeConvergedTargets
  :: Map Text ManagedTarget
  -> [(Text, Maybe ManagedTarget)]
  -> (Map Text ManagedTarget, [ManagedTarget])
mergeConvergedTargets existing observations =
  (foldl apply existing observations, superseded)
 where
  apply records (identifier, target) =
    case target of
      Nothing -> Map.delete identifier records
      Just value -> Map.insert identifier value records
  superseded =
    [ old
    | (identifier, replacement) <- observations
    , Just old <- [Map.lookup identifier existing]
    , maybe True ((/= old.snapshotPath) . (.snapshotPath)) replacement
    ]


-- | Selects records by ID only when every selected record remains orphaned.
--
-- Callers should use this after reloading state under the repository lock so a
-- concurrently refreshed active record cannot be removed through a stale ID.
selectOrphanRecords
  :: Map OsPath CurrentRoute
  -- ^ Currently applicable routes indexed by route name.
  -> Set CurrentEntry
  -- ^ Entries currently produced by the applicable routes.
  -> Set Text
  -- ^ Target identifiers selected before the lock was acquired.
  -> Map Text ManagedTarget
  -- ^ Freshly reloaded managed-target records.
  -> Maybe [ManagedTarget]
  -- ^ Selected orphan records, or 'Nothing' if any selected record is active.
selectOrphanRecords routes entries identifiers records =
  if Map.keysSet selectedRecords /= identifiers
    || any ((== Nothing) . classifyOrphan routes entries) selected
    then Nothing
    else Just selected
 where
  selectedRecords =
    Map.filterWithKey (\identifier _ -> identifier `Set.member` identifiers) records
  selected = Map.elems selectedRecords


-- | Selects snapshot paths that are no longer reachable from kept records.
--
-- Results are deterministic and deepest-first.  A candidate that is an
-- ancestor or descendant of a kept snapshot is retained so cleanup cannot
-- remove data observed through a live directory record.
unreachableSnapshots
  :: Set OsPath
  -- ^ Snapshot paths still referenced by retained records.
  -> [OsPath]
  -- ^ Snapshot paths referenced only by records being removed.
  -> [OsPath]
  -- ^ Safe cleanup candidates in deepest-first order.
unreachableSnapshots kept removed =
  sortOn ordering $
    Set.toList $
      Set.fromList
        [ normalise candidate
        | candidate <- removed
        , all
            (\keptPath -> not $ isWithin candidate keptPath || isWithin keptPath candidate)
            $ Set.toList kept
        ]
 where
  ordering path = Down (length $ splitDirectories path, path)


isWithin :: OsPath -> OsPath -> Bool
isWithin parent child =
  parentComponents `isPrefixOf` childComponents
 where
  parentComponents =
    destinationPathIdentity <$> splitDirectories (normalise parent)
  childComponents =
    destinationPathIdentity <$> splitDirectories (normalise child)


parentDirectory :: OsPath
parentDirectory = unsafePerformIO $ encodeFS ".."
{-# NOINLINE parentDirectory #-}


-- | Tests whether a managed source path stays below the repository root.
isSafeManagedRelativePath :: OsPath -> Bool
isSafeManagedRelativePath value =
  not (isAbsolute value)
    && parentDirectory `notElem` splitDirectories value
