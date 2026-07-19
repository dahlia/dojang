{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Construction of managed-target records from converged correspondences.
module Dojang.Types.TargetTracking
  ( discardTargetSnapshot
  , discardTargetSnapshotAndEmptyAncestors
  , managedTargetId
  , newTargetSnapshotTransaction
  , observeConvergedManagedTarget
  , observeOrphanStatus
  , observeManagedTarget
  ) where

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError (throwError))
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder
  ( byteString
  , toLazyByteString
  , word32BE
  , word64BE
  )
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Word (Word32)
import Numeric (showHex)
import System.OsPath
  ( OsPath
  , isAbsolute
  , makeRelative
  , normalise
  , splitDirectories
  , takeDirectory
  , toChar
  , unpack
  , (</>)
  )
import Prelude hiding (readFile)

import Dojang.MonadFileSystem (FileType, MonadFileSystem (..))
import Dojang.MonadFileSystem qualified as FileSystem
import Dojang.Types.Context
  ( FileCorrespondence (..)
  , FileDeltaKind (Unchanged)
  , FileEntry (..)
  , FileStat (..)
  , ManagedCorrespondence (..)
  , makeCorrespondBetweenThreeFiles
  , observeFileStat
  , resolveTargetFrom
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (..)
  , OrphanStatus (..)
  , SynchronizationCommand
  , TargetFingerprint (..)
  , destinationPathIdentity
  , isSafeManagedRelativePath
  )
import Dojang.Types.Repository (Repository (..), RouteResult (..))
import Dojang.Types.RouteMetadata (RouteKind (SymlinkRoute))


-- | Derives a stable identifier from one route generation and destination.
--
-- The repository identifies the source root used to make the managed source
-- relative.  The correspondence supplies the route generation and expanded
-- destination.  The returned text is a deterministic SHA-256 identifier;
-- unsafe source paths fail through 'MonadFileSystem'.
managedTargetId
  :: (MonadFileSystem m)
  => Repository
  -- ^ Repository whose source root owns the managed entry.
  -> ManagedCorrespondence
  -- ^ Converged route entry whose stable identity is required.
  -> m Text
  -- ^ Deterministic identifier, or a path-validation failure.
managedTargetId repository managed = do
  destination <- makeAbsolute managed.correspondence.destination.path
  source <- sourceRelative repository managed
  return $
    hashParts
      [ osPathBytes $ normalise managed.route.routeName
      , osPathBytes source
      , pathIdentityBytes $ destinationPathIdentity destination
      , encodeUtf8 managed.route.routeDefinition
      , fileTypeBytes managed.route.fileType
      ]


fileTypeBytes :: FileType -> ByteString.ByteString
fileTypeBytes FileSystem.Directory = "directory"
fileTypeBytes FileSystem.File = "file"
fileTypeBytes FileSystem.Symlink = "symlink"


-- | Allocates a private, unique root for one target-record transaction.
--
-- Records are published only after every baseline in this root has been
-- copied successfully.  Existing records therefore continue to reference the
-- preceding root if construction fails.  The argument is the repository's
-- managed-target snapshot root.  The returned directory is newly created and
-- owned by the caller until state publication succeeds.
newTargetSnapshotTransaction
  :: (MonadFileSystem m)
  => OsPath
  -- ^ Private managed-target snapshot root.
  -> m OsPath
  -- ^ Newly created caller-owned transaction directory.
newTargetSnapshotTransaction root = do
  createDirectories root
  transaction <- writeTemporaryFile root "baseline-set.tmp" ByteString.empty
  removeFile transaction
  createDirectory transaction
  return transaction


-- | Removes one unreferenced target snapshot or transaction tree.
--
-- The argument must be an unreferenced regular file or directory owned by the
-- caller.  Missing paths are ignored, while symbolic links are rejected.
discardTargetSnapshot
  :: (MonadFileSystem m)
  => OsPath
  -- ^ Unreferenced caller-owned snapshot or transaction tree.
  -> m ()
  -- ^ Completion, or an error when safe removal is impossible.
discardTargetSnapshot path = do
  symbolicLink <- isSymlink path
  when symbolicLink $ refuseSymbolicLink path
  directory <- isDirectory path
  file <- isFile path
  if directory
    then removeDirectoryRecursively path
    else when file $ removeFile path


-- | Removes an unreferenced snapshot and its empty private ancestors.
--
-- Ancestor pruning stops at the target-snapshot root.  This reclaims empty
-- generation and transaction directories without removing the shared root.
-- The action returns after the snapshot and every removable empty ancestor
-- have been deleted, or fails when the snapshot escapes the supplied root.
discardTargetSnapshotAndEmptyAncestors
  :: (MonadFileSystem m)
  => OsPath
  -- ^ Private root containing target-snapshot transactions.
  -> OsPath
  -- ^ Unreferenced snapshot or transaction to remove.
  -> m ()
  -- ^ Completion after safe ancestor pruning, or a validation error.
discardTargetSnapshotAndEmptyAncestors root snapshot = do
  parent <- encodePath ".."
  when (snapshot' == root' || not (isWithinRoot parent snapshot')) $ do
    rendered <- decodePath snapshot'
    throwError $
      userError $
        "Refusing to remove target snapshot outside its private root: "
          <> rendered
          <> "."
  discardTargetSnapshot snapshot'
  prune parent $ normalise $ takeDirectory snapshot'
 where
  root' = normalise root
  snapshot' = normalise snapshot

  prune parent directory
    | directory == root' = return ()
    | not $ isWithinRoot parent directory = return ()
    | otherwise = do
        symbolicLink <- isSymlink directory
        when symbolicLink $ refuseSymbolicLink directory
        present <- isDirectory directory
        when present $ do
          entries <- listDirectory directory
          when (null entries) $ do
            removeDirectory directory
            prune parent $ normalise $ takeDirectory directory

  isWithinRoot parent directory =
    let relative = makeRelative root' directory
    in not (isAbsolute relative)
         && case splitDirectories relative of
           [] -> False
           first : _ -> first /= parent


refuseSymbolicLink :: (MonadFileSystem m) => OsPath -> m ()
refuseSymbolicLink path = do
  rendered <- decodePath path
  throwError $
    userError $
      "Refusing to remove symbolic-link snapshot " <> rendered <> "."


-- | Re-observes one selected operation and returns an update only on
-- convergence.
--
-- A successful deletion is represented by @Just (identifier, Nothing)@.
-- 'Nothing' means the operation did not complete and its prior record must be
-- preserved.  The arguments provide the repository, caller-owned transaction
-- root, synchronization command, observation time, and selected
-- correspondence.  A successful non-deletion returns the identifier and new
-- record inside both 'Just' layers.
observeConvergedManagedTarget
  :: (MonadFileSystem m)
  => Repository
  -- ^ Repository whose source root owns the managed entry.
  -> OsPath
  -- ^ Caller-owned transaction root for any new baseline.
  -> SynchronizationCommand
  -- ^ Command responsible for the synchronization.
  -> UTCTime
  -- ^ Time recorded on a newly observed target.
  -> ManagedCorrespondence
  -- ^ Selected correspondence to re-observe.
  -> m (Maybe (Text, Maybe ManagedTarget))
  -- ^ No update, a successful deletion, or a new managed record.
observeConvergedManagedTarget repository snapshotRoot command now managed
  | managed.route.kind == SymlinkRoute = do
      -- A deployment link converged when the destination is a link that
      -- still projects the route source; the stored target string is the
      -- snapshot, so no filesystem baseline is materialized.
      let previous = managed.correspondence
      absoluteSource <- makeAbsolute managed.route.sourcePath
      destinationStat <- observeFileStat previous.destination.path
      case destinationStat of
        Symlink linkTarget
          | resolveTargetFrom previous.destination.path linkTarget
              == normalise absoluteSource -> do
              let refreshed =
                    previous
                      { destination =
                          FileEntry previous.destination.path destinationStat
                      , destinationDelta = Unchanged
                      }
              target <-
                observeManagedTarget
                  repository
                  snapshotRoot
                  command
                  now
                  managed{correspondence = refreshed}
              return $ (\value -> (value.targetId, Just value)) <$> target
        _ -> return Nothing
observeConvergedManagedTarget repository snapshotRoot command now managed = do
  let previous = managed.correspondence
  refreshed <-
    makeCorrespondBetweenThreeFiles
      previous.intermediate.path
      previous.source.path
      previous.destination.path
  if
    | refreshed.sourceDelta /= Unchanged -> return Nothing
    | refreshed.destinationDelta /= Unchanged -> return Nothing
    | otherwise ->
        let converged = managed{correspondence = refreshed}
        in case refreshed.destination.stat of
             Missing -> do
               identifier <- managedTargetId repository converged
               return $ Just (identifier, Nothing)
             Symlink _ -> return Nothing
             _ -> do
               target <-
                 observeManagedTarget repository snapshotRoot command now converged
               return $ (\value -> (value.targetId, Just value)) <$> target


-- | Builds a record only when source, snapshot, and destination converged.
--
-- The repository and correspondence identify the managed entry.  The caller
-- owns the transaction root until state publication, while the command and
-- timestamp describe the observation.  The result is 'Nothing' for an
-- unconverged, missing, or unsupported entry; otherwise it is a record whose
-- immutable baseline was materialized below the transaction root.
observeManagedTarget
  :: (MonadFileSystem m)
  => Repository
  -- ^ Repository whose source root owns the managed entry.
  -> OsPath
  -- ^ Private root for immutable target snapshots.
  -> SynchronizationCommand
  -- ^ Command responsible for the synchronization.
  -> UTCTime
  -- ^ Time recorded on the new target.
  -> ManagedCorrespondence
  -- ^ Correspondence to validate and snapshot.
  -> m (Maybe ManagedTarget)
  -- ^ New record on convergence, or 'Nothing' for unsupported state.
observeManagedTarget repository snapshotRoot command now managed
  | managed.route.kind == SymlinkRoute =
      case correspondence.destination.stat of
        Symlink linkTarget
          | correspondence.destinationDelta == Unchanged ->
              Just <$> makeTarget (SymlinkFingerprint linkTarget)
        _ -> return Nothing
  | correspondence.sourceDelta /= Unchanged = return Nothing
  | correspondence.destinationDelta /= Unchanged = return Nothing
  | otherwise = case correspondence.destination.stat of
      Missing -> return Nothing
      Symlink _ -> return Nothing
      Directory -> Just <$> makeTarget DirectoryFingerprint
      File size -> do
        destination <- makeAbsolute correspondence.destination.path
        contents <- readFile destination
        makeTarget (FileFingerprint size $ digestHex $ SHA256.hash contents)
          >>= return . Just
 where
  correspondence = managed.correspondence
  makeTarget fingerprint = do
    source <- sourceRelative repository managed
    identifier <- managedTargetId repository managed
    destination <- makeAbsolute correspondence.destination.path
    snapshot <- targetSnapshotPath snapshotRoot managed
    materializeSnapshot correspondence.intermediate.path snapshot fingerprint
    return $
      ManagedTarget
        identifier
        (normalise managed.route.routeName)
        source
        managed.route.fileType
        managed.route.kind
        managed.route.mode
        destination
        snapshot
        managed.route.routeDefinition
        managed.route.routeProvenance
        fingerprint
        command
        now


-- | Compares an orphan destination with its immutable baseline.
--
-- Regular files are compared byte-for-byte; a digest is not trusted as a
-- deletion decision.  Directory records compare only the entry itself because
-- descendant records own the managed subtree.  Symbolic links and type changes
-- are treated as modified.  The argument is the persisted record to inspect;
-- the result reports whether its destination is unchanged, modified, or
-- missing.  Filesystem inspection failures are propagated.
observeOrphanStatus
  :: (MonadFileSystem m)
  => ManagedTarget
  -- ^ Persisted orphan record whose destination is inspected.
  -> m OrphanStatus
  -- ^ Current comparison status, or a filesystem inspection failure.
observeOrphanStatus target = do
  destinationPresent <- exists target.destinationPath
  destinationSymlink <- isSymlink target.destinationPath
  if not destinationPresent && not destinationSymlink
    then return OrphanMissing
    else do
      snapshotPresent <- exists target.snapshotPath
      snapshotSymlink <- isSymlink target.snapshotPath
      if not snapshotPresent || destinationSymlink || snapshotSymlink
        then return OrphanModified
        else do
          destinationDirectory <- isDirectory target.destinationPath
          snapshotDirectory <- isDirectory target.snapshotPath
          if destinationDirectory || snapshotDirectory
            then case (destinationDirectory, snapshotDirectory) of
              (True, True) -> return OrphanUnchanged
              _ -> return OrphanModified
            else do
              destinationRegular <- isRegularFile target.destinationPath
              snapshotRegular <- isRegularFile target.snapshotPath
              if not destinationRegular || not snapshotRegular
                then return OrphanModified
                else do
                  destinationContents <- readFile target.destinationPath
                  snapshotContents <- readFile target.snapshotPath
                  return $
                    if destinationContents == snapshotContents
                      then OrphanUnchanged
                      else OrphanModified


sourceRelative
  :: (MonadFileSystem m)
  => Repository
  -> ManagedCorrespondence
  -> m OsPath
sourceRelative repository managed = do
  let relative =
        normalise $
          makeRelative
            repository.sourcePath
            (managed.route.sourcePath </> managed.relativePath)
  unless (isSafeManagedRelativePath relative) $ do
    rendered <- decodePath relative
    throwError $
      userError $
        "Refusing to manage a source path outside the repository: "
          <> rendered
          <> "."
  return relative


targetSnapshotPath
  :: (MonadFileSystem m)
  => OsPath
  -> ManagedCorrespondence
  -> m OsPath
targetSnapshotPath root managed = do
  destination <- makeAbsolute managed.route.destinationPath
  let generation =
        hashParts
          [ osPathBytes managed.route.routeName
          , osPathBytes destination
          , encodeUtf8 managed.route.routeDefinition
          , fileTypeBytes managed.route.fileType
          ]
  generationPath <- encodePath $ Text.unpack generation
  return $ normalise $ root </> generationPath </> managed.relativePath


materializeSnapshot
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> TargetFingerprint
  -> m ()
materializeSnapshot source destination fingerprint = do
  case fingerprint of
    FileFingerprint _ _ -> do
      createDirectories $ takeDirectory destination
      copyFileWithMetadata source destination
    DirectoryFingerprint -> createDirectories destination
    -- The stored link target in the state record is the snapshot; hosts
    -- that cannot create links must still be able to hold the record:
    SymlinkFingerprint _ -> return ()


hashParts :: [ByteString.ByteString] -> Text
hashParts =
  digestHex
    . SHA256.hash
    . LazyByteString.toStrict
    . toLazyByteString
    . foldMap frame
 where
  frame bytes = word64BE (fromIntegral $ ByteString.length bytes) <> byteString bytes


osPathBytes :: OsPath -> ByteString.ByteString
osPathBytes =
  pathIdentityBytes
    . fmap (fromIntegral . ord . toChar)
    . unpack


pathIdentityBytes :: [Word32] -> ByteString.ByteString
pathIdentityBytes =
  LazyByteString.toStrict
    . toLazyByteString
    . foldMap word32BE


digestHex :: ByteString.ByteString -> Text
digestHex = Text.pack . concatMap byteHex . ByteString.unpack
 where
  byteHex byte = case showHex byte "" of
    [digit] -> ['0', digit]
    digits -> digits
