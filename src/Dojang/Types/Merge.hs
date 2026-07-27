{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Stable text inputs and isolated files for external three-way merges.
module Dojang.Types.Merge
  ( MergeCommitReplica (..)
  , MergeInputError (..)
  , MergeInputRole (..)
  , MergeResultError (..)
  , MergeTextInput (..)
  , MergeWorkspace (..)
  , mergeCommitOrder
  , observeMergeTextInput
  , prepareMergeWorkspace
  , readMergeResult
  , revalidateMergeTextInput
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text.Encoding qualified as Text
import System.OsPath (OsPath, (</>))
import Prelude hiding (writeFile)

import Dojang.MonadFileSystem
  ( FileModeSnapshot (FileModeSnapshot)
  , FileSnapshot
  , MonadFileSystem (..)
  , fileSnapshotIdentity
  )


-- | Which authoritative replica supplied a merge input.
data MergeInputRole
  = -- | The repository source file.
    SourceInput
  | -- | The previous intermediate snapshot used as the common ancestor.
    BaseInput
  | -- | The deployed destination file.
    DestinationInput
  deriving (Eq, Ord, Show)


-- | Why an authoritative replica cannot be used as a merge input.
data MergeInputError
  = -- | The required replica is absent.
    MissingMergeInput MergeInputRole OsPath
  | -- | The replica is not a regular file.
    UnsupportedMergeInput MergeInputRole OsPath
  | -- | The replica changed while it was being observed.
    ChangedMergeInput MergeInputRole OsPath
  | -- | The replica contains a NUL byte and is treated as binary.
    NulMergeInput MergeInputRole OsPath
  | -- | The replica is not valid UTF-8.
    InvalidUtf8MergeInput MergeInputRole OsPath
  deriving (Eq, Show)


-- | A regular UTF-8 text input bound to its filesystem identity and mode.
data MergeTextInput = MergeTextInput
  { role :: MergeInputRole
  -- ^ Authoritative replica represented by this input.
  , path :: OsPath
  -- ^ Original authoritative path.
  , snapshot :: FileSnapshot
  -- ^ Identity and change metadata captured with the contents.
  , modeSnapshot :: FileModeSnapshot
  -- ^ Entry identity and portable mode captured with the contents.
  , contents :: ByteString
  -- ^ Exact UTF-8 bytes copied into the isolated workspace.
  }
  deriving (Eq, Show)


-- | Isolated regular files passed to a shell-free merge driver.
data MergeWorkspace = MergeWorkspace
  { root :: OsPath
  -- ^ Owner-only directory containing this conflict's files.
  , source :: OsPath
  -- ^ Isolated source input.
  , base :: OsPath
  -- ^ Isolated common-ancestor input.
  , destination :: OsPath
  -- ^ Isolated destination input.
  , result :: OsPath
  -- ^ Driver result initialized from the destination input.
  }
  deriving (Eq, Show)


-- | Why a driver result cannot be committed.
data MergeResultError
  = -- | The result file is absent.
    MissingMergeResult OsPath
  | -- | The result path is not a regular file.
    UnsupportedMergeResult OsPath
  | -- | The result changed while it was being read.
    ChangedMergeResult OsPath
  | -- | The result contains a NUL byte and is treated as binary.
    NulMergeResult OsPath
  | -- | The result is not valid UTF-8.
    InvalidUtf8MergeResult OsPath
  deriving (Eq, Show)


-- | Replica commit order used after a driver resolves one conflict.
--
-- The intermediate snapshot is last so it never claims convergence before
-- both authoritative replicas contain the accepted result.
data MergeCommitReplica
  = -- | Repository source.
    SourceCommitReplica
  | -- | Deployed destination.
    DestinationCommitReplica
  | -- | Intermediate common-ancestor snapshot.
    IntermediateCommitReplica
  deriving (Eq, Ord, Show)


-- | The fixed source, destination, then intermediate commit order.
mergeCommitOrder :: [MergeCommitReplica]
mergeCommitOrder =
  [ SourceCommitReplica
  , DestinationCommitReplica
  , IntermediateCommitReplica
  ]


-- | Captures a stable regular UTF-8 text input without following a special
-- file.
observeMergeTextInput
  :: (MonadFileSystem m)
  => MergeInputRole
  -- ^ Replica being observed.
  -> OsPath
  -- ^ Authoritative replica path.
  -> m (Either MergeInputError MergeTextInput)
observeMergeTextInput role path = do
  snapshotBefore <- getFileSnapshot path
  case snapshotBefore of
    Nothing -> classifyAbsentOrUnsupported role path
    Just snapshot -> do
      modeBefore <- getFileModeSnapshot path
      case matchingModeSnapshot snapshot modeBefore of
        Nothing -> return $ Left $ ChangedMergeInput role path
        Just modeSnapshot -> do
          contents <- readRegularFile path
          snapshotAfter <- getFileSnapshot path
          modeAfter <- getFileModeSnapshot path
          if
            | contents == Nothing ->
                return $ Left $ ChangedMergeInput role path
            | snapshotAfter /= Just snapshot
                || modeAfter /= Just modeSnapshot ->
                return $ Left $ ChangedMergeInput role path
            | otherwise ->
                return $
                  validateText
                    (NulMergeInput role path)
                    (InvalidUtf8MergeInput role path)
                    ( MergeTextInput
                        role
                        path
                        snapshot
                        modeSnapshot
                    )
                    (maybe ByteString.empty id contents)


-- | Rechecks an observed input's identity, mode, and exact bytes.
revalidateMergeTextInput
  :: (MonadFileSystem m) => MergeTextInput -> m Bool
revalidateMergeTextInput input = do
  snapshotBefore <- getFileSnapshot input.path
  modeBefore <- getFileModeSnapshot input.path
  if
    | snapshotBefore /= Just input.snapshot
        || modeBefore /= Just input.modeSnapshot ->
        return False
    | otherwise -> do
        contents <- readRegularFile input.path
        snapshotAfter <- getFileSnapshot input.path
        modeAfter <- getFileModeSnapshot input.path
        return $
          contents == Just input.contents
            && snapshotAfter == Just input.snapshot
            && modeAfter == Just input.modeSnapshot


-- | Creates an owner-only workspace and writes its four owner-only regular
-- files.  The supplied root must not already exist.
prepareMergeWorkspace
  :: (MonadFileSystem m)
  => OsPath
  -- ^ Fresh conflict workspace root.
  -> MergeTextInput
  -- ^ Stable source input.
  -> MergeTextInput
  -- ^ Stable common-ancestor input.
  -> MergeTextInput
  -- ^ Stable destination input.
  -> m MergeWorkspace
prepareMergeWorkspace root source base destination = do
  createPrivateDirectory root
  sourceName <- encodePathName "source"
  baseName <- encodePathName "base"
  destinationName <- encodePathName "destination"
  resultName <- encodePathName "result"
  let workspace =
        MergeWorkspace
          { root = root
          , source = root </> sourceName
          , base = root </> baseName
          , destination = root </> destinationName
          , result = root </> resultName
          }
  writePrivate workspace.source source.contents
  writePrivate workspace.base base.contents
  writePrivate workspace.destination destination.contents
  writePrivate workspace.result destination.contents
  return workspace
 where
  encodePathName = encodePath
  writePrivate path contents = do
    writeFile path contents
    setPortableMode path 0o600


-- | Reads and validates a stable regular UTF-8 merge result.
readMergeResult
  :: (MonadFileSystem m)
  => OsPath
  -- ^ Result path supplied to the driver.
  -> m (Either MergeResultError ByteString)
readMergeResult path = do
  snapshotBefore <- getFileSnapshot path
  case snapshotBefore of
    Nothing -> do
      symbolicLink <- isSymlink path
      present <- exists path
      if symbolicLink || present
        then return $ Left $ UnsupportedMergeResult path
        else return $ Left $ MissingMergeResult path
    Just snapshot -> do
      contents <- readRegularFile path
      snapshotAfter <- getFileSnapshot path
      if
        | contents == Nothing || snapshotAfter /= Just snapshot ->
            return $ Left $ ChangedMergeResult path
        | otherwise ->
            return $
              validateText
                (NulMergeResult path)
                (InvalidUtf8MergeResult path)
                id
                (maybe ByteString.empty id contents)


classifyAbsentOrUnsupported
  :: (MonadFileSystem m)
  => MergeInputRole
  -> OsPath
  -> m (Either MergeInputError MergeTextInput)
classifyAbsentOrUnsupported role path = do
  symbolicLink <- isSymlink path
  present <- exists path
  if symbolicLink || present
    then return $ Left $ UnsupportedMergeInput role path
    else return $ Left $ MissingMergeInput role path


matchingModeSnapshot
  :: FileSnapshot -> Maybe FileModeSnapshot -> Maybe FileModeSnapshot
matchingModeSnapshot snapshot modeSnapshot = case modeSnapshot of
  Just value@(FileModeSnapshot identity _)
    | identity == fileSnapshotIdentity snapshot -> Just value
  _ -> Nothing


validateText
  :: error
  -> error
  -> (ByteString -> value)
  -> ByteString
  -> Either error value
validateText nulError utf8Error makeValue contents
  | ByteString.elem 0 contents = Left nulError
  | otherwise = case Text.decodeUtf8' contents of
      Left _ -> Left utf8Error
      Right _ -> Right $ makeValue contents
