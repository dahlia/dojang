{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Stable text inputs and isolated files for external three-way merges.
module Dojang.Types.Merge
  ( MergeCommitError (..)
  , MergeCommitReplica (..)
  , MergeContentsState (..)
  , MergeInputError (..)
  , MergeInputRole (..)
  , MergeResultError (..)
  , MergeTextInput (..)
  , MergeWorkspace (..)
  , classifyMergeContents
  , commitMergeResultGuarded
  , commitMergeRecoveryGuarded
  , mergeCommitOrder
  , mergeWorkspaceRepositoryRoot
  , observeMergeTextInput
  , prepareMergeWorkspace
  , readMergeResult
  , revalidateMergeTextInput
  ) where

import Control.Monad (filterM, forM_)
import Control.Monad.Except (MonadError (catchError, throwError))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import System.OsPath (OsPath, (</>))
import Prelude hiding (writeFile)

import Dojang.MonadFileSystem
  ( FileModeSnapshot (FileModeSnapshot)
  , FileSnapshot
  , MonadFileSystem (..)
  , fileSnapshotIdentity
  , writeFileAtomically
  )
import Dojang.Types.RepositoryId (RepositoryId, repositoryIdText)
import Dojang.Types.RouteMetadata (RouteMode, posixFileModeBits)


-- | Which authoritative replica supplied a merge input.
data MergeInputRole
  = -- | The repository source file.
    SourceInput
  | -- | The previous intermediate snapshot used as the common ancestor.
    BaseInput
  | -- | The deployed destination file.
    DestinationInput
  deriving (Eq, Ord, Show)


-- | Reconciliation state derived from exact observed replica contents.
data MergeContentsState
  = -- | Both endpoints changed differently from the common ancestor.
    ConflictingMergeContents
  | -- | Both endpoints contain one accepted result but the base is stale.
    RecoverableMergeContents
  | -- | All three replicas contain the same bytes.
    ConvergedMergeContents
  | -- | Only one endpoint differs from the common ancestor.
    OneSidedMergeContents
  deriving (Bounded, Enum, Eq, Ord, Show)


-- | Classifies exact source, base, and destination bytes for merge handling.
classifyMergeContents
  :: ByteString
  -- ^ Repository source contents.
  -> ByteString
  -- ^ Intermediate common-ancestor contents.
  -> ByteString
  -- ^ Deployed destination contents.
  -> MergeContentsState
classifyMergeContents source base destination
  | source /= base
      && destination /= base
      && source /= destination =
      ConflictingMergeContents
  | source == destination && source /= base =
      RecoverableMergeContents
  | source == base && base == destination =
      ConvergedMergeContents
  | otherwise =
      OneSidedMergeContents


-- | Locates the private merge-workspace root for one repository.
mergeWorkspaceRepositoryRoot
  :: (MonadFileSystem m)
  => OsPath
  -- ^ Machine-state root.
  -> RepositoryId
  -- ^ Stable repository identity.
  -> m OsPath
  -- ^ Repository-specific merge-workspace root.
mergeWorkspaceRepositoryRoot stateRoot repositoryId = do
  workspaceName <- encodePath "merge-workspaces"
  repositoryName <-
    encodePath $ Text.unpack $ repositoryIdText repositoryId
  return $ stateRoot </> workspaceName </> repositoryName


-- | Why an authoritative replica cannot be used as a merge input.
data MergeInputError
  = -- | The required replica is absent.
    MissingMergeInput MergeInputRole OsPath
  | -- | The replica is not a regular file.
    UnsupportedMergeInput MergeInputRole OsPath
  | -- | The replica cannot be read.
    UnreadableMergeInput MergeInputRole OsPath
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
  | -- | The result file cannot be read.
    UnreadableMergeResult OsPath
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


-- | A guarded merge result could not continue because authoritative inputs
-- changed.
data MergeCommitError
  = -- | One or more remaining replicas no longer match their observations.
    MergeInputsChanged (NonEmpty MergeInputRole)
  | -- | Source and destination did not contain one accepted result.
    MergeRecoveryInputsDiffer
  deriving (Eq, Show)


-- | The fixed source, destination, then intermediate commit order.
mergeCommitOrder :: [MergeCommitReplica]
mergeCommitOrder =
  [ SourceCommitReplica
  , DestinationCommitReplica
  , IntermediateCommitReplica
  ]


-- | Commits one validated result to source, destination, and intermediate in
-- that order.
--
-- Before each step, the callback runs and every authoritative input that has
-- not already been replaced is revalidated.  This makes concurrent changes
-- fail closed while leaving a precisely recoverable prefix when a later
-- filesystem operation fails.  Existing source permissions are preserved.
-- Destination and intermediate permissions are preserved for the default
-- route mode or set to the declared file mode otherwise.
commitMergeResultGuarded
  :: (MonadFileSystem m)
  => (MergeCommitReplica -> m ())
  -- ^ Observer invoked immediately before each guarded step.
  -> RouteMode
  -- ^ Declared destination metadata.
  -> MergeTextInput
  -- ^ Stable source input.
  -> MergeTextInput
  -- ^ Stable intermediate input.
  -> MergeTextInput
  -- ^ Stable destination input.
  -> ByteString
  -- ^ Validated driver result.
  -> m (Either MergeCommitError ())
commitMergeResultGuarded observe declaredMode source base destination result =
  go
    [
      ( SourceCommitReplica
      , [source, destination, base]
      , source.path
      , False
      )
    ,
      ( DestinationCommitReplica
      , [destination, base]
      , destination.path
      , True
      )
    ,
      ( IntermediateCommitReplica
      , [base]
      , base.path
      , True
      )
    ]
 where
  go [] = return $ Right ()
  go ((replica, remaining, path, applyDeclaredMode) : rest) = do
    observe replica
    changed <- filterM (fmap not . revalidateMergeTextInput) remaining
    case NonEmpty.nonEmpty $ (.role) <$> changed of
      Just roles -> return $ Left $ MergeInputsChanged roles
      Nothing -> do
        writeFileAtomically path "dojang-merge.tmp" result
        case (applyDeclaredMode, posixFileModeBits declaredMode) of
          (True, Just bits) -> setPortableMode path bits
          _ -> return ()
        go rest


-- | Finishes an interrupted merge after source and destination already agree.
--
-- Source and destination contents are never rewritten.  All three inputs are
-- revalidated before the destination mode is restored, then the destination
-- is recaptured and checked again before the intermediate content and mode are
-- repaired.
commitMergeRecoveryGuarded
  :: (MonadFileSystem m)
  => (MergeCommitReplica -> m ())
  -- ^ Observer invoked immediately before the guarded recovery step.
  -> RouteMode
  -- ^ Declared destination metadata.
  -> MergeTextInput
  -- ^ Stable source input containing the accepted result.
  -> MergeTextInput
  -- ^ Stable intermediate input to repair.
  -> MergeTextInput
  -- ^ Stable destination input containing the accepted result.
  -> m (Either MergeCommitError ())
commitMergeRecoveryGuarded
  observe
  declaredMode
  source
  base
  destination
    | source.contents /= destination.contents =
        return $ Left MergeRecoveryInputsDiffer
    | otherwise =
        case posixFileModeBits declaredMode of
          Nothing -> repairBase [source, destination, base] Nothing
          Just bits -> do
            observe DestinationCommitReplica
            changed <- changedInputs [source, destination, base]
            case changed of
              Just roles -> return $ Left $ MergeInputsChanged roles
              Nothing -> do
                setPortableMode destination.path bits
                refreshed <- observeMergeTextInput DestinationInput destination.path
                case refreshed of
                  Right refreshedDestination
                    | refreshedDestination.contents == source.contents ->
                        repairBase
                          [source, refreshedDestination, base]
                          (Just bits)
                  _ ->
                    return $
                      Left $
                        MergeInputsChanged $
                          NonEmpty.singleton DestinationInput
   where
    changedInputs inputs =
      NonEmpty.nonEmpty . fmap (.role)
        <$> filterM (fmap not . revalidateMergeTextInput) inputs
    repairBase remaining declaredBits = do
      observe IntermediateCommitReplica
      changed <- changedInputs remaining
      case changed of
        Just roles -> return $ Left $ MergeInputsChanged roles
        Nothing -> do
          writeFileAtomically base.path "dojang-merge.tmp" source.contents
          case declaredBits of
            Just bits -> setPortableMode base.path bits
            Nothing -> return ()
          return $ Right ()


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
          contentsResult <-
            (Right <$> readRegularFile path)
              `catchError` const
                (return $ Left $ UnreadableMergeInput role path)
          case contentsResult of
            Left err -> return $ Left err
            Right contents -> do
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
-- files.  The supplied root must not already exist.  If setup fails after
-- creation, the function removes the root only while it retains the captured
-- filesystem identity, ignores cleanup errors, and rethrows the setup error.
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
  identity <- getFileIdentity root
  prepareContents `catchError` cleanAfterFailure identity
 where
  prepareContents = do
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
  cleanAfterFailure identity err = do
    forM_ identity $ \expected -> do
      _ <-
        removeDirectoryRecursivelyIfIdentity root expected
          `catchError` const (return False)
      return ()
    throwError err
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
      contentsResult <-
        (Right <$> readRegularFile path)
          `catchError` const (return $ Left $ UnreadableMergeResult path)
      case contentsResult of
        Left err -> return $ Left err
        Right contents -> do
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
  | otherwise = case TextEncoding.decodeUtf8' contents of
      Left _ -> Left utf8Error
      Right _ -> Right $ makeValue contents
