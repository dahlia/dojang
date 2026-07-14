{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}

module Dojang.MonadFileSystem
  ( DryRunIO
  , FileType (..)
  , MonadFileSystem (..)
  , dryRunIO
  , dryRunIO'
  , tryDryRunIO
  , writeFileAtomically
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (inits, isPrefixOf, sort, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)), filter, singleton, toList)
import Data.Ord (Down (Down))
import GHC.IO.Exception (IOErrorType (InappropriateType))
import GHC.Stack (HasCallStack)
import System.IO.Error
  ( alreadyExistsErrorType
  , doesNotExistErrorType
  , ioeGetLocation
  , ioeSetErrorString
  , ioeSetLocation
  , isAlreadyExistsError
  , isDoesNotExistError
  , isPermissionError
  , mkIOError
  )
import System.Info (os)
import Prelude hiding (filter, readFile, writeFile)

import Control.Monad.Except (MonadError (..), tryError)
import Control.Monad.Extra (partitionM)
import Control.Monad.State.Strict
  ( MonadState
  , StateT
  , gets
  , modify'
  , runStateT
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified (hPut, length, readFile, writeFile)
import Data.Map.Strict (Map, alter, fromList, keys, toAscList, (!?))
import System.Directory qualified as Directory
import System.Directory.OsPath
  ( doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , getSymbolicLinkTarget
  , pathIsSymbolicLink
  , removeDirectoryRecursive
  )
import System.Directory.OsPath qualified as OsDirectory
import System.FileLock qualified as FileLock


#ifndef mingw32_HOST_OS
import System.Posix.Files qualified as Posix
#endif
import System.FilePattern (FilePattern, Step (stepApply, stepDone), step_)
import System.IO
  ( hClose
  , hFlush
  , openBinaryTempFile
  )
import System.OsPath
  ( OsPath
  , decodeFS
  , encodeFS
  , isAbsolute
  , joinPath
  , normalise
  , splitDirectories
  , takeDirectory
  , takeFileName
  , (</>)
  )


-- | A type that represents a file or directory.
data FileType
  = -- | A directory.
    Directory
  | -- | A file.
    File
  | -- | A symbolic link.
    Symlink
  deriving (Eq, Ord, Show)


-- | A monad that can perform filesystem operations.  It's also based on
-- 'OsPath' instead of 'FilePath'.
class (MonadError IOError m) => MonadFileSystem m where
  -- | Encodes a 'FilePath' into an 'OsPath'.
  encodePath :: (HasCallStack) => FilePath -> m OsPath


  -- | Decodes a 'OsPath' into a 'FilePath'.
  decodePath :: (HasCallStack) => OsPath -> m FilePath


  -- | Gets the process current working directory.
  getCurrentDirectory :: (HasCallStack) => m OsPath


  -- | Makes a path absolute using the interpreter's current working directory.
  makeAbsolute :: (HasCallStack) => OsPath -> m OsPath
  makeAbsolute path
    | isAbsolute path = return $ normalise path
    | otherwise = normalise . (</> path) <$> getCurrentDirectory


  -- | Checks if a file (or directory) exists.  If a path is a symbolic link,
  -- then it tells whether the target of the symbolic link exists.
  exists :: (HasCallStack) => OsPath -> m Bool


  -- | Checks if a path exists and is a file.  If a path is a symbolic link,
  -- then it tells whether the target of the symbolic link is a file.
  isFile :: (HasCallStack) => OsPath -> m Bool


  -- | Checks if a path exists and is a regular, non-symbolic-link file.
  isRegularFile :: (HasCallStack) => OsPath -> m Bool


  -- | Checks if a path exists and is a directory.  If a path is a symbolic
  -- link, then it tells whether the target of the symbolic link is a directory.
  isDirectory :: (HasCallStack) => OsPath -> m Bool


  -- | Checks if a path exists and is a symbolic link.
  isSymlink :: (HasCallStack) => OsPath -> m Bool


  -- | Reads contents from a file.
  readFile :: (HasCallStack) => OsPath -> m ByteString


  -- | Writes contents into a file.
  writeFile :: (HasCallStack) => OsPath -> ByteString -> m ()


  -- | Replaces the destination file with the source file.
  --
  -- Both paths must be on the same filesystem.  Implementations should use
  -- the platform's atomic replacement operation where one is available.
  replaceFile :: (HasCallStack) => OsPath -> OsPath -> m ()


  -- | Writes a uniquely named temporary file in the given directory.
  --
  -- The returned path belongs to the caller, which should replace or remove
  -- it after use.
  writeTemporaryFile
    :: (HasCallStack)
    => OsPath
    -- ^ Parent directory.
    -> FilePath
    -- ^ Filename template.
    -> ByteString
    -- ^ File contents.
    -> m OsPath
  writeTemporaryFile directory template contents = do
    filename <- encodePath template
    let temporary = directory </> filename
    writeFile temporary contents
    return temporary


  -- | Runs an action while holding an exclusive inter-process file lock.
  withFileLock :: (HasCallStack) => OsPath -> m a -> m a
  withFileLock _ action = action


  -- | Resolves symbolic links and other filesystem aliases in a path.
  canonicalizePath :: (HasCallStack) => OsPath -> m OsPath
  canonicalizePath = return . normalise


  -- | Tells the target path of a symbolic link.  If the path is not a symbolic
  -- link, then it throws an 'IOError'.  The target path is relative to the
  -- symbolic link (i.e., resolved from the directory that contains the
  -- symbolic link).
  readSymlinkTarget :: OsPath -> m OsPath


  -- | Copies a file from one path to another.
  copyFile
    :: (HasCallStack)
    => OsPath
    -- ^ Source path.
    -> OsPath
    -- ^ Destination path.
    -> m ()


  -- | Copies a file together with its filesystem metadata.
  --
  -- The default implementation copies only the contents.  Filesystem-backed
  -- implementations should preserve permissions and other supported metadata.
  copyFileWithMetadata
    :: (HasCallStack)
    => OsPath
    -- ^ Source path.
    -> OsPath
    -- ^ Destination path.
    -> m ()
  copyFileWithMetadata = copyFile


  -- | Copies filesystem permissions without replacing file contents.
  --
  -- The default implementation does nothing.  Filesystem-backed
  -- implementations should preserve every supported permission bit or
  -- attribute.
  copyFilePermissions
    :: (HasCallStack)
    => OsPath
    -- ^ Source path.
    -> OsPath
    -- ^ Destination path.
    -> m ()
  copyFilePermissions _ _ = return ()


  -- | Creates a directory at the given path.
  createDirectory :: (HasCallStack) => OsPath -> m ()


  -- | Creates a directory at the given path, including all parent directories.
  createDirectories :: (HasCallStack) => OsPath -> m ()
  createDirectories path =
    ( do
        forM_ ancestors $ \ancestor -> do
          isSymlink' <- isSymlink ancestor
          when isSymlink' $ do
            ancestor' <- decodePath ancestor
            throwError $ fileError ancestor'
          isDir <- isDirectory ancestor
          unless isDir $ do
            exists' <- isFile ancestor
            if exists'
              then do
                ancestor' <- decodePath ancestor
                throwError $ fileError ancestor'
              else
                createDirectory ancestor `catchError` \err ->
                  if isAlreadyExistsError err
                    then do
                      createdByPeer <- isDirectory ancestor
                      unless createdByPeer $ throwError err
                    else throwError err
    )
      `mapError` (`ioePrependLocation` "createDirectories")
   where
    split :: [OsPath]
    split = splitDirectories path
    ancestors :: [OsPath]
    ancestors = map joinPath $ drop 1 (inits split)
    fileError :: FilePath -> IOError
    fileError path' =
      mkIOError InappropriateType "createDirectories" Nothing (Just path')
        `ioeSetErrorString` "one of its ancestors is a non-directory file"


  -- | Removes a regular file.
  removeFile :: (HasCallStack) => OsPath -> m ()


  -- | Removes a directory.  It must be empty.
  removeDirectory :: (HasCallStack) => OsPath -> m ()


  -- | Removes a directory entirely, including all its contents.
  removeDirectoryRecursively :: (HasCallStack) => OsPath -> m ()
  removeDirectoryRecursively path =
    ( do
        entries <- listDirectoryRecursively path []
        forM_ (sortOn (Down . snd) entries) $ \(fileType, entry) ->
          case fileType of
            Directory -> removeDirectoryRecursively $ path </> entry
            File -> removeFile $ path </> entry
            Symlink -> removeFile $ path </> entry
        removeDirectory path
    )
      `mapError` (`ioePrependLocation` "removeDirectoryRecursively")


  -- | Lists all files and directories in a directory except for @.@ and @..@,
  -- without recursing into subdirectories.
  listDirectory :: (HasCallStack) => OsPath -> m [OsPath]


  -- | Lists all files and directories in a directory recursively.  It doesn't
  -- include @.@ and @..@.  Paths are relative to the given directory,
  -- and directories always go before their contents.
  --
  -- Note that it doesn't follow symbolic links.  Instead, it returns the
  -- symbolic links themselves with the 'Symlink' file type.
  listDirectoryRecursively
    :: (HasCallStack)
    => OsPath
    -- ^ The directory to list recursively.
    -> [FilePattern]
    -- ^ The file patterns to ignore.  If a directory matches one of these
    -- patterns, then its contents will not be listed recursively either.
    -> m [(FileType, OsPath)]
    -- ^ The list of pairs of file types and paths.  The paths are relative
    -- to the given directory.
  listDirectoryRecursively path ignorePatterns =
    listDirectoryRecursively' path (step_ ignorePatterns)
      `mapError` (`ioePrependLocation` "listDirectoryRecursively")


  -- | Gets the size of a file in bytes.  If the file doesn't exist or is
  -- a directory, then it throws an 'IOError'.
  getFileSize :: (HasCallStack) => OsPath -> m Integer


-- | Writes a sibling temporary file and atomically replaces the destination.
writeFileAtomically
  :: (HasCallStack, MonadFileSystem m)
  => OsPath
  -- ^ Destination file.
  -> FilePath
  -- ^ Temporary filename template.
  -> ByteString
  -- ^ Complete replacement contents.
  -> m ()
writeFileAtomically destination template contents = do
  let directory = takeDirectory destination
  temporary <- writeTemporaryFile directory template contents
  ( do
      destinationExists <- exists destination
      when destinationExists $
        copyFilePermissions destination temporary
      replaceFile temporary destination
    )
    `catchError` \err -> do
      temporaryExists <- exists temporary
      when temporaryExists $ removeFile temporary
      throwError err


listDirectoryRecursively'
  :: (HasCallStack, MonadFileSystem m)
  => OsPath
  -> Step ()
  -> m [(FileType, OsPath)]
listDirectoryRecursively' path ptnStep = do
  unfilteredEntries <- listDirectory path
  entriesWithSteps <- forM unfilteredEntries $ \entry -> do
    decoded <- decodePath entry
    let nextStep = stepApply ptnStep decoded
    return (entry, nextStep)
  let filteredEntries =
        [ (entry, step)
        | (entry, step) <- entriesWithSteps
        , null $ stepDone step
        ]
  (symlinks, entries') <-
    partitionM (isSymlink . (path </>) . fst) filteredEntries
  (dirs, files) <- partitionM (isDirectory . (path </>) . fst) entries'
  symlinks' <- forM symlinks $ \(symlink, _) -> return (Symlink, symlink)
  files' <- forM files $ \(file, _) -> return (File, file)
  dirs' <- forM dirs $ \(dir, step) -> do
    subentries <- listDirectoryRecursively' (path </> dir) step
    return $ (Directory, dir) : (fmap (dir </>) <$> subentries)
  return $ files' ++ symlinks' ++ concat dirs'
#ifdef mingw32_HOST_OS
replaceFileIO :: OsPath -> OsPath -> IO ()
replaceFileIO source destination = do
  destinationExists <- doesPathExist destination
  if not destinationExists
    then OsDirectory.renameFile source destination
    else do
      permissions <- OsDirectory.getPermissions destination
      -- MoveFileEx cannot replace a destination with the read-only attribute.
      -- The source already carries the original permissions, so a successful
      -- replacement restores the attribute as part of the move.
      OsDirectory.setPermissions
        destination
        (Directory.setOwnerWritable True permissions)
      OsDirectory.renameFile source destination `catchError` \err -> do
        destinationStillExists <- doesPathExist destination
        when destinationStillExists $
          OsDirectory.setPermissions destination permissions
        sourceStillExists <- doesPathExist source
        when sourceStillExists $ do
          sourcePermissions <- OsDirectory.getPermissions source
          OsDirectory.setPermissions
            source
            (Directory.setOwnerWritable True sourcePermissions)
        throwError err


copyFilePermissionsIO :: OsPath -> OsPath -> IO ()
copyFilePermissionsIO source destination = do
  permissions <- OsDirectory.getPermissions source
  OsDirectory.setPermissions destination permissions


isRegularFileIO :: OsPath -> IO Bool
isRegularFileIO path =
  (&&) <$> doesFileExist path <*> (not <$> isSymlink path)
#else
replaceFileIO :: OsPath -> OsPath -> IO ()
replaceFileIO = OsDirectory.renameFile


copyFilePermissionsIO :: OsPath -> OsPath -> IO ()
copyFilePermissionsIO source destination = do
  source' <- decodeFS source
  destination' <- decodeFS destination
  mode <- Posix.fileMode <$> Posix.getFileStatus source'
  Posix.setFileMode destination' mode


isRegularFileIO :: OsPath -> IO Bool
isRegularFileIO path = do
  path' <- decodeFS path
  (Posix.isRegularFile <$> Posix.getSymbolicLinkStatus path')
    `catchError` \err ->
      if isDoesNotExistError err then return False else throwError err
#endif


validateFileLockPath :: OsPath -> IO ()
validateFileLockPath lockPath = do
  symbolicLink <-
    pathIsSymbolicLink lockPath `catchError` \err ->
      if isDoesNotExistError err then return False else throwError err
  present <- doesPathExist lockPath
  regularFile <- isRegularFileIO lockPath
  when (symbolicLink || present && not regularFile) $ do
    lockPath' <- decodeFS lockPath
    throwError $
      mkIOError InappropriateType "withFileLock" Nothing (Just lockPath')
        `ioeSetErrorString` "lock path is not a regular file"


instance MonadFileSystem IO where
  encodePath = encodeFS


  decodePath = decodeFS


  getCurrentDirectory = OsDirectory.getCurrentDirectory


  exists = doesPathExist


  isFile = doesFileExist


  isRegularFile = isRegularFileIO


  isDirectory = doesDirectoryExist


  isSymlink path =
    pathIsSymbolicLink path `catchError` \e ->
      if isDoesNotExistError e then return False else throwError e


  readFile src = decodePath src >>= Data.ByteString.readFile


  writeFile dst contents = do
    dst' <- decodePath dst
    Data.ByteString.writeFile dst' contents


  replaceFile = replaceFileIO


  copyFileWithMetadata = OsDirectory.copyFileWithMetadata


  copyFilePermissions = copyFilePermissionsIO


  writeTemporaryFile directory template contents = do
    directory' <- decodePath directory
    (filename, handle) <- openBinaryTempFile directory' template
    ( do
        Data.ByteString.hPut handle contents
        hFlush handle
        hClose handle
        encodePath filename
      )
      `catchError` \err -> do
        hClose handle `catchError` const (return ())
        Directory.removeFile filename `catchError` const (return ())
        throwError err


  withFileLock lockPath action = do
    validateFileLockPath lockPath
    lockPath' <- decodePath lockPath
    FileLock.withFileLock lockPath' FileLock.Exclusive $ const $ do
      validateFileLockPath lockPath
      action


  canonicalizePath = OsDirectory.canonicalizePath


  readSymlinkTarget = getSymbolicLinkTarget


  createDirectory = OsDirectory.createDirectory


  removeFile = OsDirectory.removeFile


  removeDirectory = OsDirectory.removeDirectory


  removeDirectoryRecursively =
    retryOnPermissionErrorsOnWindows 10 . removeDirectoryRecursive
   where
    -- See also: https://github.com/jaspervdj/hakyll/pull/783
    retryOnPermissionErrorsOnWindows :: Int -> IO () -> IO ()
    retryOnPermissionErrorsOnWindows retry action
      | os /= "mingw32" = action
      | retry < 1 = action
      | otherwise =
          action `catchError` \e ->
            if isPermissionError e
              then do
                threadDelay 100
                retryOnPermissionErrorsOnWindows (retry - 1) action
              else throwError e


  listDirectory = OsDirectory.listDirectory


  getFileSize path = do
    isDir <- isDirectory path
    when isDir $ do
      path' <- decodePath path
      throwError $
        mkIOError InappropriateType "getFileSize" Nothing (Just path')
          `ioeSetErrorString` "it is a directory"
    OsDirectory.getFileSize path


  copyFile = OsDirectory.copyFile


type SeqNo = Int


-- | The result of a filesystem operation.
data OverlaidFile
  = -- | A file with the given contents.
    Contents ByteString
  | -- | A directory.
    Directory'
  | -- | A file that doesn't exist (i.e., it was deleted).
    Gone
  | -- | A file that was copied from the given path.
    Copied OsPath
  deriving (Eq, Show)


-- | Internal state of 'DryRun'.
data DryRunState = DryRunState
  { overlaidFiles :: Map OsPath (NonEmpty (SeqNo, OverlaidFile))
  -- ^ The overlaid files and their list of changes.  Each change is a pair
  -- of the global sequence number and the new event that occurred.  The latest
  -- change comes first and the oldest change comes last.
  , nextSequenceNumber :: SeqNo
  }


currentSequenceNumber :: DryRunState -> SeqNo
currentSequenceNumber state = nextSequenceNumber state - 1


-- | A monad that can perform filesystem operations, but only in a sandbox.
-- Note that, however, it can bypass the sandboxing of the 'MonadFileSystem'
-- class by using 'liftIO'.
newtype DryRunIO a = DryRunIO {unDryRunIO :: StateT DryRunState IO a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadError IOError
    , MonadIO
    , MonadState DryRunState
    )


addChangeToFile :: OsPath -> OverlaidFile -> DryRunIO ()
addChangeToFile path change = modify' $ \state ->
  let oFiles = overlaidFiles state
      nextSeqNo = nextSequenceNumber state
      newOFiles = alter (appendChange nextSeqNo) (normalise path) oFiles
  in state{overlaidFiles = newOFiles, nextSequenceNumber = nextSeqNo + 1}
 where
  appendChange
    :: SeqNo
    -> Maybe (NonEmpty (SeqNo, OverlaidFile))
    -> Maybe (NonEmpty (SeqNo, OverlaidFile))
  appendChange seqNo (Just changes) = Just $ (seqNo, change) :| toList changes
  appendChange seqNo Nothing = Just $ singleton (seqNo, change)


readFileFromDryRunIO :: SeqNo -> OsPath -> DryRunIO ByteString
readFileFromDryRunIO seqOffset src = do
  oFiles <- gets overlaidFiles
  case oFiles !? normalise src of
    Nothing -> fallback
    Just changes ->
      let filteredChanges = filter (\(no, _) -> no <= seqOffset) changes
      in case filteredChanges of
           [] -> fallback
           (_, Contents contents) : _ -> return contents
           (seqNo, Copied src') : _ ->
             readFileFromDryRunIO seqNo src'
           (_, Gone) : _ -> do
             src' <- decodePath src
             throwError $
               mkIOError doesNotExistErrorType "readFile" Nothing (Just src')
                 `ioeSetErrorString` "no such file"
           (_, Directory') : _ -> do
             src' <- decodePath src
             throwError $ nonDirError src'
 where
  fallback :: DryRunIO ByteString
  fallback = liftIO $ do
    isDir <- doesDirectoryExist src
    src' <- decodeFS src
    when isDir $ throwError (nonDirError src')
    Data.ByteString.readFile src'
  nonDirError :: FilePath -> IOError
  nonDirError src' =
    mkIOError InappropriateType "readFile" Nothing (Just src')
      `ioeSetErrorString` "is a directory"


instance MonadFileSystem DryRunIO where
  encodePath = liftIO . encodeFS


  decodePath = liftIO . decodeFS


  getCurrentDirectory = liftIO OsDirectory.getCurrentDirectory


  exists path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) -> return False
      Just (_ :| _) -> return True
      Nothing -> liftIO $ doesPathExist path


  isFile path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Contents _) :| _) -> return True
      Just ((_, Copied _) :| _) -> return True
      Just (_ :| _) -> return False
      Nothing -> liftIO $ doesFileExist path


  isRegularFile path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Contents _) :| _) -> return True
      Just ((_, Copied _) :| _) -> return True
      Just (_ :| _) -> return False
      Nothing -> liftIO (isRegularFile path :: IO Bool)


  isDirectory path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Directory') :| _) -> return True
      Just (_ :| _) -> return False
      Nothing -> liftIO $ doesDirectoryExist path


  isSymlink path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just _ -> return False
      Nothing ->
        liftIO (pathIsSymbolicLink path)
          `catchError` \e ->
            if isDoesNotExistError e then return False else throwError e


  readFile src = do
    seqNo <- gets currentSequenceNumber
    readFileFromDryRunIO seqNo src


  writeFile dst contents = do
    oFiles <- gets overlaidFiles
    let dstDir = normalise $ takeDirectory dst
    dstParentExists <- liftIO $ doesPathExist dstDir
    dstDirExists <- liftIO $ doesDirectoryExist dstDir
    dst' <- decodePath dst
    dstIsDir <- liftIO $ doesDirectoryExist dst
    case (oFiles !? dstDir, oFiles !? normalise dst) of
      (Just ((_, Gone) :| _), _) -> throwError $ noParentDirError dst'
      (Nothing, _) | not dstParentExists -> throwError $ noParentDirError dst'
      (Just ((_, Contents _) :| _), _) -> throwError $ notInsideDirError dst'
      (Just ((_, Copied _) :| _), _) -> throwError $ notInsideDirError dst'
      (Nothing, _) | not dstDirExists -> throwError $ notInsideDirError dst'
      (_, Just ((_, Directory') :| _)) -> throwError $ dirError dst'
      (_, Nothing) | dstIsDir -> throwError $ dirError dst'
      _ -> do
        addChangeToFile dst $ Contents contents
        return ()
   where
    dirError :: FilePath -> IOError
    dirError dst' =
      mkIOError InappropriateType "writeFile" Nothing (Just dst')
        `ioeSetErrorString` "is a directory"
    noParentDirError :: FilePath -> IOError
    noParentDirError dst' =
      mkIOError doesNotExistErrorType "writeFile" Nothing (Just dst')
        `ioeSetErrorString` "no parent directory"
    notInsideDirError :: FilePath -> IOError
    notInsideDirError dst' =
      mkIOError InappropriateType "writeFile" Nothing (Just dst')
        `ioeSetErrorString` "not inside a directory"


  replaceFile src dst = do
    contents <- readFile src
    writeFile dst contents
    removeFile src


  writeTemporaryFile directory template contents = do
    sequenceNumber <- gets nextSequenceNumber
    filename <- encodePath $ template <> show sequenceNumber
    let temporary = directory </> filename
    writeFile temporary contents
    return temporary


  withFileLock _ action = action


  canonicalizePath path = liftIO $ OsDirectory.canonicalizePath path


  readSymlinkTarget path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) -> do
        path' <- decodePath path
        throwError $
          mkIOError
            doesNotExistErrorType
            "readSymlinkTarget"
            Nothing
            (Just path')
            `ioeSetErrorString` "no such file"
      Just _ -> do
        path' <- decodePath path
        throwError $
          mkIOError InappropriateType "readSymlinkTarget" Nothing (Just path')
            `ioeSetErrorString` "not a symbolic link"
      Nothing ->
        liftIO $
          getSymbolicLinkTarget path
            `mapError` (`ioePrependLocation` "readSymlinkTarget")


  copyFile src dst = do
    oFiles <- gets overlaidFiles
    src' <- decodePath src
    srcExists <- liftIO $ doesPathExist src
    srcIsDir <- liftIO $ doesDirectoryExist src
    let dstDir = normalise $ takeDirectory dst
    dstDirExists <- liftIO $ doesPathExist dstDir
    dstDirIsDir <- liftIO $ doesDirectoryExist dstDir
    dst' <- decodePath dst
    dstIsDir <- liftIO $ doesDirectoryExist dst
    case oFiles !? normalise src of
      Just ((_, Gone) :| _) -> throwError $ noSrcFileError src'
      Nothing | not srcExists -> throwError $ noSrcFileError src'
      Just ((_, Directory') :| _) -> throwError $ srcIsDirError src'
      Nothing | srcIsDir -> throwError $ srcIsDirError src'
      _ -> case (oFiles !? dstDir, oFiles !? normalise dst) of
        (Just ((_, Gone) :| _), _) -> throwError $ noParentDirError dst'
        (Nothing, _) | not dstDirExists -> throwError $ noParentDirError dst'
        (Just ((_, Contents _) :| _), _) -> throwError $ notInsideDirError dst'
        (Just ((_, Copied _) :| _), _) -> throwError $ notInsideDirError dst'
        (Nothing, _) | not dstDirIsDir -> throwError $ notInsideDirError dst'
        (_, Just ((_, Directory') :| _)) -> throwError $ dstIsDirError dst'
        (_, Nothing) | dstIsDir -> throwError $ dstIsDirError dst'
        _ -> do
          addChangeToFile dst $ Copied src
          return ()
   where
    noSrcFileError :: FilePath -> IOError
    noSrcFileError src' =
      mkIOError doesNotExistErrorType "copyFile" Nothing (Just src')
        `ioeSetErrorString` "source does not exist"
    srcIsDirError :: FilePath -> IOError
    srcIsDirError src' =
      mkIOError InappropriateType "copyFile" Nothing (Just src')
        `ioeSetErrorString` "source is a directory"
    noParentDirError :: FilePath -> IOError
    noParentDirError dst' =
      mkIOError doesNotExistErrorType "copyFile" Nothing (Just dst')
        `ioeSetErrorString` "no parent directory"
    notInsideDirError :: FilePath -> IOError
    notInsideDirError dst' =
      mkIOError InappropriateType "copyFile" Nothing (Just dst')
        `ioeSetErrorString` "not inside a directory"
    dstIsDirError :: FilePath -> IOError
    dstIsDirError dst' =
      mkIOError InappropriateType "copyFile" Nothing (Just dst')
        `ioeSetErrorString` "destination is a directory"


  createDirectory dst = do
    oFiles <- gets overlaidFiles
    dst' <- decodePath dst
    isFile' <- liftIO $ doesFileExist dst
    isDir <- liftIO $ doesDirectoryExist dst
    isSymlink' <-
      liftIO $
        pathIsSymbolicLink dst `catchError` \e ->
          if isDoesNotExistError e
            then return False
            else throwError $ e `ioePrependLocation` "createDirectory"
    parentExists <- liftIO $ doesPathExist parent
    parentIsDir <- liftIO $ doesDirectoryExist parent
    case (oFiles !? parent, oFiles !? normalise dst) of
      (Just ((_, Gone) :| _), _) -> throwError $ noParentDirError dst'
      (Nothing, _) | not parentExists -> throwError $ noParentDirError dst'
      (Just ((_, Contents _) :| _), _) -> throwError $ notInsideDirError dst'
      (Just ((_, Copied _) :| _), _) -> throwError $ notInsideDirError dst'
      (Nothing, _) | not parentIsDir -> throwError $ notInsideDirError dst'
      (_, Just ((_, Contents _) :| _)) -> throwError $ dstIsFileError dst'
      (_, Just ((_, Copied _) :| _)) -> throwError $ dstIsFileError dst'
      (_, Nothing) | isFile' -> throwError $ dstIsFileError dst'
      (_, Just ((_, Directory') :| _)) -> throwError $ dstIsDirError dst'
      (_, Nothing) | isDir && not isSymlink' -> throwError $ dstIsDirError dst'
      _ -> do
        addChangeToFile dst Directory'
        return ()
   where
    parent :: OsPath
    parent = normalise $ takeDirectory dst
    noParentDirError :: FilePath -> IOError
    noParentDirError dst' =
      mkIOError doesNotExistErrorType "createDirectory" Nothing (Just dst')
        `ioeSetErrorString` "no parent directory"
    notInsideDirError :: FilePath -> IOError
    notInsideDirError dst' =
      mkIOError InappropriateType "createDirectory" Nothing (Just dst')
        `ioeSetErrorString` "not inside a directory"
    dstIsFileError :: FilePath -> IOError
    dstIsFileError dst' =
      mkIOError alreadyExistsErrorType "createDirectory" Nothing (Just dst')
        `ioeSetErrorString` "destination is already a file"
    dstIsDirError :: FilePath -> IOError
    dstIsDirError dst' =
      mkIOError alreadyExistsErrorType "createDirectory" Nothing (Just dst')
        `ioeSetErrorString` "destination is already a directory"


  removeFile path = do
    oFiles <- gets overlaidFiles
    path' <- decodePath path
    exists' <- liftIO $ doesPathExist path
    isSymlink' <-
      liftIO $
        pathIsSymbolicLink path
          `catchError` \e ->
            if isDoesNotExistError e
              then return False
              else throwError $ e `ioePrependLocation` "removeFile"
    isDir <- liftIO $ doesDirectoryExist path
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) -> throwError $ noFileError path'
      Nothing | not exists' -> throwError $ noFileError path'
      Just ((_, Directory') :| _) -> throwError $ dirError path'
      Nothing | isDir && not isSymlink' -> throwError $ dirError path'
      _ -> do
        addChangeToFile path Gone
        return ()
   where
    noFileError :: FilePath -> IOError
    noFileError path' =
      mkIOError doesNotExistErrorType "removeFile" Nothing (Just path')
        `ioeSetErrorString` "no such file"
    dirError :: FilePath -> IOError
    dirError path' =
      mkIOError InappropriateType "removeFile" Nothing (Just path')
        `ioeSetErrorString` "is a directory"


  removeDirectory path = do
    oFiles <- gets overlaidFiles
    path' <- decodePath path
    exists' <- liftIO $ doesPathExist path
    isDir <- liftIO $ doesDirectoryExist path
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) ->
        throwError $ noDirError path'
      Nothing
        | not exists' ->
            throwError $ noDirError path'
      Just ((_, Contents _) :| _) ->
        throwError $ nonDirError path'
      Just ((_, Copied _) :| _) ->
        throwError $ nonDirError path'
      Nothing
        | not isDir ->
            throwError $ nonDirError path'
      _ -> do
        addChangeToFile path Gone
        return ()
   where
    noDirError :: FilePath -> IOError
    noDirError path' =
      mkIOError
        doesNotExistErrorType
        "removeDirectory"
        Nothing
        (Just path')
        `ioeSetErrorString` "no such directory"
    nonDirError :: FilePath -> IOError
    nonDirError path' =
      mkIOError
        InappropriateType
        "removeDirectory"
        Nothing
        (Just path')
        `ioeSetErrorString` "not a directory"


  listDirectory path = do
    oFiles <- gets overlaidFiles
    pathFP <- decodePath path
    case oFiles !? path' of
      Just ((_, Gone) :| _) ->
        throwError $ noDirError pathFP
      Just ((_, Contents _) :| _) ->
        throwError $ nonDirError pathFP
      Just ((_, Copied _) :| _) ->
        throwError $ nonDirError pathFP
      Just ((_, Directory') :| _) ->
        return $ map takeFileName $ keys $ directOChildren oFiles
      Nothing -> do
        isSymlink' <-
          liftIO $
            pathIsSymbolicLink path
              `catchError` \e ->
                if isDoesNotExistError e
                  then return False
                  else throwError $ e `ioePrependLocation` "listDirectory"
        isFile' <- liftIO $ doesFileExist path
        when (isSymlink' || isFile') $ throwError (nonDirError pathFP)
        files <-
          liftIO $
            OsDirectory.listDirectory path
              `mapError` (`ioePrependLocation` "listDirectory")
        let directOChildren' = directOChildren oFiles
        let result =
              [f | f <- files, directOChildren' !? (path' </> f) /= Just Gone]
                ++ [ filename
                   | (filePath, f) <- toAscList directOChildren'
                   , f /= Gone
                   , let split = splitDirectories filePath
                   , pathDirs `isPrefixOf` split
                   , length pathDirs < length split
                   , let filename = split !! length pathDirs
                   , filename `notElem` files
                   ]
        return $ sort result
   where
    path' :: OsPath
    path' = normalise path
    pathDirs :: [OsPath]
    pathDirs = splitDirectories path'
    directOChildren
      :: Map OsPath (NonEmpty (SeqNo, OverlaidFile)) -> Map OsPath OverlaidFile
    directOChildren oFiles =
      fromList
        [ (filePath, f)
        | (filePath, (_, f) :| []) <- toAscList oFiles
        , let split = splitDirectories filePath
        , pathDirs `isPrefixOf` split
        , length pathDirs + 1 == length split
        ]
    noDirError :: FilePath -> IOError
    noDirError pathFP =
      mkIOError
        doesNotExistErrorType
        "listDirectory"
        Nothing
        (Just pathFP)
        `ioeSetErrorString` "no such directory"
    nonDirError :: FilePath -> IOError
    nonDirError pathFP =
      mkIOError
        InappropriateType
        "listDirectory"
        Nothing
        (Just pathFP)
        `ioeSetErrorString` "not a directory"


  getFileSize path = do
    oFiles <- gets overlaidFiles
    path' <- decodePath path
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) ->
        throwError $ noFileError path'
      Just ((_, Directory') :| _) ->
        throwError $ nonFileError path'
      Just _ -> do
        contents <- readFile path
        return $
          fromIntegral $
            Data.ByteString.length contents
      Nothing -> do
        isDir <- isDirectory path
        if isDir
          then throwError $ nonFileError path'
          else
            liftIO (OsDirectory.getFileSize path)
              `mapError` (`ioeSetLocation` "getFileSize")
   where
    noFileError :: FilePath -> IOError
    noFileError pathFP =
      mkIOError
        doesNotExistErrorType
        "getFileSize"
        Nothing
        (Just pathFP)
        `ioeSetErrorString` "no such file"
    nonFileError :: FilePath -> IOError
    nonFileError pathFP =
      mkIOError
        InappropriateType
        "getFileSize"
        Nothing
        (Just pathFP)
        `ioeSetErrorString` "not a regular file, but a directory"


-- | Performs 'DryRunIO' action in the sandbox and returns the result.
dryRunIO :: DryRunIO a -> IO a
dryRunIO = fmap fst . dryRunIO'


-- | Performs 'DryRunIO' action in the sandbox and returns the result and
-- the total number of filesystem operations that were performed.
dryRunIO' :: DryRunIO a -> IO (a, Int)
dryRunIO' action = do
  (value, state) <- runStateT (unDryRunIO action) initialState
  return (value, nextSequenceNumber state)
 where
  initialState = DryRunState{overlaidFiles = mempty, nextSequenceNumber = 0}


-- | Performs 'DryRunIO' action in the sandbox and returns either the result
-- or an 'IOError' that occurred.
tryDryRunIO :: DryRunIO a -> IO (Either IOError a)
tryDryRunIO action = dryRunIO $ tryError action


ioePrependLocation :: IOError -> String -> IOError
ioePrependLocation e location =
  ioeSetLocation e $ case loc of
    "" -> location
    _ -> location ++ ':' : loc
 where
  loc :: String
  loc = ioeGetLocation e


mapError :: (MonadError e m) => m a -> (e -> e) -> m a
mapError action transform = catchError action (throwError . transform)
