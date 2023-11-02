{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}

module Dojang.MonadFileSystem
  ( DryRunIO
  , FileType (..)
  , MonadFileSystem (..)
  , dryRunIO
  , tryDryRunIO
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
  , ioeSetErrorString
  , ioeSetFileName
  , ioeSetLocation
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
  , evalStateT
  , gets
  , modify'
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified (length, readFile, writeFile)
import Data.Map.Strict (Map, alter, fromList, keys, toAscList, (!?))
import System.Directory.OsPath
  ( doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , getSymbolicLinkTarget
  , pathIsSymbolicLink
  , removeDirectoryRecursive
  )
import System.Directory.OsPath qualified
  ( copyFile
  , createDirectory
  , getFileSize
  , listDirectory
  , removeDirectory
  , removeFile
  )
import System.FilePattern (FilePattern, Step (stepApply, stepDone), step_)
import System.OsPath
  ( OsPath
  , decodeFS
  , encodeFS
  , joinPath
  , makeRelative
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


  -- | Checks if a file (or directory) exists.  If a path is a symbolic link,
  -- then it tells whether the target of the symbolic link exists.
  exists :: (HasCallStack) => OsPath -> m Bool


  -- | Checks if a path exists and is a file.  If a path is a symbolic link,
  -- then it tells whether the target of the symbolic link is a file.
  isFile :: (HasCallStack) => OsPath -> m Bool


  -- | Checks if a path exists and is a directory.  If a path is a symbolic
  -- link, then it tells whether the target of the symbolic link is a directory.
  isDirectory :: (HasCallStack) => OsPath -> m Bool


  -- | Checks if a path exists and is a symbolic link.
  isSymlink :: (HasCallStack) => OsPath -> m Bool


  -- | Reads contents from a file.
  readFile :: (HasCallStack) => OsPath -> m ByteString


  -- | Writes contents into a file.
  writeFile :: (HasCallStack) => OsPath -> ByteString -> m ()


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
              else createDirectory ancestor
    )
      `catchError` \e ->
        throwError $ e `ioeSetLocation` "createDirectories"
   where
    split :: [OsPath]
    split = splitDirectories path
    ancestors :: [OsPath]
    ancestors = map joinPath $ tail (inits split)
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
      `catchError` \e ->
        throwError $ e `ioeSetLocation` "removeDirectoryRecursively"


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
    listDirectoryRecursively' path $ step_ ignorePatterns


  -- | Gets the size of a file in bytes.  If the file doesn't exist or is
  -- a directory, then it throws an 'IOError'.
  getFileSize :: (HasCallStack) => OsPath -> m Integer


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


instance MonadFileSystem IO where
  encodePath = encodeFS


  decodePath = decodeFS


  exists = doesPathExist


  isFile = doesFileExist


  isDirectory = doesDirectoryExist


  isSymlink path =
    pathIsSymbolicLink path `catchError` \e ->
      if isDoesNotExistError e then return False else throwError e


  readFile src = decodePath src >>= Data.ByteString.readFile


  writeFile dst contents = do
    dst' <- decodePath dst
    Data.ByteString.writeFile dst' contents


  readSymlinkTarget = getSymbolicLinkTarget


  createDirectory = System.Directory.OsPath.createDirectory


  removeFile = System.Directory.OsPath.removeFile


  removeDirectory = System.Directory.OsPath.removeDirectory


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


  listDirectory = System.Directory.OsPath.listDirectory


  getFileSize path = do
    isDir <- isDirectory path
    when isDir $ do
      path' <- decodePath path
      throwError
        $ mkIOError InappropriateType "getFileSize" Nothing (Just path')
        `ioeSetErrorString` "it is a directory"
    System.Directory.OsPath.getFileSize path


  copyFile = System.Directory.OsPath.copyFile


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
  deriving (Eq)


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


-- | A monad that can perform filesystem operations, but only in memory.
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
            throwError
              $ mkIOError doesNotExistErrorType "readFile" Nothing (Just src')
              `ioeSetErrorString` "no such file"
          (_, Directory') : _ -> do
            src' <- decodePath src
            throwError
              $ mkIOError InappropriateType "readFile" Nothing (Just src')
              `ioeSetErrorString` "is a directory"
 where
  fallback :: DryRunIO ByteString
  fallback = liftIO $ do
    src' <- decodeFS src
    Data.ByteString.readFile src'


instance MonadFileSystem DryRunIO where
  encodePath = liftIO . encodeFS


  decodePath = liftIO . decodeFS


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


  readSymlinkTarget path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) -> do
        path' <- decodePath path
        throwError
          $ mkIOError
            doesNotExistErrorType
            "readSymlinkTarget"
            Nothing
            (Just path')
          `ioeSetErrorString` "no such file"
      Just _ -> do
        path' <- decodePath path
        throwError
          $ mkIOError InappropriateType "readSymlinkTarget" Nothing (Just path')
          `ioeSetErrorString` "not a symbolic link"
      Nothing ->
        liftIO
          $ getSymbolicLinkTarget path
          `catchError` \e -> throwError $ e `ioeSetLocation` "readSymlinkTarget"


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
      liftIO $ pathIsSymbolicLink dst `catchError` \e ->
        if isDoesNotExistError e
          then return False
          else throwError $ e `ioeSetLocation` "createDirectory"
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
      (Nothing, _) | isFile' -> throwError $ dstIsFileError dst'
      (_, Just ((_, Directory') :| _)) -> throwError $ dstIsDirError dst'
      _ | isDir && not isSymlink' -> throwError $ dstIsDirError dst'
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
      liftIO
        $ pathIsSymbolicLink path
        `catchError` \e ->
          if isDoesNotExistError e
            then return False
            else throwError $ e `ioeSetLocation` "removeFile"
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
    let normalizedPath = normalise path
    oFiles <- gets overlaidFiles
    path' <- decodePath path
    case oFiles !? normalizedPath of
      Just ((_, Gone) :| _) ->
        throwError $ noDirError path'
      Just ((_, Contents _) :| _) ->
        throwError $ nonDirError path'
      Just ((_, Copied _) :| _) ->
        throwError $ nonDirError path'
      Just ((_, Directory') :| _) ->
        return $ map takeFileName $ keys $ directOChildren oFiles
      Nothing -> do
        files <-
          liftIO $ System.Directory.OsPath.listDirectory path `catchError` \e ->
            throwError $ e `ioeSetLocation` "listDirectory"
        let directOChildren' = directOChildren oFiles
        let result =
              [f | f <- files, directOChildren' !? (path </> f) /= Just Gone]
                ++ [ filename
                   | (filePath, f) <- toAscList directOChildren'
                   , f /= Gone
                   , let filename = makeRelative path filePath
                   , filename `notElem` files
                   ]
        return $ sort result
   where
    pathDirs :: [OsPath]
    pathDirs = splitDirectories path
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
    noDirError path' =
      mkIOError
        doesNotExistErrorType
        "listDirectory"
        Nothing
        (Just path')
        `ioeSetErrorString` "no such directory"
    nonDirError :: FilePath -> IOError
    nonDirError path' =
      mkIOError
        InappropriateType
        "listDirectory"
        Nothing
        (Just path')
        `ioeSetErrorString` "not a directory"


  getFileSize path = do
    oFiles <- gets overlaidFiles
    path' <- decodePath path
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) ->
        throwError
          $ userError "getFileSize: no such file"
          `ioeSetFileName` path'
      Just ((_, Directory') :| _) ->
        throwError
          $ userError "getFileSize: it is a directory"
          `ioeSetFileName` path'
      Just _ -> do
        contents <- readFile path
        return
          $ fromIntegral
          $ Data.ByteString.length contents
      Nothing -> do
        isDir <- isDirectory path
        if isDir
          then
            throwError
              $ userError "getFileSize: it is a directory"
              `ioeSetFileName` path'
          else do
            liftIO $ System.Directory.OsPath.getFileSize path


dryRunIO :: DryRunIO a -> IO a
dryRunIO action = evalStateT (unDryRunIO action) initialState
 where
  initialState = DryRunState{overlaidFiles = mempty, nextSequenceNumber = 0}


tryDryRunIO :: DryRunIO a -> IO (Either IOError a)
tryDryRunIO action = dryRunIO $ tryError action
