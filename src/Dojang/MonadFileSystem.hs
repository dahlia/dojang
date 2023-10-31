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

import Control.Monad (forM, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (isPrefixOf, sort)
import Data.List.NonEmpty (NonEmpty ((:|)), filter, singleton, toList)
import GHC.Stack (HasCallStack)
import System.IO.Error (ioeSetFileName, isDoesNotExistError)
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
  )
import System.Directory.OsPath qualified
  ( copyFile
  , createDirectory
  , getFileSize
  , listDirectory
  , removeFile
  )
import System.OsPath
  ( OsPath
  , decodeFS
  , encodeFS
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
  createDirectories path = do
    let parent = takeDirectory path
    parent' <- decodePath parent
    parentExists <- isDirectory parent
    if parentExists
      then createDirectory path
      else do
        parentIsFile <- isFile parent
        when parentIsFile
          $ throwError
          $ userError "createDirectories: one of its ancestors is a file"
          `ioeSetFileName` parent'
        createDirectories parent
        createDirectory path


  -- | Removes a file.
  removeFile :: (HasCallStack) => OsPath -> m ()


  -- | Lists all files and directories in a directory except for @.@ and @..@,
  -- without recursing into subdirectories.
  listDirectory :: (HasCallStack) => OsPath -> m [OsPath]


  -- | Lists all files and directories in a directory recursively.  It doesn't
  -- include @.@ and @..@.  Paths are relative to the given directory,
  -- and directories always go before their contents.
  --
  -- Note that it doesn't follow symbolic links.  Instead, it returns the
  -- symbolic links themselves with the 'Symlink' file type.
  listDirectoryRecursively :: (HasCallStack) => OsPath -> m [(FileType, OsPath)]
  listDirectoryRecursively path = do
    entries <- listDirectory path
    (symlinks, entries') <- partitionM (isSymlink . (path </>)) entries
    (dirs, files) <- partitionM (isDirectory . (path </>)) entries'
    symlinks' <- forM symlinks $ \symlink -> return (Symlink, symlink)
    files' <- forM files $ \file -> return (File, file)
    dirs' <- forM dirs $ \dir -> do
      subentries <- listDirectoryRecursively (path </> dir)
      return $ (Directory, dir) : (fmap (dir </>) <$> subentries)
    return $ files' ++ symlinks' ++ concat dirs'


  -- | Gets the size of a file in bytes.  If the file doesn't exist or is
  -- a directory, then it throws an 'IOError'.
  getFileSize :: (HasCallStack) => OsPath -> m Integer


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


  listDirectory = System.Directory.OsPath.listDirectory


  getFileSize path = do
    isDir <- isDirectory path
    when isDir $ do
      path' <- decodePath path
      throwError
        $ userError "getFileSize: it is a directory"
        `ioeSetFileName` path'
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
              $ userError "readFile: no such file"
              `ioeSetFileName` src'
          (_, Directory') : _ -> do
            src' <- decodePath src
            throwError
              $ userError "readFile: it is a directory"
              `ioeSetFileName` src'
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
      (Just ((_, Gone) :| _), _) ->
        throwError
          $ userError "writeFile: no parent directory"
          `ioeSetFileName` dst'
      (Nothing, _)
        | not dstParentExists ->
            throwError
              $ userError "writeFile: no parent directory"
              `ioeSetFileName` dst'
      (Just ((_, Contents _) :| _), _) ->
        throwError
          $ userError "writeFile: destination must be inside a directory"
          `ioeSetFileName` dst'
      (Just ((_, Copied _) :| _), _) ->
        throwError
          $ userError "writeFile: destination must be inside a directory"
          `ioeSetFileName` dst'
      (Nothing, _)
        | not dstDirExists ->
            throwError
              $ userError "writeFile: destination must be inside a directory"
              `ioeSetFileName` dst'
      (_, Just ((_, Directory') :| _)) ->
        throwError
          $ userError "writeFile: destination is a directory"
          `ioeSetFileName` dst'
      (_, Nothing)
        | dstIsDir ->
            throwError
              $ userError "writeFile: destination is a directory"
              `ioeSetFileName` dst'
      _ -> do
        addChangeToFile dst $ Contents contents
        return ()


  readSymlinkTarget path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) -> do
        path' <- decodePath path
        throwError
          $ userError "readSymlinkTarget: no such file"
          `ioeSetFileName` path'
      Just _ -> do
        path' <- decodePath path
        throwError
          $ userError "readSymlinkTarget: it is not a symbolic link"
          `ioeSetFileName` path'
      Nothing -> liftIO $ getSymbolicLinkTarget path


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
      Just ((_, Gone) :| _) ->
        throwError
          $ userError "copyFile: source does not exist"
          `ioeSetFileName` src'
      Nothing
        | not srcExists ->
            throwError
              $ userError "copyFile: source does not exist"
              `ioeSetFileName` src'
      Just ((_, Directory') :| _) ->
        throwError
          $ userError "copyFile: source is a directory"
          `ioeSetFileName` src'
      Nothing
        | srcIsDir ->
            throwError
              $ userError "copyFile: source is a directory"
              `ioeSetFileName` src'
      _ -> case (oFiles !? dstDir, oFiles !? normalise dst) of
        (Just ((_, Gone) :| _), _) ->
          throwError
            $ userError "copyFile: destination has no parent directory"
            `ioeSetFileName` dst'
        (Nothing, _)
          | not dstDirExists ->
              throwError
                $ userError "copyFile: destination has no parent directory"
                `ioeSetFileName` dst'
        (Just ((_, Contents _) :| _), _) ->
          throwError
            $ userError "copyFile: destination must be inside a directory"
            `ioeSetFileName` dst'
        (Just ((_, Copied _) :| _), _) ->
          throwError
            $ userError "copyFile: destination must be inside a directory"
            `ioeSetFileName` dst'
        (Nothing, _)
          | not dstDirIsDir ->
              throwError
                $ userError "copyFile: destination must be inside a directory"
                `ioeSetFileName` dst'
        (_, Just ((_, Directory') :| _)) ->
          throwError
            $ userError "copyFile: destination is a directory"
            `ioeSetFileName` dst'
        (_, Nothing)
          | dstIsDir ->
              throwError
                $ userError "copyFile: destination is a directory"
                `ioeSetFileName` dst'
        _ -> do
          addChangeToFile dst $ Copied src
          return ()


  createDirectory dst = do
    oFiles <- gets overlaidFiles
    dst' <- decodePath dst
    isFile' <- liftIO $ doesFileExist dst
    isDir <- liftIO $ doesDirectoryExist dst
    let parent = normalise $ takeDirectory dst
    parentExists <- liftIO $ doesPathExist parent
    parentIsDir <- liftIO $ doesDirectoryExist parent
    case (oFiles !? parent, oFiles !? normalise dst) of
      (Just ((_, Gone) :| _), _) ->
        throwError
          $ userError "createDirectory: no parent directory"
          `ioeSetFileName` dst'
      (Nothing, _)
        | not parentExists ->
            throwError
              $ userError "createDirectory: no parent directory"
              `ioeSetFileName` dst'
      (Just ((_, Contents _) :| _), _) ->
        throwError
          $ userError "createDirectory: destination must be inside a directory"
          `ioeSetFileName` dst'
      (Just ((_, Copied _) :| _), _) ->
        throwError
          $ userError "createDirectory: destination must be inside a directory"
          `ioeSetFileName` dst'
      (Nothing, _)
        | not parentIsDir ->
            throwError
              $ userError
                "createDirectory: destination must be inside a directory"
              `ioeSetFileName` dst'
      (_, Just ((_, Contents _) :| _)) ->
        throwError
          $ userError "createDirectory: destination is already a file"
          `ioeSetFileName` dst'
      (_, Just ((_, Copied _) :| _)) ->
        throwError
          $ userError "createDirectory: destination is already a file"
          `ioeSetFileName` dst'
      (Nothing, _)
        | isFile' ->
            throwError
              $ userError "createDirectory: destination is already a file"
              `ioeSetFileName` dst'
      (_, Just ((_, Directory') :| _)) ->
        throwError
          $ userError "createDirectory: destination is already a directory"
          `ioeSetFileName` dst'
      _
        | isDir ->
            throwError
              $ userError "createDirectory: destination is already a directory"
              `ioeSetFileName` dst'
      _ -> do
        addChangeToFile dst Directory'
        return ()


  removeFile path = do
    oFiles <- gets overlaidFiles
    path' <- decodePath path
    exists' <- liftIO $ doesPathExist path
    isDir <- liftIO $ doesDirectoryExist path
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) ->
        throwError
          $ userError "removeFile: no such file"
          `ioeSetFileName` path'
      Nothing
        | not exists' ->
            throwError
              $ userError "removeFile: no such file"
              `ioeSetFileName` path'
      Just ((_, Directory') :| _) ->
        throwError
          $ userError "removeFile: it is a directory"
          `ioeSetFileName` path'
      Nothing
        | isDir ->
            throwError
              $ userError "removeFile: it is a directory"
              `ioeSetFileName` path'
      _ -> do
        addChangeToFile path Gone
        return ()


  listDirectory path = do
    let normalizedPath = normalise path
    oFiles <- gets overlaidFiles
    path' <- decodePath path
    case oFiles !? normalizedPath of
      Just ((_, Gone) :| _) ->
        throwError
          $ userError "listDirectory: no such directory"
          `ioeSetFileName` path'
      Just ((_, Contents _) :| _) ->
        throwError
          $ userError "listDirectory: not a directory"
          `ioeSetFileName` path'
      Just ((_, Copied _) :| _) ->
        throwError
          $ userError "listDirectory: not a directory"
          `ioeSetFileName` path'
      Just ((_, Directory') :| _) ->
        return $ map takeFileName $ keys $ directOChildren oFiles
      Nothing -> do
        files <- liftIO $ System.Directory.OsPath.listDirectory path
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
