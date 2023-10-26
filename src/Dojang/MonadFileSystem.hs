{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Dojang.MonadFileSystem
  ( DryRunIO
  , FileType (..)
  , MonadFileSystem (..)
  , dryRunIO
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, sort)
import Data.List.NonEmpty (NonEmpty ((:|)), filter, singleton, toList)
import System.IO.Error (ioeSetFileName, tryIOError)
import Prelude hiding (filter, readFile, writeFile)

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
  deriving (Eq, Ord, Show)


-- | A monad that can perform filesystem operations.  It's also based on
-- 'OsPath' instead of 'FilePath'.
class (Monad m) => MonadFileSystem m where
  -- | Encodes a 'FilePath' into an 'OsPath'.
  encodePath :: FilePath -> m OsPath


  -- | Decodes a 'OsPath' into a 'FilePath'.
  decodePath :: OsPath -> m FilePath


  -- | Checks if a file (or directory) exists.
  exists :: OsPath -> m Bool
  exists path = do
    isFile' <- isFile path
    if isFile' then return True else isDirectory path


  -- | Checks if a path exists and is a file.
  isFile :: OsPath -> m Bool


  -- | Checks if a path exists and is a directory.
  isDirectory :: OsPath -> m Bool


  -- | Reads contents from a file.
  readFile :: OsPath -> m (Either IOError ByteString)


  -- | Writes contents into a file.
  writeFile :: OsPath -> ByteString -> m (Either IOError ())


  -- | Copies a file from one path to another.
  copyFile
    :: OsPath
    -- ^ Source path.
    -> OsPath
    -- ^ Destination path.
    -> m (Either IOError ())


  -- | Creates a directory at the given path.
  createDirectory :: OsPath -> m (Either IOError ())


  -- | Creates a directory at the given path, including all parent directories.
  createDirectories :: OsPath -> m (Either IOError ())
  createDirectories path = do
    let parent = takeDirectory path
    parent' <- decodePath parent
    parentExists <- isDirectory parent
    if parentExists
      then createDirectory path
      else do
        parentIsFile <- isFile parent
        if parentIsFile
          then
            return
              $ Left
              $ userError "createDirectories: one of its ancestors is a file"
              `ioeSetFileName` parent'
          else do
            result <- createDirectories parent
            case result of
              Left e -> return $ Left e
              Right _ -> createDirectory path


  -- | Removes a file.
  removeFile :: OsPath -> m (Either IOError ())


  -- | Lists all files and directories in a directory except for @.@ and @..@,
  -- without recursing into subdirectories.
  listDirectory :: OsPath -> m (Either IOError [OsPath])


  -- | Lists all files and directories in a directory recursively.  It doesn't
  -- include @.@ and @..@.  Paths are relative to the given directory,
  -- and directories always go before their contents.
  listDirectoryRecursively :: OsPath -> m (Either IOError [(FileType, OsPath)])
  listDirectoryRecursively path = do
    result <- listDirectory path
    case result of
      Left e -> return $ Left e
      Right entries -> do
        (dirs, files) <- partitionM (isDirectory . (path </>)) entries
        files' <- forM files $ \file -> return (File, file)
        dirs' <- forM dirs $ \dir -> do
          result' <- listDirectoryRecursively (path </> dir)
          case result' of
            Left e -> return $ Left e
            Right entries' ->
              return $ Right $ (Directory, dir) : (fmap (dir </>) <$> entries')
        case partitionEithers dirs' of
          ([], dirs'') -> return $ Right $ files' ++ concat dirs''
          (e : _, _) -> return $ Left e


  -- | Gets the size of a file in bytes.  If the file doesn't exist or is
  -- a directory, then it returns 'Left'.
  getFileSize :: OsPath -> m (Either IOError Integer)


instance MonadFileSystem IO where
  encodePath = encodeFS


  decodePath = decodeFS


  exists = doesPathExist


  isFile = doesFileExist


  isDirectory = doesDirectoryExist


  readFile src = decodePath src >>= tryIOError . Data.ByteString.readFile


  writeFile dst contents = do
    dst' <- decodePath dst
    tryIOError $ Data.ByteString.writeFile dst' contents


  copyFile src = tryIOError . System.Directory.OsPath.copyFile src


  createDirectory = tryIOError . System.Directory.OsPath.createDirectory


  removeFile = tryIOError . System.Directory.OsPath.removeFile


  listDirectory = tryIOError . System.Directory.OsPath.listDirectory


  getFileSize path = do
    isDir <- isDirectory path
    if isDir
      then do
        path' <- decodePath path
        return
          $ Left
          $ userError "getFileSize: it is a directory"
          `ioeSetFileName` path'
      else tryIOError $ System.Directory.OsPath.getFileSize path


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
    (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState DryRunState)


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


readFileFromDryRunIO :: SeqNo -> OsPath -> DryRunIO (Either IOError ByteString)
readFileFromDryRunIO seqOffset src = do
  oFiles <- gets overlaidFiles
  case oFiles !? normalise src of
    Nothing -> fallback
    Just changes ->
      let filteredChanges = filter (\(no, _) -> no <= seqOffset) changes
      in case filteredChanges of
          [] -> fallback
          (_, Contents contents) : _ -> return $ Right contents
          (seqNo, Copied src') : _ ->
            readFileFromDryRunIO seqNo src'
          (_, Gone) : _ -> do
            src' <- decodePath src
            return
              $ Left
              $ userError "readFile: no such file"
              `ioeSetFileName` src'
          (_, Directory') : _ -> do
            src' <- decodePath src
            return
              $ Left
              $ userError "readFile: it is a directory"
              `ioeSetFileName` src'
 where
  fallback :: DryRunIO (Either IOError ByteString)
  fallback = liftIO $ tryIOError $ do
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
        return
          $ Left
          $ userError "writeFile: no parent directory"
          `ioeSetFileName` dst'
      (Nothing, _)
        | not dstParentExists ->
            return
              $ Left
              $ userError "writeFile: no parent directory"
              `ioeSetFileName` dst'
      (Just ((_, Contents _) :| _), _) ->
        return
          $ Left
          $ userError "writeFile: destination must be inside a directory"
          `ioeSetFileName` dst'
      (Just ((_, Copied _) :| _), _) ->
        return
          $ Left
          $ userError "writeFile: destination must be inside a directory"
          `ioeSetFileName` dst'
      (Nothing, _)
        | not dstDirExists ->
            return
              $ Left
              $ userError "writeFile: destination must be inside a directory"
              `ioeSetFileName` dst'
      (_, Just ((_, Directory') :| _)) ->
        return
          $ Left
          $ userError "writeFile: destination is a directory"
          `ioeSetFileName` dst'
      (_, Nothing)
        | dstIsDir ->
            return
              $ Left
              $ userError "writeFile: destination is a directory"
              `ioeSetFileName` dst'
      _ -> do
        addChangeToFile dst $ Contents contents
        return $ Right ()


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
        return
          $ Left
          $ userError "copyFile: source does not exist"
          `ioeSetFileName` src'
      Nothing
        | not srcExists ->
            return
              $ Left
              $ userError "copyFile: source does not exist"
              `ioeSetFileName` src'
      Just ((_, Directory') :| _) ->
        return
          $ Left
          $ userError "copyFile: source is a directory"
          `ioeSetFileName` src'
      Nothing
        | srcIsDir ->
            return
              $ Left
              $ userError "copyFile: source is a directory"
              `ioeSetFileName` src'
      _ -> case (oFiles !? dstDir, oFiles !? normalise dst) of
        (Just ((_, Gone) :| _), _) ->
          return
            $ Left
            $ userError "copyFile: destination has no parent directory"
            `ioeSetFileName` dst'
        (Nothing, _)
          | not dstDirExists ->
              return
                $ Left
                $ userError "copyFile: destination has no parent directory"
                `ioeSetFileName` dst'
        (Just ((_, Contents _) :| _), _) ->
          return
            $ Left
            $ userError "copyFile: destination must be inside a directory"
            `ioeSetFileName` dst'
        (Just ((_, Copied _) :| _), _) ->
          return
            $ Left
            $ userError "copyFile: destination must be inside a directory"
            `ioeSetFileName` dst'
        (Nothing, _)
          | not dstDirIsDir ->
              return
                $ Left
                $ userError "copyFile: destination must be inside a directory"
                `ioeSetFileName` dst'
        (_, Just ((_, Directory') :| _)) ->
          return
            $ Left
            $ userError "copyFile: destination is a directory"
            `ioeSetFileName` dst'
        (_, Nothing)
          | dstIsDir ->
              return
                $ Left
                $ userError "copyFile: destination is a directory"
                `ioeSetFileName` dst'
        _ -> do
          addChangeToFile dst $ Copied src
          return $ Right ()


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
        return
          $ Left
          $ userError "createDirectory: no parent directory"
          `ioeSetFileName` dst'
      (Nothing, _)
        | not parentExists ->
            return
              $ Left
              $ userError "createDirectory: no parent directory"
              `ioeSetFileName` dst'
      (Just ((_, Contents _) :| _), _) ->
        return
          $ Left
          $ userError "createDirectory: destination must be inside a directory"
          `ioeSetFileName` dst'
      (Just ((_, Copied _) :| _), _) ->
        return
          $ Left
          $ userError "createDirectory: destination must be inside a directory"
          `ioeSetFileName` dst'
      (Nothing, _)
        | not parentIsDir ->
            return
              $ Left
              $ userError
                "createDirectory: destination must be inside a directory"
              `ioeSetFileName` dst'
      (_, Just ((_, Contents _) :| _)) ->
        return
          $ Left
          $ userError "createDirectory: destination is already a file"
          `ioeSetFileName` dst'
      (_, Just ((_, Copied _) :| _)) ->
        return
          $ Left
          $ userError "createDirectory: destination is already a file"
          `ioeSetFileName` dst'
      (Nothing, _)
        | isFile' ->
            return
              $ Left
              $ userError "createDirectory: destination is already a file"
              `ioeSetFileName` dst'
      (_, Just ((_, Directory') :| _)) ->
        return
          $ Left
          $ userError "createDirectory: destination is already a directory"
          `ioeSetFileName` dst'
      _
        | isDir ->
            return
              $ Left
              $ userError "createDirectory: destination is already a directory"
              `ioeSetFileName` dst'
      _ -> do
        addChangeToFile dst Directory'
        return $ Right ()


  removeFile path = do
    oFiles <- gets overlaidFiles
    path' <- decodePath path
    exists' <- liftIO $ doesPathExist path
    isDir <- liftIO $ doesDirectoryExist path
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) ->
        return
          $ Left
          $ userError "removeFile: no such file"
          `ioeSetFileName` path'
      Nothing
        | not exists' ->
            return
              $ Left
              $ userError "removeFile: no such file"
              `ioeSetFileName` path'
      Just ((_, Directory') :| _) ->
        return
          $ Left
          $ userError "removeFile: it is a directory"
          `ioeSetFileName` path'
      Nothing
        | isDir ->
            return
              $ Left
              $ userError "removeFile: it is a directory"
              `ioeSetFileName` path'
      _ -> do
        addChangeToFile path Gone
        return $ Right ()


  listDirectory path = do
    let normalizedPath = normalise path
    oFiles <- gets overlaidFiles
    path' <- decodePath path
    case oFiles !? normalizedPath of
      Just ((_, Gone) :| _) ->
        return
          $ Left
          $ userError "listDirectory: no such directory"
          `ioeSetFileName` path'
      Just ((_, Contents _) :| _) ->
        return
          $ Left
          $ userError "listDirectory: not a directory"
          `ioeSetFileName` path'
      Just ((_, Copied _) :| _) ->
        return
          $ Left
          $ userError "listDirectory: not a directory"
          `ioeSetFileName` path'
      Just ((_, Directory') :| _) ->
        return $ Right $ map takeFileName $ keys $ directOChildren oFiles
      Nothing -> do
        list <- liftIO $ tryIOError $ System.Directory.OsPath.listDirectory path
        case list of
          Left e -> return $ Left e
          Right files -> do
            let directOChildren' = directOChildren oFiles
            let result =
                  [f | f <- files, directOChildren' !? (path </> f) /= Just Gone]
                    ++ [ filename
                       | (filePath, f) <- toAscList directOChildren'
                       , f /= Gone
                       , let filename = makeRelative path filePath
                       , filename `notElem` files
                       ]
            return $ Right $ sort result
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
        return
          $ Left
          $ userError "getFileSize: no such file"
          `ioeSetFileName` path'
      Just ((_, Directory') :| _) ->
        return
          $ Left
          $ userError "getFileSize: it is a directory"
          `ioeSetFileName` path'
      Just _ -> do
        result <- readFile path
        case result of
          Left e -> return $ Left e
          Right contents ->
            return
              $ Right
              $ fromIntegral
              $ Data.ByteString.length contents
      Nothing -> do
        isDir <- isDirectory path
        if isDir
          then
            return
              $ Left
              $ userError "getFileSize: it is a directory"
              `ioeSetFileName` path'
          else do
            size <-
              liftIO
                $ tryIOError
                $ System.Directory.OsPath.getFileSize path
            case size of
              Left e -> return $ Left e
              Right size' -> return $ Right size'


dryRunIO :: DryRunIO a -> IO a
dryRunIO action = evalStateT (unDryRunIO action) initialState
 where
  initialState = DryRunState{overlaidFiles = mempty, nextSequenceNumber = 0}
