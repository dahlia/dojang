{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , RouteState (..)
  , getRouteState
  , listFiles
  , makeCorrespond
  , makeCorrespondBetweenThreeDirs
  , makeCorrespondBetweenThreeFiles
  , makeCorrespondBetweenTwoDirs
  , routePaths
  ) where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (isPrefixOf)
import GHC.IO.Exception (IOErrorType (InappropriateType))
import GHC.Stack (HasCallStack)
import System.IO.Error
  ( ioeSetErrorString
  , ioeSetFileName
  , isPermissionError
  , mkIOError
  )
import Prelude hiding (readFile)

import Control.Monad.Except (MonadError (..))
import Data.Map.Strict
  ( Map
  , filter
  , findWithDefault
  , fromList
  , keysSet
  , unionWith
  , (!?)
  )
import System.Directory.OsPath (makeAbsolute)
import System.FilePattern (FilePattern, matchMany)
import System.OsPath (OsPath, joinPath, normalise, splitDirectories, (</>))
import System.OsString (OsString)

import Data.Set (toList, union)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.MonadFileSystem qualified (FileType (..))
import Dojang.Types.Environment (Environment)
import Dojang.Types.FilePathExpression (EnvironmentVariable)
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.Repository
  ( Repository (..)
  , RouteMapWarning
  , RouteResult (..)
  )
import Dojang.Types.Repository qualified (routePaths)


-- | The context in which repository operations are performed.
data (MonadFileSystem m) => Context m = Context
  { repository :: Repository
  -- ^ The repository.
  , environment :: Environment
  -- ^ The environment.
  , environmentVariableGetter :: EnvironmentVariable -> m (Maybe OsString)
  -- ^ A function to look up an environment variable.
  }


-- | Route the paths in the repository.  This will return a list of expanded
-- paths, along with any warnings that were generated.  Null routes will be
-- ignored.
routePaths
  :: (MonadFileSystem m)
  => Context m
  -- ^ The context in which to perform path routing.
  -> m ([RouteResult], [RouteMapWarning])
  -- ^ The expanded paths, along with any warnings that were generated.
routePaths ctx =
  Dojang.Types.Repository.routePaths
    ctx.repository
    ctx.environment
    ctx.environmentVariableGetter


-- | The small stat of a file.
data FileStat
  = -- | The file is missing.
    Missing
  | -- | The file is a directory.
    Directory
  | -- | The file is a regular file, and this is its size in bytes.
    File Integer
  | -- | The file is a symlink, and this is the path to the symlink target.
    Symlink OsPath
  deriving (Eq, Ord, Show)


-- | A file/directory in the repository or the destination directory.
data FileEntry = FileEntry
  { path :: OsPath
  -- ^ The path to the file.
  , stat :: FileStat
  -- ^ Whether the file is a file, a directory, or missing.
  }
  deriving (Eq, Ord, Show)


-- | The kind of change that was made to a file.
data FileDeltaKind = Unchanged | Added | Removed | Modified
  deriving (Eq, Ord, Show)


-- | A correspondence between a source file, an intermediate file, and
-- a destination file.
data FileCorrespondence = FileCorrespondence
  { source :: FileEntry
  -- ^ The source file.
  , sourceDelta :: FileDeltaKind
  -- ^ The kind of change that was made to the source file.
  , intermediate :: FileEntry
  -- ^ The intermediate file.
  , destination :: FileEntry
  -- ^ The destination file.
  , destinationDelta :: FileDeltaKind
  -- ^ The kind of change that was made to the destination file.
  }
  deriving (Eq, Ord, Show)


-- | Creates a list of file correspondences between the source files, the
-- intermediate files, and the destination files.  Throws an 'IOError' if any of
-- the files cannot be read.
makeCorrespond
  :: (HasCallStack, MonadFileSystem m)
  => Context m
  -- ^ The context in which to perform file correspondence.
  -> m ([FileCorrespondence], [RouteMapWarning])
  -- ^ The file correspondences, along with a list of warnings that occurred
  -- during path routing (if any).  The file paths in the returned
  -- 'FileCorrespondence' values are absolute, or relative to the current
  -- working directory at least.
makeCorrespond ctx = do
  (paths, warnings) <- routePaths ctx
  files <- forM paths $ \expanded -> do
    let interAbsPath = repo.intermediatePath </> expanded.routeName
    case expanded.fileType of
      Dojang.MonadFileSystem.Directory -> do
        fs <-
          makeCorrespondBetweenThreeDirs
            interAbsPath
            expanded.sourcePath
            expanded.destinationPath
            (findWithDefault [] (normalise expanded.routeName) ignorePatterns)
        return
          [ correspond
            { source =
                correspond.source{path = expanded.sourcePath </> correspond.source.path}
            , intermediate =
                correspond.intermediate
                  { path = interAbsPath </> correspond.intermediate.path
                  }
            , destination =
                correspond.destination
                  { path =
                      expanded.destinationPath </> correspond.destination.path
                  }
            }
          | correspond <- fs
          ]
      _ -> do
        f <-
          makeCorrespondBetweenThreeFiles
            interAbsPath
            expanded.sourcePath
            expanded.destinationPath
        return [f]
  return (concat files, warnings)
 where
  repo :: Repository
  repo = ctx.repository
  ignorePatterns :: Map OsPath [FilePattern]
  ignorePatterns = repo.manifest.ignorePatterns


makeCorrespondBetweenThreeFiles
  :: forall m
   . (HasCallStack, MonadFileSystem m)
  => OsPath
  -> OsPath
  -> OsPath
  -> m FileCorrespondence
makeCorrespondBetweenThreeFiles intermediatePath srcPath dstPath = do
  interStat <- getFileStat intermediatePath
  let interEntry = FileEntry intermediatePath interStat
  srcStat <- getFileStat srcPath
  let srcEntry = FileEntry srcPath srcStat
  srcDelta <- getDelta interEntry srcEntry
  dstStat <- getFileStat dstPath
  let dstEntry = FileEntry dstPath dstStat
  dstDelta <- getDelta interEntry dstEntry
  return
    FileCorrespondence
      { source = srcEntry
      , sourceDelta = srcDelta
      , intermediate = interEntry
      , destination = dstEntry
      , destinationDelta = dstDelta
      }
 where
  getFileStat :: OsPath -> m FileStat
  getFileStat path = do
    isDir <- isDirectory path
    when isDir $ do
      path' <- decodePath path
      throwError
        $ userError
          ( "makeCorrespondBetweenThreeFiles: "
              ++ "expected a file, but got a directory"
          )
        `ioeSetFileName` path'
    exists' <- exists path
    if exists'
      then do
        size <- getFileSize path
        return $ File size
      else return Missing
  getDelta :: FileEntry -> FileEntry -> m FileDeltaKind
  getDelta (FileEntry _ Missing) (FileEntry _ Missing) = return Unchanged
  getDelta (FileEntry _ Missing) (FileEntry _ _) = return Added
  getDelta (FileEntry _ File{}) (FileEntry _ Missing) = return Removed
  getDelta (FileEntry _ Symlink{}) (FileEntry _ Missing) = return Removed
  getDelta (FileEntry _ (Symlink path)) (FileEntry _ (Symlink path')) =
    if path == path'
      then return Unchanged
      else return Modified
  getDelta interEntry targetEntry =
    if interEntry.stat /= targetEntry.stat
      then return Modified
      else do
        interData <- readFile interEntry.path
        targetData <- readFile targetEntry.path
        return $ if interData == targetData then Unchanged else Modified


makeCorrespondBetweenThreeDirs
  :: forall m
   . (HasCallStack, MonadFileSystem m)
  => OsPath
  -> OsPath
  -> OsPath
  -> [FilePattern]
  -> m [FileCorrespondence]
makeCorrespondBetweenThreeDirs intermediatePath srcPath dstPath ignores = do
  srcEntries <- makeCorrespondBetweenTwoDirs intermediatePath srcPath []
  dstEntries <-
    makeCorrespondBetweenTwoDirs intermediatePath dstPath ignores
  let paths = keysSet srcEntries `union` keysSet dstEntries
  forM (Data.Set.toList paths) $ \path -> do
    let missing = FileEntry path Missing
    (interEntry, srcEntry, srcDelta) <- case srcEntries !? path of
      Nothing -> return (missing, missing, Unchanged)
      Just (interEntry', srcEntry') -> do
        delta <-
          getDelta path srcPath interEntry'.stat srcEntry'.stat
        return (interEntry', srcEntry', delta)
    (interEntry2, dstEntry, dstDelta) <- case dstEntries !? path of
      Nothing -> return (missing, missing, Unchanged)
      Just (interEntry', dstEntry') -> do
        delta <- getDelta path dstPath interEntry'.stat dstEntry'.stat
        return (interEntry', dstEntry', delta)
    (dstEntry', dstDelta') <-
      if null ignores || dstEntry.stat /= Missing
        then return (dstEntry, dstDelta)
        else do
          actualDstStat <- getFileStat (dstPath </> path)
          actualDelta <- getDelta path dstPath interEntry2.stat actualDstStat
          return (dstEntry{stat = actualDstStat}, actualDelta)
    return
      $ FileCorrespondence
        { source = srcEntry
        , sourceDelta = srcDelta
        , intermediate =
            if interEntry.stat == Missing
              then interEntry2
              else interEntry
        , destination = dstEntry'
        , destinationDelta = dstDelta'
        }
 where
  getDelta
    :: OsPath
    -- \^ The relative path to the file.
    -> OsPath
    -- \^ The path to the target directory.
    -> FileStat
    -- \^ An intermediate file stat.
    -> FileStat
    -- \^ A target (source or destination) file stat.
    -> m FileDeltaKind
  getDelta _ _ Directory Directory = return Unchanged
  getDelta _ _ Directory Missing = return Removed
  getDelta _ _ Directory _ = return Modified
  getDelta path targetPath (File size) (File size') =
    if size /= size'
      then return Modified
      else do
        interData <- readFile (intermediatePath </> path)
        targetData <- readFile (targetPath </> path)
        return $ if interData == targetData then Unchanged else Modified
  getDelta _ _ (File _) Missing = return Removed
  getDelta _ _ (File _) _ = return Modified
  getDelta _ _ (Symlink path) (Symlink path') =
    if path == path'
      then return Unchanged
      else return Modified
  getDelta _ _ (Symlink _) Missing = return Added
  getDelta _ _ (Symlink _) _ = return Modified
  getDelta _ _ Missing Missing = return Unchanged
  getDelta _ _ Missing _ = return Added
  getFileStat :: OsPath -> m FileStat
  getFileStat path' = do
    isSym <- isSymlink path'
    if isSym
      then do
        target <- readSymlinkTarget path'
        return $ Symlink target
      else do
        isDir <- isDirectory path'
        if isDir
          then return Directory
          else do
            isFle <- isFile path'
            if isFle
              then do
                size <- getFileSize path'
                return $ File size
              else return Missing


makeCorrespondBetweenTwoDirs
  :: forall m
   . (HasCallStack, MonadFileSystem m)
  => OsPath
  -> OsPath
  -> [FilePattern]
  -> m (Map OsPath (FileEntry, FileEntry))
makeCorrespondBetweenTwoDirs intermediatePath targetPath ignorePatterns = do
  intermediateDirExists <- exists intermediatePath
  intermediateFiles <-
    if intermediateDirExists
      then listFiles intermediatePath []
      else return []
  targetFiles <- listFiles targetPath ignorePatterns
  let intermediateEntries =
        fromList
          [(p, (e, e{stat = Missing})) | e@(FileEntry p _) <- intermediateFiles]
      targetEntries =
        fromList
          [(p, (e{stat = Missing}, e)) | e@(FileEntry p _) <- targetFiles]
      allEntries = unionWith combinePairs intermediateEntries targetEntries
  return $ Data.Map.Strict.filter eitherExists allEntries
 where
  combinePairs
    :: (FileEntry, FileEntry)
    -> (FileEntry, FileEntry)
    -> (FileEntry, FileEntry)
  combinePairs (a1, b1) (a2, b2)
    | isNotMissing a1 && isNotMissing b2 = (a1, b2)
    | isNotMissing a2 && isNotMissing b1 = (a2, b1)
    | isMissing a1 && isNotMissing b2 = (a2, b2)
    | otherwise = (a1, b1)
   where
    isMissing :: FileEntry -> Bool
    isMissing (FileEntry _ Missing) = True
    isMissing _ = False
    isNotMissing :: FileEntry -> Bool
    isNotMissing = not . isMissing
  eitherExists :: (FileEntry, FileEntry) -> Bool
  eitherExists (FileEntry _ Missing, FileEntry _ Missing) = False
  eitherExists _ = True


-- | Lists all 'FilEntry' values in the given directory.  Throws an 'IOError' if
-- the directory cannot be read.
listFiles
  :: (MonadFileSystem m)
  => OsPath
  -- ^ The path to the directory to list entries in.
  -> [FilePattern]
  -- ^ The file patterns to ignore.  If a directory matches one of these
  -- patterns, then its contents will not be listed either.
  -> m [FileEntry]
  -- ^ The list of 'FileEntry' values in the directory.
listFiles path ignorePatterns = do
  isSymlink' <- isSymlink path
  isFile' <- isFile path
  when (isSymlink' || isFile') $ do
    path' <- decodePath path
    throwError
      $ mkIOError InappropriateType "listFiles" Nothing (Just path')
      `ioeSetErrorString` "not a directory"
  exists' <- exists path
  if exists'
    then do
      entries <-
        listDirectoryRecursively path ignorePatterns `catchError` \e ->
          if isPermissionError e
            then return []
            else throwError e
      forM entries $ \case
        (Dojang.MonadFileSystem.Directory, d) ->
          return $ FileEntry d Directory
        (Dojang.MonadFileSystem.File, f) -> do
          size <- getFileSize $ path </> f
          return $ FileEntry f $ File size
        (Dojang.MonadFileSystem.Symlink, s) -> do
          target <- readSymlinkTarget $ path </> s
          return $ FileEntry s $ Symlink target
    else return []


-- | Represents the route state of a (potential) destination file
-- in the repository.
data RouteState
  = -- | The file is routed.  The 'OsPath' is the name of the relevant route
    -- name.
    Routed OsPath
  | -- | The file is routed but ignored.  The 'OsPath' is the name of
    -- the relevant route name, and the 'FilePattern' is the ignore pattern
    -- that matched the file.
    Ignored OsPath FilePattern
  | -- | The file is not routed at all.
    NotRouted
  deriving (Eq, Ord, Show)


-- | Gets the route state of the given path (which is a potential destination).
getRouteState
  :: (HasCallStack, MonadFileSystem m, MonadIO m)
  => Context m
  -- ^ The context in which to perform path routing.
  -> OsPath
  -- ^ The path (which is a potential destination) to get the route state of.
  -> m (RouteState, [RouteMapWarning])
  -- ^ The route state of the path, along with any warnings that were generated.
getRouteState ctx path = do
  absPath <- liftIO $ makeAbsolute path
  let dirs = splitDirectories absPath
  (routes, ws) <- routePaths ctx
  states <- forM routes $ \route -> do
    dstPath <- liftIO $ makeAbsolute route.destinationPath
    let prefix = splitDirectories dstPath
    if prefix `isPrefixOf` dirs
      then do
        let ignores =
              findWithDefault
                []
                (normalise route.routeName)
                ignorePatterns
        relPath <- decodePath $ joinPath $ drop (length prefix) dirs
        case matchMany [(i, i) | i <- ignores] [((), relPath)] of
          (pattern, _, _) : _ -> return $ Ignored route.routeName pattern
          _ -> return $ Routed route.routeName
      else return NotRouted
  return (if null states then NotRouted else minimum states, ws)
 where
  ignorePatterns :: Map OsPath [FilePattern]
  ignorePatterns = ctx.repository.manifest.ignorePatterns
