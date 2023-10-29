{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Repository
  ( Repository (..)
  , FileCorrespondence (..)
  , FileCorrespondenceWarning (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , listFiles
  , makeCorrespond
  , makeCorrespondBetweenThreeDirs
  , makeCorrespondBetweenThreeFiles
  , makeCorrespondBetweenTwoDirs
  ) where

import Control.Monad (forM, when)
import Data.List (unzip4)
import GHC.Stack (HasCallStack)
import System.IO.Error (ioeSetFileName)
import Prelude hiding (readFile)

import Control.Monad.Except (MonadError (..))
import Data.Map.Strict (Map, filter, fromList, keysSet, toList, unionWith, (!?))
import Data.Text (unpack)
import System.OsPath (OsPath, (</>))
import System.OsString (OsString)

import Data.Set (toList, union)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.MonadFileSystem qualified (FileType (..))
import Dojang.Types.Environment (Environment)
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning)
import Dojang.Types.FilePathExpression (EnvironmentVariable)
import Dojang.Types.FilePathExpression.Expansion
  ( ExpansionWarning
  , expandFilePath
  )
import Dojang.Types.FileRoute (FileRoute (..), dispatch)
import Dojang.Types.Manifest (Manifest (..))


-- | A repository, which is a directory containing a manifest file and dotfiles.
data Repository = Repository
  { sourcePath :: OsPath
  -- ^ The path to the repository.
  , intermediatePath :: OsPath
  -- ^ The path to the intermediate directory, which is managed by Dojang and
  -- contains the post-processed files.
  , manifestFilename :: OsPath
  -- ^ The filename of the manifest file.  This is relative to the repository
  -- 'path'.
  , manifest :: Manifest
  -- ^ The manifest of the repository.
  }


-- | The small stat of a file.
data FileStat
  = -- | The file is missing.
    Missing
  | -- | The file is a directory.
    Directory
  | -- | The file is a file, and this is its size in bytes.
    File Integer
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


-- | A warning that can occur during file correspondence.
data FileCorrespondenceWarning
  = -- | A warning that can occur during environment predicate evaluation.
    EnvironmentPredicateWarning EvaluationWarning
  | -- | A warning that can occur during file path expression expansion.
    FilePathExpressionWarning ExpansionWarning
  deriving (Eq, Show)


-- | Creates a list of file correspondences between the source files, the
-- intermediate files, and the destination files.  Throws an 'IOError' if any of
-- the files cannot be read.
makeCorrespond
  :: (HasCallStack, MonadFileSystem m)
  => Repository
  -- ^ The repository.
  -> Environment
  -- ^ The environment to use when evaluating environment predicates.
  -> (EnvironmentVariable -> m (Maybe OsString))
  -- ^ A function that can look up an environment variable.
  -> m ([FileCorrespondence], [FileCorrespondenceWarning])
  -- ^ The file correspondences, along with a list of warnings that occurred
  -- during file correspondence (if any).  The file paths in the returned
  -- 'FileCorrespondence' values are absolute, or relative to the current
  -- working directory at least.
makeCorrespond repo env lookupEnvVar = do
  paths <- forM fileRoutes $ \(src, route) -> do
    let (dstPathExprs, warnings) = dispatch env route
    return $ case dstPathExprs of
      (Just dstPathExpr) : _ ->
        (src, Just dstPathExpr, route.fileType, warnings)
      _ ->
        (src, Nothing, route.fileType, warnings)
  let (srcPaths, dstPathExprs, fileTypes, evalWarningLists) = unzip4 paths
  let paths' =
        [ (srcPath, dstPathExpr, t)
        | (srcPath, Just dstPathExpr, t) <- zip3 srcPaths dstPathExprs fileTypes
        ]
  pathInfos <- forM paths' $ \(srcPath, dstPathExpr, fileType') -> do
    (dstPath, warnings) <-
      expandFilePath dstPathExpr lookupEnvVar (encodePath . unpack)
    return (srcPath, dstPath, fileType', warnings)
  let expansionWarnings =
        [FilePathExpressionWarning w | (_, _, _, ws) <- pathInfos, w <- ws]
  files <- forM pathInfos $ \(srcPath, dstAbsPath, fileType', _) -> do
    let srcAbsPath = repo.sourcePath </> srcPath
    let interAbsPath = repo.intermediatePath </> srcPath
    case fileType' of
      Dojang.MonadFileSystem.Directory -> do
        fs <- makeCorrespondBetweenThreeDirs interAbsPath srcAbsPath dstAbsPath
        return
          [ correspond
            { source =
                correspond.source{path = srcAbsPath </> correspond.source.path}
            , intermediate =
                correspond.intermediate
                  { path = interAbsPath </> correspond.intermediate.path
                  }
            , destination =
                correspond.destination
                  { path = dstAbsPath </> correspond.destination.path
                  }
            }
          | correspond <- fs
          ]
      Dojang.MonadFileSystem.File -> do
        f <- makeCorrespondBetweenThreeFiles interAbsPath srcAbsPath dstAbsPath
        return [f]
  let evalWarnings =
        [EnvironmentPredicateWarning w | ws <- evalWarningLists, w <- ws]
  return (concat files, evalWarnings ++ expansionWarnings)
 where
  manifest :: Manifest
  manifest = repo.manifest
  fileRoutes :: [(OsPath, FileRoute)]
  fileRoutes = Data.Map.Strict.toList manifest.fileRoutes


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
  getDelta (FileEntry _ Missing) (FileEntry _ File{}) = return Added
  getDelta (FileEntry _ File{}) (FileEntry _ Missing) = return Removed
  getDelta interEntry targetEntry = do
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
  -> m [FileCorrespondence]
makeCorrespondBetweenThreeDirs intermediatePath srcPath dstPath = do
  srcEntries <- makeCorrespondBetweenTwoDirs intermediatePath srcPath
  dstEntries <- makeCorrespondBetweenTwoDirs intermediatePath dstPath
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
    return
      $ FileCorrespondence
        { source = srcEntry
        , sourceDelta = srcDelta
        , intermediate =
            if interEntry.stat == Missing
              then interEntry2
              else interEntry
        , destination = dstEntry
        , destinationDelta = dstDelta
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
  getDelta _ _ Directory (File _) = return Modified
  getDelta _ _ Directory Missing = return Removed
  getDelta _ _ (File _) Directory = return Modified
  getDelta path targetPath (File size) (File size')
    | size /= size' = return Modified
    | otherwise = do
        interData <- readFile (intermediatePath </> path)
        targetData <- readFile (targetPath </> path)
        return $ if interData == targetData then Unchanged else Modified
  getDelta _ _ (File _) Missing = return Removed
  getDelta _ _ Missing Missing = return Unchanged
  getDelta _ _ Missing _ = return Added


makeCorrespondBetweenTwoDirs
  :: (HasCallStack, MonadFileSystem m)
  => OsPath
  -> OsPath
  -> m (Map OsPath (FileEntry, FileEntry))
makeCorrespondBetweenTwoDirs intermediatePath targetPath = do
  intermediateDirExists <- exists intermediatePath
  intermediateFiles <-
    if intermediateDirExists
      then listFiles intermediatePath
      else return []
  targetFiles <- listFiles targetPath
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
listFiles :: (MonadFileSystem m) => OsPath -> m [FileEntry]
listFiles path = do
  isFile' <- isFile path
  when isFile' $ do
    path' <- decodePath path
    throwError
      $ userError "listFiles: path is not a directory"
      `ioeSetFileName` path'
  exists' <- exists path
  if exists'
    then do
      entries <- listDirectoryRecursively path
      forM entries $ \case
        (Dojang.MonadFileSystem.Directory, d) ->
          return $ FileEntry d Directory
        (Dojang.MonadFileSystem.File, f) -> do
          size <- getFileSize $ path </> f
          return $ FileEntry f $ File size
    else return []
