{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Repository
  ( Repository (..)
  , FileEntry (..)
  , FileStat (..)
  , listFiles
  , makeCorrespondBetweenTwoDirs
  ) where

import Control.Monad (forM, unless)
import System.IO.Error (ioeSetFileName)

import Control.Monad.Except (MonadError (..))
import Data.Map.Strict (Map, filter, fromList, unionWith)
import System.OsPath (OsPath, (</>))

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.MonadFileSystem qualified (FileType (..))
import Dojang.Types.Manifest (Manifest (..))


-- | A repository, which is a directory containing a manifest file and dotfiles.
data Repository = Repository
  { path :: OsPath
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


makeCorrespondBetweenTwoDirs
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> m (Map OsPath (Maybe FileEntry, Maybe FileEntry))
makeCorrespondBetweenTwoDirs intermediatePath targetPath = do
  intermediateDirExists <- exists intermediatePath
  intermediateFiles <-
    if intermediateDirExists
      then listFiles intermediatePath
      else return []
  targetFiles <- listFiles targetPath
  let intermediateEntries =
        fromList
          [(p, (Just e, Nothing)) | e@(FileEntry p _) <- intermediateFiles]
      targetEntries =
        fromList
          [(p, (Nothing, Just e)) | e@(FileEntry p _) <- targetFiles]
      allEntries = unionWith combinePairs intermediateEntries targetEntries
  return $ Data.Map.Strict.filter eitherExists allEntries
 where
  combinePairs
    :: (Maybe FileEntry, Maybe FileEntry)
    -> (Maybe FileEntry, Maybe FileEntry)
    -> (Maybe FileEntry, Maybe FileEntry)
  combinePairs (Just a, _) (_, Just b) = (Just a, Just b)
  combinePairs (_, Just b) (Just a, _) = (Just a, Just b)
  combinePairs (Just a, _) (_, Nothing) = (Just a, Nothing)
  combinePairs (_, Just b) (Nothing, _) = (Nothing, Just b)
  combinePairs (Nothing, Nothing) pair = pair
  eitherExists :: (Maybe FileEntry, Maybe FileEntry) -> Bool
  eitherExists (Just _, _) = True
  eitherExists (_, Just _) = True
  eitherExists _ = False


-- | Lists all 'FilEntry' values in the given directory.  Throws an 'IOError' if
-- the directory cannot be read.
listFiles :: (MonadFileSystem m) => OsPath -> m [FileEntry]
listFiles path = do
  isDir <- isDirectory path
  unless isDir $ do
    path' <- decodePath path
    throwError
      $ userError "listFiles: path is not a directory"
      `ioeSetFileName` path'
  entries <- listDirectoryRecursively path
  forM entries $ \case
    (Dojang.MonadFileSystem.Directory, d) ->
      return $ FileEntry d Directory
    (Dojang.MonadFileSystem.File, f) -> do
      size <- getFileSize $ path </> f
      return $ FileEntry f $ File size
