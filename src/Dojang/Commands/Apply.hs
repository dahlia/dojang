{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Dojang.Commands.Apply (apply) where

import Control.Monad (filterM, forM_, void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.List (nub, sort)
import System.Exit (ExitCode (..))
import System.IO (stderr)
import Prelude hiding (readFile)

import Control.Monad.Logger (logDebugSH)
import Data.Text (pack)
import System.OsPath (OsPath, addTrailingPathSeparator, takeDirectory)

import Dojang.App (App, AppEnv (debug), ensureContext)
import Dojang.Commands
  ( Admonition (..)
  , pathStyleFor
  , printStderr
  , printStderr'
  )
import Dojang.Commands.Status (printWarnings, status)
import Dojang.ExitCodes (conflictError)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Context
  ( Context
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , RouteState (..)
  , getRouteState
  , makeCorrespond
  )


apply :: (MonadFileSystem i, MonadIO i) => Bool -> App i ExitCode
apply force = do
  ctx <- ensureContext
  (files, ws) <- makeCorrespond ctx
  $(logDebugSH) files
  conflicts <- filterConflicts files
  pathStyle <- pathStyleFor stderr
  forM_ conflicts $ \c -> do
    srcPath <- decodePath c.source.path
    dstPath <- decodePath c.destination.path
    printStderr' Error
      $ "There is a conflict between "
      <> pathStyle (pack srcPath)
      <> " and "
      <> pathStyle (pack dstPath)
      <> "."
  if not force && not (null conflicts)
    then return conflictError
    else do
      debug' <- asks (.debug)
      when debug' (void $ status False)
      let ops =
            syncSourceToIntermediate
              [ (fc.source, fc.intermediate, fc.sourceDelta)
              | fc <- files
              ]
              & nub
              . sort
      forM_ ops $ \path -> printSyncOp path >> doSyncOp path
      when debug' (void $ status False)
      (files', _) <- makeCorrespond ctx
      $(logDebugSH) files'
      ops' <-
        syncIntermediateToDestination
          ctx
          [ (fc.intermediate, fc.destination, fc.destinationDelta)
          | fc <- files'
          ]
      forM_ (nub $ sort ops') $ \path -> printSyncOp path >> doSyncOp path
      printWarnings ws
      return ExitSuccess


-- TODO: This should be in another module:
filterConflicts
  :: (MonadFileSystem i, MonadIO i)
  => [FileCorrespondence]
  -> i [FileCorrespondence]
filterConflicts = filterM $ \c -> case (c.sourceDelta, c.destinationDelta) of
  (Unchanged, _) -> return False
  (_, Unchanged) -> return False
  (Removed, Removed) -> return False
  (Added, Added) -> case (c.source.stat, c.destination.stat) of
    (Directory, Directory) -> return False
    (File srcSize, File dstSize) ->
      if srcSize /= dstSize
        then return True
        else do
          src <- readFile c.source.path
          dst <- readFile c.destination.path
          return (src /= dst)
    _ -> return True
  _ -> return True


data SyncOp
  = RemoveDirs OsPath
  | RemoveFile OsPath
  | CopyFile OsPath OsPath
  | CreateDir OsPath
  | CreateDirs OsPath
  deriving (Eq, Show)


syncOpOrdKey :: SyncOp -> (OsPath, Int, OsPath)
syncOpOrdKey (RemoveFile path) = (takeDirectory path, 1, path)
syncOpOrdKey (RemoveDirs path) = (path, 2, path)
syncOpOrdKey (CreateDir path) = (path, 3, path)
syncOpOrdKey (CreateDirs path) = (path, 4, path)
syncOpOrdKey (CopyFile _ dst) = (takeDirectory dst, 5, dst)


instance Ord SyncOp where
  compare a b = compare (syncOpOrdKey a) (syncOpOrdKey b)


printSyncOp :: (MonadFileSystem i, MonadIO i) => SyncOp -> App i ()
printSyncOp (RemoveDirs path) = do
  pathStyle <- pathStyleFor stderr
  path' <- decodePath $ addTrailingPathSeparator path
  printStderr ("Remove " <> pathStyle (pack path') <> " (and its children)...")
printSyncOp (RemoveFile path) = do
  pathStyle <- pathStyleFor stderr
  path' <- decodePath path
  printStderr ("Remove " <> pathStyle (pack path') <> "...")
printSyncOp (CopyFile src dst) = do
  pathStyle <- pathStyleFor stderr
  src' <- decodePath src
  dst' <- decodePath dst
  printStderr
    ( "Copy "
        <> pathStyle (pack src')
        <> " to "
        <> pathStyle (pack dst')
        <> "..."
    )
printSyncOp (CreateDir path) = do
  pathStyle <- pathStyleFor stderr
  path' <- decodePath $ addTrailingPathSeparator path
  printStderr ("Create " <> pathStyle (pack path') <> "...")
printSyncOp (CreateDirs path) = do
  pathStyle <- pathStyleFor stderr
  path' <- decodePath $ addTrailingPathSeparator path
  printStderr ("Create " <> pathStyle (pack path') <> " (and its ancestors)...")


doSyncOp :: (MonadFileSystem i, MonadIO i) => SyncOp -> App i ()
doSyncOp (RemoveDirs path) = removeDirectoryRecursively path
doSyncOp (RemoveFile path) = removeFile path
doSyncOp (CopyFile src dst) = copyFile src dst
doSyncOp (CreateDir path) = createDirectory path
doSyncOp (CreateDirs path) = createDirectories path


syncSourceToIntermediate
  :: [(FileEntry, FileEntry, FileDeltaKind)]
  -> [SyncOp]
syncSourceToIntermediate files =
  concat
    [ case delta of
      Unchanged -> []
      Removed ->
        if to.stat == Directory
          then [RemoveDirs to.path]
          else [RemoveFile to.path]
      Modified ->
        case (from.stat, to.stat) of
          (Directory, _) -> [RemoveFile to.path, CreateDir to.path]
          (_, Directory) -> [RemoveDirs to.path, CopyFile from.path to.path]
          _ -> [CopyFile from.path to.path]
      Added ->
        if from.stat == Directory
          then [CreateDirs to.path]
          else [CreateDirs $ takeDirectory to.path, CopyFile from.path to.path]
    | (from, to, delta) <- files
    ]


syncIntermediateToDestination
  :: (MonadFileSystem i, MonadIO i)
  => Context i
  -> [(FileEntry, FileEntry, FileDeltaKind)]
  -> i [SyncOp]
syncIntermediateToDestination ctx files =
  (fmap concat . sequence)
    [ case delta of
      Unchanged -> return []
      Removed ->
        if from.stat == Directory
          then return [CreateDirs to.path]
          else return [CreateDirs $ takeDirectory to.path, CopyFile from.path to.path]
      Modified ->
        case (from.stat, to.stat) of
          (Directory, _) -> return [RemoveFile to.path, CreateDir to.path]
          (_, Directory) -> return [RemoveDirs to.path, CopyFile from.path to.path]
          _ -> return [CopyFile from.path to.path]
      Added -> do
        (state, _) <- getRouteState ctx to.path
        case state of
          Ignored _ _ -> return []
          _ ->
            if to.stat == Directory
              then return [RemoveDirs to.path]
              else return [RemoveFile to.path]
    | (from, to, delta) <- files
    ]
