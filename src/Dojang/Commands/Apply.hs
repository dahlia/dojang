{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Dojang.Commands.Apply (apply) where

import Control.Monad (filterM, forM, forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.List (nub, sort)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import Prelude hiding (readFile)

import Control.Monad.Logger (logDebugSH)
import Data.Map.Strict (fromList, notMember, toList)
import System.Directory.OsPath (makeAbsolute)
import System.OsPath (OsPath, addTrailingPathSeparator, takeDirectory)

import Dojang.App (App, AppEnv (debug, manifestFile), ensureContext, lookupEnv')
import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , die'
  , pathStyleFor
  , printStderr
  , printStderr'
  )
import Dojang.Commands.Status (defaultStatusOptions, printWarnings, status)
import Dojang.ExitCodes
  ( accidentalDeletionWarning
  , conflictError
  , fileNotRoutedError
  )
import Dojang.MonadFileSystem (MonadFileSystem (..), dryRunIO)
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , RouteState (..)
  , getRouteState
  , makeCorrespond
  )


apply :: (MonadFileSystem i, MonadIO i) => Bool -> [OsPath] -> App i ExitCode
apply force filePaths = do
  ctx <- ensureContext
  (allFiles, ws) <- makeCorrespond ctx
  fileMap <- fmap fromList $ forM allFiles $ \fc -> do
    srcAbsPath <- liftIO $ makeAbsolute fc.source.path
    return (srcAbsPath, fc)
  pathStyle <- pathStyleFor stderr
  filePaths' <- forM filePaths $ \fp -> do
    fp' <- liftIO $ makeAbsolute fp
    when (fp' `notMember` fileMap) $ do
      die' fileNotRoutedError
        $ "File "
        <> pathStyle fp
        <> " is not tracked by this repository."
    return fp'
  let files =
        if null filePaths'
          then allFiles
          else
            [ f
            | (srcAbsPath, f) <- toList fileMap
            , srcAbsPath `elem` filePaths'
            ]
  $(logDebugSH) files
  -- Check if there are any conflicts:
  conflicts <- filterConflicts files
  codeStyle <- codeStyleFor stderr
  unless (null conflicts) $ do
    forM_ conflicts $ \c -> do
      printStderr' (if force then Warning else Error)
        $ "There is a conflict between "
        <> pathStyle c.source.path
        <> " and "
        <> pathStyle c.destination.path
        <> "."
    printStderr' Hint
      $ "Use `"
      <> codeStyle "dojang diff"
      <> "' to see the actual changes on both sides."
  -- Check if there are any accidental deletions:
  let ops =
        syncSourceToIntermediate
          [ (fc.source, fc.intermediate, fc.sourceDelta)
          | fc <- files
          ]
          & nub
          . sort
  shimOps <- liftIO $ dryRunIO $ do
    forM_ ops doSyncOp
    let ctx' = Context ctx.repository ctx.environment lookupEnv'
    (files', _) <- makeCorrespond ctx'
    syncIntermediateToDestination
      ctx'
      [ (fc.intermediate, fc.destination, fc.destinationDelta)
      | fc <- files'
      ]
  when (any isDeletion shimOps) $ do
    forM_ shimOps $ \case
      RemoveDirs path -> do
        let path' = addTrailingPathSeparator path
        if force
          then
            printStderr' Warning
              $ "Would delete "
              <> pathStyle path'
              <> " (and its children)."
          else
            printStderr' Error
              $ "Cancelled applying because "
              <> pathStyle path'
              <> " (and its children) would be deleted."
      RemoveFile path -> do
        if force
          then printStderr' Warning ("Would delete " <> pathStyle path <> ".")
          else
            printStderr'
              Error
              $ "Cancelled applying because "
              <> pathStyle path
              <> " would be deleted."
      _ -> return ()
    manifestFile' <- asks (.manifestFile)
    printStderr' Hint
      $ "If these deletions are accidental, ignore them in your manifest ("
      <> pathStyle manifestFile'
      <> ")."
  -- Exit if there are any problems (unless forced):
  when (not force && (not (null conflicts) || any isDeletion shimOps)) $ do
    printStderr' Hint
      $ "Use "
      <> codeStyle "-f"
      <> "/"
      <> codeStyle "--force"
      <> " to ignore these warnings and go ahead."
    liftIO
      $ exitWith
      $ if not (null conflicts)
        then conflictError
        else accidentalDeletionWarning
  -- When everything is fine (or excused):
  debug' <- asks (.debug)
  when debug' (void $ status defaultStatusOptions)
  forM_ ops $ \path -> printSyncOp path >> doSyncOp path
  when debug' (void $ status defaultStatusOptions)
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
 where
  isDeletion :: SyncOp -> Bool
  isDeletion (RemoveDirs _) = True
  isDeletion (RemoveFile _) = True
  isDeletion _ = False


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


printSyncOp :: (MonadIO i) => SyncOp -> App i ()
printSyncOp (RemoveDirs path) = do
  pathStyle <- pathStyleFor stderr
  let path' = addTrailingPathSeparator path
  printStderr ("Remove " <> pathStyle path' <> " (and its children)...")
printSyncOp (RemoveFile path) = do
  pathStyle <- pathStyleFor stderr
  printStderr ("Remove " <> pathStyle path <> "...")
printSyncOp (CopyFile src dst) = do
  pathStyle <- pathStyleFor stderr
  printStderr
    ("Copy " <> pathStyle src <> " to " <> pathStyle dst <> "...")
printSyncOp (CreateDir path) = do
  pathStyle <- pathStyleFor stderr
  let path' = addTrailingPathSeparator path
  printStderr ("Create " <> pathStyle path' <> "...")
printSyncOp (CreateDirs path) = do
  pathStyle <- pathStyleFor stderr
  let path' = addTrailingPathSeparator path
  printStderr ("Create " <> pathStyle path' <> " (and its ancestors)...")


doSyncOp :: (MonadFileSystem i) => SyncOp -> i ()
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
