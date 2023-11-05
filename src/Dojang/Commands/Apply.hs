{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Dojang.Commands.Apply (apply) where

import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.List (nub, sort)
import System.Exit (ExitCode (..))
import System.IO (stderr)

import Control.Monad.Logger (logDebugSH)
import Data.Text (pack)
import System.OsPath (OsPath, addTrailingPathSeparator, takeDirectory)

import Dojang.App (App, AppEnv (debug), currentEnvironment', loadRepository)
import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , dieWithErrors
  , pathStyleFor
  , printStderr
  , printStderr'
  )
import Dojang.Commands.Status (formatWarning, lookupEnv', status)
import Dojang.ExitCodes
  ( conflictError
  , manifestReadError
  , manifestUninitialized
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Syntax.Manifest.Parser (formatErrors)
import Dojang.Types.Repository
  ( FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , makeCorrespond
  )


apply :: (MonadFileSystem i, MonadIO i) => Bool -> App i ExitCode
apply force = do
  $(logDebugSH) force
  repository <- loadRepository
  case repository of
    Left e ->
      dieWithErrors manifestReadError $ formatErrors e
    Right Nothing -> do
      codeStyle <- codeStyleFor stderr
      printStderr' Error "No manifest found."
      printStderr'
        Note
        ("Run `" <> codeStyle "dojang init" <> "' to create one.")
      return manifestUninitialized
    Right (Just repo) -> do
      currentEnv <- currentEnvironment'
      $(logDebugSH) currentEnv
      (files, ws) <- makeCorrespond repo currentEnv lookupEnv'
      $(logDebugSH) files
      let conflicts = filterConflicts files
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
          (files', _) <- makeCorrespond repo currentEnv lookupEnv'
          $(logDebugSH) files'
          let ops' =
                syncIntermediateToDestination
                  [ (fc.intermediate, fc.destination, fc.destinationDelta)
                  | fc <- files'
                  ]
                  & nub
                  . sort
          forM_ ops' $ \path -> printSyncOp path >> doSyncOp path
          forM_ ws $ printStderr' Warning . formatWarning
          return ExitSuccess


filterConflicts :: [FileCorrespondence] -> [FileCorrespondence]
filterConflicts = filter $ \c -> case (c.sourceDelta, c.destinationDelta) of
  (Unchanged, _) -> False
  (_, Unchanged) -> False
  (Removed, Removed) -> False
  (Added, Added) ->
    not (c.source.stat == Directory && c.destination.stat == Directory)
  _ -> True


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
  printStderr ("Removed " <> pathStyle (pack path') <> " (and its children).")
printSyncOp (RemoveFile path) = do
  pathStyle <- pathStyleFor stderr
  path' <- decodePath path
  printStderr ("Removed " <> pathStyle (pack path') <> ".")
printSyncOp (CopyFile src dst) = do
  pathStyle <- pathStyleFor stderr
  src' <- decodePath src
  dst' <- decodePath dst
  printStderr
    ( "Copied "
        <> pathStyle (pack src')
        <> " to "
        <> pathStyle (pack dst')
        <> "."
    )
printSyncOp (CreateDir path) = do
  pathStyle <- pathStyleFor stderr
  path' <- decodePath $ addTrailingPathSeparator path
  printStderr ("Created " <> pathStyle (pack path') <> ".")
printSyncOp (CreateDirs path) = do
  pathStyle <- pathStyleFor stderr
  path' <- decodePath $ addTrailingPathSeparator path
  printStderr ("Created " <> pathStyle (pack path') <> " (and its ancestors).")


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
  :: [(FileEntry, FileEntry, FileDeltaKind)]
  -> [SyncOp]
syncIntermediateToDestination files =
  concat
    [ case delta of
      Unchanged -> []
      Removed ->
        if from.stat == Directory
          then [CreateDirs to.path]
          else [CreateDirs $ takeDirectory to.path, CopyFile from.path to.path]
      Modified ->
        case (from.stat, to.stat) of
          (Directory, _) -> [RemoveFile to.path, CreateDir to.path]
          (_, Directory) -> [RemoveDirs to.path, CopyFile from.path to.path]
          _ -> [CopyFile from.path to.path]
      Added ->
        if to.stat == Directory
          then [RemoveDirs to.path]
          else [RemoveFile to.path]
    | (from, to, delta) <- files
    ]