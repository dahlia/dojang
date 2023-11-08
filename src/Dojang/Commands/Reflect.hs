{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Dojang.Commands.Reflect (reflect) where

import Control.Monad (filterM, forM, forM_, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (isPrefixOf, nub)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)

import Control.Monad.Logger (logDebug, logDebugSH)
import Data.Text (pack)
import System.Directory.OsPath (makeAbsolute)
import System.OsPath (OsPath, splitDirectories, takeDirectory)

import Dojang.App (App, ensureContext)
import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , dieWithErrors
  , pathStyleFor
  , printStderr
  , printStderr'
  )
import Dojang.Commands.Status (printWarnings)
import Dojang.ExitCodes
  ( fileNotFoundError
  , fileNotRoutedError
  , ignoredFileError
  , sourceCannotBeTargetError
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
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
import Dojang.Types.Repository (Repository (..))


reflect
  :: (MonadFileSystem i, MonadIO i) => Bool -> [FilePath] -> App i ExitCode
reflect force paths' = do
  ctx <- ensureContext
  paths <- mapM encodePath paths'
  nonExistents <- filterM (fmap not . exists) paths
  pathStyle <- pathStyleFor stderr
  unless (null nonExistents) $ do
    ps <- mapM decodePath nonExistents
    dieWithErrors
      fileNotFoundError
      ["No such file: " <> pathStyle (pack p) <> "." | p <- ps]
  absPaths <- liftIO $ mapM makeAbsolute paths
  $(logDebugSH) (absPaths :: [OsPath])
  sourcePath' <- liftIO $ makeAbsolute ctx.repository.sourcePath
  let sourcePathPrefix = splitDirectories sourcePath'
  $(logDebugSH) sourcePathPrefix
  let overlappedPaths =
        [ p
        | p <- absPaths
        , sourcePathPrefix `isPrefixOf` splitDirectories p
        ]
  $(logDebugSH) overlappedPaths
  unless (null overlappedPaths) $ do
    ps <- mapM decodePath overlappedPaths
    dieWithErrors
      sourceCannotBeTargetError
      [ "Cannot reflect "
        <> pathStyle (pack p)
        <> " because it is a file inside the repository."
      | p <- ps
      ]
  codeStyle <- codeStyleFor stderr
  warningLists <- forM absPaths $ \absPath -> do
    (state, ws) <- getRouteState ctx absPath
    absPath' <- decodePath absPath
    case state of
      NotRouted -> do
        printWarnings ws
        printStderr'
          Error
          ("File " <> pathStyle (pack absPath') <> " is not routed.")
        printStderr'
          Hint
          ("Add a route for it in " <> pathStyle "dojang.toml" <> ".")
        liftIO $ exitWith fileNotRoutedError
      Routed _ -> do
        return ws
      Ignored name pattern -> do
        routeName' <- decodePath name
        if force
          then do
            printStderr' Note
              $ "File "
              <> pathStyle (pack absPath')
              <> " is ignored due to pattern "
              <> codeStyle (pack $ show pattern)
              <> " (route name: "
              <> codeStyle (pack routeName')
              <> "), but reflect it anyway as you enforced it using "
              <> codeStyle "-f"
              <> "/"
              <> codeStyle "--force"
              <> " option."
            return ws
          else do
            printWarnings ws
            printStderr' Error
              $ "File "
              <> pathStyle (pack absPath')
              <> " is ignored due to pattern "
              <> codeStyle (pack $ show pattern)
              <> " (route name: "
              <> codeStyle (pack routeName')
              <> ")."
            printStderr'
              Hint
              $ "You can reflect it anyway by enforcing it using "
              <> codeStyle "-f"
              <> "/"
              <> codeStyle "--force"
              <> " option."
            liftIO $ exitWith ignoredFileError
  (files, ws) <- makeCorrespond ctx
  files' <- filterTargets files absPaths
  let conflicts = filterConflicts files'
  $(logDebugSH) conflicts
  unless (force || null conflicts) $ do
    ps <- forM conflicts $ \c -> do
      src <- decodePath c.source.path
      dst <- decodePath c.destination.path
      return (src, dst)
    printWarnings $ nub $ concat warningLists ++ ws
    dieWithErrors
      sourceCannotBeTargetError
      [ "Cannot reflect "
        <> pathStyle (pack dst)
        <> ", since "
        <> pathStyle (pack src)
        <> " is also changed."
      | (src, dst) <- ps
      ]
  forM_ files' $ \c -> do
    src <- decodePath c.source.path
    dst <- decodePath c.destination.path
    if c.sourceDelta == Unchanged && c.destinationDelta == Unchanged
      then
        printStderr'
          Note
          ( "File "
              <> pathStyle (pack dst)
              <> " is skipped, since it is the same as file "
              <> pathStyle (pack src)
              <> "."
          )
      else do
        printStderr
          $ "Reflect "
          <> pathStyle (pack dst)
          <> " to "
          <> pathStyle (pack src)
          <> "..."
        unless (c.destinationDelta == Unchanged) $ do
          cleanup c.intermediate
          copy c.destination c.intermediate
        unless (c.sourceDelta == Unchanged) $ do
          cleanup c.source
          copy c.intermediate c.source
  printWarnings $ nub $ concat warningLists ++ ws
  return ExitSuccess


cleanup :: (MonadFileSystem i, MonadIO i) => FileEntry -> App i ()
cleanup fileEntry = do
  path <- decodePath fileEntry.path
  case fileEntry.stat of
    Missing -> return ()
    Directory -> do
      $(logDebug) $ "Remove directory recursively: " <> pack path
      removeDirectoryRecursively fileEntry.path
    _ -> do
      $(logDebug) $ "Remove file: " <> pack path
      removeFile fileEntry.path


copy :: (MonadFileSystem i, MonadIO i) => FileEntry -> FileEntry -> App i ()
copy from to = do
  from' <- decodePath from.path
  to' <- decodePath to.path
  case from.stat of
    Directory -> do
      $(logDebug) $ "Create directory recursively: " <> pack to'
      createDirectories to.path
    _ -> do
      let parent = takeDirectory to.path
      parent' <- decodePath parent
      $(logDebug) $ "Create directory recursively: " <> pack parent'
      createDirectories parent
      $(logDebug) $ "Copy file: " <> pack from' <> " -> " <> pack to'
      copyFile from.path to.path


filterTargets
  :: forall m
   . (MonadIO m)
  => [FileCorrespondence]
  -> [OsPath]
  -> m [FileCorrespondence]
filterTargets corresponds targetFiles =
  filterM conflicts corresponds
 where
  conflicts :: FileCorrespondence -> m Bool
  conflicts correspond = do
    dst <- liftIO $ makeAbsolute correspond.destination.path
    return $ dst `elem` targetFiles


filterConflicts
  :: [FileCorrespondence]
  -> [FileCorrespondence]
filterConflicts corresponds = [c | c <- corresponds, c.sourceDelta /= Unchanged]
