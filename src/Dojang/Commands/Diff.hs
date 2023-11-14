{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.Diff (DiffMode (..), diff) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (find, nub)
import Data.Maybe (maybeToList)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (stderr)
import System.Info (os)

import Data.Text (pack)
import System.Directory.OsPath (makeAbsolute)
import System.OsPath (OsPath)
import System.Process (spawnProcess, waitForProcess)
import TextShow (TextShow (showt))

import Dojang.App (App, ensureContext)
import Dojang.Commands
  ( Admonition (..)
  , die'
  , pathStyleFor
  , pathStyleFor'
  , printStderr'
  )
import Dojang.Commands.Status (printWarnings)
import Dojang.ExitCodes (externalProgramNonZeroExit, fileNotRoutedError)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Context
  ( FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , makeCorrespond
  )


data DiffMode = ThreeWay | Source | Destination | TwoWay deriving (Show)


diff
  :: (MonadFileSystem i, MonadIO i)
  => DiffMode
  -> Maybe OsPath
  -> Maybe OsPath
  -> [OsPath]
  -> App i ExitCode
diff mode diff2 diff3 files = do
  ctx <- ensureContext
  (corresponds, ws) <- makeCorrespond ctx
  printWarnings ws
  routedFiles <- forM corresponds $ \c -> do
    sourcePath <- liftIO $ makeAbsolute c.source.path
    destinationPath <- liftIO $ makeAbsolute c.destination.path
    return (sourcePath, destinationPath)
  pathStyle <- pathStyleFor stderr
  nonExistents <- (`filterM` files) $ \file -> do
    file' <- liftIO $ makeAbsolute file
    if any (\(src, dst) -> file' == src || file' == dst) routedFiles
      then return False
      else do
        printStderr' Error $ pathStyle file <> " is not a routed file."
        return True
  unless (null nonExistents) $ liftIO $ exitWith fileNotRoutedError
  files' <-
    nub <$> case files of
      [] -> return corresponds
      _ -> do
        fs <- forM files $ \file -> do
          file' <- liftIO $ makeAbsolute file
          let found =
                find (\((src, dst), _) -> file' == src || file' == dst)
                  $ zip routedFiles corresponds
          return $ maybeToList $ fmap snd found
        return $ concat fs
  case mode of
    ThreeWay -> threeWay diff3 (.intermediate) (.source) (.destination) files'
    Source -> twoWay diff2 (.source) (.intermediate) files'
    Destination -> twoWay diff2 (.destination) (.intermediate) files'
    TwoWay -> twoWay diff2 (.source) (.destination) files'
  return ExitSuccess


nullPath :: FilePath
nullPath = if os == "mingw32" then "NUL" else "/dev/null"


getPath :: (MonadFileSystem m) => FileEntry -> m FilePath
getPath entry = case entry.stat of
  Missing -> return nullPath
  Directory -> return nullPath
  _ -> decodePath entry.path


threeWay
  :: (MonadFileSystem i, MonadIO i)
  => Maybe OsPath
  -> (FileCorrespondence -> FileEntry)
  -> (FileCorrespondence -> FileEntry)
  -> (FileCorrespondence -> FileEntry)
  -> [FileCorrespondence]
  -> App i ()
threeWay program' baseSelector selectorA selectorB files = do
  program <- case program' of
    Just p -> Just <$> decodePath p
    Nothing -> liftIO $ lookupEnv "DOJANG_DIFF3"
  forM_ files $ \file -> when (hasChange file) $ do
    let base = baseSelector file
    basePath <- getPath base
    let a = selectorA file
    pathA <- getPath a
    let b = selectorB file
    pathB <- getPath b
    case program of
      Nothing -> die' externalProgramNonZeroExit "No diff3 program found."
      Just prog -> do
        handle <- liftIO $ spawnProcess prog [pathA, basePath, pathB]
        exitCode <- liftIO $ waitForProcess handle
        when (exitCode /= ExitSuccess && exitCode /= ExitFailure 1) $ do
          cmdStyle <- pathStyleFor' stderr
          die' externalProgramNonZeroExit
            $ cmdStyle (pack prog)
            <> " terminated with exit code "
            <> showt exitCode
            <> "."


twoWay
  :: (MonadFileSystem i, MonadIO i)
  => Maybe OsPath
  -> (FileCorrespondence -> FileEntry)
  -> (FileCorrespondence -> FileEntry)
  -> [FileCorrespondence]
  -> App i ()
twoWay program' srcSelector dstSelector files = do
  program <- case program' of
    Just p -> Just <$> decodePath p
    Nothing -> liftIO $ lookupEnv "DOJANG_DIFF"
  forM_ files $ \file -> when (hasChange file) $ do
    let src = srcSelector file
    srcPath <- getPath src
    let dst = dstSelector file
    dstPath <- getPath dst
    case program of
      Nothing -> die' externalProgramNonZeroExit "No diff program found."
      Just prog -> do
        handle <- liftIO $ spawnProcess prog [srcPath, dstPath]
        exitCode <- liftIO $ waitForProcess handle
        when (exitCode /= ExitSuccess && exitCode /= ExitFailure 1) $ do
          cmdStyle <- pathStyleFor' stderr
          die' externalProgramNonZeroExit
            $ cmdStyle (pack prog)
            <> " terminated with exit code "
            <> showt exitCode
            <> "."


hasChange :: FileCorrespondence -> Bool
hasChange c = case (c.sourceDelta, c.destinationDelta) of
  (Unchanged, Unchanged) -> False
  _ -> True
