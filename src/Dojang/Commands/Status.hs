{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Dojang.Commands.Status (status) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))

import Control.Monad.Logger (logDebugSH)
import Data.CaseInsensitive (original)
import Data.Text (Text, pack, unpack)
import System.Console.Pretty (Color (..))
import System.Directory.OsPath (makeAbsolute)
import System.OsPath (makeRelative)
import System.OsString (OsString)

import Dojang.App (App, currentEnvironment', loadRepository)
import Dojang.Commands
  ( Admonition (..)
  , dieWithErrors
  , printStderr'
  , printTable
  )
import Dojang.ExitCodes (manifestReadError, manifestUninitialized)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Syntax.Manifest.Parser (formatErrors)
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning (..))
import Dojang.Types.FilePathExpression (EnvironmentVariable)
import Dojang.Types.FilePathExpression.Expansion (ExpansionWarning (..))
import Dojang.Types.MonikerName ()
import Dojang.Types.Repository
  ( FileCorrespondence (..)
  , FileCorrespondenceWarning (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , Repository (..)
  , makeCorrespond
  )


status :: (MonadFileSystem i, MonadIO i) => App i ExitCode
status = do
  repository <- loadRepository
  case repository of
    Left e ->
      dieWithErrors manifestReadError $ formatErrors e
    Right Nothing -> do
      printStderr' Error "No manifest found."
      printStderr' Note "Run `dojang init' to create one."
      return manifestUninitialized
    Right (Just repo) -> do
      currentEnv <- currentEnvironment'
      $(logDebugSH) currentEnv
      (files, ws) <- makeCorrespond repo currentEnv lookupEnv'
      sourcePath <- liftIO $ makeAbsolute repo.sourcePath
      rows <- forM files $ \file -> do
        path <- liftIO $ makeAbsolute file.source.path
        let relPath = makeRelative sourcePath path
        relPath' <- decodePath relPath
        return
          [ renderDeltaKind file.sourceDelta
          , renderDeltaKind file.destinationDelta
          , (Default, pack relPath')
          ]
      printTable ["Source", "Destination", "File"] rows
      forM_ ws $ printStderr' Warning . formatWarning
      return ExitSuccess


renderDeltaKind :: FileDeltaKind -> (Color, Text)
renderDeltaKind Unchanged = (Default, "unchanged")
renderDeltaKind Added = (Green, "added")
renderDeltaKind Removed = (Red, "removed")
renderDeltaKind Modified = (Yellow, "modified")


lookupEnv'
  :: (MonadFileSystem i, MonadIO i) => EnvironmentVariable -> i (Maybe OsString)
lookupEnv' env = do
  value <- liftIO $ lookupEnv $ unpack env
  case value of
    Just v -> do
      Just <$> encodePath v
    Nothing -> return Nothing


formatWarning :: FileCorrespondenceWarning -> Text
formatWarning (EnvironmentPredicateWarning (UndefinedMoniker moniker)) =
  "Reference to an undefined moniker: " <> original moniker.name
formatWarning
  (FilePathExpressionWarning (UndefinedEnvironmentVariable envVar)) =
    "Reference to an undefined environment variable: " <> envVar
