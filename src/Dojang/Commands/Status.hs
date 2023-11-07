{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Dojang.Commands.Status
  ( formatWarning
  , printWarnings
  , status
  ) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Exit (ExitCode (..))

import Data.CaseInsensitive (original)
import Data.Text (Text, pack)
import System.Console.Pretty (Color (..))
import System.Directory.OsPath (makeAbsolute)
import System.OsPath (addTrailingPathSeparator, makeRelative)

import Dojang.App (App, ensureContext)
import Dojang.Commands
  ( Admonition (..)
  , printStderr'
  , printTable
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileCorrespondenceWarning (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , makeCorrespond
  )
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning (..))
import Dojang.Types.FilePathExpression.Expansion (ExpansionWarning (..))
import Dojang.Types.MonikerName ()
import Dojang.Types.Repository (Repository (..))


status :: (MonadFileSystem i, MonadIO i) => Bool -> App i ExitCode
status noTrailingSlash = do
  ctx <- ensureContext
  (files, ws) <- makeCorrespond ctx
  sourcePath <- liftIO $ makeAbsolute ctx.repository.sourcePath
  rows <- forM files $ \file -> do
    path <- liftIO $ makeAbsolute file.source.path
    let relPath = makeRelative sourcePath path
    let relPathS =
          if not noTrailingSlash
            && ( (file.source.stat == Directory)
                  || (file.source.stat == Missing)
                  && (file.destination.stat == Directory)
               )
            then addTrailingPathSeparator relPath
            else relPath
    relPath' <- decodePath relPathS
    return
      [ renderDeltaKind file.sourceDelta
      , renderFileStat file.source.stat
      , renderDeltaKind file.destinationDelta
      , renderFileStat file.destination.stat
      , (Default, pack relPath')
      ]
  printTable ["Source", "ST", "Destination", "DT", "File"] rows
  printWarnings ws
  return ExitSuccess


renderDeltaKind :: FileDeltaKind -> (Color, Text)
renderDeltaKind Unchanged = (Default, "unchanged")
renderDeltaKind Added = (Green, "added")
renderDeltaKind Removed = (Red, "removed")
renderDeltaKind Modified = (Yellow, "modified")


renderFileStat :: FileStat -> (Color, Text)
renderFileStat Missing = (Red, "-")
renderFileStat Directory = (Default, "D")
renderFileStat (File _) = (Default, "F")
renderFileStat (Symlink _) = (Default, "L")


-- TODO: This should be in a separate module:
formatWarning :: FileCorrespondenceWarning -> Text
formatWarning (EnvironmentPredicateWarning (UndefinedMoniker moniker)) =
  "Reference to an undefined moniker: " <> original moniker.name
formatWarning
  (FilePathExpressionWarning (UndefinedEnvironmentVariable envVar)) =
    "Reference to an undefined environment variable: " <> envVar


-- TODO: This should be in a separate module:
printWarnings :: (MonadIO i) => [FileCorrespondenceWarning] -> App i ()
printWarnings ws =
  forM_ ws $ printStderr' Warning . formatWarning
