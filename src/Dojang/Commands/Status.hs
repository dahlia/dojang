{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.Status
  ( formatWarning
  , printWarnings
  , status
  ) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import System.Exit (ExitCode (..))
import System.IO (Handle, stderr)

import Data.CaseInsensitive (original)
import Data.Text (Text, intercalate, pack)
import System.Console.Pretty (Color (..))
import System.Directory.OsPath (makeAbsolute)
import System.OsPath (addTrailingPathSeparator, makeRelative)
import TextShow (FromStringShow (FromStringShow), TextShow (showt))

import Dojang.App (App, ensureContext)
import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , pathStyleFor
  , printStderr'
  , printTable
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , makeCorrespond
  )
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning (..))
import Dojang.Types.FilePathExpression.Expansion (ExpansionWarning (..))
import Dojang.Types.MonikerName ()
import Dojang.Types.Repository (Repository (..), RouteMapWarning (..))


status :: (MonadFileSystem i, MonadIO i) => Bool -> Bool -> App i ExitCode
status noTrailingSlash onlyChanges = do
  ctx <- ensureContext
  (files, ws) <- makeCorrespond ctx
  let files' = if onlyChanges then filter isChanged files else files
  sourcePath <- liftIO $ makeAbsolute ctx.repository.sourcePath
  rows <- forM files' $ \file -> do
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


isChanged :: FileCorrespondence -> Bool
isChanged file =
  file.sourceDelta /= Unchanged || file.destinationDelta /= Unchanged


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
formatWarning :: (MonadIO i) => Handle -> RouteMapWarning -> i Text
formatWarning handle (EnvironmentPredicateWarning w) = do
  codeStyle <- codeStyleFor handle
  case w of
    (UndefinedMoniker moniker) ->
      return
        $ "Reference to an undefined moniker: "
        <> codeStyle (original moniker.name)
        <> "."
    (UnrecognizedOperatingSystem os) ->
      return
        $ "Unrecognized operating system: "
        <> codeStyle (showt $ FromStringShow os)
        <> "."
    (UnrecognizedArchitecture arch) ->
      return
        $ "Unrecognized architecture: "
        <> codeStyle (showt $ FromStringShow arch)
        <> "."
formatWarning handle (FilePathExpressionWarning (UndefinedEnvironmentVariable envVar)) = do
  codeStyle <- codeStyleFor handle
  return
    $ "Reference to an undefined environment variable: "
    <> codeStyle envVar
    <> "."
formatWarning handle (OverlapDestinationPathsWarning name dst paths) = do
  pathStyle <- pathStyleFor handle
  pairStrings <- forM paths $ \(from, to) -> do
    return $ pathStyle from <> " -> " <> pathStyle to
  case pairStrings of
    pairString :| [] ->
      return
        $ pathStyle name
        <> " -> "
        <> pathStyle dst
        <> " overlaps with: "
        <> pairString
        <> "."
    _ ->
      return
        $ pathStyle name
        <> " -> "
        <> pathStyle dst
        <> " overlaps with:\n  "
        <> intercalate "\n  " (toList pairStrings)


-- TODO: This should be in a separate module:
printWarnings :: (MonadIO i) => [RouteMapWarning] -> App i ()
printWarnings ws =
  forM_ ws $ \w -> do
    formatted <- formatWarning stderr w
    printStderr' Warning formatted
