{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.Status
  ( StatusOptions (..)
  , defaultStatusOptions
  , formatWarning
  , printWarnings
  , status
  , statusCore
  ) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Map.Strict qualified as Map
import System.Exit (ExitCode (..))
import System.IO (Handle, stderr)

import Data.CaseInsensitive (original)
import Data.Text (Text, intercalate, pack)
import Data.Text qualified as Text
import System.Console.Pretty (Color (..))
import System.OsPath (addTrailingPathSeparator, makeRelative)
import TextShow (FromStringShow (FromStringShow), TextShow (showt))

import Dojang.App (App, ensureContext, prepareMachineState)
import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , pathStyleFor
  , printStderr'
  , printTable
  )
import Dojang.Commands.Hook (withCommandHooks)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , ManagedCorrespondence (..)
  , makeManagedCorrespond
  , routePaths
  )
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning (..))
import Dojang.Types.FilePathExpression.Expansion (ExpansionWarning (..))
import Dojang.Types.MachineState (MachineState (..))
import Dojang.Types.ManagedTarget
  ( ManagedTarget (..)
  , OrphanReason (..)
  , OrphanStatus (..)
  , TargetFingerprint (..)
  , classifyOrphan
  , makeCurrentEntries
  , makeCurrentRoutes
  )
import Dojang.Types.MonikerName ()
import Dojang.Types.Repository
  ( Repository (..)
  , RouteMapWarning (..)
  )
import Dojang.Types.TargetTracking (observeOrphanStatus)


data StatusOptions = StatusOptions
  { noTrailingSlash :: Bool
  , onlyChanges :: Bool
  , showDestinationPath :: Bool
  }
  deriving (Show)


defaultStatusOptions :: StatusOptions
defaultStatusOptions =
  StatusOptions
    { noTrailingSlash = False
    , onlyChanges = False
    , showDestinationPath = False
    }


status :: (MonadFileSystem i, MonadIO i) => StatusOptions -> App i ExitCode
status options =
  withCommandHooks "status" [] $ statusCore options


-- | Runs status reporting without lifecycle hooks for command-internal use.
statusCore :: (MonadFileSystem i, MonadIO i) => StatusOptions -> App i ExitCode
statusCore options = do
  ctx <- ensureContext
  machineState <- prepareMachineState ctx.repository.manifest
  (managed, ws) <- makeManagedCorrespond ctx
  let files = (.correspondence) <$> managed
  let files' = if options.onlyChanges then filter isChanged files else files
  sourcePath <- makeAbsolute ctx.repository.sourcePath
  rows <- forM files' $ \file -> do
    displayPath <-
      if options.showDestinationPath
        then makeAbsolute file.destination.path
        else do
          path <- makeAbsolute file.source.path
          return $ makeRelative sourcePath path
    let displayPathS =
          if not options.noTrailingSlash
            && ( (file.source.stat == Directory)
                   || (file.source.stat == Missing)
                     && (file.destination.stat == Directory)
               )
            then addTrailingPathSeparator displayPath
            else displayPath
    displayPathFP <- decodePath displayPathS
    return
      [ renderDeltaKind file.sourceDelta
      , renderFileStat file.source.stat
      , renderDeltaKind file.destinationDelta
      , renderFileStat file.destination.stat
      , (Default, pack displayPathFP)
      ]
  printTable
    [ "Source"
    , "ST"
    , "Destination"
    , "DT"
    , if options.showDestinationPath then "Destination File" else "Source File"
    ]
    rows
  printOrphans ctx managed machineState
  printWarnings ws
  return ExitSuccess


printOrphans
  :: (MonadFileSystem i, MonadIO i)
  => Context (App i)
  -> [ManagedCorrespondence]
  -> MachineState
  -> App i ()
printOrphans ctx managed machineState = do
  (routes, _) <- routePaths ctx
  current <- makeCurrentRoutes routes
  let entries = makeCurrentEntries managed
  let orphans =
        [ (target, reason)
        | target <- Map.elems machineState.targetRecords
        , Just reason <- [classifyOrphan current entries target]
        ]
  rows <- forM orphans $ \(target, reason) -> do
    targetStatus <- observeOrphanStatus target
    destination <- pack <$> decodePath target.destinationPath
    source <- pack <$> decodePath target.sourcePath
    route <- pack <$> decodePath target.routeName
    return
      [ (statusColor targetStatus, renderOrphanStatus targetStatus)
      , renderFingerprintType target.fingerprint
      , (Default, destination)
      , (Default, source <> " (" <> route <> ")")
      , (Default, renderFingerprint target.fingerprint)
      , (Yellow, renderOrphanReason reason)
      ]
  unlessNull rows $
    printTable
      ["Orphan", "Type", "Destination", "Source (route)", "Fingerprint", "Reason"]
      rows


unlessNull :: (Applicative m) => [a] -> m () -> m ()
unlessNull [] _ = pure ()
unlessNull _ action = action


statusColor :: OrphanStatus -> Color
statusColor OrphanUnchanged = Default
statusColor OrphanModified = Yellow
statusColor OrphanMissing = Red


renderOrphanStatus :: OrphanStatus -> Text
renderOrphanStatus OrphanUnchanged = "unchanged"
renderOrphanStatus OrphanModified = "modified"
renderOrphanStatus OrphanMissing = "missing"


renderOrphanReason :: OrphanReason -> Text
renderOrphanReason RouteRemoved = "route removed or inapplicable"
renderOrphanReason RouteChanged = "route definition changed"
renderOrphanReason DestinationChanged = "destination expansion changed"
renderOrphanReason EntryRemoved = "entry no longer managed"


renderFingerprintType :: TargetFingerprint -> (Color, Text)
renderFingerprintType (FileFingerprint _ _) = (Default, "F")
renderFingerprintType DirectoryFingerprint = (Default, "D")


renderFingerprint :: TargetFingerprint -> Text
renderFingerprint (FileFingerprint size digest) =
  showt size <> " B sha256:" <> Text.take 12 digest
renderFingerprint DirectoryFingerprint = "directory"


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
      return $
        "Reference to an undefined moniker: "
          <> codeStyle (original moniker.name)
          <> "."
    (UnrecognizedOperatingSystem os) ->
      return $
        "Unrecognized operating system: "
          <> codeStyle (showt $ FromStringShow os)
          <> "."
    (UnrecognizedArchitecture arch) ->
      return $
        "Unrecognized architecture: "
          <> codeStyle (showt $ FromStringShow arch)
          <> "."
formatWarning handle (FilePathExpressionWarning (UndefinedEnvironmentVariable envVar)) = do
  codeStyle <- codeStyleFor handle
  return $
    "Reference to an undefined environment variable: "
      <> codeStyle envVar
      <> "."
formatWarning handle (OverlapDestinationPathsWarning name dst paths) = do
  pathStyle <- pathStyleFor handle
  pairStrings <- forM paths $ \(from, to) -> do
    return $ pathStyle from <> " -> " <> pathStyle to
  case pairStrings of
    pairString :| [] ->
      return $
        pathStyle name
          <> " -> "
          <> pathStyle dst
          <> " overlaps with: "
          <> pairString
          <> "."
    _ ->
      return $
        pathStyle name
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
