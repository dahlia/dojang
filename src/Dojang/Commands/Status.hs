{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.Status
  ( StatusOptions (..)
  , defaultStatusOptions
  , formatWarning
  , printModeNotes
  , printUnsupportedModeWarnings
  , printWarnings
  , status
  , statusCore
  ) where

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Map.Strict qualified as Map
import System.Exit (ExitCode (..))
import System.IO (Handle, stderr)
import System.Info qualified

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
  , ensureRouteOwnership
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
import Dojang.Types.Environment (factKeyText)
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
import Dojang.Types.Reconciliation (observeModeDrift)
import Dojang.Types.Repository
  ( Repository (..)
  , RouteMapWarning (..)
  , RouteResult (..)
  )
import Dojang.Types.RouteMetadata
  ( RouteMode (DefaultMode, ReadOnly)
  , renderRouteMode
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
  (managed, ws) <- makeManagedCorrespond ctx >>= ensureRouteOwnership
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
  printModeNotes managed
  printWarnings ws
  return ExitSuccess


-- | Prints mode-drift notes and, on platforms that cannot enforce the
-- declared modes, one warning per affected route.
printModeNotes
  :: (MonadFileSystem i, MonadIO i)
  => [ManagedCorrespondence]
  -> App i ()
printModeNotes managed = do
  pathStyle <- pathStyleFor stderr
  codeStyle <- codeStyleFor stderr
  drifted <- observeModeDrift managed
  forM_ drifted $ \(m, _) ->
    printStderr' Note $
      "The mode of "
        <> pathStyle m.correspondence.destination.path
        <> " does not satisfy the declared "
        <> codeStyle (renderRouteMode m.route.mode)
        <> " mode; apply will reconcile it."
  printUnsupportedModeWarnings managed


-- | Warns once per route whose declared mode cannot be enforced on the
-- current platform.  Windows can only enforce the read-only distinction,
-- so any other declaration must be surfaced instead of silently claiming
-- success.
printUnsupportedModeWarnings
  :: (MonadIO i)
  => [ManagedCorrespondence]
  -> App i ()
printUnsupportedModeWarnings managed =
  when (System.Info.os == "mingw32") $ do
    pathStyle <- pathStyleFor stderr
    codeStyle <- codeStyleFor stderr
    forM_ unsupported $ \(name, mode) ->
      printStderr' Warning $
        "Route "
          <> pathStyle name
          <> " declares mode "
          <> codeStyle (renderRouteMode mode)
          <> ", which cannot be enforced on this platform."
 where
  unsupported =
    nub
      [ (m.route.routeName, m.route.mode)
      | m <- managed
      , m.route.mode /= DefaultMode
      , m.route.mode /= ReadOnly
      ]


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
    (UndefinedFact key) ->
      return $
        "Reference to an undefined machine fact: "
          <> codeStyle (factKeyText key)
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
