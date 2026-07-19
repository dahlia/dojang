{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Explicit lifecycle commands for machine-local managed-target records.
module Dojang.Commands.TargetLifecycle (forget, unmanage) where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (getCurrentTime)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (stderr)
import System.OsPath (OsPath, normalise, takeDirectory, (</>))

import Dojang.App
  ( App
  , AppEnv (sourceDirectory, stateDirectory)
  , clearLegacyFirstApplyHistory
  , ensureContext
  , ensureManifest
  , prepareMachineState
  , validateRepositoryStateOwnership
  )
import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , die'
  , ensureRouteOwnership
  , pathStyleFor
  , printStderr'
  )
import Dojang.Commands.Hook
  ( HookScopePath (CallerRelativePath, RepositoryRelativePath)
  , withCommandHooks
  )
import Dojang.ExitCodes
  ( accidentalDeletionWarning
  , lifecycleSelectionError
  , machineStateError
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Context
  ( Context (..)
  , makeManagedCorrespond
  , routePaths
  )
import Dojang.Types.MachineState
  ( MachineState (..)
  , StateError
  , forgetRepositoryStateWith
  , formatStateError
  , isRepositoryForgetInProgress
  , markRepositoryForgetInProgress
  , readMachineId
  , readRepositoryState
  , updateManagedTargetsWith
  , validateSelectedSnapshotLocation
  )
import Dojang.Types.ManagedTarget
  ( CurrentEntry (..)
  , ManagedTarget (..)
  , OrphanStatus (..)
  , classifyOrphan
  , equalDestinationPath
  , hasMaterializedSnapshot
  , makeCurrentEntries
  , makeCurrentRoutes
  , selectOrphanRecords
  , unreachableSnapshots
  )
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.Repository (Repository (..))
import Dojang.Types.TargetTracking (observeOrphanStatus)


-- | Stops tracking selected orphan records while leaving destinations intact.
unmanage
  :: (MonadFileSystem i, MonadIO i)
  => Maybe OsPath
  -> [OsPath]
  -> Bool
  -> App i ExitCode
unmanage routeSelector destinations force = do
  when (routeSelector == Nothing && null destinations) $
    die' lifecycleSelectionError "Select a route or at least one destination."
  withCommandHooks
    "unmanage"
    ( maybe
        (CallerRelativePath <$> destinations)
        ( \route ->
            RepositoryRelativePath route
              : (CallerRelativePath <$> destinations)
        )
        routeSelector
    )
    (unmanageCore routeSelector destinations force)


unmanageCore
  :: (MonadFileSystem i, MonadIO i)
  => Maybe OsPath
  -> [OsPath]
  -> Bool
  -> App i ExitCode
unmanageCore routeSelector destinations force = do
  ctx <- ensureContext
  state <- prepareMachineState ctx.repository.manifest
  absoluteDestinations <- mapM (fmap normalise . makeAbsolute) destinations
  let routeName = normalise <$> routeSelector
  let records = Map.elems state.targetRecords
  let sameDestination = equalDestinationPath
  let matches :: ManagedTarget -> Bool
      matches target =
        maybe False (== target.routeName) routeName
          || any
            (sameDestination $ normalise target.destinationPath)
            absoluteDestinations
  let routeKnown =
        maybe True (\name -> any ((== name) . (.routeName)) records) routeName
  let destinationsKnown =
        all
          ( \destination ->
              any
                (sameDestination destination . normalise . (.destinationPath))
                records
          )
          absoluteDestinations
  when (not routeKnown || not destinationsKnown) $
    die' lifecycleSelectionError "At least one lifecycle selector is unknown."
  let matched = filter matches records
  when (null matched) $ do
    codeStyle <- codeStyleFor stderr
    die' lifecycleSelectionError $
      "No managed target matches the selection.  Use `"
        <> codeStyle "dojang status"
        <> "' to inspect orphan records."
  (routes, _) <- routePaths ctx
  current <- makeCurrentRoutes routes
  (managed, _) <- makeManagedCorrespond ctx >>= ensureRouteOwnership
  let entries = makeCurrentEntries managed
  let selected =
        filter ((/= Nothing) . classifyOrphan current entries) matched
  when (null selected) $
    die' lifecycleSelectionError $
      "The selection includes an active target.  Change the manifest first, "
        <> "then run `dojang unmanage' again."
  statuses <- mapM observeOrphanStatus selected
  pathStyle <- pathStyleFor stderr
  forM_ (zip selected statuses) $ \(target, targetStatus) ->
    printStderr' Note $
      "Selected managed-target record "
        <> pathStyle target.destinationPath
        <> " ("
        <> renderOrphanStatus targetStatus
        <> ") for removal."
  when (not force && OrphanModified `elem` statuses) $ do
    codeStyle <- codeStyleFor stderr
    die' accidentalDeletionWarning $
      "At least one orphan destination differs from its recorded baseline.  "
        <> "Review it "
        <> "and retry with `"
        <> codeStyle "--force"
        <> "' to discard only its machine-local record."
  now <- liftIO getCurrentTime
  root <- asks (.stateDirectory)
  let selectedIds = Set.fromList $ (.targetId) <$> selected
  result <-
    updateManagedTargetsWith
      root
      now
      state
      ( \currentRecords -> do
          (lockedRoutes, _) <- routePaths ctx
          lockedCurrent <- makeCurrentRoutes lockedRoutes
          (lockedManaged, _) <-
            makeManagedCorrespond ctx >>= ensureRouteOwnership
          let lockedEntries = makeCurrentEntries lockedManaged
          removed <- case selectOrphanRecords
            lockedCurrent
            lockedEntries
            selectedIds
            currentRecords of
            Nothing ->
              die' lifecycleSelectionError $
                "The selected targets changed while the state was being "
                  <> "updated.  Inspect `dojang status' and retry."
            Just records' -> return records'
          lockedStatuses <- mapM observeOrphanStatus removed
          when (not force && OrphanModified `elem` lockedStatuses) $
            die' accidentalDeletionWarning $
              "A selected orphan changed while the state was being updated.  "
                <> "Review it and retry with `--force'."
          let kept =
                Map.filterWithKey
                  (\key _ -> key `Set.notMember` selectedIds)
                  currentRecords
          let currentEntrySources =
                Set.map (\entry -> entry.sourcePath) lockedEntries
          return (kept, (removed, currentEntrySources))
      )
      ( \updated (removed, currentEntrySources) ->
          let keptSnapshots =
                Set.fromList
                  [ record.snapshotPath
                  | record <- Map.elems updated.targetRecords
                  , hasMaterializedSnapshot record
                  ]
              baselineCandidates =
                unreachableSnapshots
                  keptSnapshots
                  [ record.snapshotPath
                  | record <- removed
                  , hasMaterializedSnapshot record
                  ]
              keptIntermediate =
                Set.fromList
                  [ updated.intermediatePath </> target.sourcePath
                  | target <- Map.elems updated.targetRecords
                  ]
                  <> Set.map
                    (updated.intermediatePath </>)
                    currentEntrySources
              intermediateCandidates =
                unreachableSnapshots
                  keptIntermediate
                  [ updated.intermediatePath </> target.sourcePath
                  | target <- removed
                  ]
          in baselineCandidates <> intermediateCandidates
      )
      (\_ _ -> return ())
      (\_ _ -> return ())
  _ <- stateOrDie result
  forM_ selected $ \target ->
    printStderr' Note $
      "No longer managing " <> pathStyle target.destinationPath <> "."
  return ExitSuccess


-- | Removes all machine-local state for the selected repository.
forget
  :: (MonadFileSystem i, MonadIO i)
  => Bool
  -> App i ExitCode
forget force = do
  manifest <- ensureManifest
  root <- asks (.stateDirectory)
  checkout <- asks (.sourceDirectory) >>= fmap normalise . makeAbsolute
  repositoryId <- case manifest.repositoryId of
    Nothing -> die' machineStateError "This repository has no stable identity."
    Just identifier -> return identifier
  machineResult <- readMachineId root
  machine <- stateOrDie machineResult
  case machine of
    Nothing -> reportAbsent
    Just machineId -> do
      existingResult <- readRepositoryState root repositoryId machineId
      existing <- stateOrDie existingResult
      case existing of
        Nothing -> do
          progressResult <- isRepositoryForgetInProgress root repositoryId
          retrying <- stateOrDie progressResult
          if retrying
            then finishForget root checkout repositoryId machineId
            else reportAbsent
        Just _ -> finishForget root checkout repositoryId machineId
 where
  finishForget root checkout repositoryId machineId = do
    forgotten <-
      forgetRepositoryStateWith root repositoryId machineId $ \state -> do
        ownership <- validateRepositoryStateOwnership checkout state
        _ <- stateOrDie ownership
        snapshotValidation <-
          validateSelectedSnapshotLocation
            root
            repositoryId
            [state.checkoutPath, checkout]
            state.intermediatePath
        _ <- stateOrDie snapshotValidation
        progressResult <-
          isRepositoryForgetInProgress root repositoryId
        retrying <- stateOrDie progressResult
        unless retrying $ do
          statuses <-
            mapM observeOrphanStatus $ Map.elems state.targetRecords
          when (not force && OrphanModified `elem` statuses) $ do
            codeStyle <- codeStyleFor stderr
            die' accidentalDeletionWarning $
              "At least one managed destination differs from its snapshot.  "
                <> "Review it and retry with `"
                <> codeStyle "--force"
                <> "' to forget only machine-local state."
          clearLegacyFirstApplyHistory checkout
          marked <- markRepositoryForgetInProgress root repositoryId
          _ <- stateOrDie marked
          return ()
        when retrying $ clearLegacyFirstApplyHistory checkout
        removeSnapshot state.targetSnapshotRoot
        removeSnapshot state.intermediatePath
        removeEmptySnapshotDirectory $ takeDirectory state.targetSnapshotRoot
    forgotten' <- stateOrDie forgotten
    case forgotten' of
      Nothing -> reportAbsent
      Just () -> do
        printStderr' Note $
          "Forgot this repository's machine-local targets, snapshots, and "
            <> "first-apply history."
        return ExitSuccess

  reportAbsent = do
    printStderr' Note "This repository has no machine-local state to forget."
    return ExitSuccess


renderOrphanStatus :: OrphanStatus -> Text
renderOrphanStatus OrphanUnchanged = "unchanged"
renderOrphanStatus OrphanModified = "modified"
renderOrphanStatus OrphanMissing = "missing"


removeSnapshot :: (MonadFileSystem i, MonadIO i) => OsPath -> App i ()
removeSnapshot path = do
  symbolicLink <- isSymlink path
  when symbolicLink $
    die' machineStateError "Refusing to remove a symbolic-link snapshot."
  directory <- isDirectory path
  file <- isFile path
  if directory
    then removeDirectoryRecursively path
    else when file $ removeFile path


removeEmptySnapshotDirectory
  :: (MonadFileSystem i, MonadIO i) => OsPath -> App i ()
removeEmptySnapshotDirectory path = do
  symbolicLink <- isSymlink path
  when symbolicLink $
    die' machineStateError "Refusing to remove a symbolic-link snapshot directory."
  directory <- isDirectory path
  when directory $ do
    entries <- listDirectory path
    when (null entries) $ removeDirectory path


stateOrDie
  :: (MonadIO i)
  => Either StateError value
  -> App i value
stateOrDie result = case result of
  Left err -> die' machineStateError $ formatStateError err
  Right value -> return value
