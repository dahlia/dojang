{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Explicit lifecycle commands for machine-local managed-target records.
module Dojang.Commands.TargetLifecycle (forget, unmanage) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Reader (asks)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath (OsPath, normalise, takeDirectory, (</>))

import Dojang.App
  ( App
  , AppEffects
  , AppEnv (sourceDirectory, stateDirectory)
  , clearLegacyFirstApplyHistory
  , ensureContext
  , ensureManifest
  , prepareMachineState
  , validateRepositoryStateOwnership
  )
import Dojang.CommandEffect (MonadCommandEffect (currentTime))
import Dojang.Commands
  ( Admonition (..)
  , StandardStream (..)
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
import Dojang.MonadFileSystem
  ( MonadFileSystem (..)
  , captureDirectoryPathIdentity
  , matchesDirectoryPathIdentity
  )
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
  , withMachineStateLock
  , withRepositoryStateLock
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
import Dojang.Types.Merge (mergeWorkspaceRepositoryRoot)
import Dojang.Types.Repository (Repository (..))
import Dojang.Types.RepositoryId (RepositoryId)
import Dojang.Types.TargetTracking (observeOrphanStatus)


-- | Stops tracking selected orphan records while leaving destinations intact.
unmanage
  :: (MonadFileSystem i, AppEffects i)
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
  :: (MonadFileSystem i, AppEffects i)
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
    codeStyle <- codeStyleFor StandardError
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
  pathStyle <- pathStyleFor StandardError
  forM_ (zip selected statuses) $ \(target, targetStatus) ->
    printStderr' Note $
      "Selected managed-target record "
        <> pathStyle target.destinationPath
        <> " ("
        <> renderOrphanStatus targetStatus
        <> ") for removal."
  when (not force && OrphanModified `elem` statuses) $ do
    codeStyle <- codeStyleFor StandardError
    die' accidentalDeletionWarning $
      "At least one orphan destination differs from its recorded baseline.  "
        <> "Review it "
        <> "and retry with `"
        <> codeStyle "--force"
        <> "' to discard only its machine-local record."
  now <- currentTime
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
  :: (MonadFileSystem i, AppEffects i)
  => Bool
  -> App i ExitCode
forget force = do
  manifest <- ensureManifest
  root <- asks (.stateDirectory)
  checkout <- asks (.sourceDirectory) >>= fmap normalise . makeAbsolute
  repositoryId <- case manifest.repositoryId of
    Nothing -> die' machineStateError "This repository has no stable identity."
    Just identifier -> return identifier
  initialMachineResult <- readMachineId root
  initialMachine <- stateOrDie initialMachineResult
  machine <- case initialMachine of
    Just machineId -> return $ Just machineId
    Nothing -> do
      stateRootExists <- isDirectory root
      if not stateRootExists
        then return Nothing
        else do
          machineResult <-
            withMachineStateLock root $ do
              currentResult <- readMachineId root
              current <- stateOrDie currentResult
              case current of
                Nothing ->
                  removeMergeWorkspaces root repositoryId >> return Nothing
                Just machineId -> return $ Just machineId
          stateOrDie machineResult
  case machine of
    Nothing -> reportAbsent
    Just machineId -> do
      finish <-
        withRepositoryStateLock root repositoryId $ do
          existingResult <- readRepositoryState root repositoryId machineId
          existing <- stateOrDie existingResult
          case existing of
            Nothing -> do
              progressResult <- isRepositoryForgetInProgress root repositoryId
              retrying <- stateOrDie progressResult
              if retrying
                then return True
                else removeMergeWorkspaces root repositoryId >> return False
            Just _ -> return True
      shouldFinish <- stateOrDie finish
      if shouldFinish
        then finishForget root checkout repositoryId machineId
        else reportAbsent
 where
  finishForget root checkout repositoryId machineId = do
    priorProgressResult <- isRepositoryForgetInProgress root repositoryId
    approvedRetry <- stateOrDie priorProgressResult
    when approvedRetry $ removeMergeWorkspaces root repositoryId
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
            codeStyle <- codeStyleFor StandardError
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
        removeMergeWorkspaces root repositoryId
        removeSnapshot state.targetSnapshotRoot
        removeSnapshot state.intermediatePath
        removeEmptySnapshotDirectory $ takeDirectory state.targetSnapshotRoot
    forgotten' <- stateOrDie forgotten
    case forgotten' of
      Nothing -> reportAbsent
      Just () -> do
        printStderr' Note $
          "Forgot this repository's machine-local targets, snapshots, merge "
            <> "workspaces, and first-apply history."
        return ExitSuccess

  reportAbsent = do
    printStderr' Note "This repository has no machine-local state to forget."
    return ExitSuccess


renderOrphanStatus :: OrphanStatus -> Text
renderOrphanStatus OrphanUnchanged = "unchanged"
renderOrphanStatus OrphanModified = "modified"
renderOrphanStatus OrphanMissing = "missing"


removeSnapshot :: (MonadFileSystem i, AppEffects i) => OsPath -> App i ()
removeSnapshot path = do
  symbolicLink <- isSymlink path
  when symbolicLink $
    die' machineStateError "Refusing to remove a symbolic-link snapshot."
  directory <- isDirectory path
  file <- isFile path
  if directory
    then removeDirectoryRecursively path
    else when file $ removeFile path


removeMergeWorkspaces
  :: (MonadFileSystem i, AppEffects i)
  => OsPath
  -> RepositoryId
  -> App i ()
removeMergeWorkspaces root repositoryId = do
  workspaceRoot <- mergeWorkspaceRepositoryRoot root repositoryId
  absoluteWorkspaceRoot <- makeAbsolute workspaceRoot
  symbolicLink <- isSymlink absoluteWorkspaceRoot
  when symbolicLink $
    die' machineStateError "Refusing to remove a symbolic-link merge workspace."
  directory <- isDirectory absoluteWorkspaceRoot
  file <- isFile absoluteWorkspaceRoot
  when file $ do
    pathStyle <- pathStyleFor StandardError
    die' machineStateError $
      "Refusing to remove the non-directory merge workspace "
        <> pathStyle absoluteWorkspaceRoot
        <> ".  Remove it manually, then retry."
  when directory $ do
    pathIdentity <- captureDirectoryPathIdentity absoluteWorkspaceRoot
    expectedIdentity <- getFileIdentity absoluteWorkspaceRoot
    case (pathIdentity, expectedIdentity) of
      (Just expectedPath, Just expectedEntry) -> do
        unchanged <- matchesDirectoryPathIdentity expectedPath
        unless unchanged $
          die'
            machineStateError
            "The merge-workspace path changed while it was being removed."
        removed <-
          removeDirectoryRecursivelyIfIdentity
            absoluteWorkspaceRoot
            expectedEntry
        unless removed $
          die'
            machineStateError
            "The merge workspace changed while it was being removed."
      _ ->
        die'
          machineStateError
          "Refusing to remove a merge workspace through an unsafe directory path."


removeEmptySnapshotDirectory
  :: (MonadFileSystem i, AppEffects i) => OsPath -> App i ()
removeEmptySnapshotDirectory path = do
  symbolicLink <- isSymlink path
  when symbolicLink $
    die' machineStateError "Refusing to remove a symbolic-link snapshot directory."
  directory <- isDirectory path
  when directory $ do
    entries <- listDirectory path
    when (null entries) $ removeDirectory path


stateOrDie
  :: (AppEffects i)
  => Either StateError value
  -> App i value
stateOrDie result = case result of
  Left err -> die' machineStateError $ formatStateError err
  Right value -> return value
