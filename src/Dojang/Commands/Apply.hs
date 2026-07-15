{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Dojang.Commands.Apply (apply) where

import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.Text (Text, pack)
import Data.Time (getCurrentTime)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import Prelude hiding (readFile)

import Control.Monad.Logger (logDebug, logDebugSH)
import Data.CaseInsensitive (original)
import Data.Map.Strict (fromList, notMember, toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import System.OsPath
  ( OsPath
  , addTrailingPathSeparator
  )

import Dojang.App
  ( App
  , AppEnv (debug, dryRun, manifestFile, sourceDirectory, stateDirectory)
  , ensureContext
  , markMachineStateApplied
  , prepareMachineState
  )
import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , die'
  , pathStyleFor
  , printStderr
  , printStderr'
  )
import Dojang.Commands.Hook (HookEnv (HookEnv), executeHooks)
import Dojang.Commands.Status (defaultStatusOptions, printWarnings, status)
import Dojang.ExitCodes
  ( accidentalDeletionWarning
  , conflictError
  , fileNotRoutedError
  , machineStateError
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileEntry (..)
  , ManagedCorrespondence (..)
  , RouteState (..)
  , makeManagedCorrespond
  )
import Dojang.Types.Environment (Environment (..))
import Dojang.Types.Hook (HookType (..))
import Dojang.Types.MachineState
  ( MachineState (..)
  , formatStateError
  , updateManagedTargetsWith
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (..)
  , SynchronizationCommand (Applied)
  , mergeConvergedTargets
  , unreachableSnapshots
  )
import Dojang.Types.Reconciliation
  ( ConflictPolicy (..)
  , PlannedSyncOp (..)
  , ReconciliationConflict (..)
  , ReconciliationDirection (..)
  , ReconciliationInput (..)
  , ReconciliationItem (..)
  , ReconciliationOutcome (..)
  , ReconciliationPlan (..)
  , ReconciliationSkipReason (..)
  , Replica (..)
  , SyncOp (..)
  , destructiveOperations
  , executeReconciliationPlanWith
  , observeReconciliationInput
  , planReconciliation
  )
import Dojang.Types.Repository (Repository (..))
import Dojang.Types.TargetTracking
  ( discardTargetSnapshot
  , newTargetSnapshotTransaction
  , observeConvergedManagedTarget
  )


apply :: (MonadFileSystem i, MonadIO i) => Bool -> [OsPath] -> App i ExitCode
apply force filePaths = do
  ctx <- ensureContext
  machineState <- prepareMachineState ctx.repository.manifest
  let isFirstApply = not machineState.firstApplied

  -- Build hook environment
  sourceDir <- asks (.sourceDirectory)
  manifestFile' <- asks (.manifestFile)
  dryRun' <- asks (.dryRun)
  let environment = ctx.environment
  let hookEnv =
        HookEnv
          sourceDir
          manifestFile'
          dryRun'
          (original environment.operatingSystem.identifier)
          (original environment.architecture.identifier)

  -- Run pre-apply hooks
  $(logDebug) "Running pre-apply hooks..."
  executeHooks hookEnv ctx PreApply
  when isFirstApply $ do
    $(logDebug) "Running pre-first-apply hooks..."
    executeHooks hookEnv ctx PreFirstApply

  (allManaged, ws) <- makeManagedCorrespond ctx
  fileMap <- fmap fromList $ forM allManaged $ \managed -> do
    srcAbsPath <- makeAbsolute managed.correspondence.source.path
    return (srcAbsPath, managed)
  pathStyle <- pathStyleFor stderr
  filePaths' <- forM filePaths $ \fp -> do
    fp' <- makeAbsolute fp
    when (fp' `notMember` fileMap) $ do
      die' fileNotRoutedError $
        "File "
          <> pathStyle fp
          <> " is not tracked by this repository."
    return fp'
  let managed =
        if null filePaths'
          then allManaged
          else
            [ f
            | (srcAbsPath, f) <- toList fileMap
            , srcAbsPath `elem` filePaths'
            ]
  let files = (.correspondence) <$> managed
  $(logDebugSH) files
  inputs <- mapM (observeSelectedReconciliationInput ctx) files
  let conflictPolicy =
        if force then PreferAuthoritative else RefuseConflicts
  let plan =
        planReconciliation SourceToDestination conflictPolicy inputs
  let conflicts = plan.conflicts
  codeStyle <- codeStyleFor stderr
  forM_ plan.items $ \item -> case item.outcome of
    Skipped reason -> printSkippedReconciliation pathStyle item.correspondence reason
    _ -> return ()
  unless (null conflicts) $ do
    forM_ conflicts $ \conflict -> do
      let c = conflict.correspondence
      printStderr' (if force then Warning else Error) $
        "There is a conflict between "
          <> pathStyle c.source.path
          <> " and "
          <> pathStyle c.destination.path
          <> "."
    printStderr' Hint $
      "Use `"
        <> codeStyle "dojang diff"
        <> "' to see the actual changes on both sides."
  let destructiveDestinationOps =
        [ operation.syncOp
        | operation <- destructiveOperations plan
        , operation.replica == DestinationReplica
        ]
  unless (null destructiveDestinationOps) $ do
    forM_ destructiveDestinationOps $ \case
      RemoveDirs path -> do
        let path' = addTrailingPathSeparator path
        if force
          then
            printStderr' Warning $
              "Would delete "
                <> pathStyle path'
                <> " (and its children)."
          else
            printStderr' Error $
              "Cancelled applying because "
                <> pathStyle path'
                <> " (and its children) would be deleted."
      RemoveFile path -> do
        if force
          then printStderr' Warning ("Would delete " <> pathStyle path <> ".")
          else
            printStderr'
              Error
              $ "Cancelled applying because "
                <> pathStyle path
                <> " would be deleted."
      _ -> return ()
    printStderr' Hint $
      "If these deletions are accidental, ignore them in your manifest ("
        <> pathStyle manifestFile'
        <> ")."
  -- Exit if there are any problems (unless forced):
  when
    (not force && (not (null conflicts) || not (null destructiveDestinationOps)))
    $ do
      printStderr' Hint $
        "Use "
          <> codeStyle "-f"
          <> "/"
          <> codeStyle "--force"
          <> " to ignore these warnings and go ahead."
      liftIO $
        exitWith $
          if not (null conflicts)
            then conflictError
            else accidentalDeletionWarning
  -- When everything is fine (or excused):
  debug' <- asks (.debug)
  when debug' (void $ status defaultStatusOptions)
  let persist = persistConvergedTargets ctx machineState managed
  void
    ( executeReconciliationPlanWith
        (printSyncOp . (.syncOp))
        plan
        `catchError` \err -> persist >> throwError err
    )
  persist
  when debug' (void $ status defaultStatusOptions)
  printWarnings ws

  -- Run post-apply hooks
  when isFirstApply $ do
    $(logDebug) "Running post-first-apply hooks..."
    executeHooks hookEnv ctx PostFirstApply
  $(logDebug) "Running post-apply hooks..."
  executeHooks hookEnv ctx PostApply
  when isFirstApply $ markMachineStateApplied machineState
  return ExitSuccess


persistConvergedTargets
  :: (MonadFileSystem i, MonadIO i)
  => Context (App i)
  -> MachineState
  -> [ManagedCorrespondence]
  -> App i ()
persistConvergedTargets ctx machineState selected =
  unless (null selected) $ do
    now <- liftIO getCurrentTime
    root <- asks (.stateDirectory)
    result <-
      updateManagedTargetsWith
        root
        now
        machineState
        ( \existing -> do
            transaction <-
              newTargetSnapshotTransaction machineState.targetSnapshotRoot
            observations <-
              ( catMaybes
                  <$> mapM
                    ( observeConvergedManagedTarget
                        ctx.repository
                        transaction
                        Applied
                        now
                    )
                    selected
              )
                `catchError` \err -> do
                  discardTargetSnapshot transaction
                    `catchError` const (return ())
                  throwError err
            let (updated, superseded) =
                  mergeConvergedTargets existing observations
            return (updated, (transaction, superseded))
        )
        ( \updated (transaction, superseded) ->
            let kept =
                  Set.fromList $
                    (.snapshotPath) <$> Map.elems updated.targetRecords
            in unreachableSnapshots
                 kept
                 (transaction : ((.snapshotPath) <$> superseded))
        )
        (\_ _ -> return ())
        (\_ (transaction, _) -> discardTargetSnapshot transaction)
    case result of
      Left err -> die' machineStateError $ formatStateError err
      Right _ -> return ()


-- | Observes a correspondence that the command has already selected.  A
-- tracked file remains managed even if its destination path also matches an
-- ignore pattern.
observeSelectedReconciliationInput
  :: (MonadFileSystem i, MonadIO i)
  => Context i
  -> FileCorrespondence
  -> i ReconciliationInput
observeSelectedReconciliationInput ctx correspondence = do
  input <- observeReconciliationInput ctx correspondence
  return $ case input.destinationRouteState of
    Ignored route _ -> input{destinationRouteState = Routed route}
    _ -> input


printSkippedReconciliation
  :: (MonadIO i)
  => (OsPath -> Text)
  -> FileCorrespondence
  -> ReconciliationSkipReason
  -> App i ()
printSkippedReconciliation pathStyle correspondence reason =
  case reason of
    IgnoredDestination _ pattern ->
      printStderr' Warning $
        "Skipping "
          <> pathStyle correspondence.destination.path
          <> " because it is ignored by pattern "
          <> pack (show pattern)
          <> "."
    UnsupportedSymlink ->
      printStderr' Warning $
        "Skipping "
          <> pathStyle correspondence.source.path
          <> " because symbolic link synchronization is not supported."


printSyncOp :: (MonadIO i) => SyncOp -> App i ()
printSyncOp (RemoveDirs path) = do
  pathStyle <- pathStyleFor stderr
  let path' = addTrailingPathSeparator path
  printStderr ("Remove " <> pathStyle path' <> " (and its children)...")
printSyncOp (RemoveFile path) = do
  pathStyle <- pathStyleFor stderr
  printStderr ("Remove " <> pathStyle path <> "...")
printSyncOp (CopyFile src dst) = do
  pathStyle <- pathStyleFor stderr
  printStderr
    ("Copy " <> pathStyle src <> " to " <> pathStyle dst <> "...")
printSyncOp (CreateDir path) = do
  pathStyle <- pathStyleFor stderr
  let path' = addTrailingPathSeparator path
  printStderr ("Create " <> pathStyle path' <> "...")
printSyncOp (CreateDirs path) = do
  pathStyle <- pathStyleFor stderr
  let path' = addTrailingPathSeparator path
  printStderr ("Create " <> pathStyle path' <> " (and its ancestors)...")
