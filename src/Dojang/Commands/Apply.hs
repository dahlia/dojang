{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Dojang.Commands.Apply (apply, applyWithCodecRuntime) where

import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.Reader (asks)
import Data.List (nub)
import System.Exit (ExitCode (..))
import Prelude hiding (readFile)

import Control.Monad.Logger (logDebug, logDebugSH)
import Data.Map.Strict (fromList, notMember, toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as Set
import System.OsPath
  ( OsPath
  , addTrailingPathSeparator
  , normalise
  )

import Dojang.App
  ( App
  , AppEffects
  , AppEnv (debug, dryRun, manifestFile, stateDirectory)
  , ensureContext
  , markMachineStateApplied
  , prepareMachineState
  )
import Dojang.CommandEffect (MonadCommandEffect (abortCommand, currentTime))
import Dojang.Commands
  ( Admonition (..)
  , StandardStream (..)
  , codeStyleFor
  , die'
  , ensureRouteOwnership
  , pathStyleFor
  , printModeRestoreFailure
  , printSkippedReconciliation
  , printStderr
  , printStderr'
  )
import Dojang.Commands.Hook
  ( HookScopePath (CallerRelativePath)
  , executeHooks
  , makeHookEnv
  )
import Dojang.Commands.Status
  ( defaultStatusOptions
  , printUnsupportedModeWarnings
  , printWarnings
  , statusCoreWithEvaluated
  )
import Dojang.ExitCodes
  ( accidentalDeletionWarning
  , codecError
  , conflictError
  , fileNotRoutedError
  , machineStateError
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Codec.BuiltIn (builtInCodecRuntime)
import Dojang.Types.Codec.Context
  ( EvaluatedManagedCorrespondence (..)
  , evaluateManagedCorrespondencesWithCache
  , evaluationWarnings
  , loadCodecCacheEntries
  , managedCodecStateFor
  , rawSourceDigestFor
  , rawSourceFor
  , renderedSourceFor
  , reuseEvaluatedManagedCorrespondence
  )
import Dojang.Types.Codec.Evaluate
  ( CodecRuntime
  , EvaluationMode (DryRunEvaluation, NormalEvaluation)
  , formatCodecError
  )
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileEntry (..)
  , ManagedCorrespondence (..)
  , RouteState (..)
  , makeManagedCorrespond
  , projectExpectedState
  )
import Dojang.Types.Hook (HookType (..))
import Dojang.Types.MachineState
  ( MachineState (..)
  , formatStateError
  , updateManagedTargetsWith
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (..)
  , SynchronizationCommand (Applied)
  , hasMaterializedSnapshot
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
  , Replica (..)
  , SyncOp (..)
  , destructiveOperations
  , executeReconciliationPlanGuarded
  , observeReconciliationInputWithRenderedSourceGuard
  , planReconciliation
  )
import Dojang.Types.Repository (Repository (..), RouteResult (..))
import Dojang.Types.RouteMetadata (renderRouteMode)
import Dojang.Types.RouteOwnership (ExpectedState (..))
import Dojang.Types.TargetTracking
  ( discardTargetSnapshot
  , newTargetSnapshotTransaction
  , observeConvergedManagedTargetWithRenderedSource
  )


apply :: (MonadFileSystem i, AppEffects i) => Bool -> [OsPath] -> App i ExitCode
apply force filePaths = do
  dryRun' <- asks (.dryRun)
  let mode = if dryRun' then DryRunEvaluation else NormalEvaluation
  applyWithCodecRuntime (builtInCodecRuntime mode) force filePaths


-- | Applies selected routes using an explicit codec runtime.
applyWithCodecRuntime
  :: (MonadFileSystem i, AppEffects i)
  => CodecRuntime (App i)
  -> Bool
  -> [OsPath]
  -> App i ExitCode
applyWithCodecRuntime codecRuntime force filePaths = do
  ctx <- ensureContext
  machineState <- prepareMachineState ctx.repository.manifest
  let isFirstApply = not machineState.firstApplied

  manifestFile' <- asks (.manifestFile)
  hookEnv <-
    makeHookEnv "apply" (CallerRelativePath <$> filePaths) ctx machineState

  -- Run pre-apply hooks
  $(logDebug) "Running pre-apply hooks..."
  executeHooks hookEnv ctx PreApply
  when isFirstApply $ do
    $(logDebug) "Running pre-first-apply hooks..."
    executeHooks hookEnv ctx PreFirstApply

  (allManaged, ws) <- makeManagedCorrespond ctx >>= ensureRouteOwnership
  (ownership, _) <- projectExpectedState ctx
  expectedState <- ensureRouteOwnership ownership
  fileMap <- fmap fromList $ forM allManaged $ \managed -> do
    srcAbsPath <- makeAbsolute managed.correspondence.source.path
    return (srcAbsPath, managed)
  pathStyle <- pathStyleFor StandardError
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
  cache <- loadCodecCacheEntries ctx machineState managed
  evaluatedResult <-
    evaluateManagedCorrespondencesWithCache codecRuntime ctx cache managed
  evaluated <- case evaluatedResult of
    Left err -> die' codecError $ formatCodecError err
    Right value -> return value
  let managed' = (.managed) <$> evaluated
  let files = (.correspondence) <$> managed'
  $(logDebugSH) files
  printUnsupportedModeWarnings managed'
  inputs <-
    mapM
      (observeSelectedReconciliationInput ctx expectedState)
      evaluated
  let conflictPolicy =
        if force then PreferAuthoritative else RefuseConflicts
  let plan =
        planReconciliation SourceToDestination conflictPolicy inputs
  let conflicts = plan.conflicts
  codeStyle <- codeStyleFor StandardError
  forM_ plan.items $ \item -> case item.outcome of
    Skipped reason ->
      printSkippedReconciliation
        pathStyle
        item.correspondence.source.path
        item.correspondence.destination.path
        reason
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
      RemoveDirsExcept path _ -> do
        let path' = addTrailingPathSeparator path
        if force
          then
            printStderr' Warning $
              "Would delete "
                <> pathStyle path'
                <> " (except entries owned by nested routes)."
          else
            printStderr' Error $
              "Cancelled applying because "
                <> pathStyle path'
                <> " (except entries owned by nested routes) would be"
                <> " deleted."
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
      abortCommand $
        if not (null conflicts)
          then conflictError
          else accidentalDeletionWarning
  -- When everything is fine (or excused):
  debug' <- asks (.debug)
  when
    debug'
    ( void $
        statusCoreWithEvaluated
          ctx
          machineState
          ws
          allManaged
          evaluated
          defaultStatusOptions
    )
  let persist = persistConvergedTargets ctx machineState evaluated
  void
    ( executeReconciliationPlanGuarded
        (printSyncOp . (.syncOp))
        printModeRestoreFailure
        plan
        `catchError` \err -> persist >> throwError err
    )
  persist
  when debug' $ do
    refreshedMachineState <- prepareMachineState ctx.repository.manifest
    (refreshedManaged, refreshedWarnings) <-
      makeManagedCorrespond ctx >>= ensureRouteOwnership
    let evaluationByPath =
          Map.fromList
            [ (correspondenceIdentity item.managed, item)
            | item <- evaluated
            ]
        selectedRefreshed =
          mapMaybe
            ( \refreshed -> do
                previous <- Map.lookup (correspondenceIdentity refreshed) evaluationByPath
                return (refreshed, previous)
            )
            refreshedManaged
    refreshedEvaluated <-
      forM selectedRefreshed $ \(refreshed, previous) ->
        reuseEvaluatedManagedCorrespondence refreshed previous
    void $
      statusCoreWithEvaluated
        ctx
        refreshedMachineState
        refreshedWarnings
        refreshedManaged
        refreshedEvaluated
        defaultStatusOptions
  printWarnings $ nub $ ws <> evaluationWarnings evaluated

  -- Run post-apply hooks
  when isFirstApply $ do
    $(logDebug) "Running post-first-apply hooks..."
    executeHooks hookEnv ctx PostFirstApply
  $(logDebug) "Running post-apply hooks..."
  executeHooks hookEnv ctx PostApply
  when isFirstApply $ markMachineStateApplied machineState
  return ExitSuccess
 where
  correspondenceIdentity :: ManagedCorrespondence -> (OsPath, OsPath)
  correspondenceIdentity managed =
    ( normalise managed.correspondence.source.path
    , normalise managed.correspondence.destination.path
    )


persistConvergedTargets
  :: (MonadFileSystem i, AppEffects i)
  => Context (App i)
  -> MachineState
  -> [EvaluatedManagedCorrespondence]
  -> App i ()
persistConvergedTargets ctx machineState selected =
  unless (null selected) $ do
    now <- currentTime
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
                    (observeWithCodecState transaction now)
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
                  Set.fromList
                    [ record.snapshotPath
                    | record <- Map.elems updated.targetRecords
                    , hasMaterializedSnapshot record
                    ]
            in unreachableSnapshots
                 kept
                 ( transaction
                     : [ record.snapshotPath
                       | record <- superseded
                       , hasMaterializedSnapshot record
                       ]
                 )
        )
        (\_ _ -> return ())
        (\_ (transaction, _) -> discardTargetSnapshot transaction)
    case result of
      Left err -> die' machineStateError $ formatStateError err
      Right _ -> return ()
 where
  observeWithCodecState transaction now evaluated = do
    observation <-
      observeConvergedManagedTargetWithRenderedSource
        ctx.repository
        transaction
        Applied
        now
        (renderedSourceFor evaluated)
        (rawSourceDigestFor evaluated)
        evaluated.managed
    let codecState' = managedCodecStateFor evaluated
    return $ case observation of
      Just (identifier, Just target) ->
        Just (identifier, Just target{codecState = codecState'})
      _ -> observation


-- | Observes a correspondence that the command has already selected.  A
-- tracked file remains managed even if its destination path also matches an
-- ignore pattern.
observeSelectedReconciliationInput
  :: (MonadFileSystem i, MonadCommandEffect i)
  => Context i
  -> ExpectedState
  -> EvaluatedManagedCorrespondence
  -> i ReconciliationInput
observeSelectedReconciliationInput ctx expectedState evaluated = do
  let managed = evaluated.managed
  input <-
    observeReconciliationInputWithRenderedSourceGuard
      ctx
      managed.route.mode
      managed.correspondence
      (renderedSourceFor evaluated)
      (rawSourceFor evaluated)
  let protected =
        Map.findWithDefault
          []
          (normalise managed.route.destinationPath)
          expectedState.nestedUnder
  let input' =
        input
          { destinationKind = managed.route.kind
          , routeFileType = managed.route.fileType
          , protectedDestinations = protected
          }
  return $ case input'.destinationRouteState of
    Ignored route _ -> input'{destinationRouteState = Routed route}
    _ -> input'


printSyncOp :: (AppEffects i) => SyncOp -> App i ()
printSyncOp (RemoveDirs path) = do
  pathStyle <- pathStyleFor StandardError
  let path' = addTrailingPathSeparator path
  printStderr ("Remove " <> pathStyle path' <> " (and its children)...")
printSyncOp (RemoveDirsExcept path _) = do
  pathStyle <- pathStyleFor StandardError
  let path' = addTrailingPathSeparator path
  printStderr
    ( "Remove "
        <> pathStyle path'
        <> " (preserving entries owned by nested routes)..."
    )
printSyncOp (RemoveLink path) = do
  pathStyle <- pathStyleFor StandardError
  printStderr ("Remove link " <> pathStyle path <> "...")
printSyncOp (RemoveFile path) = do
  pathStyle <- pathStyleFor StandardError
  printStderr ("Remove " <> pathStyle path <> "...")
printSyncOp (CopyFile src dst) = do
  pathStyle <- pathStyleFor StandardError
  printStderr
    ("Copy " <> pathStyle src <> " to " <> pathStyle dst <> "...")
printSyncOp (CopyPrivateFile src dst) = do
  pathStyle <- pathStyleFor StandardError
  printStderr
    ("Copy " <> pathStyle src <> " to " <> pathStyle dst <> "...")
printSyncOp (WriteContent _ dst) = do
  pathStyle <- pathStyleFor StandardError
  printStderr ("Write rendered content to " <> pathStyle dst <> "...")
printSyncOp (WriteContentGuarded _ _ dst) = do
  pathStyle <- pathStyleFor StandardError
  printStderr ("Write rendered content to " <> pathStyle dst <> "...")
printSyncOp (WritePrivateContent _ dst) = do
  pathStyle <- pathStyleFor StandardError
  printStderr ("Write rendered content to " <> pathStyle dst <> "...")
printSyncOp (WritePrivateContentGuarded _ _ dst) = do
  pathStyle <- pathStyleFor StandardError
  printStderr ("Write rendered content to " <> pathStyle dst <> "...")
printSyncOp (CreateDir path) = do
  pathStyle <- pathStyleFor StandardError
  let path' = addTrailingPathSeparator path
  printStderr ("Create " <> pathStyle path' <> "...")
printSyncOp (CreateDirs path) = do
  pathStyle <- pathStyleFor StandardError
  let path' = addTrailingPathSeparator path
  printStderr ("Create " <> pathStyle path' <> " (and its ancestors)...")
printSyncOp (CreateSymlink target link _) = do
  pathStyle <- pathStyleFor StandardError
  printStderr
    ("Link " <> pathStyle link <> " to " <> pathStyle target <> "...")
printSyncOp (SetEntryMode path mode _) = do
  pathStyle <- pathStyleFor StandardError
  codeStyle <- codeStyleFor StandardError
  printStderr
    ( "Set mode of "
        <> pathStyle path
        <> " to "
        <> codeStyle (renderRouteMode mode)
        <> "..."
    )
