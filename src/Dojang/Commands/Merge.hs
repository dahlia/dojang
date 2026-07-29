{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | External three-way merge orchestration for reconciliation conflicts.
module Dojang.Commands.Merge
  ( defaultMergeDriverConfigPath
  , makeMergeDriverProcessRequest
  , merge
  , mergeWithDriverRunner
  , mergeWithDriverRunnerAndPublisher
  , mergeWithDriverRunnerAndPublisherAndPreparer
  , persistMergedTarget
  ) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.Reader (asks)
import Data.List (find, isPrefixOf, nubBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import System.Exit (ExitCode (..))
import System.IO.Error (ioeGetErrorString, isDoesNotExistError)
import System.OsPath
  ( OsPath
  , isAbsolute
  , takeDirectory
  , takeFileName
  , (</>)
  )
import Prelude hiding (readFile)

import Dojang.App
  ( App
  , AppEffects
  , AppEnv (dryRun, stateDirectory)
  , catchCommandExit
  , contextFromExistingMachineState
  , ensureContext
  , ensureManifest
  , prepareMachineState
  , readExistingMachineState
  )
import Dojang.CommandEffect
  ( MonadCommandEffect (..)
  , ProcessRequest (..)
  , ProcessResult (..)
  , emptyProcessRequest
  )
import Dojang.Commands
  ( Admonition (Error, Hint, Warning)
  , StandardStream (StandardError)
  , die'
  , ensureRouteOwnership
  , pathStyleFor
  , printStderr
  , printStderr'
  )
import Dojang.Commands.Hook
  ( HookScopePath (CallerRelativePath)
  , executeHooks
  , makeHookEnv
  )
import Dojang.Commands.Status (printWarnings)
import Dojang.ExitCodes
  ( cliError
  , conflictError
  , externalProgramNonZeroExit
  , fileNotRoutedError
  , fileWriteError
  , machineStateError
  , userCancelledError
  )
import Dojang.MonadFileSystem
  ( DirectoryPathIdentity
  , FileIdentity
  , FileModeSnapshot (FileModeSnapshot)
  , MonadFileSystem (..)
  , captureDirectoryPathIdentity
  , matchesDirectoryPathIdentity
  )
import Dojang.MonadFileSystem qualified as FileSystem
import Dojang.Syntax.MergeDriver qualified as MergeDriverSyntax
import Dojang.Types.Codec (identityCodecSpec)
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (Modified, Unchanged)
  , FileEntry (..)
  , ManagedCorrespondence (..)
  , makeManagedCorrespond
  )
import Dojang.Types.Context qualified as Context
import Dojang.Types.ExternalCommand
  ( EnvironmentNameCase
      ( CaseInsensitiveEnvironment
      , CaseSensitiveEnvironment
      )
  )
import Dojang.Types.Hook (HookType (PostMerge, PreMerge))
import Dojang.Types.MachineState
  ( MachineState (..)
  , formatStateError
  , updateManagedTargetsWith
  , withRepositoryStateGeneration
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (..)
  , SynchronizationCommand (Merged)
  , hasMaterializedSnapshot
  , mergeConvergedTargets
  , unreachableSnapshots
  )
import Dojang.Types.Manifest (Manifest (repositoryId))
import Dojang.Types.Merge
  ( MergeCommitError (MergeInputsChanged, MergeRecoveryInputsDiffer)
  , MergeCommitReplica (..)
  , MergeContentsState (..)
  , MergeInputError (..)
  , MergeInputRole (..)
  , MergeResultError (..)
  , MergeTextInput (..)
  , MergeWorkspace (..)
  , classifyMergeContents
  , commitMergeRecoveryGuarded
  , commitMergeResultGuarded
  , mergeWorkspaceRepositoryRoot
  , observeMergeTextInput
  , prepareMergeWorkspace
  , readMergeResult
  )
import Dojang.Types.MergeDriver
  ( MergeDriverExit (..)
  , MergeDriverLookupError (..)
  , MergeDriverName
  , MergeDriverSpec
  , classifyMergeDriverExit
  , expandMergeDriverCommandNative
  , lookupMergeDriver
  , renderMergeDriverName
  , resolveMergeDriverEnvironmentNative
  )
import Dojang.Types.PathIdentity (pathIdentityComponents)
import Dojang.Types.Reconciliation
  ( ConflictPolicy (RefuseConflicts)
  , ReconciliationConflict (..)
  , ReconciliationDirection (SourceToDestination)
  , ReconciliationInput (..)
  , ReconciliationPlan (..)
  , ReplicaComparison (ReplicasEquivalent)
  , observeReconciliationInput
  , planReconciliation
  )
import Dojang.Types.Repository (Repository (..), RouteResult (..))
import Dojang.Types.RouteMetadata
  ( RouteKind (CopyRoute)
  , RouteMode (DefaultMode)
  , portableModeFromBits
  , posixFileModeBits
  , satisfiesPortableMode
  )
import Dojang.Types.TargetTracking
  ( discardTargetSnapshot
  , managedTargetId
  , newTargetSnapshotTransaction
  , observeConvergedManagedTarget
  )


data PendingPublication = PendingPublication
  { marker :: OsPath
  , workspace :: OsPath
  , workspaceIdentity :: FileIdentity
  , workspacePathIdentity :: DirectoryPathIdentity
  }


data MergeCandidate = MergeCandidate
  { managed :: ManagedCorrespondence
  , reconciliationCandidate :: Bool
  , pendingPublications :: [PendingPublication]
  }


data MergeAction
  = RunMergeDriver
  | RecoverMergeReplicas
  | PublishMergedTarget
  deriving (Eq, Show)


data ResolvedMergeDriver = ResolvedMergeDriver
  { platform :: String
  , name :: MergeDriverName
  , specification :: MergeDriverSpec
  }


data MergeDriverExecution = MergeDriverExecution
  { resolved :: ResolvedMergeDriver
  , hostEnvironment :: [(String, String)]
  }


data PreparedMerge = PreparedMerge
  { managed :: ManagedCorrespondence
  , action :: MergeAction
  , pendingPublications :: [PendingPublication]
  , source :: MergeTextInput
  , base :: MergeTextInput
  , destination :: MergeTextInput
  }


-- | Runs the merge command with the normal command-effect process runner.
merge
  :: (MonadFileSystem i, AppEffects i)
  => Maybe Text
  -- ^ Optional configured driver name.
  -> Maybe OsPath
  -- ^ Optional driver configuration path.
  -> [OsPath]
  -- ^ Source, destination, or containing paths to select.
  -> App i ExitCode
merge = mergeWithDriverRunner runProcess


-- | Runs the merge command with an injectable shell-free process runner.
--
-- The injected boundary lets tests drive driver outcomes without starting a
-- host process.  Production passes 'runProcess'.
mergeWithDriverRunner
  :: (MonadFileSystem i, AppEffects i)
  => (ProcessRequest -> App i ProcessResult)
  -- ^ Structured process runner.
  -> Maybe Text
  -- ^ Optional configured driver name.
  -> Maybe OsPath
  -- ^ Optional driver configuration path.
  -> [OsPath]
  -- ^ Source, destination, or containing paths to select.
  -> App i ExitCode
mergeWithDriverRunner =
  mergeWithDriverRunnerAndPublisher persistMergedTarget


-- | Runs the merge command with injectable driver and state-publication
-- boundaries.
--
-- This is the most precise testing boundary for failures after authoritative
-- replicas have converged but before their machine-state record is published.
mergeWithDriverRunnerAndPublisher
  :: (MonadFileSystem i, AppEffects i)
  => ( Context (App i)
       -> MachineState
       -> ManagedCorrespondence
       -> App i ()
     )
  -- ^ Publisher for the converged managed-target record.
  -> (ProcessRequest -> App i ProcessResult)
  -- ^ Structured process runner.
  -> Maybe Text
  -- ^ Optional configured driver name.
  -> Maybe OsPath
  -- ^ Optional driver configuration path.
  -> [OsPath]
  -- ^ Source, destination, or containing paths to select.
  -> App i ExitCode
mergeWithDriverRunnerAndPublisher
  publishTarget =
    mergeWithDriverRunnerAndPublisherAndPreparer
      publishTarget
      prepareMergeWorkspace


-- | Runs the merge command with injectable publication and workspace
-- preparation boundaries.
--
-- This boundary lets tests exercise command-level workspace setup failures
-- without weakening the owner-only production workspace implementation.
mergeWithDriverRunnerAndPublisherAndPreparer
  :: (MonadFileSystem i, AppEffects i)
  => ( Context (App i)
       -> MachineState
       -> ManagedCorrespondence
       -> App i ()
     )
  -- ^ Publisher for the converged managed-target record.
  -> ( OsPath
       -> MergeTextInput
       -> MergeTextInput
       -> MergeTextInput
       -> App i MergeWorkspace
     )
  -- ^ Owner-only merge-workspace preparer.
  -> (ProcessRequest -> App i ProcessResult)
  -- ^ Structured process runner.
  -> Maybe Text
  -- ^ Optional configured driver name.
  -> Maybe OsPath
  -- ^ Optional driver configuration path.
  -> [OsPath]
  -- ^ Source, destination, or containing paths to select.
  -> App i ExitCode
mergeWithDriverRunnerAndPublisherAndPreparer
  publishTarget
  prepareWorkspace
  runDriver
  requestedDriver
  requestedConfig
  selectedPaths =
    runMerge
      publishTarget
      prepareWorkspace
      runDriver
      requestedDriver
      requestedConfig
      selectedPaths


runMerge
  :: (MonadFileSystem i, AppEffects i)
  => ( Context (App i)
       -> MachineState
       -> ManagedCorrespondence
       -> App i ()
     )
  -> ( OsPath
       -> MergeTextInput
       -> MergeTextInput
       -> MergeTextInput
       -> App i MergeWorkspace
     )
  -> (ProcessRequest -> App i ProcessResult)
  -> Maybe Text
  -> Maybe OsPath
  -> [OsPath]
  -> App i ExitCode
runMerge publishTarget prepare runDriver driverChoice configChoice paths = do
  pathStyle <- pathStyleFor StandardError
  preHookContext <- ensureContext
  preHookState <- prepareMachineState preHookContext.repository.manifest
  preHookEnv <-
    makeHookEnv
      "merge"
      (CallerRelativePath <$> paths)
      preHookContext
      preHookState
  executeHooks preHookEnv preHookContext PreMerge
  ctx <- ensureContext
  machineState <- prepareMachineState ctx.repository.manifest
  (allManaged, warnings) <-
    (makeManagedCorrespond ctx >>= ensureRouteOwnership)
      `catchError` reportMergeInputObservationError
  printWarnings warnings
  candidates <- reconciliationCandidates ctx machineState allManaged
  selected <- selectCandidates paths allManaged candidates
  if null selected
    then do
      printStderr "No three-way merge conflicts found."
      runPostMergeHooks paths machineState
      return ExitSuccess
    else do
      prepared <- catMaybes <$> mapM (prepareCandidate pathStyle) selected
      if null prepared
        then do
          printStderr "No three-way merge conflicts found."
          runPostMergeHooks paths machineState
          return ExitSuccess
        else do
          resolvedDriver <-
            if any ((== RunMergeDriver) . (.action)) prepared
              then Just <$> resolveDriver pathStyle
              else return Nothing
          dryRunEnabled <- asks (.dryRun)
          if dryRunEnabled
            then do
              forM_ prepared $ \item -> case item.action of
                RecoverMergeReplicas ->
                  printStderr $
                    "Would finish recording the merged baseline for "
                      <> pathStyle item.managed.correspondence.source.path
                      <> "."
                PublishMergedTarget ->
                  printStderr $
                    "Would finish publishing the merged target for "
                      <> pathStyle item.managed.correspondence.source.path
                      <> "."
                RunMergeDriver ->
                  case resolvedDriver of
                    Just driver ->
                      printStderr $
                        "Would merge "
                          <> pathStyle item.managed.correspondence.source.path
                          <> " with "
                          <> pathStyle
                            item.managed.correspondence.destination.path
                          <> " using driver '"
                          <> renderMergeDriverName driver.name
                          <> "'."
                    Nothing ->
                      die' cliError "No merge driver is available."
              runPostMergeHooks paths machineState
              return ExitSuccess
            else do
              driverExecution <-
                forM resolvedDriver $ \driver -> do
                  environment <- processEnvironment
                  return $ MergeDriverExecution driver environment
              (invocationRoot, invocationIdentity) <-
                guardMergeFinalization
                  machineState
                  (createInvocationRoot machineState)
                  `catchError` reportMergeFilesystemError
              workspacesCleaned <-
                processPrepared
                  pathStyle
                  publishTarget
                  prepare
                  runDriver
                  driverExecution
                  machineState
                  invocationRoot
                  prepared
              invocationCleaned <-
                if workspacesCleaned
                  then
                    removeDirectoryRecursivelyIfIdentity
                      invocationRoot
                      invocationIdentity
                      `catchError` reportFinalCleanupError
                        pathStyle
                        invocationRoot
                  else return False
              unless invocationCleaned $
                printStderr' Warning $
                  "The completed merge workspace could not be removed: "
                    <> pathStyle invocationRoot
                    <> "."
              runPostMergeHooks paths machineState
              return ExitSuccess
 where
  reportLookupError requested = \case
    InvalidLookupMergeDriverName err ->
      die' cliError $
        "Invalid merge driver name: " <> Text.pack (show err) <> "."
    UnknownMergeDriverName name ->
      die' cliError $
        "Unknown merge driver '"
          <> maybe name id requested
          <> "'."
  resolveDriver pathStyle = do
    platform <- hostPlatform
    configPath <-
      maybe (defaultMergeDriverConfigPath platform) return configChoice
    configResult <-
      MergeDriverSyntax.readMergeDriverConfigFile configPath
        `catchError` reportConfigReadError pathStyle configPath
    config <-
      either
        (die' cliError . MergeDriverSyntax.formatError)
        return
        configResult
    (driverName, driver) <-
      either (reportLookupError driverChoice) return $
        lookupMergeDriver driverChoice config
    return $ ResolvedMergeDriver platform driverName driver
runPostMergeHooks
  :: (MonadFileSystem i, AppEffects i)
  => [OsPath]
  -> MachineState
  -> App i ()
runPostMergeHooks selectedPaths machineState = do
  manifest <- ensureManifest
  refreshedState <-
    if manifest.repositoryId == Just machineState.repositoryId
      then readExistingMachineState manifest
      else return Nothing
  (ctx, currentState) <- case refreshedState of
    Just state
      | sameMergeStateIdentity machineState state -> do
          refreshedContext <- contextFromExistingMachineState manifest state
          return (refreshedContext, state)
    _ ->
      die'
        machineStateError
        "Repository or machine-state identity changed before post-merge hooks."
  hookEnv <-
    guardMergeFinalization machineState $
      makeHookEnv
        "merge"
        (CallerRelativePath <$> selectedPaths)
        ctx
        currentState
  executeHooks hookEnv ctx PostMerge


reportConfigReadError
  :: (AppEffects i)
  => (OsPath -> Text)
  -> OsPath
  -> IOError
  -> App i a
reportConfigReadError pathStyle path err = do
  printStderr' Error $
    "Could not read merge-driver configuration "
      <> pathStyle path
      <> ": "
      <> Text.pack (ioeGetErrorString err)
      <> "."
  when (isDoesNotExistError err) $
    printStderr' Hint $
      "Create the file and configure a default merge driver, "
        <> "or pass --driver-file PATH."
  abortCommand cliError


reportMergeInputObservationError
  :: (AppEffects i)
  => IOError
  -> App i a
reportMergeInputObservationError err = do
  printStderr' Error $
    "Could not read merge inputs while detecting conflicts: "
      <> Text.pack (ioeGetErrorString err)
      <> "."
  abortCommand conflictError


reportMergeInputRefreshError
  :: (AppEffects i)
  => (OsPath -> Text)
  -> OsPath
  -> IOError
  -> App i a
reportMergeInputRefreshError pathStyle source err = do
  printStderr' Error $
    "Could not revalidate merge inputs for "
      <> pathStyle source
      <> ": "
      <> Text.pack (ioeGetErrorString err)
      <> "."
  abortCommand conflictError


reconciliationCandidates
  :: (MonadFileSystem i, AppEffects i)
  => Context (App i)
  -> MachineState
  -> [ManagedCorrespondence]
  -> App i [MergeCandidate]
reconciliationCandidates ctx machineState managed = do
  inputs <-
    ( mapM
        ( \item ->
            observeReconciliationInput
              ctx
              item.route.mode
              item.correspondence
        )
        managed
    )
      `catchError` reportMergeInputObservationError
  markerNames <-
    forM managed $ \item -> do
      identifier <- managedTargetId ctx.repository item
      encodePath $ "pending-" <> Text.unpack identifier
  pendingIndex <-
    findPendingPublications machineState $ Set.fromList markerNames
  let pending =
        [ Map.findWithDefault [] markerName pendingIndex
        | markerName <- markerNames
        ]
  let plan =
        planReconciliation SourceToDestination RefuseConflicts inputs
      pendingFor item =
        maybe
          []
          snd
          ( find
              (sameManagedCorrespondence item . fst)
              (zip managed pending)
          )
      candidate item =
        MergeCandidate item True $ pendingFor item
      publicationCandidate item pendingForItem =
        MergeCandidate item False pendingForItem
      conflicts =
        catMaybes $
          ( \conflict ->
              fmap candidate $
                find
                  ((== conflict.correspondence) . (.correspondence))
                  managed
          )
            <$> plan.conflicts
      recoveries =
        [ candidate item
        | (item, input) <- zip managed inputs
        , isSupportedRecoveryCandidate item input
        ]
      publicationRetries =
        [ publicationCandidate item pendingForItem
        | (item, pendingForItem) <- zip managed pending
        , not $ null pendingForItem
        ]
  return
    $ nubBy
      (\left right -> sameManagedCorrespondence left.managed right.managed)
    $ conflicts ++ recoveries ++ publicationRetries


isSupportedRecoveryCandidate
  :: ManagedCorrespondence -> ReconciliationInput -> Bool
isSupportedRecoveryCandidate managed input =
  managed.route.fileType == FileSystem.File
    && managed.route.kind == CopyRoute
    && managed.route.codec == identityCodecSpec
    && destinationIsActive
    && all isRegular replicas
    && isPartialMergeRecovery input
 where
  destinationIsActive = case input.destinationRouteState of
    Context.Ignored _ _ -> False
    _ -> True
  replicas =
    [ input.correspondence.source
    , input.correspondence.intermediate
    , input.correspondence.destination
    ]
  isRegular :: FileEntry -> Bool
  isRegular entry = case entry.stat of
    Context.File _ -> True
    _ -> False


isPartialMergeRecovery
  :: ReconciliationInput -> Bool
isPartialMergeRecovery input =
  input.sourceDestinationComparison == ReplicasEquivalent
    && (contentCommitIncomplete || modeCommitIncomplete)
 where
  correspondence = input.correspondence
  contentCommitIncomplete =
    correspondence.sourceDelta == Modified
      && correspondence.destinationDelta == Modified
  modeCommitIncomplete =
    correspondence.sourceDelta == Unchanged
      && correspondence.destinationDelta == Unchanged
      && input.declaredDestinationMode /= DefaultMode
      && case posixFileModeBits input.declaredDestinationMode of
        Nothing -> False
        Just bits ->
          let declared = portableModeFromBits bits
          in maybe
               False
               (`satisfiesPortableMode` declared)
               input.observedDestinationMode
               && maybe
                 True
                 (not . (`satisfiesPortableMode` declared))
                 input.observedIntermediateMode


selectCandidates
  :: (MonadFileSystem i, AppEffects i)
  => [OsPath]
  -> [ManagedCorrespondence]
  -> [MergeCandidate]
  -> App i [MergeCandidate]
selectCandidates [] _ candidates = return candidates
selectCandidates paths managed candidates = do
  selected <- mapM makeAbsolute paths
  managedPaths <- mapM absoluteManagedPaths managed
  candidatePaths <-
    forM candidates $ \candidate -> do
      absolute <- absoluteManagedPaths candidate.managed
      return (candidate, absolute)
  pathStyle <- pathStyleFor StandardError
  forM_ (zip paths selected) $ \(shown, absolute) ->
    unless (any (matchesPath absolute) managedPaths) $
      die' fileNotRoutedError $
        "Path " <> pathStyle shown <> " is not tracked by this repository."
  return
    $ nubBy
      (\left right -> sameManagedCorrespondence left.managed right.managed)
    $ [ candidate
      | (candidate, absolute) <- candidatePaths
      , any (`matchesPath` absolute) selected
      ]


absoluteManagedPaths
  :: (MonadFileSystem i, AppEffects i)
  => ManagedCorrespondence
  -> App i (OsPath, OsPath)
absoluteManagedPaths managed = do
  source <- makeAbsolute managed.correspondence.source.path
  destination <- makeAbsolute managed.correspondence.destination.path
  return (source, destination)


sameManagedCorrespondence
  :: ManagedCorrespondence -> ManagedCorrespondence -> Bool
sameManagedCorrespondence left right =
  pathIdentityComponents left.correspondence.source.path
    == pathIdentityComponents right.correspondence.source.path
    && pathIdentityComponents left.correspondence.destination.path
      == pathIdentityComponents right.correspondence.destination.path


matchesPath :: OsPath -> (OsPath, OsPath) -> Bool
matchesPath selected (source, destination) =
  selectedComponents `isPrefixOf` sourceComponents
    || selectedComponents `isPrefixOf` destinationComponents
 where
  selectedComponents = pathIdentityComponents selected
  sourceComponents = pathIdentityComponents source
  destinationComponents = pathIdentityComponents destination


prepareCandidate
  :: (MonadFileSystem i, AppEffects i)
  => (OsPath -> Text)
  -> MergeCandidate
  -> App i (Maybe PreparedMerge)
prepareCandidate pathStyle candidate
  | managed.route.fileType /= FileSystem.File
      || managed.route.kind /= CopyRoute
      || managed.route.codec /= identityCodecSpec =
      rejectOrSkip
        (cannotMerge "unsupported route type, kind, or codec.")
        "The route type, kind, or codec is no longer supported."
  | not
      ( isRegular correspondence.source.stat
          && isRegular correspondence.intermediate.stat
          && isRegular correspondence.destination.stat
      ) =
      rejectOrSkip
        (cannotMerge "unsupported missing or non-regular replica.")
        "A required replica is missing or is not a regular file."
  | otherwise = do
      sourceResult <-
        observeMergeTextInput SourceInput correspondence.source.path
      baseResult <-
        observeMergeTextInput BaseInput correspondence.intermediate.path
      destinationResult <-
        observeMergeTextInput
          DestinationInput
          correspondence.destination.path
      case (sourceResult, baseResult, destinationResult) of
        (Right source, Right base, Right destination) ->
          return $
            ( \action ->
                PreparedMerge
                  managed
                  action
                  candidate.pendingPublications
                  source
                  base
                  destination
            )
              <$> classifyAction managed candidate source base destination
        (Left err, _, _) -> rejectInput err
        (_, Left err, _) -> rejectInput err
        (_, _, Left err) -> rejectInput err
 where
  managed = candidate.managed
  correspondence = managed.correspondence
  isRegular (Context.File _) = True
  isRegular _ = False
  cannotMerge reason =
    "Cannot three-way merge "
      <> pathStyle managed.correspondence.source.path
      <> ": "
      <> reason
  rejectInput err =
    let reason = formatInputError pathStyle err
    in rejectOrSkip reason reason
  rejectOrSkip mergeReason pendingReason
    | candidate.reconciliationCandidate =
        die' conflictError mergeReason
    | otherwise = do
        printStderr' Warning $
          "Cannot retry pending merged-target publication for "
            <> pathStyle managed.correspondence.source.path
            <> ": "
            <> pendingReason
        return Nothing


classifyAction
  :: ManagedCorrespondence
  -> MergeCandidate
  -> MergeTextInput
  -> MergeTextInput
  -> MergeTextInput
  -> Maybe MergeAction
classifyAction managed candidate source base destination =
  case classifyMergeContents
    source.contents
    base.contents
    destination.contents of
    ConflictingMergeContents -> Just RunMergeDriver
    RecoverableMergeContents -> Just RecoverMergeReplicas
    ConvergedMergeContents
      | modeCommitIncomplete -> Just RecoverMergeReplicas
      | not (null candidate.pendingPublications) ->
          Just PublishMergedTarget
      | otherwise -> Nothing
    OneSidedMergeContents -> Nothing
 where
  modeCommitIncomplete =
    managed.route.mode /= DefaultMode
      && case posixFileModeBits managed.route.mode of
        Nothing -> False
        Just bits ->
          let declared = portableModeFromBits bits
              FileModeSnapshot _ baseMode = base.modeSnapshot
              FileModeSnapshot _ destinationMode =
                destination.modeSnapshot
          in not $
               satisfiesPortableMode destinationMode declared
                 && satisfiesPortableMode baseMode declared


processPrepared
  :: forall i
   . (MonadFileSystem i, AppEffects i)
  => (OsPath -> Text)
  -> ( Context (App i)
       -> MachineState
       -> ManagedCorrespondence
       -> App i ()
     )
  -> ( OsPath
       -> MergeTextInput
       -> MergeTextInput
       -> MergeTextInput
       -> App i MergeWorkspace
     )
  -> (ProcessRequest -> App i ProcessResult)
  -> Maybe MergeDriverExecution
  -> MachineState
  -> OsPath
  -> [PreparedMerge]
  -> App i Bool
processPrepared
  pathStyle
  publishTarget
  prepareWorkspace
  runDriver
  driverExecution
  expectedState
  invocationRoot
  prepared =
    and
      <$> forM
        (zip [(1 :: Int) ..] prepared)
        ( \(index, item) -> do
            workspaceName <- encodePath $ "conflict-" <> show index
            let workspaceRoot = invocationRoot </> workspaceName
            workspace <-
              prepareWorkspace
                workspaceRoot
                item.source
                item.base
                item.destination
                `catchError` reportPreparationError pathStyle workspaceRoot
            workspaceIdentity <-
              requireIdentity workspace.root `catchError` \err -> do
                printRetained pathStyle workspace.root
                reportMergeFilesystemError err
            let retainAndReport :: IOError -> App i Bool
                retainAndReport err = do
                  printRetained pathStyle workspace.root
                  reportMergeFilesystemError err
                retainAndAbort :: ExitCode -> App i Bool
                retainAndAbort exitCode = do
                  printRetained pathStyle workspace.root
                  abortCommand exitCode
                runWorkspace :: App i Bool
                runWorkspace = case item.action of
                  RecoverMergeReplicas -> do
                    (refreshedCtx, refreshedState, refreshedManaged) <-
                      refreshMergePolicy pathStyle expectedState item.managed
                    printStderr $
                      "Finishing merged baseline for "
                        <> pathStyle item.managed.correspondence.source.path
                        <> "..."
                    pending <-
                      createPendingPublication
                        workspace
                        workspaceIdentity
                        refreshedCtx
                        refreshedManaged
                    committed <-
                      guardMergeFinalization refreshedState $
                        commitMergeRecoveryGuarded
                          (printCommitStep pathStyle refreshedManaged)
                          refreshedManaged.route.mode
                          item.source
                          item.base
                          item.destination
                    reportCommitResult committed
                    publishTarget
                      refreshedCtx
                      refreshedState
                      refreshedManaged
                    completePublication
                      pathStyle
                      pending
                      item.pendingPublications
                    cleanWorkspace workspace workspaceIdentity
                  PublishMergedTarget -> do
                    (refreshedCtx, refreshedState, refreshedManaged) <-
                      refreshMergePolicy pathStyle expectedState item.managed
                    printStderr $
                      "Publishing merged target for "
                        <> pathStyle item.managed.correspondence.source.path
                        <> "..."
                    pending <-
                      createPendingPublication
                        workspace
                        workspaceIdentity
                        refreshedCtx
                        refreshedManaged
                    publishTarget
                      refreshedCtx
                      refreshedState
                      refreshedManaged
                    completePublication
                      pathStyle
                      pending
                      item.pendingPublications
                    cleanWorkspace workspace workspaceIdentity
                  RunMergeDriver -> do
                    case driverExecution of
                      Nothing ->
                        die' cliError "No merge driver is available."
                      Just execution -> do
                        let driver = execution.resolved
                        request <-
                          workspaceProcessRequest
                            driver.platform
                            execution.hostEnvironment
                            driver.specification
                            workspace
                        printStderr $
                          "Running merge driver '"
                            <> renderMergeDriverName driver.name
                            <> "' for "
                            <> pathStyle item.managed.correspondence.source.path
                            <> "..."
                        processResult <- runDriver request
                        ensureDriverResolved driver.specification processResult
                        resultRead <- readMergeResult workspace.result
                        result <-
                          either
                            ( die' externalProgramNonZeroExit
                                . formatResultError pathStyle
                            )
                            return
                            resultRead
                        (refreshedCtx, refreshedState, refreshedManaged) <-
                          refreshMergePolicy pathStyle expectedState item.managed
                        pending <-
                          createPendingPublication
                            workspace
                            workspaceIdentity
                            refreshedCtx
                            refreshedManaged
                        committed <-
                          guardMergeFinalization refreshedState $
                            commitMergeResultGuarded
                              (printCommitStep pathStyle refreshedManaged)
                              refreshedManaged.route.mode
                              item.source
                              item.base
                              item.destination
                              result
                        reportCommitResult committed
                        publishTarget
                          refreshedCtx
                          refreshedState
                          refreshedManaged
                        completePublication
                          pathStyle
                          pending
                          item.pendingPublications
                        cleanWorkspace workspace workspaceIdentity
            catchCommandExit runWorkspace retainAndAbort
              `catchError` retainAndReport
        )
   where
    reportCommitResult :: Either MergeCommitError () -> App i ()
    reportCommitResult = \case
      Left (MergeInputsChanged roles) ->
        die' conflictError $
          "Merge inputs changed or could no longer be verified before commit: "
            <> Text.intercalate
              ", "
              (formatInputRole <$> toList roles)
            <> "."
      Left MergeRecoveryInputsDiffer ->
        die' conflictError $
          "Source and destination no longer contain the same merged result."
      Right () -> return ()
    cleanWorkspace :: MergeWorkspace -> FileIdentity -> App i Bool
    cleanWorkspace workspace workspaceIdentity = do
      cleaned <-
        removeDirectoryRecursivelyIfIdentity
          workspace.root
          workspaceIdentity
      unless cleaned $
        printStderr' Warning $
          "The completed merge workspace could not be removed: "
            <> pathStyle workspace.root
            <> "."
      return cleaned
refreshMergePolicy
  :: (MonadFileSystem i, AppEffects i)
  => (OsPath -> Text)
  -> MachineState
  -> ManagedCorrespondence
  -> App i (Context (App i), MachineState, ManagedCorrespondence)
refreshMergePolicy pathStyle expectedState expected = do
  manifest <- ensureManifest
  refreshedState <-
    if manifest.repositoryId == Just expectedState.repositoryId
      then readExistingMachineState manifest
      else return Nothing
  machineState <- case refreshedState of
    Just state
      | sameMergeStateIdentity expectedState state -> return state
    _ ->
      die' conflictError $
        "Repository or machine-state identity changed while merging "
          <> pathStyle expected.correspondence.source.path
          <> "."
  ctx <- contextFromExistingMachineState manifest machineState
  (managed, warnings) <-
    (makeManagedCorrespond ctx >>= ensureRouteOwnership)
      `catchError` reportMergeInputRefreshError
        pathStyle
        expected.correspondence.source.path
  printWarnings warnings
  case find (sameMergePolicy expected) managed of
    Just refreshed -> return (ctx, machineState, refreshed)
    Nothing ->
      die' conflictError $
        "Route policy changed while merging "
          <> pathStyle expected.correspondence.source.path
          <> "."


sameMergeStateIdentity :: MachineState -> MachineState -> Bool
sameMergeStateIdentity expected refreshed =
  expected.repositoryId == refreshed.repositoryId
    && expected.machineId == refreshed.machineId
    && expected.generationId == refreshed.generationId


sameMergePolicy
  :: ManagedCorrespondence -> ManagedCorrespondence -> Bool
sameMergePolicy expected refreshed =
  expected.route == refreshed.route
    && samePath expected.relativePath refreshed.relativePath
    && samePath
      expected.correspondence.source.path
      refreshed.correspondence.source.path
    && samePath
      expected.correspondence.intermediate.path
      refreshed.correspondence.intermediate.path
    && samePath
      expected.correspondence.destination.path
      refreshed.correspondence.destination.path
 where
  samePath left right =
    pathIdentityComponents left == pathIdentityComponents right


guardMergeFinalization
  :: (MonadFileSystem i, AppEffects i)
  => MachineState
  -> App i result
  -> App i result
guardMergeFinalization machineState action = do
  root <- asks (.stateDirectory)
  guarded <- withRepositoryStateGeneration root machineState action
  case guarded of
    Left err -> die' machineStateError $ formatStateError err
    Right result -> return result


workspaceProcessRequest
  :: (MonadFileSystem i, AppEffects i)
  => String
  -> [(String, String)]
  -> MergeDriverSpec
  -> MergeWorkspace
  -> App i ProcessRequest
workspaceProcessRequest platform host driver workspace = do
  root <- decodePath workspace.root
  source <- decodePath workspace.source
  base <- decodePath workspace.base
  destination <- decodePath workspace.destination
  result <- decodePath workspace.result
  return $
    makeMergeDriverProcessRequest
      platform
      host
      driver
      root
      source
      base
      destination
      result


-- | Builds a shell-free merge-driver process request.
makeMergeDriverProcessRequest
  :: String
  -- ^ Host platform identifier.
  -> [(String, String)]
  -- ^ Complete host environment.
  -> MergeDriverSpec
  -- ^ Validated merge driver.
  -> FilePath
  -- ^ Isolated working directory.
  -> FilePath
  -- ^ Source input.
  -> FilePath
  -- ^ Common-ancestor input.
  -> FilePath
  -- ^ Destination input.
  -> FilePath
  -- ^ Result initialized from the destination.
  -> ProcessRequest
makeMergeDriverProcessRequest
  platform
  host
  driver
  root
  source
  base
  destination
  result =
    emptyProcessRequest
      { executable = executable
      , arguments = arguments
      , workingDirectory = Just root
      , environment =
          Just $
            resolveMergeDriverEnvironmentNative
              environmentNameCase
              host
              driver
      }
   where
    (executable, arguments) =
      expandMergeDriverCommandNative driver source base destination result
    environmentNameCase :: EnvironmentNameCase
    environmentNameCase =
      if platform == "mingw32"
        then CaseInsensitiveEnvironment
        else CaseSensitiveEnvironment


ensureDriverResolved
  :: (AppEffects i) => MergeDriverSpec -> ProcessResult -> App i ()
ensureDriverResolved driver = \case
  ProcessCompleted exitCode _ _ ->
    case classifyMergeDriverExit driver exitCode of
      MergeDriverResolved -> return ()
      MergeDriverUnresolved ->
        die' conflictError "The merge driver left the conflict unresolved."
      MergeDriverCanceled ->
        die' userCancelledError "The merge driver was canceled."
      MergeDriverFailed code ->
        die' externalProgramNonZeroExit $
          "The merge driver failed with exit code "
            <> Text.pack (show code)
            <> "."
  ProcessStartFailed message ->
    failed "could not be started" message
  ProcessWaitFailed message ->
    failed "could not be awaited" message
  ProcessIOFailed message ->
    failed "encountered an I/O failure" message
  ProcessUnavailable _ ->
    die' externalProgramNonZeroExit "Merge-driver execution is unavailable."
 where
  failed description message =
    die' externalProgramNonZeroExit $
      "The merge driver "
        <> description
        <> ": "
        <> message
        <> "."


findPendingPublications
  :: (MonadFileSystem i, AppEffects i)
  => MachineState
  -> Set.Set OsPath
  -> App i (Map.Map OsPath [PendingPublication])
findPendingPublications machineState markerNames = do
  repositoryRoot <- currentMergeWorkspaceRepositoryRoot machineState
  repositoryEntries <- safeDirectoryEntries repositoryRoot
  case repositoryEntries of
    Nothing -> return Map.empty
    Just (_, invocationNames) -> do
      workspaces <-
        concat
          <$> forM
            invocationNames
            ( \name -> do
                let invocation = repositoryRoot </> name
                workspaceEntries <- safeDirectoryEntries invocation
                return $ case workspaceEntries of
                  Nothing -> []
                  Just (_, workspaceNames) ->
                    (invocation </>) <$> workspaceNames
            )
      markers <-
        concat
          <$> forM
            workspaces
            ( \workspace -> do
                workspaceEntries <- safeDirectoryEntries workspace
                return $ case workspaceEntries of
                  Nothing -> []
                  Just (pathIdentity, entries) ->
                    [ (workspace </> entry, pathIdentity)
                    | entry <- entries
                    , entry `Set.member` markerNames
                    ]
            )
      pending <-
        catMaybes
          <$> forM markers observePendingPublication
      return $
        Map.fromListWith
          (<>)
          [ (takeFileName publication.marker, [publication])
          | publication <- pending
          ]
 where
  safeDirectoryEntries path =
    ( do
        pathIdentity <- captureDirectoryPathIdentity path
        case pathIdentity of
          Nothing -> return Nothing
          Just identity -> do
            entries <- listDirectoryPinned path
            unchanged <- matchesDirectoryPathIdentity identity
            return $
              if unchanged
                then Just (identity, entries)
                else Nothing
    )
      `catchError` const (return Nothing)
  observePendingPublication (marker, pathIdentity) = do
    regular <- isRegularFile marker
    let workspace = takeDirectory marker
    workspaceDirectory <- isDirectory workspace
    workspaceSymlink <- isSymlink workspace
    pathUnchanged <- matchesDirectoryPathIdentity pathIdentity
    if not regular
      || not workspaceDirectory
      || workspaceSymlink
      || not pathUnchanged
      then return Nothing
      else do
        identity <- getFileIdentity workspace
        stillUnchanged <- matchesDirectoryPathIdentity pathIdentity
        return $
          if stillUnchanged
            then
              fmap
                ( \workspaceIdentity ->
                    PendingPublication
                      marker
                      workspace
                      workspaceIdentity
                      pathIdentity
                )
                identity
            else Nothing


createPendingPublication
  :: (MonadFileSystem i, AppEffects i)
  => MergeWorkspace
  -> FileIdentity
  -> Context (App i)
  -> ManagedCorrespondence
  -> App i PendingPublication
createPendingPublication workspace workspaceIdentity ctx managed = do
  identifier <- managedTargetId ctx.repository managed
  markerName <- encodePath $ "pending-" <> Text.unpack identifier
  let marker = workspace.root </> markerName
  workspacePathIdentity <-
    captureDirectoryPathIdentity workspace.root >>= \case
      Just identity -> return identity
      Nothing ->
        throwError $
          userError "merge workspace path changed while publishing its marker."
  created <-
    createEmptyFileInDirectoryIfIdentity
      workspacePathIdentity
      workspaceIdentity
      markerName
  unless created $
    throwError $
      userError "merge workspace path changed while publishing its marker."
  pathUnchanged <- matchesDirectoryPathIdentity workspacePathIdentity
  identityUnchanged <-
    (== Just workspaceIdentity) <$> getFileIdentity workspace.root
  unless (pathUnchanged && identityUnchanged) $
    throwError $
      userError "merge workspace path changed while publishing its marker."
  return $
    PendingPublication
      marker
      workspace.root
      workspaceIdentity
      workspacePathIdentity


completePublication
  :: (MonadFileSystem i, AppEffects i)
  => (OsPath -> Text)
  -> PendingPublication
  -> [PendingPublication]
  -> App i ()
completePublication pathStyle current previous = do
  currentMarkerRemoved <- removePendingMarker current
  if currentMarkerRemoved
    then return ()
    else warnMarkerRetained current
  forM_ previous $ \pending -> do
    markerRemoved <- removePendingMarker pending
    if not markerRemoved
      then warnRetained pending
      else do
        cleanupPathUnchanged <- pathStillMatches pending
        if not cleanupPathUnchanged
          then warnRetained pending
          else do
            cleaned <-
              removeDirectoryRecursivelyIfIdentity
                pending.workspace
                pending.workspaceIdentity
            unless cleaned $ warnRetained pending
            when cleaned $
              removeDirectory (takeDirectory pending.workspace)
                `catchError` const (return ())
 where
  pathStillMatches
    :: (MonadFileSystem i, AppEffects i)
    => PendingPublication
    -> App i Bool
  pathStillMatches pending =
    matchesDirectoryPathIdentity pending.workspacePathIdentity
      `catchError` const (return False)
  warnMarkerRetained
    :: (AppEffects i) => PendingPublication -> App i ()
  warnMarkerRetained pending =
    printStderr' Warning $
      "The pending-publication marker could not be removed safely and "
        <> "will be retried: "
        <> pathStyle pending.marker
        <> "."
  warnRetained :: (AppEffects i) => PendingPublication -> App i ()
  warnRetained pending =
    printStderr' Warning $
      "The completed merge workspace could not be removed: "
        <> pathStyle pending.workspace
        <> "."


removePendingMarker
  :: (MonadFileSystem i, AppEffects i)
  => PendingPublication
  -> App i Bool
removePendingMarker pending =
  removeFileInDirectoryIfIdentity
    pending.workspacePathIdentity
    pending.workspaceIdentity
    (takeFileName pending.marker)


currentMergeWorkspaceRepositoryRoot
  :: (MonadFileSystem i, AppEffects i)
  => MachineState
  -> App i OsPath
currentMergeWorkspaceRepositoryRoot machineState = do
  stateRoot <- asks (.stateDirectory)
  mergeWorkspaceRepositoryRoot stateRoot machineState.repositoryId


createInvocationRoot
  :: (MonadFileSystem i, AppEffects i)
  => MachineState
  -> App i (OsPath, FileIdentity)
createInvocationRoot machineState = do
  repositoryRoot <- currentMergeWorkspaceRepositoryRoot machineState
  createDirectories repositoryRoot
  setPortableMode repositoryRoot 0o700
  identifier <- newUUID
  invocationName <- encodePath $ Text.unpack $ UUID.toText identifier
  let invocationRoot = repositoryRoot </> invocationName
  createPrivateDirectory invocationRoot
  identity <- requireIdentity invocationRoot
  return (invocationRoot, identity)


requireIdentity
  :: (MonadFileSystem i, AppEffects i) => OsPath -> App i FileIdentity
requireIdentity path = do
  identity <- getFileIdentity path
  case identity of
    Just value -> return value
    Nothing -> do
      rendered <- decodePath path
      throwError $
        userError $
          "Could not capture the merge workspace identity: "
            <> rendered
            <> "."


-- | Publishes a managed-target record only while all three replicas still
-- converge and the destination and intermediate replicas still satisfy the
-- route's declared mode.  Lost content or mode convergence aborts publication
-- so the caller can retain its recovery journal.  A converged deletion has no
-- mode to validate.
persistMergedTarget
  :: (MonadFileSystem i, AppEffects i)
  => Context (App i)
  -> MachineState
  -> ManagedCorrespondence
  -> App i ()
persistMergedTarget ctx machineState managed = do
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
          observation <-
            ( do
                contentObservation <-
                  observeConvergedManagedTarget
                    ctx.repository
                    transaction
                    Merged
                    now
                    managed
                case contentObservation of
                  Just (_, Just _) -> do
                    modesConverged <-
                      declaredMergeModesConverged managed
                    return $
                      if modesConverged
                        then contentObservation
                        else Nothing
                  _ -> return contentObservation
            )
              `catchError` \err -> do
                discardTargetSnapshot transaction
                  `catchError` const (return ())
                die' conflictError $
                  "Could not verify merge replicas before target publication: "
                    <> Text.pack (ioeGetErrorString err)
                    <> "."
          case observation of
            Nothing -> do
              discardTargetSnapshot transaction
                `catchError` const (return ())
              die' conflictError $
                "Merge replicas or declared modes changed before target "
                  <> "publication."
            Just converged -> do
              let (updated, superseded) =
                    mergeConvergedTargets existing [converged]
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


declaredMergeModesConverged
  :: (MonadFileSystem i)
  => ManagedCorrespondence
  -> i Bool
declaredMergeModesConverged managed =
  case posixFileModeBits managed.route.mode of
    Nothing -> return True
    Just bits -> do
      destinationMode <-
        getPortableMode managed.correspondence.destination.path
      intermediateMode <-
        getPortableMode managed.correspondence.intermediate.path
      let declared = portableModeFromBits bits
      return $
        satisfiesPortableMode destinationMode declared
          && satisfiesPortableMode intermediateMode declared


reportPreparationError
  :: (AppEffects i)
  => (OsPath -> Text)
  -> OsPath
  -> IOError
  -> App i a
reportPreparationError pathStyle workspace err = do
  printMergeFilesystemError err
  printStderr' Hint $
    "If automatic cleanup was incomplete, inspect the partial merge "
      <> "workspace at "
      <> pathStyle workspace
      <> "."
  abortCommand fileWriteError


reportFinalCleanupError
  :: (AppEffects i)
  => (OsPath -> Text)
  -> OsPath
  -> IOError
  -> App i a
reportFinalCleanupError pathStyle invocationRoot err = do
  printMergeFilesystemError err
  printStderr' Hint $
    "Inspect the merge workspace root for retained completed data at "
      <> pathStyle (takeDirectory invocationRoot)
      <> "."
  abortCommand fileWriteError


reportMergeFilesystemError
  :: (AppEffects i)
  => IOError
  -> App i a
reportMergeFilesystemError err = do
  printMergeFilesystemError err
  abortCommand fileWriteError


printMergeFilesystemError :: (AppEffects i) => IOError -> App i ()
printMergeFilesystemError err =
  printStderr' Error $
    "Could not update merge files: "
      <> Text.pack (ioeGetErrorString err)
      <> "."


printCommitStep
  :: (AppEffects i)
  => (OsPath -> Text)
  -> ManagedCorrespondence
  -> MergeCommitReplica
  -> App i ()
printCommitStep pathStyle managed = \case
  SourceCommitReplica ->
    printStderr $
      "Write merged result to "
        <> pathStyle managed.correspondence.source.path
        <> "..."
  DestinationCommitReplica ->
    printStderr $
      "Write merged result to "
        <> pathStyle managed.correspondence.destination.path
        <> "..."
  IntermediateCommitReplica ->
    printStderr $
      "Record merged baseline at "
        <> pathStyle managed.correspondence.intermediate.path
        <> "..."


formatInputError :: (OsPath -> Text) -> MergeInputError -> Text
formatInputError pathStyle = \case
  MissingMergeInput role path ->
    prefix role path <> " does not exist."
  UnsupportedMergeInput role path ->
    prefix role path <> " is not a regular file."
  UnreadableMergeInput role path ->
    prefix role path <> " could not be read."
  ChangedMergeInput role path ->
    prefix role path <> " changed while it was being read."
  NulMergeInput role path ->
    prefix role path <> " contains a NUL byte and is treated as binary."
  InvalidUtf8MergeInput role path ->
    prefix role path <> " is not valid UTF-8 text."
 where
  prefix role path =
    "The "
      <> formatInputRole role
      <> " merge input "
      <> pathStyle path


formatResultError :: (OsPath -> Text) -> MergeResultError -> Text
formatResultError pathStyle = \case
  MissingMergeResult path ->
    prefix path <> " does not exist."
  UnsupportedMergeResult path ->
    prefix path <> " is not a regular file."
  UnreadableMergeResult path ->
    prefix path <> " could not be read."
  ChangedMergeResult path ->
    prefix path <> " changed while it was being read."
  NulMergeResult path ->
    prefix path <> " contains a NUL byte and is treated as binary."
  InvalidUtf8MergeResult path ->
    prefix path <> " is not valid UTF-8 text."
 where
  prefix path =
    "The merge driver result " <> pathStyle path


formatInputRole :: MergeInputRole -> Text
formatInputRole SourceInput = "source"
formatInputRole BaseInput = "base"
formatInputRole DestinationInput = "destination"


printRetained :: (AppEffects i) => (OsPath -> Text) -> OsPath -> App i ()
printRetained pathStyle workspace =
  printStderr' Hint $
    "Merge workspace retained for recovery: " <> pathStyle workspace <> "."


toList :: (Foldable f) => f a -> [a]
toList = foldr (:) []


-- | Selects the platform-default merge-driver configuration path.
defaultMergeDriverConfigPath
  :: (MonadFileSystem i, AppEffects i) => String -> App i OsPath
defaultMergeDriverConfigPath platform
  | platform == "mingw32" = do
      appData <- lookupEnvironmentVariable "APPDATA"
      base <- case appData of
        Just value -> do
          path <- encodePath value
          if isAbsolute path then return path else defaultWindowsBase
        Nothing -> defaultWindowsBase
      appendConfigPath base
  | platform == "darwin" = do
      home <- getHomeDirectory
      libraryName <- encodePath "Library"
      applicationSupportName <- encodePath "Application Support"
      appendConfigPath $ home </> libraryName </> applicationSupportName
  | otherwise = do
      configured <- lookupEnvironmentVariable "XDG_CONFIG_HOME"
      base <- case configured of
        Just value -> do
          path <- encodePath value
          if isAbsolute path then return path else defaultPosixBase
        Nothing -> defaultPosixBase
      appendConfigPath base
 where
  defaultWindowsBase = do
    home <- getHomeDirectory
    appDataName <- encodePath "AppData"
    roamingName <- encodePath "Roaming"
    return $ home </> appDataName </> roamingName
  defaultPosixBase = do
    home <- getHomeDirectory
    suffix <- encodePath ".config"
    return $ home </> suffix
  appendConfigPath base = do
    directoryName <- encodePath "dojang"
    fileName <- encodePath "merge-drivers.toml"
    return $ base </> directoryName </> fileName
