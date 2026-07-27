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
  ) where

import Control.Monad (forM_, unless, when)
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
import System.OsPath (OsPath, isAbsolute, normalise, splitDirectories, (</>))
import Prelude hiding (readFile)

import Dojang.App
  ( App
  , AppEffects
  , AppEnv (dryRun, stateDirectory)
  , catchCommandExit
  , ensureContext
  , prepareMachineState
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
  , machineStateError
  , userCancelledError
  )
import Dojang.MonadFileSystem
  ( FileIdentity
  , MonadFileSystem (..)
  )
import Dojang.MonadFileSystem qualified as FileSystem
import Dojang.Syntax.MergeDriver qualified as MergeDriverSyntax
import Dojang.Types.Codec (identityCodecSpec)
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
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
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (..)
  , SynchronizationCommand (Merged)
  , hasMaterializedSnapshot
  , mergeConvergedTargets
  , unreachableSnapshots
  )
import Dojang.Types.Merge
  ( MergeCommitError (MergeInputsChanged)
  , MergeCommitReplica (..)
  , MergeInputError (..)
  , MergeInputRole (..)
  , MergeResultError (..)
  , MergeTextInput
  , MergeWorkspace (..)
  , commitMergeResultGuarded
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
import Dojang.Types.Reconciliation
  ( ConflictPolicy (RefuseConflicts)
  , ReconciliationConflict (..)
  , ReconciliationDirection (SourceToDestination)
  , ReconciliationPlan (..)
  , observeReconciliationInput
  , planReconciliation
  )
import Dojang.Types.Repository (Repository (..), RouteResult (..))
import Dojang.Types.RepositoryId (repositoryIdText)
import Dojang.Types.RouteMetadata
  ( RouteKind (CopyRoute)
  )
import Dojang.Types.TargetTracking
  ( discardTargetSnapshot
  , newTargetSnapshotTransaction
  , observeConvergedManagedTarget
  )


data PreparedMerge = PreparedMerge
  { managed :: ManagedCorrespondence
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
mergeWithDriverRunner runDriver requestedDriver requestedConfig selectedPaths = do
  pathStyle <- pathStyleFor StandardError
  preHookContext <- ensureContext
  preHookState <- prepareMachineState preHookContext.repository.manifest
  preHookEnv <-
    makeHookEnv
      "merge"
      (CallerRelativePath <$> selectedPaths)
      preHookContext
      preHookState
  executeHooks preHookEnv preHookContext PreMerge
  ctx <- ensureContext
  machineState <- prepareMachineState ctx.repository.manifest
  (allManaged, warnings) <- makeManagedCorrespond ctx >>= ensureRouteOwnership
  printWarnings warnings
  conflicts <- reconciliationConflicts ctx allManaged
  selected <- selectConflicts selectedPaths allManaged conflicts
  if null selected
    then do
      printStderr "No three-way merge conflicts found."
      runPostMergeHooks selectedPaths
      return ExitSuccess
    else do
      prepared <- mapM (prepareConflict pathStyle) selected
      platform <- hostPlatform
      configPath <-
        maybe (defaultMergeDriverConfigPath platform) return requestedConfig
      configResult <-
        MergeDriverSyntax.readMergeDriverConfigFile configPath
          `catchError` reportConfigReadError pathStyle configPath
      config <-
        either (die' cliError . MergeDriverSyntax.formatError) return configResult
      (driverName, driver) <-
        either (reportLookupError requestedDriver) return $
          lookupMergeDriver requestedDriver config
      dryRunEnabled <- asks (.dryRun)
      if dryRunEnabled
        then do
          forM_ prepared $ \item ->
            printStderr $
              "Would merge "
                <> pathStyle item.managed.correspondence.source.path
                <> " with "
                <> pathStyle item.managed.correspondence.destination.path
                <> " using driver '"
                <> renderMergeDriverName driverName
                <> "'."
          runPostMergeHooks selectedPaths
          return ExitSuccess
        else do
          hostEnvironment <- processEnvironment
          (invocationRoot, invocationIdentity) <-
            createInvocationRoot machineState
          processPrepared
            pathStyle
            runDriver
            platform
            hostEnvironment
            driverName
            driver
            ctx
            machineState
            invocationRoot
            prepared
          cleaned <-
            removeDirectoryRecursivelyIfIdentity
              invocationRoot
              invocationIdentity
          unless cleaned $
            printStderr' Warning $
              "The completed merge workspace could not be removed: "
                <> pathStyle invocationRoot
                <> "."
          runPostMergeHooks selectedPaths
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


runPostMergeHooks
  :: (MonadFileSystem i, AppEffects i) => [OsPath] -> App i ()
runPostMergeHooks selectedPaths = do
  ctx <- ensureContext
  machineState <- prepareMachineState ctx.repository.manifest
  hookEnv <-
    makeHookEnv
      "merge"
      (CallerRelativePath <$> selectedPaths)
      ctx
      machineState
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


reconciliationConflicts
  :: (MonadFileSystem i, AppEffects i)
  => Context (App i)
  -> [ManagedCorrespondence]
  -> App i [ManagedCorrespondence]
reconciliationConflicts ctx managed = do
  inputs <-
    mapM
      ( \item ->
          observeReconciliationInput
            ctx
            item.route.mode
            item.correspondence
      )
      managed
  let plan =
        planReconciliation SourceToDestination RefuseConflicts inputs
  return $
    catMaybes $
      ( \conflict ->
          find
            ((== conflict.correspondence) . (.correspondence))
            managed
      )
        <$> plan.conflicts


selectConflicts
  :: (MonadFileSystem i, AppEffects i)
  => [OsPath]
  -> [ManagedCorrespondence]
  -> [ManagedCorrespondence]
  -> App i [ManagedCorrespondence]
selectConflicts [] _ conflicts = return conflicts
selectConflicts paths managed conflicts = do
  selected <- mapM makeAbsolute paths
  pathStyle <- pathStyleFor StandardError
  forM_ (zip paths selected) $ \(shown, absolute) ->
    unless (any (matchesPath absolute) managed) $
      die' fileNotRoutedError $
        "Path " <> pathStyle shown <> " is not tracked by this repository."
  return $
    nubBy sameCorrespondence $
      filter (\item -> any (`matchesPath` item) selected) conflicts
 where
  sameCorrespondence
    :: ManagedCorrespondence -> ManagedCorrespondence -> Bool
  sameCorrespondence left right =
    normalise left.correspondence.source.path
      == normalise right.correspondence.source.path
      && normalise left.correspondence.destination.path
        == normalise right.correspondence.destination.path


matchesPath :: OsPath -> ManagedCorrespondence -> Bool
matchesPath selected managed =
  selectedComponents `isPrefixOf` sourceComponents
    || selectedComponents `isPrefixOf` destinationComponents
 where
  selectedComponents = splitDirectories $ normalise selected
  sourceComponents =
    splitDirectories $ normalise managed.correspondence.source.path
  destinationComponents =
    splitDirectories $ normalise managed.correspondence.destination.path


prepareConflict
  :: (MonadFileSystem i, AppEffects i)
  => (OsPath -> Text)
  -> ManagedCorrespondence
  -> App i PreparedMerge
prepareConflict pathStyle managed = do
  when
    ( managed.route.fileType /= FileSystem.File
        || managed.route.kind /= CopyRoute
        || managed.route.codec /= identityCodecSpec
    )
    $ unsupported "route type, kind, or codec"
  let correspondence = managed.correspondence
  unless
    ( isRegular correspondence.source.stat
        && isRegular correspondence.intermediate.stat
        && isRegular correspondence.destination.stat
    )
    $ unsupported "missing or non-regular replica"
  source <-
    observe SourceInput correspondence.source.path
  base <-
    observe BaseInput correspondence.intermediate.path
  destination <-
    observe DestinationInput correspondence.destination.path
  return $ PreparedMerge managed source base destination
 where
  isRegular (Context.File _) = True
  isRegular _ = False
  observe role path = do
    observed <- observeMergeTextInput role path
    either (die' conflictError . formatInputError pathStyle) return observed
  unsupported reason =
    die' conflictError $
      "Cannot three-way merge "
        <> pathStyle managed.correspondence.source.path
        <> ": unsupported "
        <> reason
        <> "."


processPrepared
  :: forall i
   . (MonadFileSystem i, AppEffects i)
  => (OsPath -> Text)
  -> (ProcessRequest -> App i ProcessResult)
  -> String
  -> [(String, String)]
  -> MergeDriverName
  -> MergeDriverSpec
  -> Context (App i)
  -> MachineState
  -> OsPath
  -> [PreparedMerge]
  -> App i ()
processPrepared
  pathStyle
  runDriver
  platform
  hostEnvironment
  driverName
  driver
  ctx
  machineState
  invocationRoot
  prepared =
    forM_ (zip [(1 :: Int) ..] prepared) $ \(index, item) -> do
      workspaceName <- encodePath $ "conflict-" <> show index
      let workspaceRoot = invocationRoot </> workspaceName
      workspace <-
        prepareMergeWorkspace
          workspaceRoot
          item.source
          item.base
          item.destination
      workspaceIdentity <- requireIdentity workspace.root
      let retainAndRethrow :: IOError -> App i ()
          retainAndRethrow err = do
            printRetained pathStyle workspace.root
            throwError err
          retainAndAbort :: ExitCode -> App i ()
          retainAndAbort exitCode = do
            printRetained pathStyle workspace.root
            abortCommand exitCode
          runWorkspace :: App i ()
          runWorkspace = do
            request <-
              workspaceProcessRequest platform hostEnvironment driver workspace
            printStderr $
              "Running merge driver '"
                <> renderMergeDriverName driverName
                <> "' for "
                <> pathStyle item.managed.correspondence.source.path
                <> "..."
            processResult <- runDriver request
            ensureDriverResolved driver processResult
            resultRead <- readMergeResult workspace.result
            result <-
              either
                (die' externalProgramNonZeroExit . formatResultError pathStyle)
                return
                resultRead
            committed <-
              commitMergeResultGuarded
                (printCommitStep pathStyle item.managed)
                item.managed.route.mode
                item.source
                item.base
                item.destination
                result
            case committed of
              Left (MergeInputsChanged roles) ->
                die' conflictError $
                  "Merge inputs changed before commit: "
                    <> Text.intercalate
                      ", "
                      (formatInputRole <$> toList roles)
                    <> "."
              Right () -> return ()
            persistMergedTarget ctx machineState item.managed
            cleaned <-
              removeDirectoryRecursivelyIfIdentity
                workspace.root
                workspaceIdentity
            unless cleaned $
              printStderr' Warning $
                "The completed merge workspace could not be removed: "
                  <> pathStyle workspace.root
                  <> "."
      catchCommandExit runWorkspace retainAndAbort
        `catchError` retainAndRethrow


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


createInvocationRoot
  :: (MonadFileSystem i, AppEffects i)
  => MachineState
  -> App i (OsPath, FileIdentity)
createInvocationRoot machineState = do
  stateRoot <- asks (.stateDirectory)
  workspaceName <- encodePath "merge-workspaces"
  repositoryName <-
    encodePath $ Text.unpack $ repositoryIdText machineState.repositoryId
  let repositoryRoot = stateRoot </> workspaceName </> repositoryName
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
            observeConvergedManagedTarget
              ctx.repository
              transaction
              Merged
              now
              managed
              `catchError` \err -> do
                discardTargetSnapshot transaction
                  `catchError` const (return ())
                throwError err
          let observations = maybe [] pure observation
              (updated, superseded) =
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
