{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Dojang.Commands.Reflect (reflect) where

import Control.Monad (filterM, forM, forM_, unless, void, when)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (isPrefixOf, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Time (getCurrentTime)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hIsTerminalDevice, stderr, stdin)

import Control.Monad.Logger (logDebug, logDebugSH)
import Control.Monad.Reader (asks)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text, pack)
import FortyTwo.Prompts.Confirm (confirm)
import FortyTwo.Prompts.Select (select)
import System.OsPath
  ( OsPath
  , makeRelative
  , normalise
  , splitDirectories
  , (</>)
  )

import Dojang.App
  ( App
  , AppEnv (manifestFile, stateDirectory)
  , ensureContext
  , prepareMachineState
  )
import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , die'
  , dieWithErrors
  , pathStyleFor
  , printStderr
  , printStderr'
  )
import Dojang.Commands.Disambiguation
  ( disambiguateRoutes
  , getAutoSelectMode
  )
import Dojang.Commands.Hook
  ( disambiguatedHookScopePaths
  , withCommandHooks
  )
import Dojang.Commands.Status (printWarnings)
import Dojang.ExitCodes
  ( ambiguousRouteError
  , conflictError
  , fileNotFoundError
  , fileNotRoutedError
  , ignoredFileError
  , machineStateError
  , sourceCannotBeTargetError
  , userCancelledError
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Context
  ( CandidateRoute (..)
  , Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (Missing)
  , IgnoredFile (..)
  , ManagedCorrespondence (..)
  , RouteMatch (..)
  , RouteState (..)
  , UnregisteredFile (..)
  , findMatchingRoutes
  , getIgnoredFiles
  , getRouteState
  , getUnregisteredFiles
  , makeCorrespond
  , makeCorrespondBetweenThreeFiles
  , makeManagedCorrespond
  )
import Dojang.Types.MachineState
  ( MachineState (..)
  , formatStateError
  , updateManagedTargetsWith
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (..)
  , SynchronizationCommand (Reflected)
  , destinationPathIdentity
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
  , SyncOp (..)
  , executeReconciliationPlanWith
  , observeReconciliationInput
  , planReconciliation
  )
import Dojang.Types.Repository (Repository (..), RouteResult (..))
import Dojang.Types.TargetTracking
  ( discardTargetSnapshot
  , newTargetSnapshotTransaction
  , observeConvergedManagedTarget
  )


reflect
  :: (MonadFileSystem i, MonadIO i)
  => Bool
  -- ^ Force flag.
  -> Bool
  -- ^ All flag (skip confirmation).
  -> Bool
  -- ^ Include unregistered files flag.
  -> Maybe OsPath
  -- ^ Explicit source path.
  -> [OsPath]
  -- ^ Target paths (may be empty for all changed files).
  -> App i ExitCode
reflect force allFlag includeUnregistered explicitSource paths =
  withCommandHooks
    "reflect"
    (disambiguatedHookScopePaths explicitSource paths)
    (reflectCore force allFlag includeUnregistered explicitSource paths)


reflectCore
  :: (MonadFileSystem i, MonadIO i)
  => Bool
  -> Bool
  -> Bool
  -> Maybe OsPath
  -> [OsPath]
  -> App i ExitCode
reflectCore force allFlag includeUnregistered _explicitSource [] = do
  -- No arguments: reflect all changed files
  ctx <- ensureContext
  pathStyle <- pathStyleFor stderr
  codeStyle <- codeStyleFor stderr
  (allFiles, ws) <- makeCorrespond ctx
  let changedFiles = filter isChanged allFiles
  printWarnings ws

  -- Get ignored files and warn about them
  ignoredFiles <- getIgnoredFiles ctx
  -- Filter to only ignored files that exist in destination
  existingIgnored <- filterM (exists . (.destinationPath)) ignoredFiles
  -- Create correspondences for ignored files if --force is used
  ignoredCorrespondences <-
    if force && not (null existingIgnored)
      then do
        forM existingIgnored $ \ignored -> do
          let interPath =
                ctx.repository.intermediatePath
                  </> ignored.routeName
                  </> makeRelative
                    (normalise ignored.routeName)
                    (normalise ignored.sourcePath)
          makeCorrespondBetweenThreeFiles
            interPath
            ignored.sourcePath
            ignored.destinationPath
      else return []
  let changedIgnored = filter isChanged ignoredCorrespondences

  -- Print warnings for ignored files
  unless (null existingIgnored) $ do
    if force
      then do
        printStderr' Note $
          "Including "
            <> pack (show $ length existingIgnored)
            <> " ignored file(s) due to "
            <> codeStyle "--force"
            <> ":"
        forM_ existingIgnored $ \ignored -> do
          printStderr $
            "  "
              <> pathStyle ignored.destinationPath
              <> " (pattern: "
              <> codeStyle (pack $ show ignored.pattern)
              <> ")"
      else do
        printStderr' Warning $
          "Skipping "
            <> pack (show $ length existingIgnored)
            <> " ignored file(s):"
        forM_ existingIgnored $ \ignored -> do
          printStderr $
            "  "
              <> pathStyle ignored.destinationPath
              <> " (pattern: "
              <> codeStyle (pack $ show ignored.pattern)
              <> ")"
        printStderr' Hint $
          "Use "
            <> codeStyle "--force"
            <> " to include ignored files."

  -- Handle unregistered files if --include-unregistered is used
  unregisteredCorrespondences <-
    if includeUnregistered
      then do
        unregisteredFiles <- getUnregisteredFiles ctx
        if null unregisteredFiles
          then return []
          else do
            printStderr $
              "Found "
                <> pack (show $ length unregisteredFiles)
                <> " unregistered file(s):"
            forM_ unregisteredFiles $ \unreg -> do
              printStderr $ "  " <> pathStyle unreg.filePath
            isTerminal <- liftIO $ hIsTerminalDevice stdin
            if not isTerminal
              then do
                printStderr' Warning $
                  "Cannot prompt for route selection in non-interactive mode."
                printStderr' Hint $
                  "Run interactively to select routes for unregistered files."
                return []
              else do
                -- Prompt for each unregistered file
                correspondences <- forM unregisteredFiles $ \unreg -> do
                  case unreg.candidateRoutes of
                    [] -> do
                      printStderr' Warning $
                        "No candidate routes for "
                          <> pathStyle unreg.filePath
                          <> ". Skipping."
                      return Nothing
                    [route] -> do
                      -- Auto-select single candidate
                      routeName' <- decodePath route.routeName
                      printStderr' Note $
                        "Auto-selecting route "
                          <> codeStyle (pack routeName')
                          <> " for "
                          <> pathStyle unreg.filePath
                      correspond <- createUnregisteredCorrespondence ctx unreg route
                      return $ Just correspond
                    routes -> do
                      -- Prompt user to select route
                      routeNames <- mapM (decodePath . (.routeName)) routes
                      printStderr $
                        "Select route for " <> pathStyle unreg.filePath <> ":"
                      selectedName <-
                        liftIO $
                          select "Route: " routeNames
                      case [r | (r, name) <- zip routes routeNames, name == selectedName] of
                        (selectedRoute : _) -> do
                          correspond <-
                            createUnregisteredCorrespondence ctx unreg selectedRoute
                          return $ Just correspond
                        [] -> do
                          printStderr' Warning "No matching route found. Skipping."
                          return Nothing
                return [c | Just c <- correspondences]
      else return []

  let selectedCorrespondences =
        [(True, file) | file <- changedFiles]
          ++ [(True, file) | file <- changedIgnored]
          ++ [(force, file) | file <- unregisteredCorrespondences]
  let allChangedFiles = fmap snd selectedCorrespondences
  if null allChangedFiles
    then do
      printStderr "No changed files to reflect."
      machineState <- prepareMachineState ctx.repository.manifest
      (managed, _) <- makeManagedCorrespond ctx
      persistConvergedTargets ctx machineState managed
      return ExitSuccess
    else do
      -- Display changed files
      printStderr $
        "Found "
          <> pack (show $ length allChangedFiles)
          <> " changed file(s):"
      forM_ allChangedFiles $ \fc -> do
        printStderr $ "  " <> pathStyle fc.destination.path
      -- Confirm unless --all is specified
      proceed <-
        if allFlag
          then return True
          else do
            isTerminal <- liftIO $ hIsTerminalDevice stdin
            if isTerminal
              then liftIO $ confirm "Reflect all changed files?"
              else return True -- Non-interactive: proceed
      if proceed
        then do
          reflectCorrespondences ctx force True selectedCorrespondences
          return ExitSuccess
        else do
          printStderr "Cancelled."
          liftIO $ exitWith userCancelledError
reflectCore force allFlag _includeUnregistered explicitSource paths = do
  ctx <- ensureContext
  pathStyle <- pathStyleFor stderr
  absPaths <- mapM makeAbsolute paths
  nonExistents <- filterM (fmap not . exists) absPaths
  let rejectUntrackedMissingPath path (correspond :: FileCorrespondence) =
        when
          ( path `elem` nonExistents
              && correspond.intermediate.stat == Missing
          )
          $ die'
            fileNotFoundError
            ("No such file: " <> pathStyle path <> ".")
  $(logDebugSH) (absPaths :: [OsPath])
  sourcePath' <- makeAbsolute ctx.repository.sourcePath
  let sourcePathPrefix = splitDirectories sourcePath'
  $(logDebugSH) sourcePathPrefix
  let overlappedPaths =
        [ p
        | p <- absPaths
        , sourcePathPrefix `isPrefixOf` splitDirectories p
        ]
  $(logDebugSH) overlappedPaths
  unless (null overlappedPaths) $ do
    dieWithErrors
      sourceCannotBeTargetError
      [ "Cannot reflect "
          <> pathStyle p
          <> " because it is a file inside the repository."
      | p <- overlappedPaths
      ]
  -- Separate directories and files
  (dirPaths, filePaths) <- partitionByType absPaths
  -- For directories: get changed files within
  dirFiles <-
    if null dirPaths
      then return []
      else do
        (allFiles, ws) <- makeCorrespond ctx
        printWarnings ws
        let changedFiles = filter isChanged allFiles
        -- Filter files within the directories
        filterFilesInDirs dirPaths changedFiles
  -- For files: process as before
  codeStyle <- codeStyleFor stderr
  warningLists <- forM filePaths $ \absPath -> do
    (state, ws) <- getRouteState ctx absPath
    case state of
      NotRouted -> do
        if absPath `elem` nonExistents
          then
            die'
              fileNotFoundError
              ("No such file: " <> pathStyle absPath <> ".")
          else do
            manifestFile' <- asks (.manifestFile)
            printWarnings ws
            printStderr'
              Error
              ("File " <> pathStyle absPath <> " is not routed.")
            printStderr'
              Hint
              ("Add a route for it in " <> pathStyle manifestFile' <> ".")
            liftIO $ exitWith fileNotRoutedError
      Routed _ -> do
        return ws
      Ignored name pattern -> do
        routeName' <- decodePath name
        if force
          then do
            printStderr' Note $
              "File "
                <> pathStyle absPath
                <> " is ignored due to pattern "
                <> codeStyle (pack $ show pattern)
                <> " (route name: "
                <> codeStyle (pack routeName')
                <> "), but reflect it anyway as you enforced it using "
                <> codeStyle "-f"
                <> "/"
                <> codeStyle "--force"
                <> " option."
            return ws
          else do
            printWarnings ws
            printStderr' Error $
              "File "
                <> pathStyle absPath
                <> " is ignored due to pattern "
                <> codeStyle (pack $ show pattern)
                <> " (route name: "
                <> codeStyle (pack routeName')
                <> ")."
            printStderr'
              Hint
              $ "You can reflect it anyway by enforcing it using "
                <> codeStyle "-f"
                <> "/"
                <> codeStyle "--force"
                <> " option."
            liftIO $ exitWith ignoredFileError
  autoSelectMode <- getAutoSelectMode
  fileCorrespondences <- forM filePaths $ \p -> do
    (routeMatch, ws) <- findMatchingRoutes ctx p
    printWarnings $ nub ws
    case routeMatch of
      NoMatch ->
        die' fileNotRoutedError ("File " <> pathStyle p <> " is not routed.")
      SingleMatch route -> do
        correspond <- makeCorrespondForRoute ctx p route
        rejectUntrackedMissingPath p correspond
        return correspond
      AmbiguousMatch candidates -> do
        maybeRoute <- disambiguateRoutes autoSelectMode explicitSource candidates
        case maybeRoute of
          Nothing -> do
            -- Disambiguation failed (ErrorOnAmbiguity mode)
            let routeNames =
                  [c.route.routeName | c <- NE.toList candidates]
            routeNameStrs <- mapM decodePath routeNames
            printStderr' Error $
              "Ambiguous source path for "
                <> pathStyle p
                <> ". Multiple routes match:"
            forM_ routeNameStrs $ \name ->
              printStderr' Note $ "  - " <> codeStyle (pack name)
            printStderr'
              Hint
              $ "Use "
                <> codeStyle "--source"
                <> " to specify which source path to use, or set "
                <> codeStyle "DOJANG_AUTO_SELECT=first"
                <> " to auto-select."
            liftIO $ exitWith ambiguousRouteError
          Just route -> do
            correspond <- makeCorrespondForRoute ctx p route
            rejectUntrackedMissingPath p correspond
            return correspond
  -- Combine directory files and individual file correspondences
  let allCorrespondences = dirFiles ++ fileCorrespondences
  -- For directory mode with confirmation
  when (not (null dirPaths) && not allFlag && not (null allCorrespondences)) $ do
    printStderr $
      "Found "
        <> pack (show $ length allCorrespondences)
        <> " changed file(s) in specified directories:"
    forM_ allCorrespondences $ \fc -> do
      printStderr $ "  " <> pathStyle fc.destination.path
    isTerminal <- liftIO $ hIsTerminalDevice stdin
    proceed <-
      if isTerminal
        then liftIO $ confirm "Reflect these files?"
        else return True
    unless proceed $ do
      printStderr "Cancelled."
      liftIO $ exitWith userCancelledError
  reflectCorrespondences
    ctx
    force
    False
    [(True, correspondence) | correspondence <- allCorrespondences]
  printWarnings $ nub $ concat warningLists
  return ExitSuccess


-- | Check if a file correspondence has changes.
isChanged :: FileCorrespondence -> Bool
isChanged fc = fc.sourceDelta /= Unchanged || fc.destinationDelta /= Unchanged


-- | Partition paths into directories and files.
partitionByType
  :: (MonadFileSystem m)
  => [OsPath]
  -> m ([OsPath], [OsPath])
partitionByType paths = do
  results <- forM paths $ \p -> do
    isDir <- isDirectory p
    return (p, isDir)
  let dirs = [p | (p, True) <- results]
  let files = [p | (p, False) <- results]
  return (dirs, files)


-- | Filter file correspondences to those within the given directories.
filterFilesInDirs
  :: (Monad m)
  => [OsPath]
  -> [FileCorrespondence]
  -> m [FileCorrespondence]
filterFilesInDirs dirPaths correspondences = do
  let dirPrefixes = map splitDirectories dirPaths
  return
    [ fc
    | fc <- correspondences
    , let dstDirs = splitDirectories fc.destination.path
    , any (`isPrefixOf` dstDirs) dirPrefixes
    ]


-- | Perform the actual reflect operation on file correspondences.
reflectCorrespondences
  :: (MonadFileSystem i, MonadIO i)
  => Context (App i)
  -> Bool
  -- ^ Force flag.
  -> Bool
  -- ^ Whether every converged route should be persisted.
  -> [(Bool, FileCorrespondence)]
  -- ^ Selected correspondences and whether an ignored destination was
  -- explicitly admitted by command-level selection.
  -> App i ()
reflectCorrespondences ctx force persistAll selectedCorrespondences = do
  machineState <- prepareMachineState ctx.repository.manifest
  (initialManaged, _) <- makeManagedCorrespond ctx
  let selectedFiles = snd <$> selectedCorrespondences
  let matchesSelection :: ManagedCorrespondence -> Bool
      matchesSelection managed =
        any
          ( \file ->
              destinationPathIdentity file.source.path
                == destinationPathIdentity managed.correspondence.source.path
                && destinationPathIdentity file.destination.path
                  == destinationPathIdentity managed.correspondence.destination.path
          )
          selectedFiles
  let initialSelectedManaged = filter matchesSelection initialManaged
  pathStyle <- pathStyleFor stderr
  inputs <-
    mapM
      (uncurry $ observeSelectedReconciliationInput ctx)
      selectedCorrespondences
  let policy = if force then PreferAuthoritative else RefuseConflicts
  let plan = planReconciliation DestinationToSource policy inputs
  $(logDebugSH) plan
  unless (force || null plan.conflicts) $ do
    dieWithErrors
      conflictError
      [ "Cannot reflect "
          <> pathStyle c.destination.path
          <> ", since "
          <> pathStyle c.source.path
          <> " is also changed."
      | conflict <- plan.conflicts
      , let c = conflict.correspondence
      ]
  forM_ plan.items $ \item -> do
    let c = item.correspondence
    case item.outcome of
      NoChange ->
        printStderr'
          Note
          ( "File "
              <> pathStyle c.destination.path
              <> " is skipped, since it is the same as file "
              <> pathStyle c.source.path
              <> "."
          )
      WillReconcile ->
        printStderr $
          "Reflect "
            <> pathStyle c.destination.path
            <> " to "
            <> pathStyle c.source.path
            <> "..."
      ConflictDetected -> return ()
      Skipped reason -> printSkippedReconciliation pathStyle c reason
  let persist = do
        (refreshedManaged, _) <- makeManagedCorrespond ctx
        let selectedManaged =
              nub $
                (if persistAll then initialManaged else initialSelectedManaged)
                  <> if persistAll
                    then refreshedManaged
                    else filter matchesSelection refreshedManaged
        persistConvergedTargets ctx machineState selectedManaged
  void
    ( executeReconciliationPlanWith
        (logSyncOp . (.syncOp))
        plan
        `catchError` \err -> persist >> throwError err
    )
  persist


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
                        Reflected
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


-- | Observes a correspondence that the command has already selected.  Route
-- selection and the force flag remain responsible for admitting ignored files.
observeSelectedReconciliationInput
  :: (MonadFileSystem i, MonadIO i)
  => Context (App i)
  -> Bool
  -- ^ Whether command-level selection explicitly admitted an ignored path.
  -> FileCorrespondence
  -> App i ReconciliationInput
observeSelectedReconciliationInput ctx allowIgnored correspondence = do
  input <- observeReconciliationInput ctx correspondence
  return $
    if allowIgnored
      then case input.destinationRouteState of
        Ignored route _ -> input{destinationRouteState = Routed route}
        _ -> input
      else input


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
          <> pathStyle correspondence.destination.path
          <> " because symbolic link synchronization is not supported."


logSyncOp :: (MonadFileSystem i, MonadIO i) => SyncOp -> App i ()
logSyncOp (RemoveDirs path) = do
  path' <- decodePath path
  $(logDebug) $ "Remove directory recursively: " <> pack path'
logSyncOp (RemoveFile path) = do
  path' <- decodePath path
  $(logDebug) $ "Remove file: " <> pack path'
logSyncOp (CopyFile source destination) = do
  source' <- decodePath source
  destination' <- decodePath destination
  $(logDebug) $ "Copy file: " <> pack source' <> " -> " <> pack destination'
logSyncOp (CreateDir path) = do
  path' <- decodePath path
  $(logDebug) $ "Create directory: " <> pack path'
logSyncOp (CreateDirs path) = do
  path' <- decodePath path
  $(logDebug) $ "Create directory recursively: " <> pack path'


-- | Create a 'FileCorrespondence' from a route result and destination path.
makeCorrespondForRoute
  :: (MonadFileSystem m)
  => Context m
  -> OsPath
  -- ^ The destination path.
  -> RouteResult
  -- ^ The selected route.
  -> m FileCorrespondence
makeCorrespondForRoute ctx dstPath route = do
  let normalized = normalise route.destinationPath
  let relPath = makeRelative normalized dstPath
  period <- encodePath "."
  let (interPath, srcPath) =
        if normalized == dstPath || relPath == period
          then
            let interPath' =
                  normalise
                    (ctx.repository.intermediatePath </> route.routeName)
                srcPath' = normalise route.sourcePath
            in (interPath', srcPath')
          else
            let interPath' =
                  normalise $
                    ctx.repository.intermediatePath
                      </> route.routeName
                      </> relPath
                srcPath' = normalise $ route.sourcePath </> relPath
            in (interPath', srcPath')
  makeCorrespondBetweenThreeFiles interPath srcPath dstPath


-- | Create a FileCorrespondence for an unregistered file.
-- The source file doesn't exist yet, so we create a correspondence that
-- will copy from destination to source.
createUnregisteredCorrespondence
  :: forall m
   . (MonadFileSystem m)
  => Context m
  -> UnregisteredFile
  -> RouteResult
  -> m FileCorrespondence
createUnregisteredCorrespondence ctx unreg route = do
  -- Calculate the relative path from the route's destination to the file
  let relPath = makeRelative (normalise route.destinationPath) (normalise unreg.filePath)
  period <- encodePath "."
  let (interPath, srcPath) =
        if relPath == period
          then
            ( normalise (ctx.repository.intermediatePath </> route.routeName)
            , normalise route.sourcePath
            )
          else
            ( normalise $
                ctx.repository.intermediatePath </> route.routeName </> relPath
            , normalise $ route.sourcePath </> relPath
            )
  makeCorrespondBetweenThreeFiles interPath srcPath unreg.filePath
