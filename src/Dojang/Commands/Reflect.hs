{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Dojang.Commands.Reflect (reflect, reflectWithCodecRuntime) where

import Control.Monad (filterM, forM, forM_, unless, void, when)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (isPrefixOf, nub, nubBy, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (Down))
import Data.Set qualified as Set
import Data.Time (getCurrentTime)
import Data.Word (Word32)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hIsTerminalDevice, stderr, stdin)
import Prelude hiding (readFile)

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
  , AppEnv (dryRun, manifestFile, stateDirectory)
  , ensureContext
  , prepareMachineState
  )
import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , die'
  , dieWithErrors
  , ensureRouteOwnership
  , pathStyleFor
  , printModeRestoreFailure
  , printSkippedReconciliation
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
import Dojang.Commands.Status (printUnsupportedModeWarnings, printWarnings)
import Dojang.ExitCodes
  ( ambiguousRouteError
  , codecError
  , conflictError
  , fileNotFoundError
  , fileNotRoutedError
  , ignoredFileError
  , machineStateError
  , sourceCannotBeTargetError
  , userCancelledError
  )
import Dojang.MonadFileSystem (FileType (File), MonadFileSystem (..))
import Dojang.Types.Codec
  ( CodecSpec (..)
  , ReflectPolicy (ReflectIdentity, ReflectReAdd, ReflectReject)
  , identityCodecSpec
  , renderCodecName
  )
import Dojang.Types.Codec.Context
  ( EvaluatedManagedCorrespondence (..)
  , evaluateManagedCorrespondencesWithCache
  , loadCodecCacheEntries
  , managedCodecStateFor
  , rawSourceDigestFor
  , reevaluateManagedCorrespondence
  , reflectManagedCorrespondence
  , renderedSourceFor
  )
import Dojang.Types.Codec.Evaluate
  ( CodecRuntime
  , EvaluatedCodec
  , EvaluationMode (DryRunEvaluation, NormalEvaluation)
  , OpaqueBytes
  , codecReflectPolicy
  , formatCodecError
  , identityCodecRuntime
  , opaqueBytes
  )
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
  , makeCorrespondBetweenThreeFiles
  , makeManagedCorrespond
  , routePaths
  )
import Dojang.Types.Context qualified as Context
import Dojang.Types.MachineState
  ( MachineState (..)
  , formatStateError
  , updateManagedTargetsWith
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (..)
  , SynchronizationCommand (Reflected)
  , destinationPathIdentity
  , hasMaterializedSnapshot
  , mergeConvergedTargets
  , unreachableSnapshots
  )
import Dojang.Types.Reconciliation
  ( ConflictPolicy (..)
  , ContentGuard (ContentGuard)
  , PlannedSyncOp (..)
  , ReconciliationConflict (..)
  , ReconciliationDirection (..)
  , ReconciliationInput (..)
  , ReconciliationItem (..)
  , ReconciliationOutcome (..)
  , ReconciliationPlan (..)
  , Replica (SourceReplica)
  , SyncOp (..)
  , executeReconciliationPlanGuarded
  , observeModeDrift
  , observeReconciliationInputWithRenderedSource
  , planReconciliation
  )
import Dojang.Types.Repository
  ( Repository (..)
  , RouteMapWarning
  , RouteResult (..)
  )
import Dojang.Types.RouteMetadata
  ( RouteKind (CopyRoute)
  , RouteMode (DefaultMode)
  , renderRouteMode
  )
import Dojang.Types.TargetTracking
  ( discardTargetSnapshot
  , newTargetSnapshotTransaction
  , observeConvergedManagedTargetWithRenderedSource
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
  do
    dryRun' <- asks (.dryRun)
    let mode = if dryRun' then DryRunEvaluation else NormalEvaluation
    reflectWithCodecRuntime
      (identityCodecRuntime mode)
      force
      allFlag
      includeUnregistered
      explicitSource
      paths


-- | Reflects selected routes using an explicit codec runtime.
reflectWithCodecRuntime
  :: (MonadFileSystem i, MonadIO i)
  => CodecRuntime (App i)
  -> Bool
  -> Bool
  -> Bool
  -> Maybe OsPath
  -> [OsPath]
  -> App i ExitCode
reflectWithCodecRuntime runtime force allFlag includeUnregistered explicitSource paths =
  withCommandHooks
    "reflect"
    (disambiguatedHookScopePaths explicitSource paths)
    (reflectCore runtime force allFlag includeUnregistered explicitSource paths)


reflectCore
  :: (MonadFileSystem i, MonadIO i)
  => CodecRuntime (App i)
  -> Bool
  -> Bool
  -> Bool
  -> Maybe OsPath
  -> [OsPath]
  -> App i ExitCode
reflectCore runtime force allFlag includeUnregistered _explicitSource [] = do
  -- No arguments: reflect all changed files
  ctx <- ensureContext
  pathStyle <- pathStyleFor stderr
  codeStyle <- codeStyleFor stderr
  (allEvaluated, ws) <- makeCodecAwareEvaluated runtime ctx
  let allManaged = (.managed) <$> allEvaluated
  let allFiles = (.correspondence) <$> allManaged
  -- Metadata-only drift also needs reconciliation, even when contents
  -- converged:
  drifted <- observeModeDrift allManaged
  let driftedFiles =
        [ m.correspondence
        | (m, _) <- drifted
        , not $ isChanged m.correspondence
        ]
  let changedFiles = filter isChanged allFiles ++ driftedFiles
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
  evaluatedIgnored <-
    if null ignoredCorrespondences
      then return []
      else do
        (resolvedRoutes, _) <- routePaths ctx
        let routesByName =
              Map.fromList
                [ (normalise route.routeName, route)
                | route <- resolvedRoutes
                ]
            managedIdentities =
              Set.fromList $ correspondenceIdentity <$> allFiles
            ignoredManaged =
              [ ManagedCorrespondence
                  route
                  (makeRelative route.sourcePath correspondence.source.path)
                  correspondence
              | (ignored, correspondence) <-
                  zip existingIgnored ignoredCorrespondences
              , Just route <-
                  [Map.lookup (normalise ignored.routeName) routesByName]
              , correspondenceIdentity correspondence
                  `Set.notMember` managedIdentities
              ]
        if null ignoredManaged
          then return []
          else evaluateManaged runtime ctx ignoredManaged
  let changedIgnored =
        filter isChanged $ (.managed.correspondence) <$> evaluatedIgnored

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
        unregisteredFiles <- getUnregisteredFiles ctx >>= ensureRouteOwnership
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
      persistConvergedTargets ctx machineState allEvaluated
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
          reflectCorrespondences
            runtime
            ctx
            force
            True
            (allEvaluated <> evaluatedIgnored)
            selectedCorrespondences
          return ExitSuccess
        else do
          printStderr "Cancelled."
          liftIO $ exitWith userCancelledError
reflectCore runtime force allFlag _includeUnregistered explicitSource paths = do
  ctx <- ensureContext
  pathStyle <- pathStyleFor stderr
  absPaths <-
    nubBy
      (\left right -> destinationPathIdentity left == destinationPathIdentity right)
      <$> mapM makeAbsolute paths
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
  (dirFiles, directoryEvaluated) <-
    if null dirPaths
      then return ([], [])
      else do
        (rawManaged, ws) <- makeManagedCorrespond ctx >>= ensureRouteOwnership
        selectedManaged <- filterManagedInDirs dirPaths rawManaged
        evaluated <- evaluateManaged runtime ctx selectedManaged
        let allManaged = (.managed) <$> evaluated
        let allFiles = (.correspondence) <$> allManaged
        printWarnings ws
        drifted <- observeModeDrift allManaged
        let driftedFiles =
              [ m.correspondence
              | (m, _) <- drifted
              , not $ isChanged m.correspondence
              ]
        let changedFiles = filter isChanged allFiles ++ driftedFiles
        -- Filter files within the directories
        files <- filterFilesInDirs dirPaths changedFiles
        return (files, evaluated)
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
  let allCorrespondences =
        nubBy
          ( \left right ->
              correspondenceIdentity left == correspondenceIdentity right
          )
          (dirFiles ++ fileCorrespondences)
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
    runtime
    ctx
    force
    False
    directoryEvaluated
    [(True, correspondence) | correspondence <- allCorrespondences]
  printWarnings $ nub $ concat warningLists
  return ExitSuccess


-- | Check if a file correspondence has changes.
isChanged :: FileCorrespondence -> Bool
isChanged fc = fc.sourceDelta /= Unchanged || fc.destinationDelta /= Unchanged


correspondenceIdentity
  :: FileCorrespondence -> ([Word32], [Word32])
correspondenceIdentity file =
  ( destinationPathIdentity file.source.path
  , destinationPathIdentity file.destination.path
  )


makeCodecAwareEvaluated
  :: (MonadFileSystem i, MonadIO i)
  => CodecRuntime (App i)
  -> Context (App i)
  -> App i ([EvaluatedManagedCorrespondence], [RouteMapWarning])
makeCodecAwareEvaluated runtime ctx = do
  (managed, warnings) <- makeManagedCorrespond ctx >>= ensureRouteOwnership
  evaluated <- evaluateManaged runtime ctx managed
  return (evaluated, warnings)


evaluateManaged
  :: (MonadFileSystem i, MonadIO i)
  => CodecRuntime (App i)
  -> Context (App i)
  -> [ManagedCorrespondence]
  -> App i [EvaluatedManagedCorrespondence]
evaluateManaged runtime ctx managed = do
  machineState <- prepareMachineState ctx.repository.manifest
  cache <- loadCodecCacheEntries ctx machineState managed
  result <-
    evaluateManagedCorrespondencesWithCache runtime ctx cache managed
  case result of
    Left err -> die' codecError $ formatCodecError err
    Right evaluated -> return evaluated


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


filterManagedInDirs
  :: (Monad m)
  => [OsPath]
  -> [ManagedCorrespondence]
  -> m [ManagedCorrespondence]
filterManagedInDirs dirPaths managed = do
  let dirPrefixes = map splitDirectories dirPaths
  return
    [ item
    | item <- managed
    , let dstDirs = splitDirectories item.correspondence.destination.path
    , any (`isPrefixOf` dstDirs) dirPrefixes
    ]


-- | Perform the actual reflect operation on file correspondences.
reflectCorrespondences
  :: forall i
   . (MonadFileSystem i, MonadIO i)
  => CodecRuntime (App i)
  -> Context (App i)
  -> Bool
  -- ^ Force flag.
  -> Bool
  -- ^ Whether every converged route should be persisted.
  -> [EvaluatedManagedCorrespondence]
  -- ^ Evaluations already performed by this command.
  -> [(Bool, FileCorrespondence)]
  -- ^ Selected correspondences and whether an ignored destination was
  -- explicitly admitted by command-level selection.
  -> App i ()
reflectCorrespondences
  runtime
  ctx
  force
  persistAll
  initialEvaluated
  selectedCorrespondences = do
    machineState <- prepareMachineState ctx.repository.manifest
    (initialManaged, _) <- makeManagedCorrespond ctx >>= ensureRouteOwnership
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
    pathStyle <- pathStyleFor stderr
    let attachSelected
          :: FileCorrespondence
          -> ManagedCorrespondence
          -> ManagedCorrespondence
        attachSelected file managed =
          ManagedCorrespondence
            managed.route
            (makeRelative managed.route.sourcePath file.source.path)
            file
        ownerFor :: FileCorrespondence -> Maybe ManagedCorrespondence
        ownerFor file =
          case sortOn
            ( Down
                . length
                . splitDirectories
                . normalise
                . (.route.destinationPath)
            )
            [ m
            | m <- initialManaged
            , -- Compare by native identity so case-variant Windows
            -- destinations still find their owning route:
            fmap
              destinationPathIdentity
              (splitDirectories $ normalise m.route.destinationPath)
              `isPrefixOf` fmap
                destinationPathIdentity
                (splitDirectories $ normalise file.destination.path)
            ] of
            m : _ -> Just m
            [] -> Nothing
        attachRoute
          :: FileCorrespondence
          -> RouteResult
          -> ManagedCorrespondence
        attachRoute file route =
          ManagedCorrespondence
            route
            (makeRelative route.sourcePath file.source.path)
            file
        routeMatchesSelected
          :: FileCorrespondence
          -> RouteResult
          -> Bool
        routeMatchesSelected file route =
          let relative =
                makeRelative
                  (normalise route.destinationPath)
                  (normalise file.destination.path)
              expectedSource = normalise $ route.sourcePath </> relative
          in destinationPathIdentity expectedSource
               == destinationPathIdentity file.source.path
        routeSpecificity :: RouteResult -> Down Int
        routeSpecificity =
          Down . length . splitDirectories . normalise . (.destinationPath)
        routeOwnerFor :: FileCorrespondence -> App i ManagedCorrespondence
        routeOwnerFor file = do
          (routeMatch, _) <- findMatchingRoutes ctx file.destination.path
          let routes = case routeMatch of
                NoMatch -> []
                SingleMatch route -> [route]
                AmbiguousMatch candidates -> (.route) <$> NE.toList candidates
          case sortOn routeSpecificity $ filter (routeMatchesSelected file) routes of
            route : _ -> return $ attachRoute file route
            [] ->
              die'
                fileNotRoutedError
                ("File " <> pathStyle file.destination.path <> " is not routed.")
    selectedOwnedRaw <- forM selectedCorrespondences $ \(allowIgnored, file) -> do
      owner <- case ownerFor file of
        Just managed -> return $ attachSelected file managed
        Nothing -> routeOwnerFor file
      return (allowIgnored, file, owner)
    let
      initialEvaluatedByPath =
        Map.fromList
          [ (correspondenceIdentity item.managed.correspondence, item)
          | item <- initialEvaluated
          ]
      ownersToEvaluate =
        [ owner
        | (_, _, owner) <- selectedOwnedRaw
        , Map.notMember
            (correspondenceIdentity owner.correspondence)
            initialEvaluatedByPath
        ]
    newlyEvaluated <- evaluateManaged runtime ctx ownersToEvaluate
    let evaluatedByPath =
          Map.union
            initialEvaluatedByPath
            ( Map.fromList
                [ (correspondenceIdentity item.managed.correspondence, item)
                | item <- newlyEvaluated
                ]
            )
        selectedOwned =
          [ case Map.lookup
              (correspondenceIdentity owner.correspondence)
              evaluatedByPath of
              Just evaluated ->
                ( allowIgnored
                , evaluated.managed.correspondence
                , evaluated.managed
                , renderedSourceFor evaluated
                , evaluated.codecResult
                )
              Nothing -> (allowIgnored, file, owner, Nothing, Nothing)
          | (allowIgnored, file, owner) <- selectedOwnedRaw
          ]
    -- Reflection still reconciles declared modes toward the destination, so
    -- unenforceable declarations must be surfaced here too:
    printUnsupportedModeWarnings $
      [owner | (_, _, owner, _, _) <- selectedOwned]
    inputs <-
      mapM
        ( \(allowIgnored, file, owner, rendered, _codecSnapshot) ->
            observeSelectedReconciliationInput
              ctx
              allowIgnored
              (Just owner)
              file
              rendered
        )
        selectedOwned
    let policy = if force then PreferAuthoritative else RefuseConflicts
    let initialPlan = planReconciliation DestinationToSource policy inputs
    forM_ selectedOwned $
      \(_allowIgnored, file, managed, _rendered, _codecSnapshot) -> do
        routeName <- pack <$> decodePath managed.route.routeName
        when
          ( managed.route.codec /= identityCodecSpec
              && codecAppliesToEntry managed file
          )
          $ rejectUnsupportedDestination routeName managed file
    let sourceWriters =
          Map.fromListWith
            (<>)
            [ (destinationPathIdentity file.source.path, [file])
            | (_allowIgnored, file, _managed, _rendered, _codecSnapshot) <-
                selectedOwned
            , willWriteSource initialPlan file
            ]
        ambiguousSources = filter ((> 1) . length) $ Map.elems sourceWriters
    unless (null ambiguousSources) $
      dieWithErrors
        conflictError
        [ "Cannot reflect multiple selected destinations to "
            <> pathStyle file.source.path
            <> "."
        | file : _ <- ambiguousSources
        ]
    reflected <-
      catMaybes <$> mapM (prepareReflectedSource initialPlan) selectedOwned
    let reflectedSources =
          Map.fromList
            [ (sourcePath, (source, guard))
            | (_identity, sourcePath, source, guard, _snapshot) <- reflected
            ]
        reflectedCodecSnapshots =
          Map.fromList
            [ (identity, snapshot)
            | (identity, _sourcePath, _source, _guard, Just snapshot) <- reflected
            ]
    let plan =
          initialPlan
            { operations = rewriteReflectedOperations reflectedSources initialPlan.operations
            }
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
        Skipped reason ->
          printSkippedReconciliation
            pathStyle
            c.destination.path
            c.destination.path
            reason
    let persist = do
          (rawRefreshed, _) <- makeManagedCorrespond ctx >>= ensureRouteOwnership
          let selectedRaw =
                if persistAll
                  then rawRefreshed
                  else filter matchesSelection rawRefreshed
              writtenSources =
                Set.fromList
                  [ normalise $ syncOpTargetPath operation.syncOp
                  | operation <- plan.operations
                  , operation.replica == SourceReplica
                  ]
              canReuse :: ManagedCorrespondence -> Bool
              canReuse managed =
                normalise managed.correspondence.source.path
                  `Set.notMember` writtenSources
                  && Map.member
                    (correspondenceIdentity managed.correspondence)
                    evaluatedByPath
              frozenReevaluation
                :: ManagedCorrespondence
                -> Maybe (ManagedCorrespondence, EvaluatedCodec)
              frozenReevaluation managed = do
                let key = correspondenceIdentity managed.correspondence
                snapshot <- case Map.lookup key reflectedCodecSnapshots of
                  Just reflectedSnapshot -> Just reflectedSnapshot
                  Nothing -> do
                    previous <- Map.lookup key evaluatedByPath
                    previous.codecResult
                case managed.correspondence.source.stat of
                  Context.File _
                    | normalise managed.correspondence.source.path
                        `Set.member` writtenSources ->
                        Just (managed, snapshot)
                  _ -> Nothing
              toReevaluate = mapMaybe frozenReevaluation selectedRaw
              reevaluationKeys =
                Set.fromList
                  [ correspondenceIdentity managed.correspondence
                  | (managed, _) <- toReevaluate
                  ]
              toEvaluate =
                [ managed
                | managed <- selectedRaw
                , not $ canReuse managed
                , correspondenceIdentity managed.correspondence
                    `Set.notMember` reevaluationKeys
                ]
          refreshedEvaluated <- evaluateManaged runtime ctx toEvaluate
          frozenEvaluated <- forM toReevaluate $ \(managed, snapshot) -> do
            result <-
              reevaluateManagedCorrespondence runtime ctx managed snapshot
            case result of
              Left err -> die' codecError $ formatCodecError err
              Right evaluated -> return evaluated
          let refreshedByPath =
                Map.fromList
                  [ (correspondenceIdentity item.managed.correspondence, item)
                  | item <- refreshedEvaluated <> frozenEvaluated
                  ]
              selected =
                mapMaybe
                  ( \managed ->
                      let key = correspondenceIdentity managed.correspondence
                      in case Map.lookup key refreshedByPath of
                           Just evaluated -> Just evaluated
                           Nothing -> do
                             evaluated <- Map.lookup key evaluatedByPath
                             return
                               EvaluatedManagedCorrespondence
                                 { managed = managed
                                 , codecResult = evaluated.codecResult
                                 , rawSourceDigest = evaluated.rawSourceDigest
                                 }
                  )
                  selectedRaw
          persistConvergedTargets ctx machineState selected
    void
      ( executeReconciliationPlanGuarded
          (logSyncOp . (.syncOp))
          printModeRestoreFailure
          plan
          `catchError` \err -> persist >> throwError err
      )
    persist
   where
    prepareReflectedSource
      plan
      (_allowIgnored, file, managed, _rendered, codecSnapshot) = do
        routeName <- pack <$> decodePath managed.route.routeName
        if not $ willWriteSource plan file
          then return Nothing
          else do
            policy <- case codecReflectPolicy runtime routeName managed.route.codec of
              Left err -> die' codecError $ formatCodecError err
              Right value -> return value
            case policy of
              ReflectIdentity -> return Nothing
              ReflectReject ->
                die'
                  codecError
                  ( "Route "
                      <> routeName
                      <> " codec "
                      <> renderCodecName managed.route.codec.name
                      <> " rejects reflection."
                  )
              ReflectReAdd -> case file.destination.stat of
                Context.File _ -> do
                  deployed <- opaqueBytes <$> readFile file.destination.path
                  result <-
                    reflectManagedCorrespondence
                      runtime
                      ctx
                      managed
                      codecSnapshot
                      deployed
                  case result of
                    Left err -> die' codecError $ formatCodecError err
                    Right (source, snapshot) ->
                      return $
                        Just
                          (
                            ( destinationPathIdentity file.source.path
                            , destinationPathIdentity file.destination.path
                            )
                          , normalise file.source.path
                          , source
                          , ContentGuard file.destination.path deployed
                          , snapshot
                          )
                Missing -> return Nothing
                Context.Directory ->
                  die'
                    codecError
                    ( "Route "
                        <> routeName
                        <> " codec "
                        <> renderCodecName managed.route.codec.name
                        <> " cannot reflect a directory destination."
                    )
                Context.Symlink _ ->
                  die'
                    codecError
                    ( "Route "
                        <> routeName
                        <> " codec "
                        <> renderCodecName managed.route.codec.name
                        <> " cannot reflect a symbolic-link destination."
                    )
    codecAppliesToEntry
      :: ManagedCorrespondence -> FileCorrespondence -> Bool
    codecAppliesToEntry managed file =
      managed.route.fileType == File || case file.source.stat of
        Context.File _ -> True
        _ -> False
    rejectUnsupportedDestination
      :: Text
      -> ManagedCorrespondence
      -> FileCorrespondence
      -> App i ()
    rejectUnsupportedDestination routeName managed file =
      case file.destination.stat of
        Context.Directory ->
          die'
            codecError
            ( "Route "
                <> routeName
                <> " codec "
                <> renderCodecName managed.route.codec.name
                <> " cannot reflect a directory destination."
            )
        Context.Symlink _ ->
          die'
            codecError
            ( "Route "
                <> routeName
                <> " codec "
                <> renderCodecName managed.route.codec.name
                <> " cannot reflect a symbolic-link destination."
            )
        _ -> return ()
    willWriteSource :: ReconciliationPlan -> FileCorrespondence -> Bool
    willWriteSource plan file =
      any
        ( \operation ->
            operation.replica == SourceReplica
              && normalise (syncOpTargetPath operation.syncOp)
                == normalise file.source.path
        )
        plan.operations
    syncOpTargetPath :: SyncOp -> OsPath
    syncOpTargetPath operation = case operation of
      RemoveDirs path -> path
      RemoveDirsExcept path _ -> path
      RemoveFile path -> path
      RemoveLink path -> path
      CopyFile _ path -> path
      WriteContent _ path -> path
      WriteContentGuarded _ _ path -> path
      CreateDir path -> path
      CreateDirs path -> path
      CreateSymlink _ path _ -> path
      SetEntryMode path _ _ -> path
    rewriteReflectedOperations
      :: Map.Map OsPath (OpaqueBytes, ContentGuard)
      -> [PlannedSyncOp]
      -> [PlannedSyncOp]
    rewriteReflectedOperations reflected = fmap rewrite
     where
      rewrite operation@(PlannedSyncOp SourceReplica (CopyFile _ destination)) =
        case Map.lookup (normalise destination) reflected of
          Just (content, guard) ->
            PlannedSyncOp SourceReplica $
              WriteContentGuarded content guard destination
          Nothing -> operation
      rewrite operation = operation


persistConvergedTargets
  :: (MonadFileSystem i, MonadIO i)
  => Context (App i)
  -> MachineState
  -> [EvaluatedManagedCorrespondence]
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
        Reflected
        now
        (renderedSourceFor evaluated)
        (rawSourceDigestFor evaluated)
        evaluated.managed
    let codecState' = managedCodecStateFor evaluated
    return $ case observation of
      Just (identifier, Just target) ->
        Just (identifier, Just target{codecState = codecState'})
      _ -> observation


-- | Observes a correspondence that the command has already selected.  Route
-- selection and the force flag remain responsible for admitting ignored files.
observeSelectedReconciliationInput
  :: (MonadFileSystem i, MonadIO i)
  => Context (App i)
  -> Bool
  -- ^ Whether command-level selection explicitly admitted an ignored path.
  -> Maybe ManagedCorrespondence
  -- ^ The most-specific managed route owning the destination, if any.
  -- Descendants of a deployment link resolve to the link route, so
  -- reflection can never reach through a deployed link.
  -> FileCorrespondence
  -> Maybe OpaqueBytes
  -- ^ Codec-rendered source bytes used for source/destination comparison.
  -> App i ReconciliationInput
observeSelectedReconciliationInput
  ctx
  allowIgnored
  owner
  correspondence
  renderedSource = do
    input <-
      observeReconciliationInputWithRenderedSource
        ctx
        (maybe DefaultMode (.route.mode) owner)
        correspondence
        renderedSource
    let input' =
          input
            { destinationKind = maybe CopyRoute (.route.kind) owner
            , routeFileType =
                maybe File (.route.fileType) owner
            }
    return $
      if allowIgnored
        then case input'.destinationRouteState of
          Ignored route _ -> input'{destinationRouteState = Routed route}
          _ -> input'
        else input'


logSyncOp :: (MonadFileSystem i, MonadIO i) => SyncOp -> App i ()
logSyncOp (RemoveDirs path) = do
  path' <- decodePath path
  $(logDebug) $ "Remove directory recursively: " <> pack path'
logSyncOp (RemoveDirsExcept path _) = do
  path' <- decodePath path
  $(logDebug) $
    "Remove directory recursively (preserving nested-owned entries): "
      <> pack path'
logSyncOp (RemoveFile path) = do
  path' <- decodePath path
  $(logDebug) $ "Remove file: " <> pack path'
logSyncOp (RemoveLink path) = do
  path' <- decodePath path
  $(logDebug) $ "Remove link: " <> pack path'
logSyncOp (CopyFile source destination) = do
  source' <- decodePath source
  destination' <- decodePath destination
  $(logDebug) $ "Copy file: " <> pack source' <> " -> " <> pack destination'
logSyncOp (WriteContent _ destination) = do
  destination' <- decodePath destination
  $(logDebug) $ "Write rendered content: " <> pack destination'
logSyncOp (WriteContentGuarded _ _ destination) = do
  destination' <- decodePath destination
  $(logDebug) $ "Write rendered content: " <> pack destination'
logSyncOp (CreateDir path) = do
  path' <- decodePath path
  $(logDebug) $ "Create directory: " <> pack path'
logSyncOp (CreateDirs path) = do
  path' <- decodePath path
  $(logDebug) $ "Create directory recursively: " <> pack path'
logSyncOp (CreateSymlink target link _) = do
  target' <- decodePath target
  link' <- decodePath link
  $(logDebug) $
    "Create symbolic link: " <> pack link' <> " -> " <> pack target'
logSyncOp (SetEntryMode path mode _) = do
  path' <- decodePath path
  $(logDebug) $
    "Set mode of " <> pack path' <> " to " <> renderRouteMode mode <> "."


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
