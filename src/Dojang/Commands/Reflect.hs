{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Dojang.Commands.Reflect (reflect) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (isPrefixOf, nub)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hIsTerminalDevice, stderr, stdin)

import Control.Monad.Logger (logDebug, logDebugSH)
import Control.Monad.Reader (asks)
import Data.List.NonEmpty qualified as NE
import Data.Text (pack)
import FortyTwo.Prompts.Confirm (confirm)
import System.Directory.OsPath (makeAbsolute)
import System.OsPath
  ( OsPath
  , makeRelative
  , normalise
  , splitDirectories
  , takeDirectory
  , (</>)
  )

import Dojang.App (App, AppEnv (manifestFile), ensureContext)
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
import Dojang.Commands.Status (printWarnings)
import Dojang.ExitCodes
  ( ambiguousRouteError
  , fileNotFoundError
  , fileNotRoutedError
  , ignoredFileError
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
  , FileStat (..)
  , IgnoredFile (..)
  , RouteMatch (..)
  , RouteState (..)
  , findMatchingRoutes
  , getIgnoredFiles
  , getRouteState
  , makeCorrespond
  , makeCorrespondBetweenThreeFiles
  )
import Dojang.Types.Repository (Repository (..), RouteResult (..))


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
reflect force allFlag _includeUnregistered _explicitSource [] = do
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

  let allChangedFiles = changedFiles ++ changedIgnored
  if null allChangedFiles
    then do
      printStderr "No changed files to reflect."
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
          reflectCorrespondences force allChangedFiles
          return ExitSuccess
        else do
          printStderr "Cancelled."
          liftIO $ exitWith userCancelledError
reflect force allFlag _includeUnregistered explicitSource paths = do
  ctx <- ensureContext
  nonExistents <- filterM (fmap not . exists) paths
  pathStyle <- pathStyleFor stderr
  unless (null nonExistents) $ do
    dieWithErrors
      fileNotFoundError
      ["No such file: " <> pathStyle p <> "." | p <- nonExistents]
  absPaths <- liftIO $ mapM makeAbsolute paths
  $(logDebugSH) (absPaths :: [OsPath])
  sourcePath' <- liftIO $ makeAbsolute ctx.repository.sourcePath
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
  let conflicts = filterConflicts allCorrespondences
  $(logDebugSH) conflicts
  unless (force || null conflicts) $ do
    printWarnings $ nub $ concat warningLists
    dieWithErrors
      sourceCannotBeTargetError
      [ "Cannot reflect "
          <> pathStyle c.destination.path
          <> ", since "
          <> pathStyle c.source.path
          <> " is also changed."
      | c <- conflicts
      ]
  reflectCorrespondences force allCorrespondences
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
  => Bool
  -- ^ Force flag.
  -> [FileCorrespondence]
  -> App i ()
reflectCorrespondences force files = do
  pathStyle <- pathStyleFor stderr
  let conflicts = filterConflicts files
  unless (force || null conflicts) $ do
    dieWithErrors
      sourceCannotBeTargetError
      [ "Cannot reflect "
          <> pathStyle c.destination.path
          <> ", since "
          <> pathStyle c.source.path
          <> " is also changed."
      | c <- conflicts
      ]
  forM_ files $ \c -> do
    if c.sourceDelta == Unchanged && c.destinationDelta == Unchanged
      then
        printStderr'
          Note
          ( "File "
              <> pathStyle c.destination.path
              <> " is skipped, since it is the same as file "
              <> pathStyle c.source.path
              <> "."
          )
      else do
        printStderr $
          "Reflect "
            <> pathStyle c.destination.path
            <> " to "
            <> pathStyle c.source.path
            <> "..."
        cleanup c.intermediate
        copy c.destination c.intermediate
        cleanup c.source
        copy c.intermediate c.source


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


cleanup :: (MonadFileSystem i, MonadIO i) => FileEntry -> App i ()
cleanup fileEntry = do
  path <- decodePath fileEntry.path
  case fileEntry.stat of
    Missing -> return ()
    Directory -> do
      $(logDebug) $ "Remove directory recursively: " <> pack path
      removeDirectoryRecursively fileEntry.path
    _ -> do
      $(logDebug) $ "Remove file: " <> pack path
      removeFile fileEntry.path


copy :: (MonadFileSystem i, MonadIO i) => FileEntry -> FileEntry -> App i ()
copy from to = do
  from' <- decodePath from.path
  to' <- decodePath to.path
  case from.stat of
    Directory -> do
      $(logDebug) $ "Create directory recursively: " <> pack to'
      createDirectories to.path
    _ -> do
      let parent = takeDirectory to.path
      parent' <- decodePath parent
      $(logDebug) $ "Create directory recursively: " <> pack parent'
      createDirectories parent
      $(logDebug) $ "Copy file: " <> pack from' <> " -> " <> pack to'
      copyFile from.path to.path


filterConflicts
  :: [FileCorrespondence]
  -> [FileCorrespondence]
filterConflicts corresponds = [c | c <- corresponds, c.sourceDelta /= Unchanged]
