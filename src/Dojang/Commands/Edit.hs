{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides the @edit@ command which opens source files
-- in the user's editor and applies changes after editing.
module Dojang.Commands.Edit
  ( defaultEditor
  , edit
  , getEditor
  , runEditor
  ) where

import Control.Monad (filterM, foldM, forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (isPrefixOf, nub)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hIsTerminalDevice, stderr, stdin)
import System.Process (spawnProcess, waitForProcess)

import Control.Monad.Logger (logDebugSH)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text, pack)
import FortyTwo.Prompts.Confirm (confirm)
import FortyTwo.Prompts.Select (select)
import System.Directory.OsPath (makeAbsolute)
import System.OsPath (OsPath, makeRelative, normalise, splitDirectories, (</>))
import TextShow (showt)

import Dojang.App (App, ensureContext)
import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , die'
  , dieWithErrors
  , pathStyleFor
  , printStderr
  , printStderr'
  )
import Dojang.Commands.Apply qualified (apply)
import Dojang.Commands.Disambiguation
  ( disambiguateRoutes
  , getAutoSelectMode
  )
import Dojang.Commands.Status (printWarnings)
import Dojang.ExitCodes
  ( ambiguousRouteError
  , externalProgramNonZeroExit
  , fileNotRoutedError
  , sourceCannotBeTargetError
  , userCancelledError
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.MonadFileSystem qualified as FS
import Dojang.Types.Context
  ( CandidateRoute (..)
  , Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , IgnoredFile (..)
  , RouteMatch (..)
  , findCandidateRoutesFor
  , findMatchingRoutes
  , getIgnoredFiles
  , makeCorrespond
  , makeCorrespondBetweenThreeFiles
  )
import Dojang.Types.Repository (Repository (..), RouteResult (..))


-- | The default editor for the platform.
defaultEditor :: String
#ifdef mingw32_HOST_OS
defaultEditor = "notepad"
#else
defaultEditor = "vi"
#endif


-- | Get the editor to use, checking --editor option, then VISUAL, then EDITOR.
-- Returns Nothing if no editor is specified (caller should use defaultEditor).
getEditor :: (MonadIO m) => Maybe String -> m (Maybe String)
getEditor (Just editor) = return $ Just editor
getEditor Nothing = liftIO $ do
  visual <- lookupEnv "VISUAL"
  case visual of
    Just v -> return $ Just v
    Nothing -> lookupEnv "EDITOR"


-- | Run the editor with the given files.
runEditor
  :: (MonadIO m)
  => String
  -- ^ The editor program to run.
  -> [FilePath]
  -- ^ The files to edit.
  -> m ExitCode
runEditor editor files = liftIO $ do
  handle <- spawnProcess editor files
  waitForProcess handle


-- | The edit command: open source files in an editor and apply changes.
edit
  :: (MonadFileSystem i, MonadIO i)
  => Maybe String
  -- ^ The @--editor@ option.
  -> Bool
  -- ^ The @--no-apply@ flag.
  -> Bool
  -- ^ The @--force@ flag.
  -> Bool
  -- ^ The @--sequential@ flag.
  -> Bool
  -- ^ The @--all@ flag (skip confirmation).
  -> Bool
  -- ^ The @--include-unregistered@ flag.
  -> Maybe OsPath
  -- ^ The @--source@ option for disambiguation.
  -> [OsPath]
  -- ^ The target file paths to edit (may be empty for all changed files).
  -> App i ExitCode
edit editorOpt noApply force sequential allFlag _includeUnregistered _explicitSource [] = do
  -- No arguments: edit source files of all changed files
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
      printStderr "No changed files to edit."
      return ExitSuccess
    else do
      -- Display changed files
      printStderr $
        "Found "
          <> pack (show $ length allChangedFiles)
          <> " changed file(s):"
      forM_ allChangedFiles $ \fc -> do
        printStderr $ "  " <> pathStyle fc.source.path
      -- Confirm unless --all is specified
      proceed <-
        if allFlag
          then return True
          else do
            isTerminal <- liftIO $ hIsTerminalDevice stdin
            if isTerminal
              then liftIO $ confirm "Edit all changed source files?"
              else return True -- Non-interactive: proceed
      if proceed
        then do
          let sourceFiles = map (.source.path) allChangedFiles
          runEditorOnFiles editorOpt noApply force sequential sourceFiles
        else do
          printStderr "Cancelled."
          liftIO $ exitWith userCancelledError
edit editorOpt noApply force sequential _allFlag _includeUnregistered explicitSource paths = do
  ctx <- ensureContext
  pathStyle <- pathStyleFor stderr
  codeStyle <- codeStyleFor stderr

  -- Make paths absolute first.
  absPaths <- liftIO $ mapM makeAbsolute paths
  $(logDebugSH) (absPaths :: [OsPath])

  -- Check if any target is inside the source repository (not allowed).
  sourcePath' <- liftIO $ makeAbsolute ctx.repository.sourcePath
  let sourcePathPrefix = splitDirectories sourcePath'
  let overlappedPaths =
        [ p
        | p <- absPaths
        , sourcePathPrefix `isPrefixOf` splitDirectories p
        ]
  $(logDebugSH) overlappedPaths
  unless (null overlappedPaths) $
    dieWithErrors
      sourceCannotBeTargetError
      [ "Cannot edit "
          <> pathStyle p
          <> " because it is a file inside the repository."
      | p <- overlappedPaths
      ]

  -- Separate existing and non-existing files.
  (existingPaths, nonExistentPaths) <- partitionM exists absPaths
  $(logDebugSH) (nonExistentPaths :: [OsPath])

  -- Map each existing target path to its source file.
  autoSelectMode <- getAutoSelectMode
  existingSourceFiles <- forM existingPaths $ \targetPath -> do
    (routeMatch, ws) <- findMatchingRoutes ctx targetPath
    printWarnings $ nub ws
    case routeMatch of
      NoMatch -> do
        printStderr' Error ("File " <> pathStyle targetPath <> " is not routed.")
        printStderr' Hint "Add a route for it in your manifest (dojang.toml)."
        liftIO $ exitWith fileNotRoutedError
      SingleMatch route -> do
        srcPath <- computeSourcePath ctx targetPath route
        return srcPath
      AmbiguousMatch candidates -> do
        maybeRoute <- disambiguateRoutes autoSelectMode explicitSource candidates
        case maybeRoute of
          Nothing -> do
            let routeNames = [c.route.routeName | c <- NE.toList candidates]
            routeNameStrs <- mapM decodePath routeNames
            printStderr' Error $
              "Ambiguous source path for "
                <> pathStyle targetPath
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
            srcPath <- computeSourcePath ctx targetPath route
            return srcPath

  -- Handle non-existent files: find candidate routes and create source files.
  newSourceFiles <- forM nonExistentPaths $ \targetPath -> do
    candidates <- findCandidateRoutesFor ctx targetPath
    case candidates of
      [] -> do
        printStderr' Error ("File " <> pathStyle targetPath <> " is not routed.")
        printStderr' Hint "Add a route for it in your manifest (dojang.toml)."
        liftIO $ exitWith fileNotRoutedError
      [route] -> do
        srcPath <- computeSourcePath ctx targetPath route
        createEmptySourceFile pathStyle srcPath
        return srcPath
      (firstRoute : restRoutes) -> do
        let routes = firstRoute : restRoutes
        -- Multiple routes: prompt for selection.
        isTerminal <- liftIO $ hIsTerminalDevice stdin
        if isTerminal
          then do
            routeLabels <- forM routes $ \route -> do
              routeName' <- decodePath route.routeName
              return routeName'
            printStderr' Note $
              "Multiple routes can create " <> pathStyle targetPath <> ":"
            selectedLabel <-
              liftIO $ select "Select route to use:" routeLabels
            -- Find the route that matches the selected label.
            matchingRoutes <-
              filterM
                ( \r -> do
                    name <- decodePath r.routeName
                    return (name == selectedLabel)
                )
                routes
            let selectedRoute = case matchingRoutes of
                  (match : _) -> match
                  [] -> firstRoute -- Fallback to first if no match.
            srcPath <- computeSourcePath ctx targetPath selectedRoute
            createEmptySourceFile pathStyle srcPath
            return srcPath
          else do
            -- Non-interactive: use first route.
            routeName' <- decodePath firstRoute.routeName
            printStderr' Note $
              "Auto-selecting route "
                <> codeStyle (pack routeName')
                <> " for "
                <> pathStyle targetPath
            srcPath <- computeSourcePath ctx targetPath firstRoute
            createEmptySourceFile pathStyle srcPath
            return srcPath

  let sourceFiles = existingSourceFiles ++ newSourceFiles
  runEditorOnFiles editorOpt noApply force sequential sourceFiles


-- | Check if a file correspondence has changes.
isChanged :: FileCorrespondence -> Bool
isChanged fc = fc.sourceDelta /= Unchanged || fc.destinationDelta /= Unchanged


-- | Run the editor on the given source files.
runEditorOnFiles
  :: (MonadFileSystem i, MonadIO i)
  => Maybe String
  -- ^ The @--editor@ option.
  -> Bool
  -- ^ The @--no-apply@ flag.
  -> Bool
  -- ^ The @--force@ flag.
  -> Bool
  -- ^ The @--sequential@ flag.
  -> [OsPath]
  -- ^ The source file paths to edit.
  -> App i ExitCode
runEditorOnFiles editorOpt noApply force sequential sourceFiles = do
  pathStyle <- pathStyleFor stderr

  -- Get the editor to use.
  maybeEditor <- getEditor editorOpt
  let editor = maybe defaultEditor id maybeEditor

  -- Convert source paths to FilePath for the editor.
  sourceFilePaths <- mapM decodePath sourceFiles

  -- Early return if no files.
  case sourceFiles of
    [] -> return ExitSuccess
    (firstFile : _) -> do
      -- Run the editor.
      printStderr $
        "Opening " <> pathStyle firstFile <> " in " <> pack editor <> "..."
      if sequential
        then do
          -- Sequential mode: edit one file at a time.
          forM_ sourceFilePaths $ \srcPath -> do
            srcOsPath <- encodePath srcPath
            printStderr $ "Editing " <> pathStyle srcOsPath <> "..."
            exitCode <- runEditor editor [srcPath]
            when (exitCode /= ExitSuccess) $ do
              die' externalProgramNonZeroExit $
                pack editor <> " exited with code " <> showt exitCode <> "."
            -- Apply after each file if not disabled.
            unless noApply $ do
              printStderr "Applying changes..."
              _ <- Dojang.Commands.Apply.apply force []
              return ()
        else do
          -- Concurrent mode: open all files at once.
          exitCode <- runEditor editor sourceFilePaths
          when (exitCode /= ExitSuccess) $ do
            die' externalProgramNonZeroExit $
              pack editor <> " exited with code " <> showt exitCode <> "."
          -- Apply changes if not disabled.
          unless noApply $ do
            printStderr "Applying changes..."
            _ <- Dojang.Commands.Apply.apply force []
            return ()
      return ExitSuccess


-- | Compute the source path for a target path given a route.
-- This follows the same pattern as Reflect.hs 'makeCorrespondForRoute'.
computeSourcePath
  :: (MonadFileSystem m)
  => Context m
  -> OsPath
  -- ^ The target (destination) path.
  -> RouteResult
  -- ^ The matching route.
  -> m OsPath
computeSourcePath _ctx targetPath route = do
  period <- encodePath "."
  let normalized = normalise route.destinationPath
  let relPath = makeRelative normalized targetPath
  if normalized == targetPath || relPath == period
    then return $ normalise route.sourcePath
    else return $ normalise $ route.sourcePath </> relPath


-- | Partition a list based on a monadic predicate.
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs = foldM go ([], []) xs
 where
  go (ts, fs) x = do
    b <- p x
    return $ if b then (ts ++ [x], fs) else (ts, fs ++ [x])


-- | Create an empty source file for a new file.
createEmptySourceFile
  :: (MonadFileSystem m, MonadIO m)
  => (OsPath -> Text)
  -- ^ Path style function for display.
  -> OsPath
  -- ^ The source file path to create.
  -> m ()
createEmptySourceFile pathStyle srcPath = do
  srcExists <- exists srcPath
  unless srcExists $ do
    printStderr $ "Creating new source file: " <> pathStyle srcPath
    FS.writeFile srcPath ""
