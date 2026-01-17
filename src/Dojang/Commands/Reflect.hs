{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Dojang.Commands.Reflect (reflect) where

import Control.Monad (filterM, forM, forM_, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (isPrefixOf, nub)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)

import Control.Monad.Logger (logDebug, logDebugSH)
import Control.Monad.Reader (asks)
import Data.List.NonEmpty qualified as NE
import Data.Text (pack)
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
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Context
  ( CandidateRoute (..)
  , Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , RouteMatch (..)
  , RouteState (..)
  , findMatchingRoutes
  , getRouteState
  , makeCorrespondBetweenThreeFiles
  )
import Dojang.Types.Repository (Repository (..), RouteResult (..))


reflect
  :: (MonadFileSystem i, MonadIO i)
  => Bool
  -- ^ Force flag.
  -> Maybe OsPath
  -- ^ Explicit source path.
  -> [OsPath]
  -- ^ Target paths.
  -> App i ExitCode
reflect force explicitSource paths = do
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
  codeStyle <- codeStyleFor stderr
  warningLists <- forM absPaths $ \absPath -> do
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
  files <- forM absPaths $ \p -> do
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
  let conflicts = filterConflicts files
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
  printWarnings $ nub $ concat warningLists
  return ExitSuccess


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
