{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Context
  ( CandidateRoute (..)
  , Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , ManagedCorrespondence (..)
  , IgnoredFile (..)
  , RouteMatch (..)
  , RouteState (..)
  , UnregisteredFile (..)
  , calculateFileDelta
  , calculateSpecificity
  , filterBySpecificity
  , findCandidateRoutesFor
  , findMatchingRoutes
  , getIgnoredFiles
  , getRouteState
  , getUnregisteredFiles
  , listFiles
  , makeCorrespond
  , makeManagedCorrespond
  , makeCorrespondBetweenThreeDirs
  , makeCorrespondBetweenThreeFiles
  , makeCorrespondBetweenTwoDirs
  , makeCorrespondWithDestination
  , observeFileStat
  , projectExpectedState
  , resolveTargetFrom
  , routePaths
  ) where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (MonadIO)
import Data.List (isPrefixOf, sortOn)
import Data.Ord (Down (Down))
import GHC.IO.Exception (IOErrorType (InappropriateType))
import GHC.Stack (HasCallStack)
import System.IO.Error
  ( ioeSetErrorString
  , isPermissionError
  , mkIOError
  )
import Prelude hiding (readFile)

import Control.Monad.Except (MonadError (..))
import Data.Map.Strict
  ( Map
  , elems
  , filter
  , findWithDefault
  , fromList
  , keysSet
  , unionWith
  , (!?)
  )
import System.FilePattern (FilePattern, matchMany)
import System.OsPath
  ( OsPath
  , joinPath
  , makeRelative
  , normalise
  , splitDirectories
  , takeDirectory
  , (</>)
  )
import System.OsPath qualified

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Set (filter, toList, union)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.MonadFileSystem qualified (FileType (..))
import Dojang.Types.Environment (Environment)
import Dojang.Types.FilePathExpression.Expansion (VariableGetter)
import Dojang.Types.FileRoute
  ( RouteKind (SymlinkRoute)
  , RouteMode (DefaultMode)
  )
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.Repository
  ( Repository (..)
  , RouteMapWarning
  , RouteResult (..)
  )
import Dojang.Types.Repository qualified (routePathsWithVariables)
import Dojang.Types.RouteOwnership
  ( ExpectedState (..)
  , OwnershipError
  , ownedExclusions
  , ownerOf
  , selectOwnership
  , verifyResolvedIdentities
  )


-- | The context in which repository operations are performed.
data (MonadFileSystem m) => Context m = Context
  { repository :: Repository
  -- ^ The repository.
  , environment :: Environment
  -- ^ The environment.
  , variableGetter :: VariableGetter m
  -- ^ Shared manifest and inherited variable lookup.
  }


-- | Route the paths in the repository.  This will return a list of expanded
-- paths, along with any warnings that were generated.  Null routes will be
-- ignored.
routePaths
  :: (MonadFileSystem m)
  => Context m
  -- ^ The context in which to perform path routing.
  -> m ([RouteResult], [RouteMapWarning])
  -- ^ The expanded paths, along with any warnings that were generated.
routePaths ctx =
  Dojang.Types.Repository.routePathsWithVariables
    ctx.repository
    ctx.environment
    ctx.variableGetter


-- | The small stat of a file.
data FileStat
  = -- | The file is missing.
    Missing
  | -- | The file is a directory.
    Directory
  | -- | The file is a regular file, and this is its size in bytes.
    File Integer
  | -- | The file is a symlink, and this is the path to the symlink target.
    Symlink OsPath
  deriving (Eq, Ord, Show)


-- | A file/directory in the repository or the destination directory.
data FileEntry = FileEntry
  { path :: OsPath
  -- ^ The path to the file.
  , stat :: FileStat
  -- ^ Whether the file is a file, a directory, or missing.
  }
  deriving (Eq, Ord, Show)


-- | A file correspondence together with the route entry that produced it.
data ManagedCorrespondence = ManagedCorrespondence
  { route :: RouteResult
  -- ^ Currently selected manifest route.
  , relativePath :: OsPath
  -- ^ Entry path relative to the route root.
  , correspondence :: FileCorrespondence
  -- ^ Observed source, intermediate, and destination replicas.
  }
  deriving (Eq, Show)


-- | The kind of change that was made to a file.
data FileDeltaKind = Unchanged | Added | Removed | Modified
  deriving (Eq, Ord, Show)


-- | Resolves a symbolic-link target from the directory containing the
-- link.
resolveTargetFrom :: OsPath -> OsPath -> OsPath
resolveTargetFrom link target
  | System.OsPath.isAbsolute target = normalise target
  | otherwise = normalise $ takeDirectory link </> target


-- | Observes the state of a filesystem entry without following symbolic links.
observeFileStat
  :: (HasCallStack, MonadFileSystem m)
  => OsPath
  -> m FileStat
observeFileStat path = do
  isSymlink' <- isSymlink path
  if isSymlink'
    then observeKnownFileStat Dojang.MonadFileSystem.Symlink path
    else do
      isDirectory' <- isDirectory path
      if isDirectory'
        then observeKnownFileStat Dojang.MonadFileSystem.Directory path
        else do
          isFile' <- isFile path
          if isFile'
            then observeKnownFileStat Dojang.MonadFileSystem.File path
            else return Missing


-- | Converts a known filesystem entry type to its observed state.
observeKnownFileStat
  :: (HasCallStack, MonadFileSystem m)
  => Dojang.MonadFileSystem.FileType
  -> OsPath
  -> m FileStat
observeKnownFileStat Dojang.MonadFileSystem.Directory _ = return Directory
observeKnownFileStat Dojang.MonadFileSystem.File path = File <$> getFileSize path
observeKnownFileStat Dojang.MonadFileSystem.Symlink path =
  Symlink <$> readSymlinkTarget path


-- | Calculates how a current filesystem entry differs from its intermediate
-- state.
calculateFileDelta
  :: (HasCallStack, MonadFileSystem m)
  => FileEntry
  -> FileEntry
  -> m FileDeltaKind
calculateFileDelta (FileEntry _ Missing) (FileEntry _ Missing) =
  return Unchanged
calculateFileDelta (FileEntry _ Missing) (FileEntry _ _) = return Added
calculateFileDelta (FileEntry _ _) (FileEntry _ Missing) = return Removed
calculateFileDelta (FileEntry _ Directory) (FileEntry _ Directory) =
  return Unchanged
calculateFileDelta
  (FileEntry _ (Symlink intermediateTarget))
  (FileEntry _ (Symlink currentTarget)) =
    return $
      if intermediateTarget == currentTarget
        then Unchanged
        else Modified
calculateFileDelta
  intermediateEntry@(FileEntry _ (File intermediateSize))
  currentEntry@(FileEntry _ (File currentSize))
    | intermediateSize /= currentSize = return Modified
    | otherwise = do
        intermediateContents <- readFile intermediateEntry.path
        currentContents <- readFile currentEntry.path
        return $
          if intermediateContents == currentContents
            then Unchanged
            else Modified
calculateFileDelta _ _ = return Modified


-- | A correspondence between a source file, an intermediate file, and
-- a destination file.
data FileCorrespondence = FileCorrespondence
  { source :: FileEntry
  -- ^ The source file.
  , sourceDelta :: FileDeltaKind
  -- ^ The kind of change that was made to the source file.
  , intermediate :: FileEntry
  -- ^ The intermediate file.
  , destination :: FileEntry
  -- ^ The destination file.
  , destinationDelta :: FileDeltaKind
  -- ^ The kind of change that was made to the destination file.
  }
  deriving (Eq, Ord, Show)


-- | Creates a 'FileCorrespondence' from a single destination file.
makeCorrespondWithDestination
  :: forall m
   . (MonadFileSystem m)
  => Context m
  -- ^ The context in which to perform file correspondence.
  -> OsPath
  -- ^ The path to the destination file.
  -> m (Maybe FileCorrespondence, [RouteMapWarning])
  -- ^ The file correspondence, along with a list of warnings that occurred
  -- during path routing (if any).  The file paths in the returned
  -- 'FileCorrespondence' value are absolute, or relative to the current
  -- working directory at least.
makeCorrespondWithDestination ctx dstPath = do
  (routeMatch, warnings) <- findMatchingRoutes ctx dstPath
  case routeMatch of
    NoMatch -> return (Nothing, warnings)
    SingleMatch route -> do
      correspond <- makeCorrespondForRoute route
      return (Just correspond, warnings)
    AmbiguousMatch candidates -> do
      -- For backward compatibility: auto-select by existence if possible,
      -- or return the first route.
      let existingCandidates =
            [c | c <- NE.toList candidates, c.sourceExists]
      route <- case existingCandidates of
        [c] -> return c.route
        _ -> return (NE.head candidates).route
      correspond <- makeCorrespondForRoute route
      return (Just correspond, warnings)
 where
  makeCorrespondForRoute :: RouteResult -> m FileCorrespondence
  makeCorrespondForRoute route = do
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


-- | Projects the expected per-destination route policy for the current
-- environment: which route owns each destination, which destinations are
-- traversal boundaries, and which owned subtrees are nested inside broader
-- routes.  Commands should consume this shared projection instead of
-- reconstructing route policy from matching paths.
projectExpectedState
  :: (MonadFileSystem m)
  => Context m
  -- ^ The context whose routes are projected.
  -> m (Either OwnershipError ExpectedState, [RouteMapWarning])
  -- ^ The expected state, or the ownership error that makes the route
  -- configuration unsafe, along with any routing warnings.
projectExpectedState ctx = do
  (paths, warnings) <- routePaths ctx
  case selectOwnership ctx.repository.sourcePath paths of
    Left err -> return (Left err, warnings)
    Right state -> do
      verified <- verifyResolvedIdentities ctx.repository.sourcePath state
      return (state <$ verified, warnings)


-- | Creates a list of file correspondences between the source files, the
-- intermediate files, and the destination files.  Throws an 'IOError' if any of
-- the files cannot be read.
makeCorrespond
  :: (HasCallStack, MonadFileSystem m)
  => Context m
  -- ^ The context in which to perform file correspondence.
  -> m (Either OwnershipError ([FileCorrespondence], [RouteMapWarning]))
  -- ^ The file correspondences, along with a list of warnings that occurred
  -- during path routing (if any), or the ownership error that makes the
  -- route configuration unsafe.  The file paths in the returned
  -- 'FileCorrespondence' values are absolute, or relative to the current
  -- working directory at least.
makeCorrespond ctx = do
  result <- makeManagedCorrespond ctx
  return $ do
    (managed, warnings) <- result
    return ((.correspondence) <$> managed, warnings)


-- | Makes correspondences while retaining their producing route metadata.
-- Entries owned by routes nested inside a broader route are excluded from
-- the broader route's own correspondences, so every destination entry has
-- exactly one producing route.
makeManagedCorrespond
  :: (HasCallStack, MonadFileSystem m)
  => Context m
  -- ^ The context in which to perform path routing.
  -> m (Either OwnershipError ([ManagedCorrespondence], [RouteMapWarning]))
  -- ^ Managed correspondences and route warnings, or the ownership error
  -- that makes the route configuration unsafe.
makeManagedCorrespond ctx = do
  (ownership, warnings) <- projectExpectedState ctx
  case ownership of
    Left err -> return $ Left err
    Right state -> do
      files <- forM (elems state.owners) $ \expanded -> do
        let interAbsPath = repo.intermediatePath </> expanded.routeName
        case expanded.fileType of
          _ | expanded.kind == SymlinkRoute -> do
            -- A deployment link is a single one-way entry: the destination
            -- is never enumerated (it is a traversal boundary), and no
            -- intermediate snapshot participates.  The source is
            -- absolutized because it becomes the stored link target, which
            -- must stay valid regardless of the working directory.  The
            -- destination delta reports whether the link still projects
            -- the source, so status and selection can see missing or
            -- diverged links.
            absoluteSource <- makeAbsolute expanded.sourcePath
            sourceStat <- observeFileStat absoluteSource
            destinationStat <- observeFileStat expanded.destinationPath
            let converged = case destinationStat of
                  Symlink target ->
                    resolveTargetFrom expanded.destinationPath target
                      == absoluteSource
                  _ -> False
            return
              [ ManagedCorrespondence
                  expanded
                  mempty
                  FileCorrespondence
                    { source = FileEntry absoluteSource sourceStat
                    , sourceDelta = Unchanged
                    , intermediate = FileEntry interAbsPath Missing
                    , destination =
                        FileEntry expanded.destinationPath destinationStat
                    , destinationDelta =
                        if converged then Unchanged else Modified
                    }
              ]
          Dojang.MonadFileSystem.Directory -> do
            fs <-
              makeCorrespondBetweenThreeDirs
                interAbsPath
                expanded.sourcePath
                expanded.destinationPath
                (findWithDefault [] (normalise expanded.routeName) ignorePatterns)
                (ownedExclusions state expanded.destinationPath)
            -- A declared directory mode applies to the route's destination
            -- root itself, not only to the entries inside it, so the root
            -- needs its own correspondence.  Routes without a declared
            -- mode keep their previous entry-only enumeration, and a
            -- missing source directory stays a no-op rather than becoming
            -- a removal:
            sourceRootStat <- observeFileStat expanded.sourcePath
            rootEntries <-
              if expanded.mode == DefaultMode
                || sourceRootStat /= Directory
                then return []
                else do
                  rootCorrespondence <-
                    makeCorrespondBetweenThreeFiles
                      interAbsPath
                      expanded.sourcePath
                      expanded.destinationPath
                  return [ManagedCorrespondence expanded mempty rootCorrespondence]
            return $
              rootEntries
                ++ [ ManagedCorrespondence
                       expanded
                       correspond.source.path
                       correspond
                         { source =
                             correspond.source
                               { path = expanded.sourcePath </> correspond.source.path
                               }
                         , intermediate =
                             correspond.intermediate
                               { path = interAbsPath </> correspond.intermediate.path
                               }
                         , destination =
                             correspond.destination
                               { path =
                                   expanded.destinationPath
                                     </> correspond.destination.path
                               }
                         }
                   | correspond <- fs
                   ]
          _ -> do
            f <-
              makeCorrespondBetweenThreeFiles
                interAbsPath
                expanded.sourcePath
                expanded.destinationPath
            return [ManagedCorrespondence expanded mempty f]
      return $ Right (concat files, warnings)
 where
  repo :: Repository
  repo = ctx.repository
  ignorePatterns :: Map OsPath [FilePattern]
  ignorePatterns = repo.manifest.ignorePatterns


makeCorrespondBetweenThreeFiles
  :: forall m
   . (HasCallStack, MonadFileSystem m)
  => OsPath
  -> OsPath
  -> OsPath
  -> m FileCorrespondence
makeCorrespondBetweenThreeFiles intermediatePath srcPath dstPath = do
  interStat <- observeFileStat intermediatePath
  let interEntry = FileEntry intermediatePath interStat
  srcStat <- observeFileStat srcPath
  let srcEntry = FileEntry srcPath srcStat
  srcDelta <- calculateFileDelta interEntry srcEntry
  dstStat <- observeFileStat dstPath
  let dstEntry = FileEntry dstPath dstStat
  dstDelta <- calculateFileDelta interEntry dstEntry
  return
    FileCorrespondence
      { source = srcEntry
      , sourceDelta = srcDelta
      , intermediate = interEntry
      , destination = dstEntry
      , destinationDelta = dstDelta
      }


makeCorrespondBetweenThreeDirs
  :: forall m
   . (HasCallStack, MonadFileSystem m)
  => OsPath
  -> OsPath
  -> OsPath
  -> [FilePattern]
  -> [OsPath]
  -- ^ Relative subtree roots owned by nested routes; entries at or under
  -- these roots are excluded from the correspondence.
  -> m [FileCorrespondence]
makeCorrespondBetweenThreeDirs intermediatePath srcPath dstPath ignores exclusions = do
  srcEntries <- makeCorrespondBetweenTwoDirs intermediatePath srcPath []
  dstEntries <-
    makeCorrespondBetweenTwoDirs intermediatePath dstPath ignores
  let paths =
        Data.Set.filter (not . excluded) $
          keysSet srcEntries `union` keysSet dstEntries
  forM (Data.Set.toList paths) $ \path -> do
    let missing = FileEntry path Missing
    (interEntry, srcEntry, srcDelta) <- case srcEntries !? path of
      Nothing -> return (missing, missing, Unchanged)
      Just (interEntry', srcEntry') -> do
        delta <- getDelta path srcPath interEntry' srcEntry'
        return (interEntry', srcEntry', delta)
    (interEntry2, dstEntry, dstDelta) <- case dstEntries !? path of
      Nothing -> return (missing, missing, Unchanged)
      Just (interEntry', dstEntry') -> do
        delta <- getDelta path dstPath interEntry' dstEntry'
        return (interEntry', dstEntry', delta)
    (dstEntry', dstDelta') <-
      if null ignores || dstEntry.stat /= Missing
        then return (dstEntry, dstDelta)
        else do
          actualDstStat <- observeFileStat (dstPath </> path)
          let actualDstEntry = FileEntry path actualDstStat
          actualDelta <- getDelta path dstPath interEntry2 actualDstEntry
          return (dstEntry{stat = actualDstStat}, actualDelta)
    return $
      FileCorrespondence
        { source = srcEntry
        , sourceDelta = srcDelta
        , intermediate =
            if interEntry.stat == Missing
              then interEntry2
              else interEntry
        , destination = dstEntry'
        , destinationDelta = dstDelta'
        }
 where
  excluded :: OsPath -> Bool
  excluded path =
    any
      (\root -> splitDirectories root `isPrefixOf` splitDirectories path)
      exclusions
  getDelta
    :: OsPath
    -- \^ The relative path to the file.
    -> OsPath
    -- \^ The path to the target directory.
    -> FileEntry
    -- \^ An intermediate file entry.
    -> FileEntry
    -- \^ A target (source or destination) file entry.
    -> m FileDeltaKind
  getDelta path targetPath intermediateEntry targetEntry =
    calculateFileDelta
      intermediateEntry{path = intermediatePath </> path}
      targetEntry{path = targetPath </> path}


makeCorrespondBetweenTwoDirs
  :: forall m
   . (HasCallStack, MonadFileSystem m)
  => OsPath
  -> OsPath
  -> [FilePattern]
  -> m (Map OsPath (FileEntry, FileEntry))
makeCorrespondBetweenTwoDirs intermediatePath targetPath ignorePatterns = do
  intermediateDirExists <- exists intermediatePath
  intermediateFiles <-
    if intermediateDirExists
      then listFiles intermediatePath []
      else return []
  targetFiles <- listFiles targetPath ignorePatterns
  let intermediateEntries =
        fromList
          [(p, (e, e{stat = Missing})) | e@(FileEntry p _) <- intermediateFiles]
      targetEntries =
        fromList
          [(p, (e{stat = Missing}, e)) | e@(FileEntry p _) <- targetFiles]
      allEntries = unionWith combinePairs intermediateEntries targetEntries
  return $ Data.Map.Strict.filter eitherExists allEntries
 where
  combinePairs
    :: (FileEntry, FileEntry)
    -> (FileEntry, FileEntry)
    -> (FileEntry, FileEntry)
  combinePairs (a1, b1) (a2, b2)
    | isNotMissing a1 && isNotMissing b2 = (a1, b2)
    | isNotMissing a2 && isNotMissing b1 = (a2, b1)
    | isMissing a1 && isNotMissing b2 = (a2, b2)
    | otherwise = (a1, b1)
   where
    isMissing :: FileEntry -> Bool
    isMissing (FileEntry _ Missing) = True
    isMissing _ = False
    isNotMissing :: FileEntry -> Bool
    isNotMissing = not . isMissing
  eitherExists :: (FileEntry, FileEntry) -> Bool
  eitherExists (FileEntry _ Missing, FileEntry _ Missing) = False
  eitherExists _ = True


-- | Lists all 'FilEntry' values in the given directory.  Throws an 'IOError' if
-- the directory cannot be read.
listFiles
  :: (MonadFileSystem m)
  => OsPath
  -- ^ The path to the directory to list entries in.
  -> [FilePattern]
  -- ^ The file patterns to ignore.  If a directory matches one of these
  -- patterns, then its contents will not be listed either.
  -> m [FileEntry]
  -- ^ The list of 'FileEntry' values in the directory.
listFiles path ignorePatterns = do
  isSymlink' <- isSymlink path
  isFile' <- isFile path
  when (isSymlink' || isFile') $ do
    path' <- decodePath path
    throwError $
      mkIOError InappropriateType "listFiles" Nothing (Just path')
        `ioeSetErrorString` "not a directory"
  exists' <- exists path
  if exists'
    then do
      entries <-
        listDirectoryRecursively path ignorePatterns `catchError` \e ->
          if isPermissionError e
            then return []
            else throwError e
      forM entries $ \(fileType, entryPath) -> do
        stat <- observeKnownFileStat fileType $ path </> entryPath
        return $ FileEntry entryPath stat
    else return []


-- | Represents the route state of a (potential) destination file
-- in the repository.
data RouteState
  = -- | The file is routed.  The 'OsPath' is the name of the relevant route
    -- name.
    Routed OsPath
  | -- | The file is routed but ignored.  The 'OsPath' is the name of
    -- the relevant route name, and the 'FilePattern' is the ignore pattern
    -- that matched the file.
    Ignored OsPath FilePattern
  | -- | The file is not routed at all.
    NotRouted
  deriving (Eq, Ord, Show)


-- | Represents a file that is ignored by a route's ignore patterns.
data IgnoredFile = IgnoredFile
  { routeName :: OsPath
  -- ^ The name of the route that contains the ignore pattern.
  , sourcePath :: OsPath
  -- ^ The path to the source file (in the repository).
  , destinationPath :: OsPath
  -- ^ The path to the destination file.
  , pattern :: FilePattern
  -- ^ The ignore pattern that matched the file.
  }
  deriving (Eq, Show)


-- | A candidate route with computed metadata for disambiguation.
data CandidateRoute = CandidateRoute
  { route :: RouteResult
  -- ^ The route result.
  , specificity :: Int
  -- ^ The specificity of the route, i.e., the number of path components
  -- from the route's destination to the target path.  Lower is more specific.
  , sourceFilePath :: OsPath
  -- ^ The computed source file path (combining route source path with relative
  -- path from destination to target).
  , sourceExists :: Bool
  -- ^ Whether the source file for this route already exists.
  }
  deriving (Eq, Show)


-- | Result of finding routes for a destination path.
data RouteMatch
  = -- | No matching routes found.
    NoMatch
  | -- | A single unambiguous route was found.
    SingleMatch RouteResult
  | -- | Multiple routes match with the same specificity.
    AmbiguousMatch (NonEmpty CandidateRoute)
  deriving (Eq, Show)


-- | Calculate the specificity of a route relative to a target path.
-- Returns the number of path components between the route's destination
-- and the target path.  Lower values indicate more specific routes.
--
-- ==== Examples
--
-- >>> calculateSpecificity "/foo/bar/baz" route  -- route.destinationPath = "/foo"
-- 2  -- bar/baz
--
-- >>> calculateSpecificity "/foo" route  -- route.destinationPath = "/foo"
-- 0  -- exact match
calculateSpecificity
  :: OsPath
  -- ^ The target path.
  -> RouteResult
  -- ^ The route result.
  -> Int
  -- ^ The specificity (number of path components from route destination to
  -- target).
calculateSpecificity targetPath route =
  let dstDirs = splitDirectories $ normalise route.destinationPath
      targetDirs = splitDirectories $ normalise targetPath
  in length targetDirs - length dstDirs


-- | Filter routes by specificity, keeping only those with the lowest
-- (most specific) specificity value.
filterBySpecificity
  :: OsPath
  -- ^ The target path.
  -> [RouteResult]
  -- ^ The list of routes to filter.
  -> [RouteResult]
  -- ^ The routes with the lowest specificity.
filterBySpecificity _ [] = []
filterBySpecificity targetPath routes =
  let specificityCounts = [(r, calculateSpecificity targetPath r) | r <- routes]
      minSpec = minimum $ map snd specificityCounts
  in [r | (r, s) <- specificityCounts, s == minSpec]


-- | Find matching routes for a destination path.
-- Returns a 'RouteMatch' indicating whether no routes, a single route,
-- or multiple ambiguous routes match.
findMatchingRoutes
  :: (MonadFileSystem m)
  => Context m
  -- ^ The context in which to perform route matching.
  -> OsPath
  -- ^ The target path.
  -> m (RouteMatch, [RouteMapWarning])
  -- ^ The route match result, along with any warnings that were generated.
findMatchingRoutes ctx targetPath = do
  (routes, warnings) <- routePaths ctx
  let targetDirs = splitDirectories $ normalise targetPath
  let matching = [r | r <- routes, startsWithRoute r targetDirs]
  case matching of
    [] -> return (NoMatch, warnings)
    [r] -> return (SingleMatch r, warnings)
    rs -> do
      let filtered = filterBySpecificity targetPath rs
      case filtered of
        [] -> return (NoMatch, warnings)
        [r] -> return (SingleMatch r, warnings)
        (r : rs') -> do
          candidates <- mapM (makeCandidateRoute targetPath) (r :| rs')
          return (AmbiguousMatch candidates, warnings)
 where
  startsWithRoute :: RouteResult -> [OsPath] -> Bool
  startsWithRoute route targetDirs' =
    splitDirectories (normalise route.destinationPath) `isPrefixOf` targetDirs'
  makeCandidateRoute
    :: (MonadFileSystem m) => OsPath -> RouteResult -> m CandidateRoute
  makeCandidateRoute target route = do
    let spec = calculateSpecificity target route
    let relPath = makeRelative (normalise route.destinationPath) (normalise target)
    period <- encodePath "."
    let srcPath =
          if relPath == period
            then normalise route.sourcePath
            else normalise $ route.sourcePath </> relPath
    srcExists <- exists srcPath
    return $ CandidateRoute route spec srcPath srcExists


-- | Gets the route state of the given path (which is a potential destination).
getRouteState
  :: (HasCallStack, MonadFileSystem m, MonadIO m)
  => Context m
  -- ^ The context in which to perform path routing.
  -> OsPath
  -- ^ The path (which is a potential destination) to get the route state of.
  -> m (RouteState, [RouteMapWarning])
  -- ^ The route state of the path, along with any warnings that were generated.
getRouteState ctx path = do
  absPath <- makeAbsolute path
  let dirs = splitDirectories absPath
  (routes, ws) <- routePaths ctx
  matches <- forM routes $ \route -> do
    dstPath <- makeAbsolute route.destinationPath
    let prefix = splitDirectories dstPath
    return $
      if prefix `isPrefixOf` dirs
        then Just (length prefix, route, drop (length prefix) dirs)
        else Nothing
  -- The most-specific containing route owns the path; broader routes must
  -- not override its routing or ignore rules:
  case sortOn (\(specificity', _, _) -> Down specificity') [m | Just m <- matches] of
    [] -> return (NotRouted, ws)
    (_, route, relDirs) : _ -> do
      let ignores =
            findWithDefault
              []
              (normalise route.routeName)
              ignorePatterns
      relPath <- decodePath $ joinPath relDirs
      case matchMany [(i, i) | i <- ignores] [((), relPath)] of
        (pattern, _, _) : _ -> return (Ignored route.routeName pattern, ws)
        _ -> return (Routed route.routeName, ws)
 where
  ignorePatterns :: Map OsPath [FilePattern]
  ignorePatterns = ctx.repository.manifest.ignorePatterns


-- | Gets a list of files that are ignored by the repository's ignore patterns.
-- This function scans all directory routes for files that would be excluded
-- by their ignore patterns.
getIgnoredFiles
  :: forall m
   . (HasCallStack, MonadFileSystem m)
  => Context m
  -- ^ The context in which to perform the operation.
  -> m [IgnoredFile]
  -- ^ The list of ignored files.
getIgnoredFiles ctx = do
  (routes, _) <- routePaths ctx
  let ownership = selectOwnership ctx.repository.sourcePath routes
  let exclusionsFor :: RouteResult -> [OsPath]
      exclusionsFor route =
        case ownership of
          Right state -> ownedExclusions state route.destinationPath
          Left _ -> []
  ignoredLists <- forM routes $ \route ->
    case route.fileType of
      -- A deployment link is a traversal boundary; its destination is the
      -- link itself, never a directory to enumerate:
      Dojang.MonadFileSystem.Directory | route.kind /= SymlinkRoute -> do
        let ignores = findWithDefault [] (normalise route.routeName) ignorePatterns
        if null ignores
          then return []
          else do
            -- List all files (including those that would be ignored),
            -- except entries owned by routes nested inside this one:
            let nestedRoots = exclusionsFor route
            let owned :: FileEntry -> Bool
                owned entry =
                  not $
                    any
                      ( \root ->
                          splitDirectories root
                            `isPrefixOf` splitDirectories entry.path
                      )
                      nestedRoots
            allFiles <- Prelude.filter owned <$> listFiles route.destinationPath []
            -- Find which files match ignore patterns
            forM allFiles $ \entry -> do
              relPath <- decodePath entry.path
              case matchMany [(i, i) | i <- ignores] [((), relPath)] of
                (ptn, _, _) : _ ->
                  return $
                    Just $
                      IgnoredFile
                        { routeName = route.routeName
                        , sourcePath = route.sourcePath </> entry.path
                        , destinationPath = route.destinationPath </> entry.path
                        , pattern = ptn
                        }
                _ -> return Nothing
      _ -> return []
  return $ concat [[f | Just f <- fs] | fs <- ignoredLists]
 where
  ignorePatterns :: Map OsPath [FilePattern]
  ignorePatterns = ctx.repository.manifest.ignorePatterns


-- | Represents a file that is not registered in any route.
data UnregisteredFile = UnregisteredFile
  { filePath :: OsPath
  -- ^ The path to the unregistered file in the destination.
  , candidateRoutes :: [RouteResult]
  -- ^ The candidate routes that could contain this file (based on path prefix).
  }
  deriving (Eq, Show)


-- | Gets a list of files in destination directories that are not registered
-- in any route.  These are files that exist in destination directories but
-- are not part of the repository.
getUnregisteredFiles
  :: forall m
   . (HasCallStack, MonadFileSystem m)
  => Context m
  -- ^ The context in which to perform the operation.
  -> m (Either OwnershipError [UnregisteredFile])
  -- ^ The list of unregistered files, or the ownership error that makes
  -- the route configuration unsafe.
getUnregisteredFiles ctx = do
  correspondResult <- makeCorrespond ctx
  case correspondResult of
    Left err -> return $ Left err
    Right (registeredCorrespondences, _) ->
      Right <$> getUnregisteredFiles' ctx registeredCorrespondences


getUnregisteredFiles'
  :: forall m
   . (MonadFileSystem m)
  => Context m
  -> [FileCorrespondence]
  -> m [UnregisteredFile]
getUnregisteredFiles' ctx registeredCorrespondences = do
  (routes, _) <- routePaths ctx
  let registeredPaths =
        [ normalise c.destination.path
        | c <- registeredCorrespondences
        , c.destination.stat /= Missing
        ]
  let ownership = selectOwnership ctx.repository.sourcePath routes
  let exclusionsFor :: RouteResult -> [OsPath]
      exclusionsFor route = case ownership of
        Right state -> ownedExclusions state route.destinationPath
        Left _ -> []

  -- For each directory route, find files not in the registered set;
  -- entries owned by nested routes belong to those routes alone:
  unregisteredLists <- forM routes $ \route ->
    case route.fileType of
      -- A deployment link is a traversal boundary; everything behind it
      -- belongs to the repository already:
      Dojang.MonadFileSystem.Directory | route.kind /= SymlinkRoute -> do
        let nestedRoots = exclusionsFor route
        let owned :: FileEntry -> Bool
            owned entry =
              not $
                any
                  ( \root ->
                      splitDirectories root
                        `isPrefixOf` splitDirectories entry.path
                  )
                  nestedRoots
        allFiles <- Prelude.filter owned <$> listFiles route.destinationPath []
        let unregistered =
              [ f
              | f <- allFiles
              , normalise (route.destinationPath </> f.path)
                  `notElem` registeredPaths
              ]
        -- For each unregistered file, find candidate routes
        forM unregistered $ \entry -> do
          let fullPath = route.destinationPath </> entry.path
          candidates <- findCandidateRoutesFor ctx fullPath
          return $
            UnregisteredFile
              { filePath = fullPath
              , candidateRoutes = candidates
              }
      _ -> return []
  return $ concat unregisteredLists


-- | Finds candidate routes for an unregistered file.
-- Returns routes whose destination path is a prefix of the file path.
findCandidateRoutesFor
  :: forall m
   . (MonadFileSystem m)
  => Context m
  -- ^ The context in which to perform the operation.
  -> OsPath
  -- ^ The path to the unregistered file.
  -> m [RouteResult]
  -- ^ The list of candidate routes.
findCandidateRoutesFor ctx filePath = do
  (routes, _) <- routePaths ctx
  let containing =
        [ route
        | route <- routes
        , route.fileType == Dojang.MonadFileSystem.Directory
        , let routeDirs = splitDirectories $ normalise route.destinationPath
        , routeDirs `isPrefixOf` splitDirectories (normalise filePath)
        ]
  -- The most-specific containing route owns the path; broader routes are
  -- not candidates for entries inside a nested route's subtree:
  return $ case selectOwnership ctx.repository.sourcePath routes of
    Right state ->
      [ owner
      | Just owner <- [ownerOf state filePath]
      , owner.fileType == Dojang.MonadFileSystem.Directory
      ]
    Left _ -> containing
