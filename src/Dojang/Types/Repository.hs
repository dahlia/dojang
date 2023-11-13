{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Repository
  ( Repository (..)
  , RouteMapWarning (..)
  , RouteResult (..)
  , findOverlappingRouteResults
  , overlaps
  , routePaths
  ) where

import Control.Monad (forM)
import Data.Function ((&))
import Data.List (isPrefixOf, nub)
import Data.List.NonEmpty
  ( NonEmpty ((:|))
  , append
  , nub
  , singleton
  , sort
  , toList
  )

import Data.Map.Strict (Map, findWithDefault, fromListWith, toList)
import System.FilePattern (FilePattern, matchMany)
import System.OsPath (OsPath, makeRelative, normalise, splitDirectories, (</>))
import System.OsString (OsString)

import Dojang.MonadFileSystem (FileType, MonadFileSystem (..))
import Dojang.Types.Environment (Environment)
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning)
import Dojang.Types.FilePathExpression (EnvironmentVariable)
import Dojang.Types.FilePathExpression.Expansion (ExpansionWarning)
import Dojang.Types.FileRoute (FileRoute (..), RouteWarning, routePath)
import Dojang.Types.FileRoute qualified as FileRoute (RouteWarning (..))
import Dojang.Types.Manifest (Manifest (..))


-- | A repository, which is a directory containing a manifest file and dotfiles.
data Repository = Repository
  { sourcePath :: OsPath
  -- ^ The path to the repository.
  , intermediatePath :: OsPath
  -- ^ The path to the intermediate directory, which is managed by Dojang and
  -- contains the post-processed files.
  , manifest :: Manifest
  -- ^ The manifest of the repository.
  }


-- | The expanded dispatch paths of a 'FileRoute'.
data RouteResult = RouteResult
  { sourcePath :: OsPath
  -- ^ The source path.  It is either absolute or relative to the current
  -- working directory.
  , routeName :: OsPath
  -- ^ The source path relative from the repository root, i.e., the route name.
  , destinationPath :: OsPath
  -- ^ The destination path.   It is either absolute or relative to
  -- the current working directory.
  , fileType :: FileType
  }
  deriving (Eq, Show)


-- | A warning that can occur during routing paths.
data RouteMapWarning
  = -- | A warning that can occur during environment predicate evaluation.
    EnvironmentPredicateWarning EvaluationWarning
  | -- | A warning that can occur during file path expression expansion.
    FilePathExpressionWarning ExpansionWarning
  | -- | A warning that two or more routes overlap in their destination paths
    -- (e.g., @/foo/bar@ and @/foo/bar/baz@).
    OverlapDestinationPathsWarning
      OsPath
      -- ^ The source path of the route that have overlapping destination paths.
      OsPath
      -- ^ The destination path that overlaps.
      (NonEmpty (OsPath, OsPath))
      -- ^ The pairs of corresponding source and destination paths that overlap.
  deriving (Eq, Show)


-- | Route the paths in the repository.  This will return a list of expanded
-- paths, along with any warnings that were generated.  Null routes will be
-- ignored.
routePaths
  :: forall m
   . (MonadFileSystem m)
  => Repository
  -- ^ The repository.
  -> Environment
  -- ^ The environment to use when evaluating the environment predicates.
  -> (EnvironmentVariable -> m (Maybe OsString))
  -- ^ The function to look up environment variables.
  -> m ([RouteResult], [RouteMapWarning])
  -- ^ The expanded paths, along with any warnings that were generated.
routePaths repo env lookupEnvVar = do
  paths <- forM fileRoutes $ \(src, route) -> do
    (dstPath, warnings) <- routePath route env lookupEnvVar
    return (src, dstPath, route.fileType, warnings)
  let paths' =
        [ RouteResult (repo.sourcePath </> src) src dst' ft
        | (src, Just dst', ft, _) <- paths
        ]
  overlappingRoutes <-
    forM (Data.Map.Strict.toList $ findOverlappingRouteResults paths')
      $ \((name, dst), overlaps') -> do
        let ignorePatterns =
              findWithDefault [] name repo.manifest.ignorePatterns
        filtered <- filterIgnoredPathsFromOverlaps dst ignorePatterns overlaps'
        return ((name, dst), filtered)
  let warnings =
        [translateWarning w | (_, _, _, ws) <- paths, w <- ws]
          ++ [ OverlapDestinationPathsWarning name dst (o :| overlaps')
             | ((name, dst), o : overlaps') <- overlappingRoutes
             ]
  return (paths', Data.List.nub warnings)
 where
  fileRoutes :: [(OsPath, FileRoute)]
  fileRoutes = Data.Map.Strict.toList repo.manifest.fileRoutes
  filterIgnoredPathsFromOverlaps
    :: OsPath
    -> [FilePattern]
    -> NonEmpty (OsPath, OsPath)
    -> m [(OsPath, OsPath)]
  filterIgnoredPathsFromOverlaps base patterns paths = do
    paths' <- forM pathList $ \pair@(_, dst') -> do
      dstFP <- decodePath $ makeRelative base' $ normalise dst'
      return (pair, dstFP)
    let ignored = [pair | (_, pair, _) <- matchMany patterns' paths']
    return [pair | pair <- pathList, pair `notElem` ignored]
   where
    base' :: OsPath
    base' = normalise base
    pathList :: [(OsPath, OsPath)]
    pathList = Data.List.NonEmpty.toList paths
    patterns' :: [((), FilePattern)]
    patterns' =
      [((), pattern) | pattern <- patterns]
        ++ [((), pattern ++ "/**/*") | pattern <- patterns]


translateWarning :: RouteWarning -> RouteMapWarning
translateWarning (FileRoute.EnvironmentPredicateWarning w) =
  EnvironmentPredicateWarning w
translateWarning (FileRoute.FilePathExpressionWarning w) =
  FilePathExpressionWarning w


findOverlappingRouteResults
  :: [RouteResult] -> Map (OsPath, OsPath) (NonEmpty (OsPath, OsPath))
findOverlappingRouteResults routes =
  fromListWith append found & fmap (Data.List.NonEmpty.nub . sort)
 where
  pairs :: [(OsPath, OsPath)]
  pairs =
    [ (name, dst)
    | RouteResult{routeName = name, destinationPath = dst} <- routes
    ]
  found :: [((OsPath, OsPath), NonEmpty (OsPath, OsPath))]
  found =
    [ ((name, dst), singleton (name', dst'))
    | (name, dst) <- pairs
    , (name', dst') <- pairs
    , name /= name'
    , dst `overlaps` dst'
    ]


-- | Returns whether the two paths overlap.  The former path is assumed to be
-- shorter than or equal to the latter path.
overlaps :: OsPath -> OsPath -> Bool
overlaps a b =
  (a' == b') || aDirs `isPrefixOf` bDirs
 where
  a' :: OsPath
  a' = normalise a
  b' :: OsPath
  b' = normalise b
  aDirs :: [OsPath]
  aDirs = splitDirectories a'
  bDirs :: [OsPath]
  bDirs = splitDirectories b'
