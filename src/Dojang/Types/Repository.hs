{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Repository
  ( Repository (..)
  , RouteResult (..)
  , routePaths
  ) where

import Control.Monad (forM)
import Data.List (nub)

import Data.Map.Strict (toList)
import System.OsPath (OsPath, (</>))
import System.OsString (OsString)

import Dojang.MonadFileSystem (FileType, MonadFileSystem)
import Dojang.Types.Environment (Environment)
import Dojang.Types.FilePathExpression (EnvironmentVariable)
import Dojang.Types.FileRoute (FileRoute (..), RouteWarning, routePath)
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


-- | Route the paths in the repository.  This will return a list of expanded
-- paths, along with any warnings that were generated.  Null routes will be
-- ignored.
routePaths
  :: (MonadFileSystem m)
  => Repository
  -- ^ The repository.
  -> Environment
  -- ^ The environment to use when evaluating the environment predicates.
  -> (EnvironmentVariable -> m (Maybe OsString))
  -- ^ The function to look up environment variables.
  -> m ([RouteResult], [RouteWarning])
  -- ^ The expanded paths, along with any warnings that were generated.
routePaths repo env lookupEnvVar = do
  paths <- forM fileRoutes $ \(src, route) -> do
    (dstPath, warnings) <- routePath route env lookupEnvVar
    return (src, dstPath, route.fileType, warnings)
  let paths' =
        [ RouteResult (repo.sourcePath </> src) src dst' ft
        | (src, Just dst', ft, _) <- paths
        ]
  let warnings = [w | (_, _, _, ws) <- paths, w <- ws]
  return (paths', nub warnings)
 where
  fileRoutes :: [(OsPath, FileRoute)]
  fileRoutes = toList repo.manifest.fileRoutes
