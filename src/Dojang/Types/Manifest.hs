{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Manifest
  ( IgnoreMap
  , Manifest (..)
  , manifest
  ) where

import Data.Map.Strict (Map, fromList, toAscList, toList)
import System.FilePattern (FilePattern)
import System.OsPath (OsPath, normalise)

import Dojang.MonadFileSystem (FileType (..))
import Dojang.Types.FilePathExpression (FilePathExpression)
import Dojang.Types.FileRoute (FileRoute, fileRoute)
import Dojang.Types.FileRouteMap (FileRouteMap)
import Dojang.Types.Hook (HookMap)
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (MonikerName)


-- | A map of directory routes to file patterns that should be ignored.
type IgnoreMap = Map OsPath [FilePattern]


-- | A manifest of the directory routes and the definitions of monikers that
-- are used to resolve them.
data Manifest = Manifest
  { monikers :: MonikerMap
  -- ^ The definitions of monikers that are used to resolve the directory routes.
  , fileRoutes :: FileRouteMap
  -- ^ The directory routes that are resolved by the monikers.
  , ignorePatterns :: IgnoreMap
  -- ^ The file patterns that should be ignored for each directory route.
  , hooks :: HookMap
  -- ^ The hooks to run before and after applying.
  }
  deriving (Eq, Show)


-- | Makes a 'Manifest' from definitions of monikers and directory routes.
manifest
  :: MonikerMap
  -- ^ The monikers that are used to resolve the directory routes.
  -> Map OsPath [(MonikerName, Maybe FilePathExpression)]
  -- ^ The file routes that are resolved by the monikers.  The keys are
  -- the file names, and the values are the pairs of 'MonikerName's and
  -- 'FilePathExpression's that make up each 'FileRoute'.
  -- If the same 'MonikerName' is used more than once, then the latest one
  -- will be used.  If the same file name appears in the directory routes
  -- (the next parameter), then the directory route will take precedence.
  -> Map OsPath [(MonikerName, Maybe FilePathExpression)]
  -- ^ The directory routes that are resolved by the monikers.  The keys are
  -- the names of the directories, and the values are the pairs of
  -- 'MonikerName's and 'FilePathExpression's that make up each 'FileRoute'.
  -- If the same 'MonikerName' is used more than once, then the latest one
  -- will be used.  If the same file name appears in the file routes
  -- (the previous parameter), then the directory route will take precedence.
  -> IgnoreMap
  -- ^ The file patterns that should be ignored for each directory route.
  -> HookMap
  -- ^ The hooks to run before and after applying.
  -> Manifest
  -- ^ The made 'Manifest'.
manifest monikers' fileRoutes' dirRoutes' ignorePatterns' hooks' =
  Manifest
    { monikers = monikers'
    , fileRoutes = fromList $ files ++ dirs
    , ignorePatterns = ignores
    , hooks = hooks'
    }
 where
  files :: [(OsPath, FileRoute)]
  files = do
    (filename, monikerPairs) <- toList fileRoutes'
    pure (normalise filename, fileRoute monikers' monikerPairs File)
  dirs :: [(OsPath, FileRoute)]
  dirs = do
    (dirName, monikerPairs) <- toList dirRoutes'
    pure (normalise dirName, fileRoute monikers' monikerPairs Directory)
  ignores :: IgnoreMap
  ignores =
    fromList
      [ (normalise p, pattern)
      | (p, pattern) <- toAscList ignorePatterns'
      ]
