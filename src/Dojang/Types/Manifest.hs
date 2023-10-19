{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Manifest (Manifest (..), manifest) where

import Data.Map.Strict (Map, fromList, toList)
import System.OsPath (OsPath)

import Dojang.Types.FilePathExpression (FilePathExpression)
import Dojang.Types.FileRoute (FileRoute, FileType (..), fileRoute)
import Dojang.Types.FileRouteMap (FileRouteMap)
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (MonikerName)


-- | A manifest of the directory routes and the definitions of monikers that
-- are used to resolve them.
data Manifest = Manifest
  { monikers :: MonikerMap
  -- ^ The definitions of monikers that are used to resolve the directory routes.
  , fileRoutes :: FileRouteMap
  -- ^ The directory routes that are resolved by the monikers.
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
  -> Manifest
  -- ^ The made 'Manifest'.
manifest monikers' fileRoutes' dirRoutes' =
  Manifest{monikers = monikers', fileRoutes = fromList $ files ++ dirs}
 where
  files :: [(OsPath, FileRoute)]
  files = do
    (dirName, monikerPairs) <- toList fileRoutes'
    pure (dirName, fileRoute monikers' monikerPairs File)
  dirs :: [(OsPath, FileRoute)]
  dirs = do
    (dirName, monikerPairs) <- toList dirRoutes'
    pure (dirName, fileRoute monikers' monikerPairs Directory)
