{-# LANGUAGE ImportQualifiedPost #-}

module Dojang.Types.FileRouteMap
  ( FileRouteMap
  , empty
  , fromList
  , keys
  , lookup
  , singleton
  , size
  , toList
  , union
  , (!?)
  ) where

import Prelude hiding (lookup)

import Data.Map.Strict qualified as Map
  ( Map
  , empty
  , fromList
  , keys
  , lookup
  , singleton
  , size
  , toList
  , union
  , (!?)
  )
import System.OsPath (OsPath)

import Dojang.Types.FileRoute (FileRoute)


-- | A map of directories and their routes.
type FileRouteMap = Map.Map OsPath FileRoute


-- | An empty 'FileRouteMap'.
empty :: FileRouteMap
empty = Map.empty


-- | Makes a 'FileRouteMap' with a single directory name and its route.
singleton :: OsPath -> FileRoute -> FileRouteMap
singleton = Map.singleton


-- | Looks up a 'FileRoute' by its directory name.
lookup :: OsPath -> FileRouteMap -> Maybe FileRoute
lookup = Map.lookup


-- | Flipped version of 'lookup'.
(!?) :: FileRouteMap -> OsPath -> Maybe FileRoute
(!?) = (Map.!?)
{-# INLINE (!?) #-}


infixl 9 !?


-- | Makes a 'FileRouteMap' from a list of directory names and their routes.
fromList :: [(OsPath, FileRoute)] -> FileRouteMap
fromList = Map.fromList


-- | Gets the directory names from a 'FileRouteMap'.
keys :: FileRouteMap -> [OsPath]
keys = Map.keys


-- | Makes pairs of directory names and their routes from a 'FileRouteMap'.
toList :: FileRouteMap -> [(OsPath, FileRoute)]
toList = Map.toList


-- | Gets the number of directory mappings in a 'FileRouteMap'.
size :: FileRouteMap -> Int
size = Map.size


-- | Merges two 'FileRouteMap's.
union :: FileRouteMap -> FileRouteMap -> FileRouteMap
union = Map.union
