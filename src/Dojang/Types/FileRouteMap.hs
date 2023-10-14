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

import Data.HashMap.Strict qualified as HashMap
  ( HashMap
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
import Data.Text (Text)

import Dojang.Types.FileRoute (FileRoute)


-- | A map of directories and their routes.
type FileRouteMap = HashMap.HashMap Text FileRoute


-- | An empty 'FileRouteMap'.
empty :: FileRouteMap
empty = HashMap.empty


-- | Makes a 'FileRouteMap' with a single directory name and its route.
singleton :: Text -> FileRoute -> FileRouteMap
singleton = HashMap.singleton


-- | Looks up a 'FileRoute' by its directory name.
lookup :: Text -> FileRouteMap -> Maybe FileRoute
lookup = HashMap.lookup


-- | Flipped version of 'lookup'.
(!?) :: FileRouteMap -> Text -> Maybe FileRoute
(!?) = (HashMap.!?)
{-# INLINE (!?) #-}


infixl 9 !?


-- | Makes a 'FileRouteMap' from a list of directory names and their routes.
fromList :: [(Text, FileRoute)] -> FileRouteMap
fromList = HashMap.fromList


-- | Gets the directory names from a 'FileRouteMap'.
keys :: FileRouteMap -> [Text]
keys = HashMap.keys


-- | Makes pairs of directory names and their routes from a 'FileRouteMap'.
toList :: FileRouteMap -> [(Text, FileRoute)]
toList = HashMap.toList


-- | Gets the number of directory mappings in a 'FileRouteMap'.
size :: FileRouteMap -> Int
size = HashMap.size


-- | Merges two 'FileRouteMap's.
union :: FileRouteMap -> FileRouteMap -> FileRouteMap
union = HashMap.union
