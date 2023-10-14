{-# LANGUAGE ImportQualifiedPost #-}

module Dojang.Types.MonikerMap
  ( MonikerMap
  , MonikerResolver
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

import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate)
import Dojang.Types.MonikerName (MonikerName)


-- | A map that defines the monikers and what conditions they match.
type MonikerMap = HashMap.HashMap MonikerName EnvironmentPredicate


-- | A function that resolves a 'MonikerName' to an 'EnvironmentPredicate'.
type MonikerResolver = MonikerName -> Maybe EnvironmentPredicate


-- | An empty 'MonikerMap'.
empty :: MonikerMap
empty = HashMap.empty


-- | Makes a 'MonikerMap' with a single moniker and its condition.
singleton :: MonikerName -> EnvironmentPredicate -> MonikerMap
singleton = HashMap.singleton


-- | Makes a 'MonikerMap' from a list of monikers and their conditions.
fromList :: [(MonikerName, EnvironmentPredicate)] -> MonikerMap
fromList = HashMap.fromList


-- | Gets the moniker names from a 'MonikerMap'.
keys :: MonikerMap -> [MonikerName]
keys = HashMap.keys


-- | Looks up an 'EnvironmentPredicate' by its 'MonikerName'.
lookup :: MonikerName -> MonikerMap -> Maybe EnvironmentPredicate
lookup = HashMap.lookup


-- | Flipped version of 'lookup'.
(!?) :: MonikerMap -> MonikerName -> Maybe EnvironmentPredicate
(!?) = (HashMap.!?)
{-# INLINE (!?) #-}


infixl 9 !?


-- | Gets the number of moniker definitions in a 'MonikerMap'.
size :: MonikerMap -> Int
size = HashMap.size


-- | Makes pairs of moniker names and associated predicates from a 'MonikerMap'.
toList :: MonikerMap -> [(MonikerName, EnvironmentPredicate)]
toList = HashMap.toList


-- | Merges two 'MonikerMap's.
union :: MonikerMap -> MonikerMap -> MonikerMap
union = HashMap.union
