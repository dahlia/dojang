module Dojang.Types.MonikerMap (MonikerMap, MonikerResolver) where

import Data.HashMap.Strict (HashMap)

import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate)
import Dojang.Types.MonikerName (MonikerName)


-- | A map that defines the monikers and what conditions they match.
type MonikerMap = HashMap MonikerName EnvironmentPredicate


-- | A function that resolves a 'MonikerName' to an 'EnvironmentPredicate'.
type MonikerResolver = MonikerName -> Maybe EnvironmentPredicate
