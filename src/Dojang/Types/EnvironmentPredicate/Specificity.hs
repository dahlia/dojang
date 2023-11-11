-- | This module contains the 'Specificity' type and the 'specificity' function,
-- which are used to determine how specific an 'EnvironmentPredicate' is, i.e.,
-- when there are multiple predicates that could match, which one should be
-- chosen.
module Dojang.Types.EnvironmentPredicate.Specificity
  ( Specificity (..)
  , specificity
  ) where

import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.MonikerName (MonikerName)


-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> :seti -XOverloadedLists


-- | The specificity of an 'EnvironmentPredicate'.  It guarantees that
-- the more specific predicate will always be greater than the less specific
-- predicate (e.g. @And [OperatingSystem "linux", Architecture "x86_64"]@ is
-- more specific than @OperatingSystem "linux"@).
data Specificity = Specificity
  { maxPredicates :: Int
  -- ^ The number of predicates that must be evaluated at a maximum.
  , totalPredicates :: Int
  -- ^ The total number of predicates regardless of whether they would be
  -- evaluated or not (e.g. if the first predicate in an 'Or' matches,
  -- then the second predicate would not be evaluated, but it would still be
  -- counted).
  }
  deriving (Eq, Show, Read)


instance Ord Specificity where
  compare (Specificity maxA totalA) (Specificity maxB totalB) =
    case compare maxA maxB of
      EQ -> compare totalB totalA
      x -> x


-- | Calculates the 'Specificity' of an 'EnvironmentPredicate'.  For details
-- on how the 'Specificity' is calculated, see the documentation for
-- 'Specificity'.
--
-- >>> import Data.List (sortOn)
-- >>> import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
-- >>> :{
-- let predicates =
--       [ OperatingSystem "linux"
--       , Architecture "x86_64"
--       , And [OperatingSystem "linux", Architecture "x86_64"]
--       , Or [OperatingSystem "linux", Architecture "x86_64"]
--       ] :: [EnvironmentPredicate]
-- :}
--
-- >>> sortOn (specificity (const Nothing)) predicates
-- [Or (...),OperatingSystem Linux,Architecture X86_64,And (...)]
specificity
  :: (MonikerName -> Maybe EnvironmentPredicate)
  -> EnvironmentPredicate
  -> Specificity
specificity _ Always = Specificity 0 0
specificity monikerResolver (Not predicate) =
  specificity monikerResolver predicate
specificity monikerResolver (And predicates) =
  let qs = specificity monikerResolver <$> predicates
  in Specificity (sum $ maxPredicates <$> qs) (sum $ totalPredicates <$> qs)
specificity monikerResolver (Or predicates) =
  let qs = specificity monikerResolver <$> predicates
  in Specificity (maximum $ maxPredicates <$> qs) (sum $ totalPredicates <$> qs)
specificity monikerResolver (Moniker monikerName) =
  case monikerResolver monikerName of
    Nothing -> Specificity 0 0
    Just predicate -> specificity monikerResolver predicate
specificity _ (OperatingSystem _) = Specificity 1 1
specificity _ (Architecture _) = Specificity 1 1
specificity _ (KernelName _) = Specificity 1 1
specificity _ (KernelRelease _) = Specificity 1 1
specificity _ (KernelReleasePrefix _) = Specificity 1 1
specificity _ (KernelReleaseSuffix _) = Specificity 1 1
