{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

module Dojang.Evaluate
  ( EvaluationWarning (..)
  , evaluate
  , evaluate'
  ) where

import Dojang.Types.Environment (Environment (architecture, operatingSystem))
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.MonikerMap (MonikerMap, MonikerResolver)
import Dojang.Types.MonikerName (MonikerName)

import Data.HashMap.Strict (lookup)

import Prelude hiding (lookup)


-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> :seti -XOverloadedLists


-- | A warning that occurred during evaluation.
newtype EvaluationWarning
  = -- | A moniker was referenced that was not defined.
    UndefinedMoniker MonikerName
  deriving (Eq, Show)


-- | Evaluates the given predicate against the given environment.
--
-- >>> import Dojang.Types.Environment (Environment (..))
-- >>> import Dojang.Types.MonikerMap (MonikerMap)
-- >>> import Dojang.Types.MonikerName (MonikerName, parseMonikerName)
-- >>> let (Right linuxAmd64) = parseMonikerName "linux-amd64"
-- >>> :{
-- let monikerMap =
--       [ (linuxAmd64, And [OperatingSystem "linux", Architecture "x86_64"])
--       ] :: MonikerMap
-- :}
--
-- >>> let environment = Environment "linux" "x86_64"
-- >>> evaluate environment monikerMap (OperatingSystem "linux")
-- (True,[])
-- >>> evaluate environment monikerMap (Architecture "aarch64")
-- (False,[])
--
-- The 'snd' of the result is a list of warnings that occurred during
-- evaluation:
--
-- >>> let (Right nonExistentMoniker) = parseMonikerName "non-existent"
-- >>> evaluate environment monikerMap (Moniker nonExistentMoniker)
-- (False,[UndefinedMoniker (MonikerName "non-existent")])
evaluate
  :: Environment
  -- ^ The 'Environment' to evaluate against.
  -> MonikerMap
  -- ^ The 'MonikerMap' to resolve 'MonikerName's against.
  -> EnvironmentPredicate
  -- ^ The 'EnvironmentPredicate' to evaluate.
  -> (Bool, [EvaluationWarning])
  -- ^ The result of the evaluation. The 'snd' of the result is a list of
  -- warnings that occurred during evaluation.  Note that warnings can be
  -- present even if the result is 'True'.
evaluate environment monikerMap =
  evaluate' environment (`lookup` monikerMap)


-- | Same as 'evaluate', but takes a resolver function instead of a
-- 'MonikerMap'.
evaluate'
  :: Environment
  -- ^ The 'Environment' to evaluate against.
  -> MonikerResolver
  -- ^ A function that resolves 'MonikerName's to 'EnvironmentPredicate's.
  -> EnvironmentPredicate
  -- ^ The 'EnvironmentPredicate' to evaluate.
  -> (Bool, [EvaluationWarning])
  -- ^ The result of the evaluation. The 'snd' of the result is a list of
  -- warnings that occurred during evaluation.  Note that warnings can be
  -- present even if the result is 'True'.
evaluate' _ _ Always =
  (True, [])
evaluate' environment resolver (Not predicate) =
  let (result, warnings) = evaluate' environment resolver predicate
  in (not result, warnings)
evaluate' environment resolver (And predicates) =
  let results = fmap (evaluate' environment resolver) predicates
  in (all fst results, concatMap snd results)
evaluate' environment resolver (Or predicates) =
  let results = fmap (evaluate' environment resolver) predicates
  in (any fst results, concatMap snd results)
evaluate' environment resolver (Moniker monikerName) =
  case resolver monikerName of
    Nothing -> (False, [UndefinedMoniker monikerName])
    Just predicate -> evaluate' environment resolver predicate
evaluate' environment _ (OperatingSystem os) =
  (os == environment.operatingSystem, [])
evaluate' environment _ (Architecture arch) =
  (arch == environment.architecture, [])
