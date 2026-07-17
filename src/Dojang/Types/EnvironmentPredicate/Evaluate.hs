{-# LANGUAGE OverloadedRecordDot #-}

module Dojang.Types.EnvironmentPredicate.Evaluate
  ( EvaluationWarning (..)
  , evaluate
  , evaluate'
  ) where

import Prelude hiding (length, lookup, take)

import Data.CaseInsensitive (mk, original)
import Data.HashMap.Strict (lookup)
import Data.Text (Text, length, take, takeEnd)

import Dojang.Types.Environment
  ( Architecture (Etc)
  , Environment (..)
  , FactKey
  , Kernel (..)
  , OperatingSystem (OtherOS)
  , lookupFact
  )
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.MonikerMap (MonikerMap, MonikerResolver)
import Dojang.Types.MonikerName (MonikerName)


-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> :seti -XOverloadedLists


-- | A warning that occurred during evaluation.
data EvaluationWarning
  = -- | A moniker was referenced that was not defined.
    UndefinedMoniker MonikerName
  | -- | The operating system was not recognized.
    UnrecognizedOperatingSystem OperatingSystem
  | -- | The architecture was not recognized.
    UnrecognizedArchitecture Architecture
  | -- | A named machine fact was not defined.
    UndefinedFact FactKey
  deriving (Eq, Show)


-- | Evaluates the given predicate against the given environment.
--
-- >>> import Dojang.Types.Environment (Kernel (..), emptyEnvironment)
-- >>> import Dojang.Types.MonikerMap (MonikerMap)
-- >>> import Dojang.Types.MonikerName (MonikerName, parseMonikerName)
-- >>> let (Right linuxAmd64) = parseMonikerName "linux-amd64"
-- >>> :{
-- let monikerMap =
--       [ (linuxAmd64, And [OperatingSystem "linux", Architecture "x86_64"])
--       ] :: MonikerMap
-- :}
--
-- >>> let environment = emptyEnvironment "linux" "x86_64" (Kernel "Linux" "6.0")
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
evaluate' environment resolver predicate =
  let (truth, warnings) = evaluateTruth environment resolver predicate
  in (truth == TrueTruth, warnings)


data Truth = FalseTruth | UnknownTruth | TrueTruth
  deriving (Eq)


evaluateTruth
  :: Environment
  -> MonikerResolver
  -> EnvironmentPredicate
  -> (Truth, [EvaluationWarning])
evaluateTruth _ _ Always =
  (TrueTruth, [])
evaluateTruth environment resolver (Not predicate) =
  let (truth, warnings) = evaluateTruth environment resolver predicate
  in (negateTruth truth, warnings)
evaluateTruth environment resolver (And predicates) =
  let results = fmap (evaluateTruth environment resolver) predicates
  in (andTruth $ fmap fst results, concatMap snd results)
evaluateTruth environment resolver (Or predicates) =
  let results = fmap (evaluateTruth environment resolver) predicates
  in (orTruth $ fmap fst results, concatMap snd results)
evaluateTruth environment resolver (Moniker monikerName) =
  case resolver monikerName of
    Nothing -> (FalseTruth, [UndefinedMoniker monikerName])
    Just predicate -> evaluateTruth environment resolver predicate
evaluateTruth environment _ (OperatingSystem os) =
  ( boolTruth $ os == environment.operatingSystem
  , case os of
      OtherOS _ -> [UnrecognizedOperatingSystem os]
      _ -> []
  )
evaluateTruth environment _ (Architecture arch) =
  ( boolTruth $ arch == environment.architecture
  , case arch of
      Etc _ -> [UnrecognizedArchitecture arch]
      _ -> []
  )
evaluateTruth environment _ (KernelName kernel) =
  (boolTruth $ kernel == environment.kernel.name, [])
evaluateTruth environment _ (KernelRelease ver) =
  (boolTruth $ ver == environment.kernel.release, [])
evaluateTruth environment _ (KernelReleasePrefix prefix) =
  (boolTruth $ length rel >= prefixLen && mk (take prefixLen rel) == prefix, [])
 where
  rel :: Text
  rel = original environment.kernel.release
  prefixLen :: Int
  prefixLen = length $ original prefix
evaluateTruth environment _ (KernelReleaseSuffix suffix) =
  ( boolTruth $ length rel >= suffixLen && mk (takeEnd suffixLen rel) == suffix
  , []
  )
 where
  rel :: Text
  rel = original environment.kernel.release
  suffixLen :: Int
  suffixLen = length $ original suffix
evaluateTruth environment _ (Fact key value) =
  case lookupFact key environment of
    Nothing -> (UnknownTruth, [UndefinedFact key])
    Just actual -> (boolTruth $ actual == value, [])
evaluateTruth environment _ (FactDefined key) =
  case lookupFact key environment of
    Nothing -> (UnknownTruth, [UndefinedFact key])
    Just _ -> (TrueTruth, [])


boolTruth :: Bool -> Truth
boolTruth True = TrueTruth
boolTruth False = FalseTruth


negateTruth :: Truth -> Truth
negateTruth TrueTruth = FalseTruth
negateTruth FalseTruth = TrueTruth
negateTruth UnknownTruth = UnknownTruth


andTruth :: (Foldable f) => f Truth -> Truth
andTruth values
  | FalseTruth `elem` values = FalseTruth
  | UnknownTruth `elem` values = UnknownTruth
  | otherwise = TrueTruth


orTruth :: (Foldable f) => f Truth -> Truth
orTruth values
  | TrueTruth `elem` values = TrueTruth
  | UnknownTruth `elem` values = UnknownTruth
  | otherwise = FalseTruth
