module Dojang.Gen (evaluationWarning) where

import qualified Hedgehog.Gen as Gen
import Test.Hspec.Hedgehog (MonadGen)

import Dojang.Types.EnvironmentPredicate.Evaluate
  ( EvaluationWarning (..)
  )
import Dojang.Types.Gen (factKey, monikerName)


evaluationWarning :: (MonadGen m) => m EvaluationWarning
evaluationWarning =
  Gen.choice
    [ UndefinedMoniker <$> monikerName
    , UndefinedFact <$> factKey
    ]
