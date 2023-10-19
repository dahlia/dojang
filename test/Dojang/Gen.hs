module Dojang.Gen (evaluationWarning) where

import Test.Hspec.Hedgehog (MonadGen)

import Dojang.Types.EnvironmentPredicate.Evaluate
  ( EvaluationWarning (UndefinedMoniker)
  )
import Dojang.Types.Gen (monikerName)


evaluationWarning :: (MonadGen m) => m EvaluationWarning
evaluationWarning = UndefinedMoniker <$> monikerName
