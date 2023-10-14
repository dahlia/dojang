module Dojang.Gen (evaluationWarning) where

import Dojang.Evaluate (EvaluationWarning (UndefinedMoniker))
import Dojang.Types.Gen (monikerName)

import Test.Hspec.Hedgehog (MonadGen)


evaluationWarning :: (MonadGen m) => m EvaluationWarning
evaluationWarning = UndefinedMoniker <$> monikerName
