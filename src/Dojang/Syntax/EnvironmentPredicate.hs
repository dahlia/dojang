-- | A human-friendly syntax for describing
-- 'Dojang.Types.EnvironmentPredicate.EnvironmentPredicate's.  It is mainly
-- used for @when@ statements in /dojang.toml/ files.
module Dojang.Syntax.EnvironmentPredicate
  ( errorBundlePretty
  , parseEnvironmentPredicate
  , writeEnvironmentPredicate
  ) where

import Dojang.Syntax.EnvironmentPredicate.Parser
  ( errorBundlePretty
  , parseEnvironmentPredicate
  )
import Dojang.Syntax.EnvironmentPredicate.Writer (writeEnvironmentPredicate)

