module Main where

import qualified Spec

import Test.Hspec.Api.Formatters.V1 (specdoc, useFormatter)
import Test.Hspec.JUnit.Formatter ()
import Test.Hspec.Runner (defaultConfig, hspecWith)


main :: IO ()
main = hspecWith (useFormatter ("specdoc", specdoc) defaultConfig) Spec.spec

-- cSpell:ignore specdoc
