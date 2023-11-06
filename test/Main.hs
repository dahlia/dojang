module Main where

import qualified Spec

import Test.Hspec.Formatters (specdoc)
import Test.Hspec.JUnit (hspecJUnitWith)
import Test.Hspec.Runner (configFormatter, defaultConfig)


main :: IO ()
main = hspecJUnitWith defaultConfig{configFormatter = Just specdoc} Spec.spec

-- cSpell:ignore specdoc
