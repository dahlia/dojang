module Dojang.Syntax.EnvSpec (spec) where

import Test.Hspec (Spec, specify)
import Test.Hspec.Hedgehog (annotateShow, forAll, hedgehog, (===))

import Dojang.Syntax.Env (readEnvironment, writeEnvironment)
import Dojang.Types.Gen as Gen (environment)


spec :: Spec
spec = do
  specify "writeEnvironment & readEnvironment" $ hedgehog $ do
    env <- forAll Gen.environment
    let toml = writeEnvironment env
    annotateShow toml
    let parsed = readEnvironment toml
    annotateShow parsed
    parsed === Right (env, [])
