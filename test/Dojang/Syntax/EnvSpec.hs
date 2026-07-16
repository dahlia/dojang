{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.EnvSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (unpack)
import Test.Hspec (Spec, expectationFailure, specify)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldContain)
import Test.Hspec.Hedgehog (annotateShow, forAll, hedgehog, (===))

import Dojang.Syntax.Env
  ( readEnvironment
  , readFacts
  , readFactsOnly
  , writeEnvironment
  )
import Dojang.Types.Environment
  ( Kernel (Kernel)
  , emptyEnvironment
  , lookupFact
  , withFacts
  )
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

  specify "named machine facts" $ do
    let env =
          withFacts
            ( Map.fromList
                [ ("class", "work")
                , ("hostname", "atlas")
                , ("org.team", "platform")
                ]
            )
            (emptyEnvironment "linux" "x86_64" $ Kernel "Linux" "6.0")
    let toml = writeEnvironment env
    unpack toml `shouldContain` "hostname = \"atlas\""
    unpack toml `shouldContain` "[facts]"
    let Right (parsed, []) = readEnvironment toml
    lookupFact "hostname" parsed `shouldBe` Just "atlas"
    lookupFact "class" parsed `shouldBe` Just "work"
    lookupFact "org.team" parsed `shouldBe` Just "platform"
    readFacts toml
      `shouldBe` Right
        ( Map.fromList
            [ ("class", "work")
            , ("org.team", "platform")
            ]
        , []
        )

  specify "reserved facts cannot be persisted" $
    case readFacts "[facts]\nos = \"windows\"\n" of
      Left _ -> return ()
      Right result -> expectationFailure $ "Unexpected facts: " <> show result

  specify "facts-only documents exclude environment fields" $ do
    readFactsOnly "[facts]\nclass = \"work\"\n"
      `shouldBe` Right (Map.singleton "class" "work", [])
    case readFactsOnly "os = \"linux\"\n[facts]\nclass = \"work\"\n" of
      Left _ -> return ()
      Right result -> expectationFailure $ "Unexpected facts: " <> show result
