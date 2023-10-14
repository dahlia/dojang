{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.EnvironmentPredicateSpec (spec) where

import Data.Hashable (Hashable (hash, hashWithSalt))
import Test.Hspec (Spec, describe, it, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (/==), (===))

import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (..)
  , normalizePredicate
  )
import Dojang.Types.Gen qualified as Gen
import Dojang.Types.MonikerName (parseMonikerName)


spec :: Spec
spec = do
  describe "EnvironmentPredicate" $ do
    specify "Eq" $ hedgehog $ do
      predicate <- forAll Gen.environmentPredicate
      predicate' <- forAll Gen.environmentPredicate
      (predicate == predicate') /== (predicate /= predicate')

    specify "Hashable" $ hedgehog $ do
      predicate <- forAll Gen.environmentPredicate
      predicate' <- forAll Gen.environmentPredicate
      (hash predicate == hash predicate') === (predicate == predicate')
      (hashWithSalt 0 predicate == hashWithSalt 0 predicate')
        === (predicate == predicate')

    specify "Show" $ hedgehog $ do
      predicate <- forAll Gen.environmentPredicate
      predicate' <- forAll Gen.environmentPredicate
      showList [predicate, predicate'] ""
        === ("[" ++ show predicate ++ "," ++ show predicate' ++ "]")

  describe "normalizePredicate" $ do
    it "reduces an unary And/Or to its child" $ do
      normalizePredicate (And [Always]) `shouldBe` Always
      normalizePredicate (Or [Always]) `shouldBe` Always

    it "reduces an And predicate into Not Always if it has any Not Always" $ do
      normalizePredicate (And [Not Always]) `shouldBe` Not Always
      normalizePredicate (And [Always, Not Always]) `shouldBe` Not Always
      normalizePredicate (And [Not Always, OperatingSystem "linux"])
        `shouldBe` Not Always
      normalizePredicate (And [OperatingSystem "linux", Architecture "x86_64"])
        `shouldBe` And [OperatingSystem "linux", Architecture "x86_64"]

    it "reduces an Or predicate into Always if it has any Always" $ do
      normalizePredicate (Or [Always]) `shouldBe` Always
      normalizePredicate (Or [Always, Not Always]) `shouldBe` Always
      normalizePredicate (Or [Always, OperatingSystem "linux"])
        `shouldBe` Always
      normalizePredicate (Or [OperatingSystem "linux", OperatingSystem "macos"])
        `shouldBe` Or [OperatingSystem "linux", OperatingSystem "macos"]

    it "removes all Always predicates in an And predicate" $ do
      normalizePredicate (And [Always, Always, OperatingSystem "linux"])
        `shouldBe` OperatingSystem "linux"
      normalizePredicate
        (And [Always, Always, OperatingSystem "linux", Architecture "x86_64"])
        `shouldBe` And [OperatingSystem "linux", Architecture "x86_64"]
      normalizePredicate (And [Always, Always]) `shouldBe` Always

    it "removes all Not Always predicates in an Or predicate" $ do
      normalizePredicate (Or [Not Always, Not Always, OperatingSystem "linux"])
        `shouldBe` OperatingSystem "linux"
      normalizePredicate
        ( Or
            [ Not Always
            , Not Always
            , OperatingSystem "linux"
            , OperatingSystem "macos"
            ]
        )
        `shouldBe` Or [OperatingSystem "linux", OperatingSystem "macos"]
      normalizePredicate (Or [Not Always, Not Always]) `shouldBe` Not Always

    it
      ( "reduces an And predicate into Not Always if it has two contradicting "
          ++ "predicates"
      )
      $ do
        normalizePredicate
          (And [OperatingSystem "linux", Not $ OperatingSystem "linux"])
          `shouldBe` Not Always
        normalizePredicate (And [OperatingSystem "linux", Architecture "x86_64"])
          `shouldBe` And [OperatingSystem "linux", Architecture "x86_64"]

    it
      ( "reduces an Or predicate into Always if it has two contradicting "
          ++ "predicates"
      )
      $ do
        normalizePredicate
          (Or [OperatingSystem "linux", Not $ OperatingSystem "linux"])
          `shouldBe` Always
        normalizePredicate
          (Or [OperatingSystem "linux", OperatingSystem "macos"])
          `shouldBe` Or [OperatingSystem "linux", OperatingSystem "macos"]

    let Right foo = parseMonikerName "foo"

    it "flattens nested Ands" $ do
      normalizePredicate
        ( And
            [ OperatingSystem "linux"
            , And [Architecture "x86_64", Moniker foo]
            ]
        )
        `shouldBe` And
          [ OperatingSystem "linux"
          , Architecture "x86_64"
          , Moniker foo
          ]

    it "flattens nested Ors" $ do
      normalizePredicate
        ( Or
            [ OperatingSystem "linux"
            , Or [OperatingSystem "macos", Moniker foo]
            ]
        )
        `shouldBe` Or
          [ OperatingSystem "linux"
          , OperatingSystem "macos"
          , Moniker foo
          ]

    it "reduces double negation" $ do
      normalizePredicate (Not $ Not Always) `shouldBe` Always
      normalizePredicate (Not $ Not $ Not Always) `shouldBe` Not Always
      normalizePredicate (Not $ Not $ OperatingSystem "linux")
        `shouldBe` OperatingSystem "linux"
