{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.EnvironmentPredicate.SpecificitySpec (spec) where

import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (/==), (===))

import Dojang.Types.Environment
  ( Architecture (X86_64)
  , OperatingSystem (Linux)
  )
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.EnvironmentPredicate.Specificity
  ( Specificity (..)
  , specificity
  )
import Dojang.Types.Gen qualified as Gen
import Dojang.Types.MonikerName (parseMonikerName)


spec :: Spec
spec = do
  describe "Specificity" $ do
    specify "Eq" $ hedgehog $ do
      specificityA <- forAll Gen.specificity
      specificityB <- forAll Gen.specificity
      (specificityA == specificityB) /== (specificityA /= specificityB)

    describe "Ord" $ do
      specify "examples" $ do
        max (Specificity 0 0) (Specificity 1 0) `shouldBe` Specificity 1 0
        max (Specificity 0 0) (Specificity 0 1) `shouldBe` Specificity 0 0
        min (Specificity 1 0) (Specificity 1 1) `shouldBe` Specificity 1 1

      specify "properties" $ hedgehog $ do
        specificityA <- forAll Gen.specificity
        specificityB <- forAll Gen.specificity
        (specificityA `compare` specificityB)
          === (specificityA.maxPredicates, specificityB.totalPredicates)
          `compare` (specificityB.maxPredicates, specificityA.totalPredicates)
        (specificityA < specificityB) /== (specificityA >= specificityB)
        (specificityA <= specificityB) /== (specificityA > specificityB)
        (specificityA > specificityB) /== (specificityA <= specificityB)
        (specificityA >= specificityB) /== (specificityA < specificityB)

    specify "Read, Show" $ hedgehog $ do
      specificityA <- forAll Gen.specificity
      specificityB <- forAll Gen.specificity
      showList [specificityA, specificityB] ""
        === ("[" ++ show specificityA ++ "," ++ show specificityB ++ "]")
      read (show specificityA) === specificityA
      read (showList [specificityA, specificityB] "")
        === ([specificityA, specificityB] :: [Specificity])

  specify "specificity" $ do
    specificity undefined (Always) `shouldBe` Specificity 0 0
    specificity undefined (OperatingSystem Linux) `shouldBe` Specificity 1 1
    specificity undefined (Architecture X86_64) `shouldBe` Specificity 1 1
    specificity undefined (Not Always) `shouldBe` Specificity 0 0
    specificity undefined (Not (Architecture X86_64)) `shouldBe` Specificity 1 1
    specificity undefined (And [Always, Architecture X86_64])
      `shouldBe` Specificity 1 1
    specificity undefined (And [OperatingSystem Linux, Architecture X86_64])
      `shouldBe` Specificity 2 2
    specificity undefined (Or [Always, Architecture X86_64])
      `shouldBe` Specificity 1 1
    specificity undefined (Or [OperatingSystem Linux, Architecture X86_64])
      `shouldBe` Specificity 1 2
    let Right foo = parseMonikerName "foo"
        Right bar = parseMonikerName "bar"
        Right baz = parseMonikerName "baz"
        Right nonExistent = parseMonikerName "non-existent"
        lookup' m
          | m == foo = Just (OperatingSystem Linux)
          | m == bar = Just (And [OperatingSystem Linux, Architecture X86_64])
          | m == baz = Just (Moniker foo)
          | otherwise = Nothing
    specificity lookup' (Moniker foo) `shouldBe` Specificity 1 1
    specificity lookup' (Moniker nonExistent) `shouldBe` Specificity 0 0
    specificity lookup' (Not $ Moniker foo) `shouldBe` Specificity 1 1
    specificity lookup' (And [Moniker foo, Moniker bar])
      `shouldBe` Specificity 3 3
    specificity lookup' (Or [Moniker foo, Moniker bar])
      `shouldBe` Specificity 2 3
    specificity lookup' (And [Moniker baz, Moniker bar])
      `shouldBe` Specificity 3 3
