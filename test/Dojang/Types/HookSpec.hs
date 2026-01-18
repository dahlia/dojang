{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.HookSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

import Dojang.Types.Hook (HookType (..), allHookTypes)


spec :: Spec
spec = do
  describe "HookType" $ do
    it "has correct ordering (execution order)" $ do
      allHookTypes `shouldBe` [PreApply, PreFirstApply, PostFirstApply, PostApply]

    it "PreApply < PreFirstApply < PostFirstApply < PostApply" $ do
      (PreApply < PreFirstApply) `shouldBe` True
      (PreFirstApply < PostFirstApply) `shouldBe` True
      (PostFirstApply < PostApply) `shouldBe` True

    it "Eq works correctly" $ do
      (PreApply == PreApply) `shouldBe` True
      (PreApply == PostApply) `shouldBe` False

    it "Show works correctly" $ do
      show PreApply `shouldBe` "PreApply"
      show PreFirstApply `shouldBe` "PreFirstApply"
      show PostFirstApply `shouldBe` "PostFirstApply"
      show PostApply `shouldBe` "PostApply"
