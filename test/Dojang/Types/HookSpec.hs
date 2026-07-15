{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.HookSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Text (cons)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.Types.Hook
  ( HookPolicy (..)
  , HookType (..)
  , allHookTypes
  , parseHookId
  , renderHookId
  , renderHookPolicy
  )


spec :: Spec
spec = do
  describe "HookType" $ do
    it "has correct ordering (execution order)" $ do
      allHookTypes
        `shouldBe` [ PreApply
                   , PreFirstApply
                   , PostFirstApply
                   , PostApply
                   , PreReflect
                   , PostReflect
                   , PreDiff
                   , PostDiff
                   , PreStatus
                   , PostStatus
                   , PreEdit
                   , PostEdit
                   , PreUnmanage
                   , PostUnmanage
                   ]

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

  describe "HookPolicy" $ do
    it "uses stable manifest spellings" $ do
      renderHookPolicy HookAlways `shouldBe` "always"
      renderHookPolicy HookOnce `shouldBe` "once"
      renderHookPolicy HookOnChange `shouldBe` "on-change"

  describe "parseHookId" $ do
    it "accepts portable identifiers" $ do
      parseHookId "build-cache" `shouldSatisfy` isRight
      parseHookId "A.b_2" `shouldSatisfy` isRight

    it "rejects empty, path-like, and digit-leading identifiers" $ do
      parseHookId "" `shouldSatisfy` isLeft
      parseHookId "../hook" `shouldSatisfy` isLeft
      parseHookId "2hook" `shouldSatisfy` isLeft

    it "round-trips arbitrary portable identifiers" $ hedgehog $ do
      first <- forAll $ Gen.element $ ['a' .. 'z'] <> ['A' .. 'Z']
      rest <-
        forAll $
          Gen.text (Range.linear 0 80) $
            Gen.element $
              ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "._-"
      let identifier = cons first rest
      (renderHookId <$> parseHookId identifier) === Right identifier
