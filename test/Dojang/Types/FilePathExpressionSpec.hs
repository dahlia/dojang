{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.FilePathExpressionSpec (spec) where

import Data.Hashable (hash)
import Data.Set qualified as Set
import Data.Text (unpack)
import Test.Hspec (Spec, describe, it, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Hedgehog (annotateShow, footnote, forAll, hedgehog, (===))

import Dojang.Syntax.FilePathExpression.Parser
  ( errorBundlePretty
  , parseFilePathExpression
  )
import Dojang.Types.FilePathExpression
  ( FilePathExpression (..)
  , environmentVariables
  , toPathText
  )
import Dojang.Types.Gen qualified as Gen


spec :: Spec
spec = do
  describe "FilePathExpression" $ do
    specify "Eq" $ hedgehog $ do
      x <- forAll Gen.filePathExpression
      y <- forAll Gen.filePathExpression
      (x == y) === (y == x)
      (x /= y) === (y /= x)

    specify "Hashable" $ hedgehog $ do
      x <- forAll Gen.filePathExpression
      y <- forAll Gen.filePathExpression
      (x == y) === (hash x == hash y)
      (x /= y) === (hash x /= hash y)

    specify "Show" $ hedgehog $ do
      x <- forAll Gen.filePathExpression
      y <- forAll Gen.filePathExpression
      showList [x, y] "" === ("[" ++ show x ++ "," ++ show y ++ "]")

  describe "toPathText" $ do
    it "ends with a slash for a single Root expression" $ do
      toPathText (Root Nothing) `shouldBe` "/"
      toPathText (Root $ Just 'C') `shouldBe` "C:/"

    it "yields what parseFilePathExpression can parse" $ hedgehog $ do
      expr <- forAll Gen.filePathExpression
      let exprText = toPathText expr
      annotateShow exprText
      let parseResult = parseFilePathExpression "test" exprText
      case parseResult of
        Left err -> footnote $ unpack $ errorBundlePretty err
        Right _ -> return ()
      let Right expr' = parseResult
      expr' === expr

  describe "environmentVariables" $ do
    it "collects variables from all substitution branches without duplicates" $ do
      environmentVariables
        ( PathSeparator
            (Substitution "HOME")
            ( SubstitutionWithDefault
                "CONFIG"
                (ConditionalSubstitution "HOME" $ Substitution "SUFFIX")
            )
        )
        `shouldBe` Set.fromList ["HOME", "CONFIG", "SUFFIX"]
