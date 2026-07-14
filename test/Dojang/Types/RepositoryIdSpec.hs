{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.RepositoryIdSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (constant)
import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Hedgehog
  ( annotateShow
  , assert
  , forAll
  , hedgehog
  , (===)
  )

import Dojang.Types.RepositoryId
  ( parseRepositoryId
  , repositoryIdText
  )


spec :: Spec
spec = describe "RepositoryId" $ do
  specify "parses and renders canonical UUIDs" $ do
    let text = "123e4567-e89b-42d3-a456-426614174000"
    (repositoryIdText <$> parseRepositoryId text) `shouldBe` Right text

  specify "rejects malformed identifiers" $ do
    parseRepositoryId "not-a-uuid" `shouldBe` Left "Invalid repository ID."

  specify "round-trips arbitrary UUID-shaped identifiers" $ hedgehog $ do
    groups <-
      forAll $
        traverse
          (\length' -> Gen.text (constant length' length') Gen.hexit)
          [8, 4, 4, 4, 12]
    let text = foldr1 (\a b -> a <> "-" <> b) groups :: Text
    case parseRepositoryId text of
      Left err -> annotateShow err >> assert False
      Right identifier -> repositoryIdText identifier === Text.toLower text
