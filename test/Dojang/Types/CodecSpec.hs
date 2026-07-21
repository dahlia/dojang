{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.CodecSpec (spec) where

import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldNotBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.Types.Codec
  ( CodecConfiguration (..)
  , CodecDefinition (..)
  , CodecDependency (..)
  , CodecSpec (..)
  , CodecValue (..)
  , ReflectPolicy (..)
  , codecCacheKey
  , identityCodecDefinition
  , identityCodecSpec
  , parseCodecName
  , renderCodecName
  )


spec :: Spec
spec = do
  describe "CodecSpec" $ do
    specify "identity is the default lossless codec" $ do
      renderCodecName identityCodecSpec.name `shouldBe` "identity"
      identityCodecSpec.configuration `shouldBe` CodecConfiguration Map.empty
      identityCodecDefinition.reflectPolicy `shouldBe` ReflectIdentity

    specify "codec names are nonempty and case-sensitive" $ do
      parseCodecName "" `shouldBe` Nothing
      renderCodecName <$> parseCodecName "identity" `shouldBe` Just "identity"
      parseCodecName "Identity" `shouldNotBe` parseCodecName "identity"

  describe "codecCacheKey" $ do
    specify "is deterministic for arbitrary binary source input" $ hedgehog $ do
      source <- forAll $ Gen.bytes (Range.linear 0 4096)
      let spec' = identityCodecSpec
      codecCacheKey spec' "1" source []
        === codecCacheKey spec' "1" source []

    specify "normalizes configuration and dependency ordering" $ do
      let Just name = parseCodecName "example"
          spec' =
            CodecSpec
              name
              ( CodecConfiguration $
                  Map.fromList
                    [ ("z", CodecBoolean True)
                    , ("a", CodecArray [CodecInteger 1, CodecString "two"])
                    ]
              )
          dependencies =
            [ CodecDependency "variable:HOME" "home-fingerprint"
            , CodecDependency "fact:class" "class-fingerprint"
            ]
      codecCacheKey spec' "7" "source" dependencies
        `shouldBe` codecCacheKey spec' "7" "source" (reverse dependencies)

    specify "changes for every cache input dimension" $ do
      let Just name = parseCodecName "example"
          base = CodecSpec name (CodecConfiguration Map.empty)
          configured =
            base
              { configuration =
                  CodecConfiguration $ Map.singleton "key" $ CodecString "value"
              }
          dependency = [CodecDependency "fact:class" "personal"]
          key = codecCacheKey base "1" "source" []
      codecCacheKey base "1" "changed" [] `shouldNotBe` key
      codecCacheKey base "2" "source" [] `shouldNotBe` key
      codecCacheKey configured "1" "source" [] `shouldNotBe` key
      codecCacheKey base "1" "source" dependency `shouldNotBe` key
      ByteString.length key `shouldBe` 64
