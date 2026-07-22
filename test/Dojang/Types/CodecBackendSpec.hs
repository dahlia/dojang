{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.CodecBackendSpec (spec) where

import Data.ByteString qualified as ByteString
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.Types.CodecBackend.Protocol
  ( BackendFailure (..)
  , BackendOperation (..)
  , BackendProtocolRequest (..)
  , decodeBackendFailure
  , encodeBackendStandardInput
  , formatBackendFailure
  )


spec :: Spec
spec = describe "codec backend protocol" $ do
  it "preserves every arbitrary binary payload after the header" $ hedgehog $ do
    payload <- forAll $ Gen.bytes (Range.linear 0 16384)
    let request =
          BackendProtocolRequest
            "vault"
            "2026-07"
            Decrypt
            mempty
        encoded = encodeBackendStandardInput request payload
        (_, framedPayload) = ByteString.break (== 10) encoded
    ByteString.drop 1 framedPayload === payload

  it "encodes lookup item identities in the JSON header" $ do
    let request =
          BackendProtocolRequest
            "vault"
            "1"
            (Lookup "prod/token")
            mempty
        (header, _) = ByteString.break (== 10) $ encodeBackendStandardInput request ""
    header
      `shouldBe` "{\"backend\":\"vault\",\"item\":\"prod/token\",\"operation\":\"lookup\",\"options\":{},\"protocol\":\"dojang-codec-backend-v1\",\"version\":\"1\"}"

  it "accepts only allowlisted backend diagnostic codes" $ do
    decodeBackendFailure "{\"code\":\"missing-item\",\"detail\":\"secret\"}"
      `shouldBe` BackendMissingItem
    decodeBackendFailure "{\"code\":\"secret-value\"}"
      `shouldBe` BackendProtocolFailure
    decodeBackendFailure "not JSON" `shouldBe` BackendProtocolFailure

  it "formats failures without retaining backend diagnostics" $ do
    formatBackendFailure BackendInvalidInput
      `shouldBe` "The backend rejected the supplied input."
