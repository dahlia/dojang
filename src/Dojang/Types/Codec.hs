{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Declarative route codecs and their cache identities.
--
-- A codec specification is serializable route metadata.  Executable codec
-- definitions are kept separate so a manifest cannot claim a reflect policy
-- that the registered implementation does not provide.
module Dojang.Types.Codec
  ( CodecConfiguration (..)
  , CodecDefinition (..)
  , CodecDependency (..)
  , CodecName
  , CodecSpec (..)
  , CodecValue (..)
  , ReflectPolicy (..)
  , codecCacheKey
  , codecSpecDigest
  , identityCodecDefinition
  , identityCodecSpec
  , parseCodecName
  , renderCodecName
  ) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder
  ( Builder
  , byteString
  , toLazyByteString
  , word64BE
  , word8
  )
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Numeric (showHex)


-- | A nonempty, case-sensitive codec identifier.
newtype CodecName = CodecName Text
  deriving (Eq, Ord, Show)


-- | Parses a codec identifier.  The spelling is preserved exactly.
parseCodecName :: Text -> Maybe CodecName
parseCodecName name
  | Text.null name = Nothing
  | otherwise = Just $ CodecName name


-- | Renders a codec identifier exactly as declared.
renderCodecName :: CodecName -> Text
renderCodecName (CodecName name) = name


-- | A deterministic subset of TOML values accepted as codec configuration.
data CodecValue
  = CodecString Text
  | CodecInteger Integer
  | CodecBoolean Bool
  | CodecArray [CodecValue]
  | CodecTable (Map Text CodecValue)
  deriving (Eq, Ord, Show)


-- | Normalized codec configuration, ordered by key.
newtype CodecConfiguration = CodecConfiguration (Map Text CodecValue)
  deriving (Eq, Ord, Show)


-- | Serializable codec metadata attached to one route branch.
data CodecSpec = CodecSpec
  { name :: CodecName
  -- ^ Registered codec name.
  , configuration :: CodecConfiguration
  -- ^ Codec-specific declarative configuration.
  }
  deriving (Eq, Ord, Show)


-- | How a codec handles destination-to-source reflection.
data ReflectPolicy
  = -- | Deployed bytes are the repository representation.
    ReflectIdentity
  | -- | Destination changes cannot reconstruct repository input.
    ReflectReject
  | -- | A reverse operation reconstructs input and must round-trip.
    ReflectReAdd
  deriving (Bounded, Enum, Eq, Ord, Show)


-- | Stable properties of a registered codec implementation.
data CodecDefinition = CodecDefinition
  { name :: CodecName
  -- ^ Name matched against route metadata.
  , version :: Text
  -- ^ Implementation version included in cache keys.
  , reflectPolicy :: ReflectPolicy
  -- ^ Destination-to-source behavior.
  }
  deriving (Eq, Ord, Show)


-- | One input actually used by a codec, represented only by a safe identity
-- and fingerprint.
data CodecDependency = CodecDependency
  { identity :: Text
  -- ^ Stable, redacted dependency identity.
  , fingerprint :: Text
  -- ^ Fingerprint of the value used by the codec.
  }
  deriving (Eq, Ord, Show)


-- | The default route codec.
identityCodecSpec :: CodecSpec
identityCodecSpec =
  CodecSpec
    { name = identityName
    , configuration = CodecConfiguration Map.empty
    }


-- | Definition of the built-in byte-preserving codec.
identityCodecDefinition :: CodecDefinition
identityCodecDefinition =
  CodecDefinition
    { name = identityName
    , version = "1"
    , reflectPolicy = ReflectIdentity
    }


identityName :: CodecName
identityName = CodecName "identity"


-- | Computes a deterministic, hexadecimal SHA-256 cache key.
--
-- Each part is length-framed and tagged.  Configuration tables use ascending
-- key order, and dependency order is normalized by identity and fingerprint.
codecCacheKey
  :: CodecSpec
  -- ^ Normalized route codec specification.
  -> Text
  -- ^ Registered codec implementation version.
  -> ByteString
  -- ^ Raw repository source bytes.
  -> [CodecDependency]
  -- ^ Inputs actually referenced by the codec.
  -> ByteString
  -- ^ Lowercase hexadecimal SHA-256 digest.
codecCacheKey spec version source dependencies =
  digestHex $
    SHA256.hash $
      LazyByteString.toStrict $
        toLazyByteString $
          frame "dojang-route-codec-v1"
            <> frame (encodeUtf8 $ renderCodecName spec.name)
            <> frame (encodeUtf8 version)
            <> frameBuilder (encodeConfiguration spec.configuration)
            <> frame source
            <> frameBuilder
              ( foldMap
                  encodeDependency
                  ( sortOn
                      (\dependency -> (dependency.identity, dependency.fingerprint))
                      dependencies
                  )
              )
 where
  encodeDependency :: CodecDependency -> Builder
  encodeDependency dependency =
    frame (encodeUtf8 dependency.identity)
      <> frame (encodeUtf8 dependency.fingerprint)


-- | Computes a deterministic digest of declarative codec metadata.
codecSpecDigest :: CodecSpec -> ByteString
codecSpecDigest spec =
  digestHex $
    SHA256.hash $
      LazyByteString.toStrict $
        toLazyByteString $
          frame "dojang-route-codec-spec-v1"
            <> frame (encodeUtf8 $ renderCodecName spec.name)
            <> frameBuilder (encodeConfiguration spec.configuration)


encodeConfiguration :: CodecConfiguration -> Builder
encodeConfiguration (CodecConfiguration values) = encodeTable values


encodeTable :: Map Text CodecValue -> Builder
encodeTable values =
  word64BE (fromIntegral $ Map.size values)
    <> foldMap encodePair (Map.toAscList values)
 where
  encodePair (key, value) = frame (encodeUtf8 key) <> encodeValue value


encodeValue :: CodecValue -> Builder
encodeValue (CodecString value) = word8 0 <> frame (encodeUtf8 value)
encodeValue (CodecInteger value) =
  word8 1 <> frame (ByteString.Char8.pack $ show value)
encodeValue (CodecBoolean value) = word8 2 <> word8 (if value then 1 else 0)
encodeValue (CodecArray values) =
  word8 3
    <> word64BE (fromIntegral $ length values)
    <> foldMap encodeValue values
encodeValue (CodecTable values) = word8 4 <> encodeTable values


frame :: ByteString -> Builder
frame bytes = word64BE (fromIntegral $ ByteString.length bytes) <> byteString bytes


frameBuilder :: Builder -> Builder
frameBuilder builder = frame $ LazyByteString.toStrict $ toLazyByteString builder


digestHex :: ByteString -> ByteString
digestHex = ByteString.Char8.pack . concatMap byteHex . ByteString.unpack
 where
  byteHex byte = case showHex byte "" of
    [digit] -> ['0', digit]
    digits -> digits
