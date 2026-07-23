{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Binary-safe, backend-neutral protocol framing for sensitive codecs.
module Dojang.Types.CodecBackend.Protocol
  ( BackendFailure (..)
  , BackendOperation (..)
  , BackendProtocolRequest (..)
  , decodeBackendFailure
  , encodeBackendStandardInput
  , formatBackendFailure
  ) where

import Data.Aeson
  ( FromJSON (parseJSON)
  , Value (..)
  , eitherDecodeStrict'
  , encode
  , object
  , toJSON
  , withObject
  , (.:)
  , (.=)
  )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Map.Strict (Map)
import Data.Text (Text)

import Dojang.Types.Codec (CodecValue (..))


-- | Operation implemented by a codec backend.
data BackendOperation
  = Decrypt
  | Encrypt
  | Lookup Text
  deriving (Eq, Ord, Show)


-- | Safe metadata sent before a backend's raw binary payload.
data BackendProtocolRequest = BackendProtocolRequest
  { backend :: Text
  -- ^ Manifest-local backend identity.
  , version :: Text
  -- ^ Declared backend implementation version.
  , operation :: BackendOperation
  -- ^ Requested transformation or lookup.
  , options :: Map Text CodecValue
  -- ^ Non-secret backend options.
  }
  deriving (Eq, Show)


-- | Allowlisted backend failure category parsed without retaining stderr.
data BackendFailure
  = BackendMissingItem
  | BackendInvalidInput
  | BackendPermissionDenied
  | BackendUnavailable
  | BackendProtocolFailure
  deriving (Eq, Ord, Show)


-- | Encodes one UTF-8 JSON header line followed by the exact binary payload.
encodeBackendStandardInput
  :: BackendProtocolRequest
  -- ^ Safe request metadata encoded in the JSON header.
  -> ByteString
  -- ^ Exact binary payload appended after the header newline.
  -> ByteString
  -- ^ Framed standard input containing the header, newline, and payload.
encodeBackendStandardInput request payload =
  LazyByteString.toStrict (encode $ requestHeader request) <> "\n" <> payload


requestHeader :: BackendProtocolRequest -> Value
requestHeader request =
  object $
    [ "backend" .= request.backend
    , "operation" .= operationName request.operation
    , "options" .= fmap codecValue request.options
    , "protocol" .= ("dojang-codec-backend-v1" :: Text)
    , "version" .= request.version
    ]
      <> case request.operation of
        Lookup item -> ["item" .= item]
        _ -> []


operationName :: BackendOperation -> Text
operationName Decrypt = "decrypt"
operationName Encrypt = "encrypt"
operationName Lookup{} = "lookup"


codecValue :: CodecValue -> Value
codecValue (CodecString value) = String value
codecValue (CodecInteger value) = Number $ fromInteger value
codecValue (CodecBoolean value) = Bool value
codecValue (CodecArray values) = toJSON $ codecValue <$> values
codecValue (CodecTable values) = toJSON $ fmap codecValue values


newtype BackendDiagnostic = BackendDiagnostic Text


instance FromJSON BackendDiagnostic where
  parseJSON = withObject "backend diagnostic" $ \value ->
    BackendDiagnostic <$> value .: "code"


-- | Parses only an allowlisted diagnostic code and discards all other stderr.
decodeBackendFailure
  :: ByteString
  -- ^ Backend standard error containing a diagnostic JSON object.
  -> BackendFailure
  -- ^ Allowlisted failure category, or 'BackendProtocolFailure' when invalid.
decodeBackendFailure bytes = case eitherDecodeStrict' bytes of
  Left _ -> BackendProtocolFailure
  Right (BackendDiagnostic code) -> case code of
    "missing-item" -> BackendMissingItem
    "invalid-input" -> BackendInvalidInput
    "permission-denied" -> BackendPermissionDenied
    "unavailable" -> BackendUnavailable
    _ -> BackendProtocolFailure


-- | Formats a public diagnostic that never includes backend output.
formatBackendFailure
  :: BackendFailure
  -- ^ Sanitized backend failure category.
  -> Text
  -- ^ Public message that does not retain arbitrary backend diagnostics.
formatBackendFailure = \case
  BackendMissingItem -> "The requested backend item does not exist."
  BackendInvalidInput -> "The backend rejected the supplied input."
  BackendPermissionDenied -> "The backend denied access to the request."
  BackendUnavailable -> "The backend is unavailable."
  BackendProtocolFailure -> "The backend returned an invalid diagnostic."
