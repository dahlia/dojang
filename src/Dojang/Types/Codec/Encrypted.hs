{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Backend-neutral encrypted-file codecs.
module Dojang.Types.Codec.Encrypted
  ( encryptedCodecImplementation
  , encryptedCodecName
  , encryptedReAddCodecImplementation
  , encryptedReAddCodecName
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

import Dojang.Types.Codec
  ( CodecConfiguration (CodecConfiguration)
  , CodecDefinition (CodecDefinition)
  , CodecName
  , CodecValue (CodecString)
  , ReflectPolicy (ReflectReAdd, ReflectReject)
  , parseCodecName
  )
import Dojang.Types.Codec.Evaluate
  ( CodecDryRunPolicy (CachedOnly)
  , CodecFailure
  , CodecImplementation
  , CodecInputs (..)
  , CodecProgram (CodecDone, CodecRequest)
  , CodecRequirements (CodecRequirements)
  , ExternalInput (..)
  , ExternalInputRequest (BackendInputRequest)
  , OpaqueBytes
  , codecImplementationWithEffects
  , noCodecInputs
  , revealBytes
  )
import Dojang.Types.CodecBackend.Protocol
  ( BackendOperation (Decrypt, Encrypt)
  )


-- | Built-in encrypted codec that rejects reflection.
encryptedCodecName :: CodecName
encryptedCodecName = validCodecName "encrypted"


-- | Built-in encrypted codec that re-encrypts reflected destination bytes.
encryptedReAddCodecName :: CodecName
encryptedReAddCodecName = validCodecName "encrypted-re-add"


-- | Decrypts repository bytes and rejects destination-to-source reflection.
encryptedCodecImplementation :: CodecImplementation
encryptedCodecImplementation = encryptedImplementation encryptedCodecName ReflectReject


-- | Decrypts repository bytes and validates re-encrypted reflected bytes by
-- decrypting them again before accepting the new repository representation.
encryptedReAddCodecImplementation :: CodecImplementation
encryptedReAddCodecImplementation =
  encryptedImplementation encryptedReAddCodecName ReflectReAdd


encryptedImplementation :: CodecName -> ReflectPolicy -> CodecImplementation
encryptedImplementation name reflectPolicy =
  codecImplementationWithEffects
    (CodecDefinition name "1" reflectPolicy)
    validateConfiguration
    (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
    decryptSource
    reverseProgram
    CachedOnly
 where
  reverseProgram = case reflectPolicy of
    ReflectReAdd -> Just encryptDeployed
    _ -> Nothing


validateConfiguration
  :: CodecConfiguration -> Either CodecFailure CodecRequirements
validateConfiguration configuration = do
  _ <- configuredBackend configuration
  Right $ CodecRequirements noCodecInputs noCodecInputs []


decryptSource :: CodecInputs -> CodecProgram ByteString
decryptSource inputs = case configuredBackend inputs.configuration of
  Left failure -> CodecDone $ Left failure
  Right backend ->
    CodecRequest
      (BackendInputRequest backend Decrypt inputs.rawSource)
      (CodecDone . Right . revealBytes . (.value))


encryptDeployed
  :: CodecInputs
  -> OpaqueBytes
  -> CodecProgram ByteString
encryptDeployed inputs deployed = case configuredBackend inputs.configuration of
  Left failure -> CodecDone $ Left failure
  Right backend ->
    CodecRequest
      (BackendInputRequest backend Encrypt deployed)
      (CodecDone . Right . revealBytes . (.value))


configuredBackend :: CodecConfiguration -> Either CodecFailure Text
configuredBackend (CodecConfiguration values) = case Map.toList values of
  [("backend", CodecString backend)]
    | not $ Text.null backend -> Right backend
  _ -> Left "encrypted codecs require only one nonempty backend string"


validCodecName :: Text -> CodecName
validCodecName name = case parseCodecName name of
  Just parsed -> parsed
  Nothing -> error "Built-in encrypted codec names are nonempty."
