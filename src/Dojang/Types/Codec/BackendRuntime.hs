{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Controlled execution of external sensitive-codec backends.
module Dojang.Types.Codec.BackendRuntime
  ( executeCodecBackendProcess
  , resolveCodecBackendInput
  ) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric (showHex)
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath (OsPath, isAbsolute, normalise)

import Dojang.CommandEffect
  ( BinaryProcessRequest (BinaryProcessRequest)
  , BinaryProcessResult (..)
  , MonadCommandEffect (hostPlatform, runBinaryProcess)
  , redactedProcessBytes
  , revealProcessBytes
  )
import Dojang.MonadFileSystem
  ( MonadFileSystem (decodePath, encodePath, makeAbsolute)
  )
import Dojang.Types.Codec.Evaluate
  ( ExternalInput (ExternalInput)
  , ExternalInputRequest (..)
  , OpaqueBytes
  , opaqueBytes
  , revealBytes
  )
import Dojang.Types.CodecBackend
  ( CodecBackend (..)
  , CodecBackendMap
  , CodecBackendOptions (CodecBackendOptions)
  )
import Dojang.Types.CodecBackend.Protocol
  ( BackendOperation
  , BackendProtocolRequest (BackendProtocolRequest)
  , decodeBackendFailure
  , encodeBackendStandardInput
  , formatBackendFailure
  )
import Dojang.Types.FilePathExpression.Expansion
  ( VariableGetter
  , expandFilePathWithVariables
  )


-- | Resolves a structured backend request from the active manifest.
--
-- Backend commands must expand to an absolute path.  The initial release is
-- intentionally unavailable on Windows until its process and owner-only
-- storage guarantees have been verified there.
resolveCodecBackendInput
  :: (MonadFileSystem m, MonadCommandEffect m)
  => CodecBackendMap
  -> VariableGetter m
  -> OsPath
  -> ExternalInputRequest
  -> m (Either Text ExternalInput)
resolveCodecBackendInput _ _ _ (ExternalInputRequest _) =
  return $ Left "The declared external input has no codec backend."
resolveCodecBackendInput backends variableGetter sourcePath (BackendInputRequest backendName operation payload) = do
  platform <- hostPlatform
  if platform == "mingw32" || platform == "win32"
    then return $ Left "Sensitive codec backends are not supported on Windows."
    else case Map.lookup backendName backends of
      Nothing -> return $ Left "The selected codec backend is not declared."
      Just backend -> do
        (command, warnings, _) <-
          expandFilePathWithVariables
            backend.command
            variableGetter
            encodePathText
        if not $ null warnings
          then return $ Left "The codec backend command could not be expanded."
          else
            if not $ isAbsolute command
              then return $ Left "The codec backend command is not absolute."
              else do
                root <- makeAbsolute sourcePath
                executable <- decodePath $ normalise command
                workingDirectory <- decodePath $ normalise root
                executeCodecBackendProcess
                  backendName
                  backend
                  executable
                  workingDirectory
                  operation
                  payload
 where
  encodePathText = encodePath . Text.unpack


-- | Executes one already-expanded backend declaration with a hermetic child
-- environment and no command-line arguments.
executeCodecBackendProcess
  :: (MonadCommandEffect m)
  => Text
  -- ^ Manifest-local backend name.
  -> CodecBackend
  -- ^ Validated backend declaration.
  -> FilePath
  -- ^ Expanded absolute executable path.
  -> FilePath
  -- ^ Absolute repository working directory.
  -> BackendOperation
  -- ^ Requested backend operation.
  -> OpaqueBytes
  -- ^ Exact binary payload, kept redacted in effect values.
  -> m (Either Text ExternalInput)
executeCodecBackendProcess
  backendName
  backend
  executable
  workingDirectory
  operation
  payload = do
    let CodecBackendOptions options = backend.options
        protocolRequest =
          BackendProtocolRequest
            backendName
            backend.version
            operation
            options
        standardInput =
          encodeBackendStandardInput protocolRequest $ revealBytes payload
        request =
          BinaryProcessRequest
            executable
            (Just workingDirectory)
            []
            (redactedProcessBytes standardInput)
            backend.timeoutSeconds
    result <- runBinaryProcess request
    return $ case result of
      BinaryProcessCompleted ExitSuccess output _ ->
        let bytes = revealProcessBytes output
        in Right $
             ExternalInput
               (opaqueBytes bytes)
               (backendFingerprint protocolRequest)
      BinaryProcessCompleted _ _ stderr ->
        Left $ formatBackendFailure $ decodeBackendFailure $ revealProcessBytes stderr
      BinaryProcessStartFailed _ -> Left "The codec backend could not be started."
      BinaryProcessIOFailed _ -> Left "The codec backend failed during I/O."
      BinaryProcessTimedOut -> Left "The codec backend timed out."
      BinaryProcessUnavailable _ -> Left "Codec backend execution is unavailable."


backendFingerprint :: BackendProtocolRequest -> Text
backendFingerprint request =
  digestText $ encodeBackendStandardInput request ""


digestText :: ByteString -> Text
digestText = Text.pack . concatMap byteHex . ByteString.unpack . SHA256.hash
 where
  byteHex byte = case showHex byte "" of
    [digit] -> ['0', digit]
    digits -> digits
