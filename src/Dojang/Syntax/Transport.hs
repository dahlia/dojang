{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Parser for machine-local external transport configuration.
module Dojang.Syntax.Transport
  ( Error (..)
  , formatError
  , readTransportConfig
  , readTransportConfigFile
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import System.OsPath (OsPath)
import Toml (Result (..), decode)
import Toml.FromValue
  ( FromValue (fromValue)
  , optKey
  , parseTableFromValue
  )
import Prelude hiding (readFile)

import Dojang.MonadFileSystem (MonadFileSystem (readFile))
import Dojang.Types.Transport
  ( TransportConfig
  , TransportConfigurationError (..)
  , TransportName
  , TransportNameError (..)
  , TransportSpec
  , makeTransportConfig
  , makeTransportSpec
  , parseTransportName
  )


data TransportDocument = TransportDocument (Map Text RawTransportSpec)


data RawTransportSpec = RawTransportSpec
  { command :: [Text]
  , inheritedEnvironment :: [Text]
  , environment :: Map Text Text
  }


instance FromValue TransportDocument where
  fromValue =
    parseTableFromValue $
      TransportDocument . fromMaybe Map.empty <$> optKey "transports"


instance FromValue RawTransportSpec where
  fromValue =
    parseTableFromValue $
      RawTransportSpec
        <$> (fromMaybe [] <$> optKey "command")
        <*> (fromMaybe [] <$> optKey "inherit-environment")
        <*> (fromMaybe Map.empty <$> optKey "environment")


-- | A malformed external transport document.
data Error
  = InvalidUtf8 Text
  | TomlErrors (NonEmpty String)
  | TomlWarnings [String]
  | InvalidTransportName Text TransportNameError
  | InvalidTransport Text TransportConfigurationError
  deriving (Eq, Show)


-- | Parses a transport document and rejects unknown fields.
readTransportConfig :: Text -> Either Error TransportConfig
readTransportConfig source =
  case decode $ Text.unpack source of
    Failure (err : errors) -> Left $ TomlErrors $ err :| errors
    Failure [] -> Left $ TomlErrors $ "unknown error" :| []
    Success warnings _
      | not $ null warnings -> Left $ TomlWarnings warnings
    Success _ (TransportDocument transports) ->
      makeTransportConfig . Map.fromList
        <$> traverse mapEntry (Map.toAscList transports)
 where
  mapEntry
    :: (Text, RawTransportSpec)
    -> Either Error (TransportName, TransportSpec)
  mapEntry (name, raw) = do
    parsedName <-
      case parseTransportName name of
        Left err -> Left $ InvalidTransportName name err
        Right value -> Right value
    spec <-
      case makeTransportSpec
        raw.command
        raw.inheritedEnvironment
        raw.environment of
        Left err -> Left $ InvalidTransport name err
        Right value -> Right value
    Right (parsedName, spec)


-- | Reads and parses a UTF-8 transport configuration file.
readTransportConfigFile
  :: (MonadFileSystem m) => OsPath -> m (Either Error TransportConfig)
readTransportConfigFile path = do
  bytes <- readFile path
  return $ case decodeUtf8' bytes of
    Left err -> Left $ InvalidUtf8 $ Text.pack $ show err
    Right source -> readTransportConfig source


-- | Formats a transport configuration error for a user-facing diagnostic.
formatError :: Error -> Text
formatError (InvalidUtf8 message) =
  "The transport configuration is not valid UTF-8: " <> message <> "."
formatError (TomlErrors errors) =
  "The transport configuration is not valid TOML:\n"
    <> Text.unlines (Text.pack <$> toList errors)
formatError (TomlWarnings warnings) =
  "The transport configuration has unknown or unused fields:\n"
    <> Text.unlines (Text.pack <$> warnings)
formatError (InvalidTransportName name err) =
  "Invalid transport name " <> quote name <> ": " <> formatNameError err
formatError (InvalidTransport name err) =
  "Invalid transport " <> quote name <> ": " <> formatTransportError err


formatNameError :: TransportNameError -> Text
formatNameError EmptyTransportName = "transport names cannot be empty."
formatNameError InvalidTransportNameStart =
  "transport names must start with an ASCII letter."
formatNameError InvalidTransportNameCharacter =
  "transport names may contain only ASCII letters, digits, hyphens, and "
    <> "underscores."
formatNameError ReservedTransportName =
  "the names 'directory' and 'archive' are reserved for built-in transports."


formatTransportError :: TransportConfigurationError -> Text
formatTransportError EmptyTransportCommand = "the command cannot be empty."
formatTransportError EmptyTransportExecutable =
  "the command executable cannot be empty."
formatTransportError (PlaceholderInExecutable value) =
  "the executable "
    <> quote value
    <> " cannot contain a source or destination placeholder."
formatTransportError MissingSourcePlaceholder =
  "the command must contain one whole-argument '{source}' placeholder."
formatTransportError DuplicateSourcePlaceholder =
  "the command must not contain more than one '{source}' placeholder."
formatTransportError MissingDestinationPlaceholder =
  "the command must contain one whole-argument '{destination}' placeholder."
formatTransportError DuplicateDestinationPlaceholder =
  "the command must not contain more than one '{destination}' placeholder."
formatTransportError (EmbeddedPlaceholder value) =
  "the argument "
    <> quote value
    <> " embeds a placeholder; placeholders must occupy a whole argument."
formatTransportError (InvalidEnvironmentName name) =
  "the environment name " <> quote name <> " is not portable."
formatTransportError (DuplicateInheritedEnvironmentName name) =
  "the environment name "
    <> quote name
    <> " is inherited more than once."


toList :: NonEmpty a -> [a]
toList (value :| values) = value : values


quote :: Text -> Text
quote value = "'" <> value <> "'"
