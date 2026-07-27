{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Parser for machine-local merge-driver configuration.
module Dojang.Syntax.MergeDriver
  ( Error (..)
  , formatError
  , readMergeDriverConfig
  , readMergeDriverConfigFile
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
  , reqKey
  )
import Prelude hiding (readFile)

import Dojang.MonadFileSystem (MonadFileSystem (readFile))
import Dojang.Types.MergeDriver
  ( MergeDriverConfig (..)
  , MergeDriverConfigurationError (..)
  , MergeDriverName
  , MergeDriverNameError (..)
  , MergeDriverSpec
  , makeMergeDriverSpec
  , parseMergeDriverName
  )


data MergeDriverDocument = MergeDriverDocument
  { defaultDriver :: Text
  , drivers :: Map Text RawMergeDriverSpec
  }


data RawMergeDriverSpec = RawMergeDriverSpec
  { command :: [Text]
  , inheritedEnvironment :: [Text]
  , environment :: Map Text Text
  , unresolvedExitCodes :: [Integer]
  , canceledExitCodes :: [Integer]
  }


instance FromValue MergeDriverDocument where
  fromValue =
    parseTableFromValue $
      MergeDriverDocument
        <$> reqKey "default-driver"
        <*> reqKey "merge-drivers"


instance FromValue RawMergeDriverSpec where
  fromValue =
    parseTableFromValue $
      RawMergeDriverSpec
        <$> (fromMaybe [] <$> optKey "command")
        <*> (fromMaybe [] <$> optKey "inherit-environment")
        <*> (fromMaybe Map.empty <$> optKey "environment")
        <*> reqKey "unresolved-exit-codes"
        <*> reqKey "canceled-exit-codes"


-- | A malformed merge-driver configuration document.
data Error
  = InvalidUtf8 Text
  | TomlErrors (NonEmpty String)
  | TomlWarnings [String]
  | InvalidDefaultMergeDriverName Text MergeDriverNameError
  | InvalidMergeDriverName Text MergeDriverNameError
  | InvalidMergeDriver Text MergeDriverConfigurationError
  | UnknownDefaultMergeDriver Text
  deriving (Eq, Show)


-- | Parses a merge-driver document and rejects unknown fields.
readMergeDriverConfig :: Text -> Either Error MergeDriverConfig
readMergeDriverConfig source =
  case decoded of
    Failure (err : errors) -> Left $ TomlErrors $ err :| errors
    Failure [] -> Left $ TomlErrors $ "unknown error" :| []
    Success warnings _
      | not $ null warnings -> Left $ TomlWarnings warnings
    Success _ document -> mapDocument document
 where
  decoded :: Result String MergeDriverDocument
  decoded = decode $ Text.unpack source
  mapDocument :: MergeDriverDocument -> Either Error MergeDriverConfig
  mapDocument document = do
    defaultName <-
      case parseMergeDriverName document.defaultDriver of
        Left err ->
          Left $
            InvalidDefaultMergeDriverName document.defaultDriver err
        Right value -> Right value
    drivers <-
      Map.fromList <$> traverse mapEntry (Map.toAscList document.drivers)
    if Map.member defaultName drivers
      then Right $ MergeDriverConfig defaultName drivers
      else Left $ UnknownDefaultMergeDriver document.defaultDriver
  mapEntry
    :: (Text, RawMergeDriverSpec)
    -> Either Error (MergeDriverName, MergeDriverSpec)
  mapEntry (name, raw) = do
    parsedName <-
      case parseMergeDriverName name of
        Left err -> Left $ InvalidMergeDriverName name err
        Right value -> Right value
    driver <-
      case makeMergeDriverSpec
        raw.command
        raw.inheritedEnvironment
        raw.environment
        raw.unresolvedExitCodes
        raw.canceledExitCodes of
        Left err -> Left $ InvalidMergeDriver name err
        Right value -> Right value
    Right (parsedName, driver)


-- | Reads and parses a UTF-8 merge-driver configuration file.
readMergeDriverConfigFile
  :: (MonadFileSystem m) => OsPath -> m (Either Error MergeDriverConfig)
readMergeDriverConfigFile path = do
  bytes <- readFile path
  return $ case decodeUtf8' bytes of
    Left err -> Left $ InvalidUtf8 $ Text.pack $ show err
    Right source -> readMergeDriverConfig source


-- | Formats a merge-driver configuration error for a user-facing diagnostic.
formatError :: Error -> Text
formatError (InvalidUtf8 message) =
  "The merge-driver configuration is not valid UTF-8: " <> message <> "."
formatError (TomlErrors errors) =
  "The merge-driver configuration is not valid TOML:\n"
    <> Text.unlines (Text.pack <$> toList errors)
formatError (TomlWarnings warnings) =
  "The merge-driver configuration has unknown or unused fields:\n"
    <> Text.unlines (Text.pack <$> warnings)
formatError (InvalidDefaultMergeDriverName name err) =
  "Invalid default merge-driver name "
    <> quote name
    <> ": "
    <> formatNameError err
formatError (InvalidMergeDriverName name err) =
  "Invalid merge-driver name " <> quote name <> ": " <> formatNameError err
formatError (InvalidMergeDriver name err) =
  "Invalid merge driver " <> quote name <> ": " <> formatDriverError err
formatError (UnknownDefaultMergeDriver name) =
  "The default merge driver " <> quote name <> " is not configured."


formatNameError :: MergeDriverNameError -> Text
formatNameError EmptyMergeDriverName = "merge-driver names cannot be empty."
formatNameError InvalidMergeDriverNameStart =
  "merge-driver names must start with an ASCII letter."
formatNameError InvalidMergeDriverNameCharacter =
  "merge-driver names may contain only ASCII letters, digits, hyphens, and "
    <> "underscores."


formatDriverError :: MergeDriverConfigurationError -> Text
formatDriverError EmptyMergeDriverCommand = "the command cannot be empty."
formatDriverError EmptyMergeDriverExecutable =
  "the command executable cannot be empty."
formatDriverError (MergePlaceholderInExecutable value) =
  "the executable " <> quote value <> " cannot contain a merge placeholder."
formatDriverError MissingMergeSourcePlaceholder =
  "the command must contain one whole-argument '{source}' placeholder."
formatDriverError DuplicateMergeSourcePlaceholder =
  "the command must not contain more than one '{source}' placeholder."
formatDriverError MissingMergeBasePlaceholder =
  "the command must contain one whole-argument '{base}' placeholder."
formatDriverError DuplicateMergeBasePlaceholder =
  "the command must not contain more than one '{base}' placeholder."
formatDriverError DuplicateMergeDestinationPlaceholder =
  "the command must not contain more than one '{destination}' placeholder."
formatDriverError MissingMergeResultPlaceholder =
  "the command must contain one whole-argument '{result}' placeholder."
formatDriverError DuplicateMergeResultPlaceholder =
  "the command must not contain more than one '{result}' placeholder."
formatDriverError (EmbeddedMergePlaceholder value) =
  "the argument "
    <> quote value
    <> " embeds a placeholder; placeholders must occupy a whole argument."
formatDriverError (InvalidMergeDriverExitCode code) =
  "exit code " <> Text.pack (show code) <> " must be a positive machine integer."
formatDriverError (DuplicateMergeDriverExitCode code) =
  "exit code " <> Text.pack (show code) <> " is listed more than once."
formatDriverError (AmbiguousMergeDriverExitCode code) =
  "exit code "
    <> Text.pack (show code)
    <> " denotes both an unresolved and a canceled merge."
formatDriverError (InvalidMergeDriverEnvironmentName name) =
  "the environment name " <> quote name <> " is not portable."
formatDriverError (DuplicateMergeDriverInheritedEnvironmentName name) =
  "the environment name " <> quote name <> " is inherited more than once."


toList :: NonEmpty a -> [a]
toList (value :| values) = value : values


quote :: Text -> Text
quote value = "'" <> value <> "'"
