{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Configuration for shell-free repository acquisition commands.
module Dojang.Types.Transport
  ( EnvironmentNameCase (..)
  , TransportConfig (..)
  , TransportConfigurationError (..)
  , TransportLookupError (..)
  , TransportName
  , TransportNameError (..)
  , TransportSpec (..)
  , expandTransportCommandNative
  , lookupTransport
  , makeTransportConfig
  , makeTransportSpec
  , parseTransportName
  , resolveTransportEnvironmentNative
  , transportNameText
  ) where

import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

import Dojang.Types.ExternalCommand
  ( EnvironmentNameCase (..)
  , ExternalEnvironmentConfigurationError (..)
  , resolveExternalEnvironmentNative
  , validateExternalEnvironment
  )


-- | A case-sensitive name for a configured external transport.
newtype TransportName = TransportName Text
  deriving (Eq, Ord, Show)


-- | A problem with a transport name.
data TransportNameError
  = -- | The configured name is empty.
    EmptyTransportName
  | -- | The configured name does not start with an ASCII letter.
    InvalidTransportNameStart
  | -- | The configured name contains a character outside the portable grammar.
    InvalidTransportNameCharacter
  | -- | The configured name collides with a built-in transport.
    ReservedTransportName
  deriving (Eq, Show)


-- | One external command and its deliberately limited environment.
data TransportSpec = TransportSpec
  { command :: NonEmpty Text
  -- ^ Executable followed by individual arguments.
  , inheritedEnvironment :: [Text]
  -- ^ Host environment names copied into the child environment.
  , environment :: Map Text Text
  -- ^ Fixed child environment values, overriding inherited values.
  }
  deriving (Eq, Show)


-- | Named external transports available to a bootstrap command.
newtype TransportConfig = TransportConfig (Map TransportName TransportSpec)
  deriving (Eq, Show)


-- | A problem with a configured external transport.
data TransportConfigurationError
  = -- | No executable or arguments were configured.
    EmptyTransportCommand
  | -- | The configured executable is the empty string.
    EmptyTransportExecutable
  | -- | A source or destination placeholder appears in the executable.
    PlaceholderInExecutable Text
  | -- | No whole-argument source placeholder is present.
    MissingSourcePlaceholder
  | -- | More than one source placeholder is present.
    DuplicateSourcePlaceholder
  | -- | No whole-argument destination placeholder is present.
    MissingDestinationPlaceholder
  | -- | More than one destination placeholder is present.
    DuplicateDestinationPlaceholder
  | -- | A placeholder is embedded inside another argument.
    EmbeddedPlaceholder Text
  | -- | An environment entry has a non-portable name.
    InvalidEnvironmentName Text
  | -- | An environment name is inherited more than once.
    DuplicateInheritedEnvironmentName Text
  deriving (Eq, Show)


-- | A problem resolving a configured transport.
data TransportLookupError
  = -- | The requested name is not syntactically valid.
    InvalidLookupTransportName TransportNameError
  | -- | No configured transport has the requested name.
    UnknownTransportName Text
  deriving (Eq, Show)


-- | Parses a case-sensitive transport name.
parseTransportName :: Text -> Either TransportNameError TransportName
parseTransportName value
  | Text.null value = Left EmptyTransportName
  | value == "directory" || value == "archive" = Left ReservedTransportName
  | not $ validFirst $ Text.head value = Left InvalidTransportNameStart
  | not $ Text.all validRest value = Left InvalidTransportNameCharacter
  | otherwise = Right $ TransportName value
 where
  validFirst character =
    isAscii character && (isAsciiLower character || isAsciiUpper character)
  validRest character =
    validFirst character
      || isDigit character
      || character == '-'
      || character == '_'


-- | Renders a transport name exactly as configured.
transportNameText :: TransportName -> Text
transportNameText (TransportName value) = value


-- | Validates and constructs an external transport.
makeTransportSpec
  :: [Text]
  -- ^ Executable followed by individual arguments.
  -> [Text]
  -- ^ Host environment names to inherit.
  -> Map Text Text
  -- ^ Fixed environment entries.
  -> Either TransportConfigurationError TransportSpec
makeTransportSpec command inherited environment = do
  validatedCommand <- case command of
    [] -> Left EmptyTransportCommand
    value : values -> Right $ value :| values
  let executable = NonEmpty.head validatedCommand
  if Text.null executable
    then Left EmptyTransportExecutable
    else Right ()
  if placeholder executable
    then Left $ PlaceholderInExecutable executable
    else Right ()
  mapM_ rejectEmbeddedPlaceholder $ drop 1 command
  requireExactlyOne
    "{source}"
    MissingSourcePlaceholder
    DuplicateSourcePlaceholder
  requireExactlyOne
    "{destination}"
    MissingDestinationPlaceholder
    DuplicateDestinationPlaceholder
  case validateExternalEnvironment inherited environment of
    Left (InvalidExternalEnvironmentName name) ->
      Left $ InvalidEnvironmentName name
    Left (DuplicateExternalInheritedEnvironmentName name) ->
      Left $ DuplicateInheritedEnvironmentName name
    Right () -> Right $ TransportSpec validatedCommand inherited environment
 where
  placeholder value =
    Text.isInfixOf "{source}" value
      || Text.isInfixOf "{destination}" value
  rejectEmbeddedPlaceholder value
    | placeholder value
        && value /= "{source}"
        && value /= "{destination}" =
        Left $ EmbeddedPlaceholder value
    | otherwise = Right ()
  requireExactlyOne value missing duplicate =
    case length $ filter (== value) command of
      0 -> Left missing
      1 -> Right ()
      _ -> Left duplicate


-- | Constructs a transport configuration from already validated entries.
makeTransportConfig :: Map TransportName TransportSpec -> TransportConfig
makeTransportConfig = TransportConfig


-- | Looks up an external transport by its public name.
lookupTransport
  :: Text -> TransportConfig -> Either TransportLookupError TransportSpec
lookupTransport name (TransportConfig transports) = do
  parsed <-
    case parseTransportName name of
      Left err -> Left $ InvalidLookupTransportName err
      Right value -> Right value
  case Map.lookup parsed transports of
    Nothing -> Left $ UnknownTransportName name
    Just transport -> Right transport


-- | Builds a deterministic child environment without transcoding opaque host
-- values. Fixed entries override inherited entries according to the selected
-- platform's environment-name comparison rules.  If fixed names differ only
-- by case on a case-insensitive platform, the lexicographically greatest
-- configured spelling wins.
resolveTransportEnvironmentNative
  :: EnvironmentNameCase
  -- ^ Platform-specific environment-name comparison.
  -> [(String, String)]
  -- ^ Complete host environment in its native representation.
  -> TransportSpec
  -- ^ Validated transport specification.
  -> [(String, String)]
resolveTransportEnvironmentNative nameCase host transport =
  resolveExternalEnvironmentNative
    nameCase
    host
    transport.inheritedEnvironment
    transport.environment


-- | Expands placeholders without transcoding opaque source and destination
-- arguments.  Configured command literals are decoded from their validated
-- Unicode representation, while native arguments remain unchanged.
expandTransportCommandNative
  :: TransportSpec
  -> String
  -- ^ Opaque native source argument.
  -> String
  -- ^ Opaque native destination argument.
  -> (FilePath, [String])
expandTransportCommandNative transport source destination =
  case transport.command of
    executable :| arguments ->
      (Text.unpack executable, replace <$> arguments)
 where
  replace "{source}" = source
  replace "{destination}" = destination
  replace argument = Text.unpack argument
