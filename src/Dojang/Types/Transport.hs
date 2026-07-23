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
  , expandTransportCommand
  , lookupTransport
  , makeTransportConfig
  , makeTransportSpec
  , parseTransportName
  , resolveTransportEnvironment
  , transportNameText
  ) where

import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Dojang.Types.ManifestVariable (parseManifestVariableName)


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


-- | Whether environment names use POSIX or Windows comparison rules.
data EnvironmentNameCase
  = -- | Treat differently cased names as distinct, as on POSIX.
    CaseSensitiveEnvironment
  | -- | Treat differently cased names as equal, as on Windows.
    CaseInsensitiveEnvironment
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
  mapM_ validateEnvironmentName inherited
  mapM_ validateEnvironmentName $ Map.keys environment
  case firstDuplicate inherited of
    Just duplicate -> Left $ DuplicateInheritedEnvironmentName duplicate
    Nothing -> Right $ TransportSpec validatedCommand inherited environment
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
  validateEnvironmentName name =
    case parseManifestVariableName name of
      Left _ -> Left $ InvalidEnvironmentName name
      Right _ -> Right ()
  firstDuplicate = go Set.empty
   where
    go :: Set Text -> [Text] -> Maybe Text
    go _ [] = Nothing
    go seen (name : names)
      | Set.member name seen = Just name
      | otherwise = go (Set.insert name seen) names


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


-- | Builds a deterministic child environment from permitted host and fixed
-- entries. Fixed entries override inherited entries according to the selected
-- platform's environment-name comparison rules.
resolveTransportEnvironment
  :: EnvironmentNameCase
  -- ^ Platform-specific environment-name comparison.
  -> [(Text, Text)]
  -- ^ Complete host environment.
  -> TransportSpec
  -- ^ Validated transport specification.
  -> [(Text, Text)]
resolveTransportEnvironment nameCase host transport =
  Map.elems $
    foldl'
      insertFixed
      (foldl' inherit Map.empty transport.inheritedEnvironment)
      (Map.toAscList transport.environment)
 where
  canonical =
    case nameCase of
      CaseSensitiveEnvironment -> id
      CaseInsensitiveEnvironment -> Text.toCaseFold
  inherit result requested =
    case find ((== canonical requested) . canonical . fst) host of
      Nothing -> result
      Just (_, value) -> Map.insert (canonical requested) (requested, value) result
  insertFixed result (name, value) =
    Map.insert (canonical name) (name, value) result


-- | Expands the two whole-argument placeholders in a validated command.
expandTransportCommand :: TransportSpec -> Text -> Text -> (Text, [Text])
expandTransportCommand transport source destination =
  case transport.command of
    executable :| arguments ->
      (executable, replace <$> arguments)
 where
  replace "{source}" = source
  replace "{destination}" = destination
  replace argument = argument
