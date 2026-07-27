{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Configuration and argument expansion for shell-free merge drivers.
module Dojang.Types.MergeDriver
  ( MergeDriverConfig (..)
  , MergeDriverConfigurationError (..)
  , MergeDriverExit (..)
  , MergeDriverLookupError (..)
  , MergeDriverName
  , MergeDriverNameError (..)
  , MergeDriverSpec (..)
  , classifyMergeDriverExit
  , expandMergeDriverCommandNative
  , lookupMergeDriver
  , makeMergeDriverSpec
  , parseMergeDriverName
  , renderMergeDriverName
  , resolveMergeDriverEnvironmentNative
  ) where

import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.List (find, group, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.Exit (ExitCode (..))

import Dojang.Types.ExternalCommand
  ( EnvironmentNameCase
  , ExternalEnvironmentConfigurationError (..)
  , resolveExternalEnvironmentNative
  , validateExternalEnvironment
  )


-- | A case-sensitive, portable merge-driver name.
newtype MergeDriverName = MergeDriverName Text
  deriving (Eq, Ord, Show)


-- | A problem with a merge-driver name.
data MergeDriverNameError
  = -- | The name is empty.
    EmptyMergeDriverName
  | -- | The name does not start with an ASCII letter.
    InvalidMergeDriverNameStart
  | -- | The name contains a character outside the portable grammar.
    InvalidMergeDriverNameCharacter
  deriving (Eq, Show)


-- | Parses a case-sensitive merge-driver name.
parseMergeDriverName :: Text -> Either MergeDriverNameError MergeDriverName
parseMergeDriverName value
  | Text.null value = Left EmptyMergeDriverName
  | not $ validFirst $ Text.head value = Left InvalidMergeDriverNameStart
  | not $ Text.all validRest value = Left InvalidMergeDriverNameCharacter
  | otherwise = Right $ MergeDriverName value
 where
  validFirst character =
    isAscii character && (isAsciiLower character || isAsciiUpper character)
  validRest character =
    validFirst character
      || isDigit character
      || character == '-'
      || character == '_'


-- | Renders a merge-driver name exactly as configured.
renderMergeDriverName :: MergeDriverName -> Text
renderMergeDriverName (MergeDriverName value) = value


-- | One merge command and its deliberately limited environment.
data MergeDriverSpec = MergeDriverSpec
  { command :: NonEmpty Text
  -- ^ Executable followed by individual arguments.
  , inheritedEnvironment :: [Text]
  -- ^ Host environment names copied into the child environment.
  , environment :: Map Text Text
  -- ^ Fixed child environment values, overriding inherited values.
  , unresolvedExitCodes :: [Int]
  -- ^ Nonzero exit codes that mean the conflict remains unresolved.
  , canceledExitCodes :: [Int]
  -- ^ Nonzero exit codes that mean the user canceled the merge.
  }
  deriving (Eq, Show)


-- | Named merge drivers and the default selection.
data MergeDriverConfig = MergeDriverConfig
  { defaultDriver :: MergeDriverName
  -- ^ Driver selected when the command does not name one explicitly.
  , drivers :: Map MergeDriverName MergeDriverSpec
  -- ^ All configured drivers.
  }
  deriving (Eq, Show)


-- | A problem with one configured merge driver.
data MergeDriverConfigurationError
  = -- | No executable or arguments were configured.
    EmptyMergeDriverCommand
  | -- | The configured executable is empty.
    EmptyMergeDriverExecutable
  | -- | A merge placeholder appears in the executable.
    MergePlaceholderInExecutable Text
  | -- | No whole-argument source placeholder is present.
    MissingMergeSourcePlaceholder
  | -- | More than one source placeholder is present.
    DuplicateMergeSourcePlaceholder
  | -- | No whole-argument base placeholder is present.
    MissingMergeBasePlaceholder
  | -- | More than one base placeholder is present.
    DuplicateMergeBasePlaceholder
  | -- | More than one destination placeholder is present.
    DuplicateMergeDestinationPlaceholder
  | -- | No whole-argument result placeholder is present.
    MissingMergeResultPlaceholder
  | -- | More than one result placeholder is present.
    DuplicateMergeResultPlaceholder
  | -- | A placeholder is embedded inside another argument.
    EmbeddedMergePlaceholder Text
  | -- | An exit code is zero, negative, or does not fit in an 'Int'.
    InvalidMergeDriverExitCode Integer
  | -- | An outcome list repeats one exit code.
    DuplicateMergeDriverExitCode Int
  | -- | One exit code denotes both unresolved and canceled outcomes.
    AmbiguousMergeDriverExitCode Int
  | -- | An environment entry has a non-portable name.
    InvalidMergeDriverEnvironmentName Text
  | -- | An environment name is inherited more than once.
    DuplicateMergeDriverInheritedEnvironmentName Text
  deriving (Eq, Show)


-- | A problem selecting a configured merge driver.
data MergeDriverLookupError
  = -- | An explicit name is syntactically invalid.
    InvalidLookupMergeDriverName MergeDriverNameError
  | -- | The requested name is not configured.
    UnknownMergeDriverName Text
  deriving (Eq, Show)


-- | The semantic outcome of a merge-driver exit status.
data MergeDriverExit
  = -- | The driver produced a candidate resolved result.
    MergeDriverResolved
  | -- | The driver left the conflict unresolved.
    MergeDriverUnresolved
  | -- | The user canceled the merge.
    MergeDriverCanceled
  | -- | The driver failed with an unclassified exit code.
    MergeDriverFailed Int
  deriving (Eq, Show)


-- | Validates and constructs one merge-driver specification.
makeMergeDriverSpec
  :: [Text]
  -- ^ Executable followed by individual arguments.
  -> [Text]
  -- ^ Host environment names to inherit.
  -> Map Text Text
  -- ^ Fixed environment entries.
  -> [Integer]
  -- ^ Exit codes that denote unresolved conflicts.
  -> [Integer]
  -- ^ Exit codes that denote cancellation.
  -> Either MergeDriverConfigurationError MergeDriverSpec
makeMergeDriverSpec command inherited fixed unresolved canceled = do
  validatedCommand <- case command of
    [] -> Left EmptyMergeDriverCommand
    executable : arguments -> Right $ executable :| arguments
  let executable = NonEmpty.head validatedCommand
  if Text.null executable
    then Left EmptyMergeDriverExecutable
    else Right ()
  if containsPlaceholder executable
    then Left $ MergePlaceholderInExecutable executable
    else Right ()
  mapM_ rejectEmbeddedPlaceholder $ drop 1 command
  requireExactlyOne
    "{source}"
    MissingMergeSourcePlaceholder
    DuplicateMergeSourcePlaceholder
  requireExactlyOne
    "{base}"
    MissingMergeBasePlaceholder
    DuplicateMergeBasePlaceholder
  requireAtMostOne "{destination}" DuplicateMergeDestinationPlaceholder
  requireExactlyOne
    "{result}"
    MissingMergeResultPlaceholder
    DuplicateMergeResultPlaceholder
  unresolved' <- validateExitCodes unresolved
  canceled' <- validateExitCodes canceled
  case find (`elem` canceled') unresolved' of
    Just code -> Left $ AmbiguousMergeDriverExitCode code
    Nothing -> Right ()
  case validateExternalEnvironment inherited fixed of
    Left (InvalidExternalEnvironmentName name) ->
      Left $ InvalidMergeDriverEnvironmentName name
    Left (DuplicateExternalInheritedEnvironmentName name) ->
      Left $ DuplicateMergeDriverInheritedEnvironmentName name
    Right () -> Right ()
  Right $
    MergeDriverSpec
      validatedCommand
      inherited
      fixed
      unresolved'
      canceled'
 where
  placeholders = ["{source}", "{base}", "{destination}", "{result}"]
  containsPlaceholder value =
    any (`Text.isInfixOf` value) placeholders
  rejectEmbeddedPlaceholder value
    | containsPlaceholder value && value `notElem` placeholders =
        Left $ EmbeddedMergePlaceholder value
    | otherwise = Right ()
  requireExactlyOne placeholder missing duplicate =
    case length $ filter (== placeholder) command of
      0 -> Left missing
      1 -> Right ()
      _ -> Left duplicate
  requireAtMostOne placeholder duplicate =
    case length $ filter (== placeholder) command of
      0 -> Right ()
      1 -> Right ()
      _ -> Left duplicate
  validateExitCodes values = do
    converted <- traverse validateExitCode values
    case repeated converted of
      Just code -> Left $ DuplicateMergeDriverExitCode code
      Nothing -> Right converted
  validateExitCode value
    | value <= 0 = Left $ InvalidMergeDriverExitCode value
    | value > fromIntegral (maxBound :: Int) =
        Left $ InvalidMergeDriverExitCode value
    | otherwise = Right $ fromIntegral value
  repeated values =
    case [value | values'@(value : _) <- group $ sort values, length values' > 1] of
      value : _ -> Just value
      [] -> Nothing


-- | Selects the explicit or configured-default merge driver.
lookupMergeDriver
  :: Maybe Text
  -- ^ Optional explicit driver name.
  -> MergeDriverConfig
  -- ^ Available drivers and default.
  -> Either MergeDriverLookupError (MergeDriverName, MergeDriverSpec)
lookupMergeDriver requested config = do
  name <- case requested of
    Nothing -> Right config.defaultDriver
    Just value ->
      case parseMergeDriverName value of
        Left err -> Left $ InvalidLookupMergeDriverName err
        Right parsed -> Right parsed
  case Map.lookup name config.drivers of
    Nothing ->
      Left $
        UnknownMergeDriverName $
          maybe (renderMergeDriverName name) id requested
    Just driver -> Right (name, driver)


-- | Classifies a merge-driver process exit status.
classifyMergeDriverExit :: MergeDriverSpec -> ExitCode -> MergeDriverExit
classifyMergeDriverExit _ ExitSuccess = MergeDriverResolved
classifyMergeDriverExit driver (ExitFailure code)
  | code `elem` driver.unresolvedExitCodes = MergeDriverUnresolved
  | code `elem` driver.canceledExitCodes = MergeDriverCanceled
  | otherwise = MergeDriverFailed code


-- | Expands merge path placeholders as complete native arguments.
expandMergeDriverCommandNative
  :: MergeDriverSpec
  -> String
  -- ^ Isolated source input.
  -> String
  -- ^ Isolated common-ancestor input.
  -> String
  -- ^ Isolated destination input.
  -> String
  -- ^ Driver result path, initialized from the destination.
  -> (FilePath, [String])
expandMergeDriverCommandNative driver source base destination result =
  case driver.command of
    executable :| arguments ->
      (Text.unpack executable, replace <$> arguments)
 where
  replace "{source}" = source
  replace "{base}" = base
  replace "{destination}" = destination
  replace "{result}" = result
  replace argument = Text.unpack argument


-- | Resolves the driver's allowlisted child environment.
--
-- On a case-insensitive platform, a fixed name overrides inherited spellings.
-- If multiple fixed names differ only by case, the lexicographically greatest
-- configured spelling wins.
resolveMergeDriverEnvironmentNative
  :: EnvironmentNameCase
  -> [(String, String)]
  -> MergeDriverSpec
  -> [(String, String)]
resolveMergeDriverEnvironmentNative nameCase host driver =
  resolveExternalEnvironmentNative
    nameCase
    host
    driver.inheritedEnvironment
    driver.environment
