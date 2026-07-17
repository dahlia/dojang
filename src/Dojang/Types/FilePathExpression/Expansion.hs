{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.FilePathExpression.Expansion
  ( ExpansionWarning (..)
  , VariableGetter
  , VariableLookup (..)
  , expandFilePath
  , expandFilePathWithVariables
  , simpleVariableGetter
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.OsPath
  ( OsPath
  , OsString
  , pack
  , pathSeparator
  , unsafeFromChar
  , (</>)
  )

import Dojang.Types.FilePathExpression
  ( EnvironmentVariable
  , FilePathExpression (..)
  )


-- | A set of warnings that can occur during expansion.
newtype ExpansionWarning
  = -- | The case where an environment variable that is referenced in a
    -- 'Substitution' is not defined.
    UndefinedEnvironmentVariable EnvironmentVariable
  deriving (Eq, Show)


-- | The result of looking up one path-expression variable.
data VariableLookup = VariableLookup
  { value :: Maybe OsString
  -- ^ Expanded value, or 'Nothing' when the variable is undefined.
  , warnings :: [ExpansionWarning]
  -- ^ Warnings produced while resolving a declarative value.
  , provenance :: Map Text Text
  -- ^ Privacy-preserving fingerprints of inputs used to derive the value.
  }
  deriving (Eq, Show)


-- | A warning- and provenance-aware variable lookup function.
type VariableGetter m = EnvironmentVariable -> m VariableLookup


-- | Adapts a value-only lookup to a lookup without nested warnings or
-- provenance.  This is useful for compatibility APIs and isolated tests.
simpleVariableGetter
  :: (Monad m)
  => (EnvironmentVariable -> m (Maybe OsString))
  -- ^ Value-only variable lookup to adapt.
  -> VariableGetter m
  -- ^ Lookup that returns no nested warnings or provenance.
simpleVariableGetter getter variable = do
  value <- getter variable
  return $ VariableLookup value [] Map.empty


-- | Expands a 'FilePathExpression' into an 'OsPath'.
expandFilePath
  :: (Monad m)
  => FilePathExpression
  -- ^ The 'FilePathExpression' to expand.
  -> (EnvironmentVariable -> m (Maybe OsString))
  -- ^ A function that can look up an environment variable.
  -> (Text -> m OsString)
  -- ^ A function that encodes a 'Text' into an 'OsString'.
  -> m (OsPath, [ExpansionWarning])
  -- ^ The expanded 'OsPath', along with warnings.
expandFilePath expression lookupEnv encode = do
  (path, warnings, _) <-
    expandFilePathWithVariables expression lookupVariable encode
  return (path, warnings)
 where
  lookupVariable = simpleVariableGetter lookupEnv


-- | Expands a path expression with lookup warnings and input provenance.
expandFilePathWithVariables
  :: (Monad m)
  => FilePathExpression
  -- ^ File-path expression to expand.
  -> VariableGetter m
  -- ^ Warning- and provenance-aware variable lookup.
  -> (Text -> m OsString)
  -- ^ Text encoder for literal path components.
  -> m (OsPath, [ExpansionWarning], Map Text Text)
  -- ^ Expanded path, warnings, and privacy-preserving provenance.
expandFilePathWithVariables (BareComponent component) _ encode = do
  value <- encode component
  return (value, [], Map.empty)
expandFilePathWithVariables (Root Nothing) _ _ =
  return (pack [pathSeparator], [], Map.empty)
expandFilePathWithVariables (Root (Just driveLetter)) _ _ =
  return
    ( pack
        [ unsafeFromChar driveLetter
        , unsafeFromChar ':'
        , pathSeparator
        ]
    , []
    , Map.empty
    )
expandFilePathWithVariables (Concatenation left right) lookupEnv encode = do
  (leftPath, leftWarnings, leftProvenance) <-
    expandFilePathWithVariables left lookupEnv encode
  (rightPath, rightWarnings, rightProvenance) <-
    expandFilePathWithVariables right lookupEnv encode
  return
    ( leftPath <> rightPath
    , leftWarnings <> rightWarnings
    , leftProvenance <> rightProvenance
    )
expandFilePathWithVariables (PathSeparator left right) lookupEnv encode = do
  (leftPath, leftWarnings, leftProvenance) <-
    expandFilePathWithVariables left lookupEnv encode
  (rightPath, rightWarnings, rightProvenance) <-
    expandFilePathWithVariables right lookupEnv encode
  return
    ( leftPath </> rightPath
    , leftWarnings <> rightWarnings
    , leftProvenance <> rightProvenance
    )
expandFilePathWithVariables (Substitution variable) lookupEnv _ = do
  found <- lookupEnv variable
  case found.value of
    Just value -> return (value, found.warnings, found.provenance)
    Nothing ->
      return
        ( mempty
        , found.warnings <> [UndefinedEnvironmentVariable variable]
        , found.provenance
        )
expandFilePathWithVariables
  (SubstitutionWithDefault variable fallback)
  lookupEnv
  encode = do
    found <- lookupEnv variable
    case found.value of
      Just value
        | value /= mempty ->
            return (value, found.warnings, found.provenance)
      _ -> do
        (fallbackPath, warnings, provenance) <-
          expandFilePathWithVariables fallback lookupEnv encode
        return
          ( fallbackPath
          , found.warnings <> warnings
          , found.provenance <> provenance
          )
expandFilePathWithVariables
  (ConditionalSubstitution variable conditional)
  lookupEnv
  encode = do
    found <- lookupEnv variable
    case found.value of
      Just value | value /= mempty -> do
        (conditionalPath, warnings, provenance) <-
          expandFilePathWithVariables conditional lookupEnv encode
        return
          ( conditionalPath
          , found.warnings <> warnings
          , found.provenance <> provenance
          )
      _ -> return (mempty, found.warnings, found.provenance)
