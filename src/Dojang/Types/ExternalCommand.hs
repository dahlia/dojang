{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared validation and environment resolution for shell-free commands.
module Dojang.Types.ExternalCommand
  ( EnvironmentNameCase (..)
  , ExternalEnvironmentConfigurationError (..)
  , resolveExternalEnvironmentNative
  , validateExternalEnvironment
  ) where

import Data.Char (toLower)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Dojang.Types.ManifestVariable (parseManifestVariableName)


-- | Whether environment names use POSIX or Windows comparison rules.
data EnvironmentNameCase
  = -- | Treat differently cased names as distinct, as on POSIX.
    CaseSensitiveEnvironment
  | -- | Treat differently cased names as equal, as on Windows.
    CaseInsensitiveEnvironment
  deriving (Eq, Show)


-- | A problem with a shell-free command's child environment.
data ExternalEnvironmentConfigurationError
  = -- | An environment entry has a non-portable name.
    InvalidExternalEnvironmentName Text
  | -- | An environment name is inherited more than once.
    DuplicateExternalInheritedEnvironmentName Text
  deriving (Eq, Show)


-- | Validates portable inherited and fixed environment names.
validateExternalEnvironment
  :: [Text]
  -- ^ Host environment names to inherit.
  -> Map Text Text
  -- ^ Fixed child environment entries.
  -> Either ExternalEnvironmentConfigurationError ()
validateExternalEnvironment inherited fixed = do
  mapM_ validateName inherited
  mapM_ validateName $ Map.keys fixed
  case firstDuplicate inherited of
    Just duplicate ->
      Left $ DuplicateExternalInheritedEnvironmentName duplicate
    Nothing -> Right ()
 where
  validateName name =
    case parseManifestVariableName name of
      Left _ -> Left $ InvalidExternalEnvironmentName name
      Right _ -> Right ()
  firstDuplicate = go Set.empty
   where
    go _ [] = Nothing
    go seen (name : names)
      | Set.member name seen = Just name
      | otherwise = go (Set.insert name seen) names


-- | Builds a deterministic child environment from an explicit allowlist.
--
-- Fixed entries override inherited entries according to the platform's
-- environment-name comparison rules.  Values remain in the native 'String'
-- representation used at the process boundary.  If fixed names differ only
-- by case on a case-insensitive platform, the lexicographically greatest
-- configured spelling wins.  This preserves case-distinct entries on POSIX
-- while making the unavoidable Windows collapse deterministic.
resolveExternalEnvironmentNative
  :: EnvironmentNameCase
  -- ^ Platform-specific environment-name comparison.
  -> [(String, String)]
  -- ^ Complete host environment in its native representation.
  -> [Text]
  -- ^ Host environment names to inherit.
  -> Map Text Text
  -- ^ Fixed child environment entries.
  -> [(String, String)]
resolveExternalEnvironmentNative nameCase host inherited fixed =
  Map.elems $
    foldl'
      insertFixed
      (foldl' inherit Map.empty inherited)
      (Map.toAscList fixed)
 where
  canonical =
    case nameCase of
      CaseSensitiveEnvironment -> id
      CaseInsensitiveEnvironment -> fmap toLower
  inherit result requestedText =
    let requested = Text.unpack requestedText
    in case find ((== canonical requested) . canonical . fst) host of
         Nothing -> result
         Just (_, value) ->
           Map.insert (canonical requested) (requested, value) result
  insertFixed result (nameText, valueText) =
    let name = Text.unpack nameText
    in Map.insert
         (canonical name)
         (name, Text.unpack valueText)
         result
