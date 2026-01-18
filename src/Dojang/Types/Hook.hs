{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Hook
  ( Hook (..)
  , HookType (..)
  , HookMap
  , allHookTypes
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import System.OsPath (OsPath)

import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate)


-- | The type of hook, determining when it runs.
data HookType
  = -- | Runs before file sync (every apply).
    PreApply
  | -- | Runs before file sync (first apply only).
    PreFirstApply
  | -- | Runs after file sync (first apply only).
    PostFirstApply
  | -- | Runs after file sync (every apply).
    PostApply
  deriving (Eq, Show, Ord, Enum, Bounded)


-- | All hook types in execution order.
allHookTypes :: [HookType]
allHookTypes = [minBound .. maxBound]


-- | A hook configuration.
data Hook = Hook
  { command :: OsPath
  -- ^ The executable path (required).
  , args :: [Text]
  -- ^ Command arguments (default: []).
  , condition :: EnvironmentPredicate
  -- ^ Condition to run (combined from moniker and when fields).
  , workingDirectory :: Maybe OsPath
  -- ^ Working directory (default: repository root).
  , ignoreFailure :: Bool
  -- ^ Continue on failure (default: False).
  }
  deriving (Eq, Show)


-- | A map from hook types to lists of hooks.
type HookMap = Map HookType [Hook]
