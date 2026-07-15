{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Hook
  ( Hook (..)
  , HookId
  , HookPolicy (..)
  , HookType (..)
  , HookMap
  , allHookTypes
  , duplicateStatefulHookIds
  , parseHookId
  , renderHookId
  , renderHookPolicy
  , renderHookType
  , validateHookConfiguration
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (group, sort)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
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
  | -- | Runs before reflecting target files.
    PreReflect
  | -- | Runs after reflecting target files.
    PostReflect
  | -- | Runs before comparing source and target files.
    PreDiff
  | -- | Runs after comparing source and target files.
    PostDiff
  | -- | Runs before reporting repository status.
    PreStatus
  | -- | Runs after reporting repository status.
    PostStatus
  | -- | Runs before editing a managed file.
    PreEdit
  | -- | Runs after editing a managed file.
    PostEdit
  | -- | Runs before stopping management of paths.
    PreUnmanage
  | -- | Runs after stopping management of paths.
    PostUnmanage
  deriving (Eq, Show, Ord, Enum, Bounded)


-- | All hook types in execution order.
allHookTypes :: [HookType]
allHookTypes = [minBound .. maxBound]


-- | Renders a hook lifecycle event as its manifest spelling.
renderHookType :: HookType -> Text
renderHookType PreApply = "pre-apply"
renderHookType PreFirstApply = "pre-first-apply"
renderHookType PostFirstApply = "post-first-apply"
renderHookType PostApply = "post-apply"
renderHookType PreReflect = "pre-reflect"
renderHookType PostReflect = "post-reflect"
renderHookType PreDiff = "pre-diff"
renderHookType PostDiff = "post-diff"
renderHookType PreStatus = "pre-status"
renderHookType PostStatus = "post-status"
renderHookType PreEdit = "pre-edit"
renderHookType PostEdit = "post-edit"
renderHookType PreUnmanage = "pre-unmanage"
renderHookType PostUnmanage = "post-unmanage"


-- | A stable identifier used to record stateful hook executions.
newtype HookId = HookId {unHookId :: Text}
  deriving (Eq, Show, Ord)


-- | Parses a portable hook identifier.
--
-- Identifiers start with an ASCII letter and otherwise contain only ASCII
-- letters, digits, periods, underscores, or hyphens.
parseHookId :: Text -> Either Text HookId
parseHookId value =
  case Text.uncons value of
    Just (first, rest)
      | isAsciiLetter first && Text.all isContinuation rest ->
          Right $ HookId value
    _ ->
      Left "expected an ASCII letter followed by letters, digits, '.', '_', or '-'"
 where
  isAsciiLetter c = isAsciiLower c || isAsciiUpper c
  isContinuation c = isAsciiLetter c || isDigit c || c `elem` ("._-" :: String)


-- | Renders a hook identifier.
renderHookId :: HookId -> Text
renderHookId (HookId value) = value


-- | Controls when a hook is eligible to run.
data HookPolicy
  = -- | Runs whenever its lifecycle event occurs.
    HookAlways
  | -- | Runs only until its first successful execution on this machine.
    HookOnce
  | -- | Runs when its explicitly keyed configuration or context changes.
    HookOnChange
  deriving (Eq, Show, Ord, Enum, Bounded)


-- | Renders a hook policy as its manifest spelling.
renderHookPolicy :: HookPolicy -> Text
renderHookPolicy HookAlways = "always"
renderHookPolicy HookOnce = "once"
renderHookPolicy HookOnChange = "on-change"


-- | A hook configuration.
data Hook = Hook
  { hookId :: Maybe HookId
  -- ^ Stable identity for stateful policies.
  , policy :: HookPolicy
  -- ^ Execution policy (default: 'HookAlways').
  , changeKey :: Maybe Text
  -- ^ Explicit revision key required by 'HookOnChange'.
  , command :: OsPath
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


-- | Validates the relationships between a hook's policy fields.
--
-- Stateful policies require a stable identifier.  Only 'HookOnChange' accepts
-- a nonempty change key, and that policy requires one.
validateHookConfiguration :: Hook -> Either Text ()
validateHookConfiguration (Hook identifier policy' key _ _ _ _ _) =
  case (policy', identifier, key) of
    (HookAlways, _, Nothing) -> Right ()
    (HookAlways, _, Just _) ->
      Left "change-key is only valid with policy = on-change."
    (HookOnce, Nothing, _) ->
      Left "Hooks with policy = once require an id."
    (HookOnce, Just _, Nothing) -> Right ()
    (HookOnce, Just _, Just _) ->
      Left "change-key is only valid with policy = on-change."
    (HookOnChange, Nothing, _) ->
      Left "Hooks with policy = on-change require an id."
    (HookOnChange, Just _, Nothing) ->
      Left "Hooks with policy = on-change require change-key."
    (HookOnChange, Just _, Just "") ->
      Left "Hook change-key must not be empty."
    (HookOnChange, Just _, Just _) -> Right ()


-- | Finds stable IDs repeated by stateful hooks in one lifecycle event.
duplicateStatefulHookIds
  :: [Hook]
  -- ^ Hooks belonging to one lifecycle event.
  -> [HookId]
  -- ^ Duplicate IDs in sorted order, with each ID returned once.
duplicateStatefulHookIds hooks =
  [ identifier
  | identifiers@(identifier : _) <- group $ sort statefulIds
  , length identifiers > 1
  ]
 where
  statefulIds =
    [ identifier
    | Hook (Just identifier) policy' _ _ _ _ _ _ <- hooks
    , policy' /= HookAlways
    ]


-- | A map from hook types to lists of hooks.
type HookMap = Map HookType [Hook]
