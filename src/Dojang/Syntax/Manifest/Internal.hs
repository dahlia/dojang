{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dojang.Syntax.Manifest.Internal
  ( FileRoute' (..)
  , FileRouteBranch' (..)
  , FileRouteMap'
  , EnvironmentPredicate' (..)
  , FlatOrNonEmptyStrings (..)
  , Hook' (..)
  , Hooks' (..)
  , IgnoreMap'
  , Manifest' (..)
  , MonikerMap'
  , always
  , emptyHooks
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Prelude hiding (all, any)

import Data.CaseInsensitive (CI (original))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import Dojang.Types.MonikerName (MonikerName, parseMonikerName)
import System.FilePattern (FilePattern)
import Toml (Value (..))
import Toml.FromValue
  ( FromKey (..)
  , FromValue (..)
  , getTable
  , optKey
  , parseTableFromValue
  , setTable
  )
import Toml.FromValue.Matcher (Matcher)
import Toml.ToValue
  ( ToKey (..)
  , ToTable (toTable)
  , ToValue (toValue)
  , defaultTableToValue
  , table
  )


data FlatOrNonEmptyStrings
  = Flat String
  | NonEmpty (NonEmpty String)
  deriving (Eq, Show)


typeError :: Value -> String -> Matcher a
typeError actual wanted =
  fail $
    "type error. wanted: " ++ wanted ++ " got: " ++ case actual of
      Integer{} -> "integer"
      Float{} -> "float"
      String{} -> "string"
      Table{} -> "table"
      Array{} -> "array"
      Bool{} -> "boolean"
      TimeOfDay{} -> "local time"
      LocalTime{} -> "local date-time"
      Day{} -> "locate date"
      ZonedTime{} -> "offset date-time"


instance FromValue FlatOrNonEmptyStrings where
  fromValue v@Array{} = NonEmpty <$> fromValue v
  fromValue v@String{} = Flat <$> fromValue v
  fromValue v = typeError v "string or array of strings"


instance ToValue FlatOrNonEmptyStrings where
  toValue (Flat a) = toValue a
  toValue (NonEmpty as) = toValue as


instance FromKey MonikerName where
  fromKey key = case parseMonikerName $ pack key of
    Right monikerName -> return monikerName
    Left _ -> fail $ "invalid moniker name: " ++ show key


instance FromValue MonikerName where
  fromValue v@String{} = do
    result <- fromValue v
    case parseMonikerName result of
      Right monikerName -> return monikerName
      Left _ -> fail $ "invalid moniker name: " ++ show v
  fromValue v = typeError v "string"


instance ToKey MonikerName where
  toKey = unpack . original . (.name)


instance ToValue MonikerName where
  toValue monikerName = toValue $ original monikerName.name


data EnvironmentPredicate' = EnvironmentPredicate'
  { os :: Maybe FlatOrNonEmptyStrings
  , arch :: Maybe FlatOrNonEmptyStrings
  , kernel :: Maybe FlatOrNonEmptyStrings
  , kernelRelease :: Maybe FlatOrNonEmptyStrings
  , all :: Maybe (NonEmpty MonikerName)
  , any :: Maybe (NonEmpty MonikerName)
  , when :: Maybe Text
  }
  deriving (Eq, Show)


always :: EnvironmentPredicate'
always =
  EnvironmentPredicate'
    { os = Nothing
    , arch = Nothing
    , kernel = Nothing
    , kernelRelease = Nothing
    , all = Nothing
    , any = Nothing
    , when = Nothing
    }


instance FromValue EnvironmentPredicate' where
  fromValue =
    parseTableFromValue $
      EnvironmentPredicate'
        <$> optKey "os"
        <*> optKey "arch"
        <*> optKey "kernel"
        <*> optKey "kernel-release"
        <*> optKey "all"
        <*> optKey "any"
        <*> optKey "when"


instance ToValue EnvironmentPredicate' where
  toValue = defaultTableToValue


instance ToTable EnvironmentPredicate' where
  toTable pred' =
    table $
      fieldsToValue fields
        ++ fieldsToValue fields'
        ++ fieldsToValue
          [("when", pred'.when)]
   where
    fields :: [(String, Maybe FlatOrNonEmptyStrings)]
    fields =
      [ ("os", pred'.os)
      , ("arch", pred'.arch)
      , ("kernel", pred'.kernel)
      , ("kernel-release", pred'.kernelRelease)
      ]
    fields' :: [(String, Maybe (NonEmpty MonikerName))]
    fields' =
      [ ("all", pred'.all)
      , ("any", pred'.any)
      ]
    fieldsToValue :: (ToValue a) => [(String, Maybe a)] -> [(String, Value)]
    fieldsToValue fs = [(key, toValue value) | (key, Just value) <- fs]


type MonikerMap' = Map MonikerName EnvironmentPredicate'


data FileRoute'
  = CompactFileRoute (Map MonikerName Text)
  | DetailedFileRoute [FileRouteBranch']
  deriving (Eq, Show)


instance FromValue FileRoute' where
  fromValue value@Table{} = CompactFileRoute <$> fromValue value
  fromValue value@Array{} = DetailedFileRoute <$> fromValue value
  fromValue value = typeError value "table or array of route branches"


instance ToValue FileRoute' where
  toValue (CompactFileRoute route) = toValue route
  toValue (DetailedFileRoute branches) = toValue branches


data FileRouteBranch' = FileRouteBranch'
  { routeMoniker :: Maybe MonikerName
  , routeCondition :: Maybe Text
  , routePath :: Maybe Text
  , routeUnexpectedFields :: [Text]
  }
  deriving (Eq, Show)


instance FromValue FileRouteBranch' where
  fromValue = parseTableFromValue $ do
    moniker <- optKey "moniker"
    condition <- optKey "when"
    path <- optKey "path"
    unexpectedFields <- fmap pack . Map.keys <$> getTable
    setTable Map.empty
    return $ FileRouteBranch' moniker condition path unexpectedFields


instance ToValue FileRouteBranch' where
  toValue = defaultTableToValue


instance ToTable FileRouteBranch' where
  toTable branch =
    table $
      maybeField "moniker" branch.routeMoniker
        ++ maybeField "when" branch.routeCondition
        ++ maybeField "path" branch.routePath
   where
    maybeField :: (ToValue a) => String -> Maybe a -> [(String, Value)]
    maybeField key (Just value) = [(key, toValue value)]
    maybeField _ Nothing = []


type FileRouteMap' = Map FilePath FileRoute'


type IgnoreMap' = Map FilePath [FilePattern]


-- | A single hook configuration in detailed form.
data Hook' = Hook'
  { command :: Text
  -- ^ The executable path (required).
  , args :: Maybe [Text]
  -- ^ Command arguments (default: []).
  , moniker :: Maybe MonikerName
  -- ^ Run only when this moniker matches.
  , condition :: Maybe Text
  -- ^ Run only when this predicate matches (parsed from "when" in TOML).
  , workingDirectory :: Maybe FilePath
  -- ^ Working directory (default: repository root).
  , ignoreFailure :: Maybe Bool
  -- ^ Continue on failure (default: False).
  }
  deriving (Eq, Show)


instance FromValue Hook' where
  fromValue =
    parseTableFromValue $
      Hook'
        <$> reqKey "command"
        <*> optKey "args"
        <*> optKey "moniker"
        <*> optKey "when"
        <*> optKey "working-directory"
        <*> optKey "ignore-failure"
   where
    reqKey key =
      optKey key >>= \case
        Just v -> return v
        Nothing -> fail $ "missing required key: " ++ key


instance ToValue Hook' where
  toValue = defaultTableToValue


instance ToTable Hook' where
  toTable hook =
    table $
      [("command", toValue hook.command)]
        ++ maybeField "args" hook.args
        ++ maybeField "moniker" hook.moniker
        ++ maybeField "when" hook.condition
        ++ maybeField "working-directory" hook.workingDirectory
        ++ maybeField "ignore-failure" hook.ignoreFailure
   where
    maybeField :: (ToValue a) => String -> Maybe a -> [(String, Value)]
    maybeField key (Just v) = [(key, toValue v)]
    maybeField _ Nothing = []


-- | Hooks section of the manifest.
data Hooks' = Hooks'
  { preApply :: Maybe [Hook']
  , preFirstApply :: Maybe [Hook']
  , postFirstApply :: Maybe [Hook']
  , postApply :: Maybe [Hook']
  }
  deriving (Eq, Show)


-- | Empty hooks (no hooks defined).
emptyHooks :: Hooks'
emptyHooks =
  Hooks'
    { preApply = Nothing
    , preFirstApply = Nothing
    , postFirstApply = Nothing
    , postApply = Nothing
    }


instance FromValue Hooks' where
  fromValue =
    parseTableFromValue $
      Hooks'
        <$> optKey "pre-apply"
        <*> optKey "pre-first-apply"
        <*> optKey "post-first-apply"
        <*> optKey "post-apply"


instance ToValue Hooks' where
  toValue = defaultTableToValue


instance ToTable Hooks' where
  toTable hooks =
    table $
      maybeField "pre-apply" hooks.preApply
        ++ maybeField "pre-first-apply" hooks.preFirstApply
        ++ maybeField "post-first-apply" hooks.postFirstApply
        ++ maybeField "post-apply" hooks.postApply
   where
    maybeField :: (ToValue a) => String -> Maybe a -> [(String, Value)]
    maybeField key (Just v) = [(key, toValue v)]
    maybeField _ Nothing = []


data Manifest' = Manifest'
  { repositoryId :: Maybe Text
  , monikers :: MonikerMap'
  , dirs :: FileRouteMap'
  , files :: FileRouteMap'
  , ignores :: IgnoreMap'
  , hooks :: Maybe Hooks'
  }
  deriving (Eq, Show)


instance FromValue Manifest' where
  fromValue =
    parseTableFromValue $
      Manifest'
        <$> optKey "repository-id"
        <*> (fromMaybe Map.empty <$> optKey "monikers")
        <*> (fromMaybe Map.empty <$> optKey "dirs")
        <*> (fromMaybe Map.empty <$> optKey "files")
        <*> (fromMaybe Map.empty <$> optKey "ignores")
        <*> optKey "hooks"


instance ToValue Manifest' where
  toValue = defaultTableToValue


instance ToTable Manifest' where
  toTable manifest =
    table $
      maybeField "repository-id" manifest.repositoryId
        ++ [ ("monikers", toValue manifest.monikers)
           , ("dirs", toValue manifest.dirs)
           , ("files", toValue manifest.files)
           , ("ignores", toValue manifest.ignores)
           ]
        ++ maybeField "hooks" manifest.hooks
   where
    maybeField :: (ToValue a) => String -> Maybe a -> [(String, Value)]
    maybeField key (Just value) = [(key, toValue value)]
    maybeField _ Nothing = []
