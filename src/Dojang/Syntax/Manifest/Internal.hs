{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dojang.Syntax.Manifest.Internal
  ( FileRoute' (..)
  , CodecBackend' (..)
  , FileRouteBranch' (..)
  , FileRouteMap'
  , EnvironmentPredicate' (..)
  , FlatOrNonEmptyStrings (..)
  , Hook' (..)
  , Hooks' (..)
  , IgnoreMap'
  , Manifest' (..)
  , MonikerMap'
  , ManifestVariable' (..)
  , ManifestVariableBranch' (..)
  , ManifestVariableMap'
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
import Dojang.Types.Codec
  ( CodecConfiguration (CodecConfiguration)
  , CodecSpec (CodecSpec)
  , CodecValue (..)
  , identityCodecSpec
  , parseCodecName
  , renderCodecName
  )
import Dojang.Types.ManifestVariable
  ( ManifestVariableName
  , parseManifestVariableName
  , renderManifestVariableName
  )
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
import qualified Toml.FromValue as FromValue
import Toml.FromValue.Matcher (Matcher)
import Toml.FromValue.ParseTable (ParseTable)
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


instance FromKey ManifestVariableName where
  fromKey key = case parseManifestVariableName $ pack key of
    Right name -> return name
    Left message -> fail $ unpack message


instance ToKey ManifestVariableName where
  toKey = unpack . renderManifestVariableName


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


data ManifestVariable'
  = CompactManifestVariable Text
  | DetailedManifestVariable [ManifestVariableBranch']
  deriving (Eq, Show)


instance FromValue ManifestVariable' where
  fromValue value@String{} = CompactManifestVariable <$> fromValue value
  fromValue value@Array{} = DetailedManifestVariable <$> fromValue value
  fromValue value = typeError value "string or array of variable branches"


instance ToValue ManifestVariable' where
  toValue (CompactManifestVariable value) = toValue value
  toValue (DetailedManifestVariable branches) = toValue branches


data ManifestVariableBranch' = ManifestVariableBranch'
  { variableMoniker :: Maybe MonikerName
  , variableCondition :: Maybe Text
  , variableValue :: Maybe Text
  , variableUnexpectedFields :: [Text]
  }
  deriving (Eq, Show)


instance FromValue ManifestVariableBranch' where
  fromValue = parseTableFromValue $ do
    moniker <- optKey "moniker"
    condition <- optKey "when"
    value <- optKey "value"
    unexpectedFields <- fmap pack . Map.keys <$> getTable
    setTable Map.empty
    return $
      ManifestVariableBranch' moniker condition value unexpectedFields


instance ToValue ManifestVariableBranch' where
  toValue = defaultTableToValue


instance ToTable ManifestVariableBranch' where
  toTable branch =
    table $
      maybeField "moniker" branch.variableMoniker
        ++ maybeField "when" branch.variableCondition
        ++ maybeField "value" branch.variableValue
   where
    maybeField :: (ToValue a) => String -> Maybe a -> [(String, Value)]
    maybeField key (Just value) = [(key, toValue value)]
    maybeField _ Nothing = []


type ManifestVariableMap' = Map ManifestVariableName ManifestVariable'


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
  , routeMode :: Maybe Text
  , routeKind :: Maybe Text
  , routeCodec :: Maybe CodecSpec
  , routeUnexpectedFields :: [Text]
  }
  deriving (Eq, Show)


instance FromValue FileRouteBranch' where
  fromValue = parseTableFromValue $ do
    moniker <- optKey "moniker"
    condition <- optKey "when"
    path <- optKey "path"
    mode <- optKey "mode"
    kind <- optKey "kind"
    codec <- optKey "codec"
    unexpectedFields <- fmap pack . Map.keys <$> getTable
    setTable Map.empty
    return $
      FileRouteBranch' moniker condition path mode kind codec unexpectedFields


instance ToValue FileRouteBranch' where
  toValue = defaultTableToValue


instance ToTable FileRouteBranch' where
  toTable branch =
    table $
      maybeField "moniker" branch.routeMoniker
        ++ maybeField "when" branch.routeCondition
        ++ maybeField "path" branch.routePath
        ++ maybeField "mode" branch.routeMode
        ++ maybeField "kind" branch.routeKind
        ++ maybeField "codec" branch.routeCodec
   where
    maybeField :: (ToValue a) => String -> Maybe a -> [(String, Value)]
    maybeField key (Just value) = [(key, toValue value)]
    maybeField _ Nothing = []


instance FromValue CodecValue where
  fromValue (String value) = return $ CodecString $ pack value
  fromValue (Integer value) = return $ CodecInteger value
  fromValue (Bool value) = return $ CodecBoolean value
  fromValue (Array values) = CodecArray <$> traverse fromValue values
  fromValue (Table values) =
    CodecTable . Map.fromList
      <$> traverse
        (\(key, value) -> (,) (pack key) <$> fromValue value)
        (Map.toList values)
  fromValue value =
    typeError value "string, integer, boolean, array, or table"


instance ToValue CodecValue where
  toValue (CodecString value) = String $ unpack value
  toValue (CodecInteger value) = Integer value
  toValue (CodecBoolean value) = Bool value
  toValue (CodecArray values) = Array $ toValue <$> values
  toValue (CodecTable values) =
    Table $
      Map.fromList
        [(unpack key, toValue value) | (key, value) <- Map.toAscList values]


instance FromValue CodecConfiguration where
  fromValue value@(Table _) = do
    CodecTable values <- fromValue value
    return $ CodecConfiguration values
  fromValue value = typeError value "table"


instance ToValue CodecConfiguration where
  toValue (CodecConfiguration values) = toValue $ CodecTable values


instance FromValue CodecSpec where
  fromValue (String value) = makeCodecSpec (pack value) $ CodecConfiguration Map.empty
  fromValue value@(Table _) = parseTableFromValue parseCodecTable value
  fromValue value = typeError value "string or codec table"


parseCodecTable :: ParseTable CodecSpec
parseCodecTable = do
  name <- FromValue.reqKey "name"
  configuration <- fromMaybe (CodecConfiguration Map.empty) <$> optKey "config"
  unexpectedFields <- Map.keys <$> getTable
  setTable Map.empty
  case unexpectedFields of
    [] -> makeCodecSpec (pack name) configuration
    fields -> fail $ "unexpected codec fields: " ++ show fields


makeCodecSpec :: (MonadFail m) => Text -> CodecConfiguration -> m CodecSpec
makeCodecSpec name configuration = case parseCodecName name of
  Just name' -> return $ CodecSpec name' configuration
  Nothing -> fail "codec name must not be empty"


instance ToValue CodecSpec where
  toValue codec@(CodecSpec name configuration)
    | codec == identityCodecSpec = toValue (renderCodecName name)
    | configuration == CodecConfiguration Map.empty =
        toValue $ renderCodecName name
    | otherwise =
        Table $
          Map.fromList
            [ ("name", toValue $ renderCodecName name)
            , ("config", toValue configuration)
            ]


type FileRouteMap' = Map FilePath FileRoute'


type IgnoreMap' = Map FilePath [FilePattern]


-- | A single hook configuration in detailed form.
data Hook' = Hook'
  { hookId :: Maybe Text
  -- ^ Stable identity for stateful execution policies.
  , policy :: Maybe Text
  -- ^ Execution policy (default: always).
  , changeKey :: Maybe Text
  -- ^ Explicit revision key for the on-change policy.
  , command :: Text
  -- ^ The executable path (required).
  , args :: Maybe [Text]
  -- ^ Command arguments (default: []).
  , moniker :: Maybe MonikerName
  -- ^ Run only when this moniker matches.
  , condition :: Maybe Text
  -- ^ Run only when this predicate matches (parsed from "when" in TOML).
  , workingDirectory :: Maybe Text
  -- ^ Working-directory expression (default: repository root).
  , ignoreFailure :: Maybe Bool
  -- ^ Continue on failure (default: False).
  }
  deriving (Eq, Show)


instance FromValue Hook' where
  fromValue =
    parseTableFromValue $
      Hook'
        <$> optKey "id"
        <*> optKey "policy"
        <*> optKey "change-key"
        <*> reqKey "command"
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
        ++ maybeField "id" hook.hookId
        ++ maybeField "policy" hook.policy
        ++ maybeField "change-key" hook.changeKey
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
  , preReflect :: Maybe [Hook']
  , postReflect :: Maybe [Hook']
  , preDiff :: Maybe [Hook']
  , postDiff :: Maybe [Hook']
  , preStatus :: Maybe [Hook']
  , postStatus :: Maybe [Hook']
  , preEdit :: Maybe [Hook']
  , postEdit :: Maybe [Hook']
  , preUnmanage :: Maybe [Hook']
  , postUnmanage :: Maybe [Hook']
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
    , preReflect = Nothing
    , postReflect = Nothing
    , preDiff = Nothing
    , postDiff = Nothing
    , preStatus = Nothing
    , postStatus = Nothing
    , preEdit = Nothing
    , postEdit = Nothing
    , preUnmanage = Nothing
    , postUnmanage = Nothing
    }


instance FromValue Hooks' where
  fromValue =
    parseTableFromValue $
      Hooks'
        <$> optKey "pre-apply"
        <*> optKey "pre-first-apply"
        <*> optKey "post-first-apply"
        <*> optKey "post-apply"
        <*> optKey "pre-reflect"
        <*> optKey "post-reflect"
        <*> optKey "pre-diff"
        <*> optKey "post-diff"
        <*> optKey "pre-status"
        <*> optKey "post-status"
        <*> optKey "pre-edit"
        <*> optKey "post-edit"
        <*> optKey "pre-unmanage"
        <*> optKey "post-unmanage"


instance ToValue Hooks' where
  toValue = defaultTableToValue


instance ToTable Hooks' where
  toTable hooks =
    table $
      maybeField "pre-apply" hooks.preApply
        ++ maybeField "pre-first-apply" hooks.preFirstApply
        ++ maybeField "post-first-apply" hooks.postFirstApply
        ++ maybeField "post-apply" hooks.postApply
        ++ maybeField "pre-reflect" hooks.preReflect
        ++ maybeField "post-reflect" hooks.postReflect
        ++ maybeField "pre-diff" hooks.preDiff
        ++ maybeField "post-diff" hooks.postDiff
        ++ maybeField "pre-status" hooks.preStatus
        ++ maybeField "post-status" hooks.postStatus
        ++ maybeField "pre-edit" hooks.preEdit
        ++ maybeField "post-edit" hooks.postEdit
        ++ maybeField "pre-unmanage" hooks.preUnmanage
        ++ maybeField "post-unmanage" hooks.postUnmanage
   where
    maybeField :: (ToValue a) => String -> Maybe a -> [(String, Value)]
    maybeField key (Just v) = [(key, toValue v)]
    maybeField _ Nothing = []


data Manifest' = Manifest'
  { repositoryId :: Maybe Text
  , monikers :: MonikerMap'
  , variables :: ManifestVariableMap'
  , codecBackends :: Map Text CodecBackend'
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
        <*> (fromMaybe Map.empty <$> optKey "vars")
        <*> (fromMaybe Map.empty <$> optKey "codec-backends")
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
        ++ ( if Map.null manifest.variables
               then []
               else [("vars", toValue manifest.variables)]
           )
        ++ ( if Map.null manifest.codecBackends
               then []
               else [("codec-backends", toValue manifest.codecBackends)]
           )
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


data CodecBackend' = CodecBackend'
  { backendCommand :: Text
  , backendVersion :: Text
  , backendTimeoutSeconds :: Maybe Integer
  , backendOptions :: Map Text CodecValue
  }
  deriving (Eq, Show)


instance FromValue CodecBackend' where
  fromValue =
    parseTableFromValue $
      CodecBackend'
        <$> FromValue.reqKey "command"
        <*> FromValue.reqKey "version"
        <*> optKey "timeout-seconds"
        <*> (fromMaybe Map.empty <$> optKey "options")


instance ToValue CodecBackend' where
  toValue = defaultTableToValue


instance ToTable CodecBackend' where
  toTable backend =
    table $
      [ ("command", toValue backend.backendCommand)
      , ("version", toValue backend.backendVersion)
      ]
        ++ maybe
          []
          (\seconds -> [("timeout-seconds", toValue seconds)])
          backend.backendTimeoutSeconds
        ++ if Map.null backend.backendOptions
          then []
          else [("options", toValue backend.backendOptions)]
