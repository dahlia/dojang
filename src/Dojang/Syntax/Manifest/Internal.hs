{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dojang.Syntax.Manifest.Internal
  ( FileRoute'
  , FileRouteMap'
  , EnvironmentPredicate' (..)
  , FlatOrNonEmptyStrings (..)
  , Manifest' (..)
  , MonikerMap'
  , always
  ) where

import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Prelude hiding (all, any)

import Data.CaseInsensitive (CI (original))
import Data.Map.Strict (Map)
import Data.Text (Text, pack, unpack)
import Dojang.Types.MonikerName (MonikerName, parseMonikerName)
import Toml (Value (..))
import Toml.FromValue (FromKey (..), FromValue (..), parseTableFromValue)
import Toml.FromValue.Generic (genericParseTable)
import Toml.FromValue.Matcher (Matcher)
import Toml.ToValue
  ( ToKey (..)
  , ToTable (toTable)
  , ToValue (toValue)
  , defaultTableToValue
  )
import Toml.ToValue.Generic (genericToTable)


data FlatOrNonEmptyStrings
  = Flat String
  | NonEmpty (NonEmpty String)
  deriving (Eq, Show)


typeError :: Value -> String -> Matcher a
typeError actual wanted =
  fail $ "type error. wanted: " ++ wanted ++ " got: " ++ case actual of
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
  , all :: Maybe (NonEmpty MonikerName)
  , any :: Maybe (NonEmpty MonikerName)
  , when :: Maybe Text
  }
  deriving (Eq, Show, Generic)


always :: EnvironmentPredicate'
always =
  EnvironmentPredicate'
    { os = Nothing
    , arch = Nothing
    , all = Nothing
    , any = Nothing
    , when = Nothing
    }


instance FromValue EnvironmentPredicate' where
  fromValue = parseTableFromValue genericParseTable


instance ToValue EnvironmentPredicate' where
  toValue = defaultTableToValue


instance ToTable EnvironmentPredicate' where
  toTable = genericToTable


type MonikerMap' = Map MonikerName EnvironmentPredicate'


type FileRoute' = Map MonikerName Text


type FileRouteMap' = Map FilePath FileRoute'


data Manifest' = Manifest'
  { monikers :: MonikerMap'
  , dirs :: FileRouteMap'
  , files :: FileRouteMap'
  }
  deriving (Eq, Show, Generic)


instance FromValue Manifest' where
  fromValue = parseTableFromValue genericParseTable


instance ToValue Manifest' where
  toValue = defaultTableToValue


instance ToTable Manifest' where
  toTable = genericToTable
