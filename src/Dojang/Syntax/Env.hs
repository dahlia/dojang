{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dojang.Syntax.Env
  ( TomlError
  , TomlWarning
  , readEnvFile
  , readEnvironment
  , readFacts
  , readFactsFile
  , writeEnvFile
  , writeEnvironment
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Prelude hiding (readFile, writeFile)

import Control.DeepSeq (force)
import Data.CaseInsensitive (CI (original))
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.OsPath (OsPath)
import TextShow (FromStringShow (FromStringShow), TextShow (showt))
import Toml (decode, encode)
import Toml.FromValue
  ( FromKey (fromKey)
  , FromValue (fromValue)
  , Result (Failure, Success)
  , optKey
  , parseTableFromValue
  , reqKey
  )
import Toml.ToValue
  ( ToKey (toKey)
  , ToTable (toTable)
  , ToValue (toValue)
  , defaultTableToValue
  , table
  , (.=)
  )

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Environment
  ( Architecture
  , Environment (..)
  , FactKey
  , FactMap
  , FactValue
  , Kernel (..)
  , OperatingSystem
  , environmentFacts
  , factKeyText
  , factValueText
  , isBuiltInFact
  , lookupFact
  , parseFactKey
  , withFacts
  )
import Toml.FromValue.Matcher (Matcher)


instance FromValue OperatingSystem where
  fromValue os = fromString <$> fromValue os


instance ToValue OperatingSystem where
  toValue = toValue . original . (.identifier)


instance FromValue Architecture where
  fromValue arch = fromString <$> fromValue arch


instance ToValue Architecture where
  toValue = toValue . original . (.identifier)


instance FromKey FactKey where
  fromKey key = case parseFactKey $ pack key of
    Left message -> fail $ unpack message
    Right factKey -> return factKey


instance ToKey FactKey where
  toKey = unpack . factKeyText


instance FromValue FactValue where
  fromValue value = fromString <$> (fromValue value :: Matcher String)


instance ToValue FactValue where
  toValue = toValue . factValueText


instance FromValue Kernel where
  fromValue =
    parseTableFromValue $
      Kernel
        <$> (fromString <$> reqKey "name")
        <*> (fromString <$> reqKey "release")


instance ToValue Kernel where
  toValue = defaultTableToValue


instance ToTable Kernel where
  toTable kernel =
    table
      [ "name" .= original kernel.name
      , "release" .= original kernel.release
      ]


instance FromValue Environment where
  fromValue = parseTableFromValue $ do
    os' <- reqKey "os"
    arch' <- reqKey "arch"
    kernel' <- reqKey "kernel"
    hostname <- optKey "hostname"
    facts <- fromMaybe Map.empty <$> optKey "facts"
    let facts' = insertHostname hostname facts
    return $ withFacts facts' $ Environment os' arch' kernel'
   where
    insertHostname :: Maybe FactValue -> FactMap -> FactMap
    insertHostname Nothing = id
    insertHostname (Just value) = Map.insert "hostname" value


instance ToValue Environment where
  toValue = defaultTableToValue


instance ToTable Environment where
  toTable env =
    table $
      [ "os" .= env.operatingSystem
      , "arch" .= env.architecture
      , "kernel" .= env.kernel
      ]
        ++ maybe
          []
          (\hostname -> ["hostname" .= hostname])
          (lookupFact "hostname" env)
        ++ if Map.null customFacts then [] else ["facts" .= customFacts]
   where
    customFacts :: FactMap
    customFacts =
      Map.filterWithKey (\key _ -> not $ isBuiltInFact key) $
        environmentFacts env


newtype FactsDocument = FactsDocument FactMap


instance FromValue FactsDocument where
  fromValue =
    parseTableFromValue $
      makeFactsDocument
        <$> optKey "os"
        <*> optKey "arch"
        <*> optKey "kernel"
        <*> optKey "hostname"
        <*> optKey "facts"
   where
    makeFactsDocument
      :: Maybe OperatingSystem
      -> Maybe Architecture
      -> Maybe Kernel
      -> Maybe FactValue
      -> Maybe FactMap
      -> FactsDocument
    makeFactsDocument _ _ _ _ = FactsDocument . fromMaybe Map.empty


-- | An error made during parsing.
type TomlError = String


-- | A warning message made during parsing.
type TomlWarning = String


-- | Decodes a TOML-encoded 'Environment'.
readEnvironment
  :: Text
  -- ^ A TOML document text to parse.
  -> Either (NonEmpty TomlError) (Environment, [TomlWarning])
  -- ^ A decoded 'Environment' with warnings, or a list of errors.
readEnvironment toml = case decode $ unpack toml of
  Success warnings env -> Right (env, warnings)
  Failure (e : es) -> Left $ e :| es
  Failure [] -> Left $ "unknown error" :| []


-- | Decodes the user-defined @[facts]@ table from an environment document.
readFacts
  :: Text
  -> Either (NonEmpty TomlError) (FactMap, [TomlWarning])
readFacts toml = case decode $ unpack toml of
  Success warnings (FactsDocument facts) ->
    case filter isBuiltInFact $ Map.keys facts of
      [] -> Right (facts, warnings)
      key : keys ->
        Left $
          ( "The facts table must not override built-in machine fact: "
              ++ unpack (factKeyText key)
              ++ "."
          )
            :| [ "The facts table must not override built-in machine fact: "
                   ++ unpack (factKeyText reserved)
                   ++ "."
               | reserved <- keys
               ]
  Failure (e : es) -> Left $ e :| es
  Failure [] -> Left $ "unknown error" :| []


-- | Reads a TOML-encoded 'Environment' from the given file path.  It assumes
-- that the file is encoded in UTF-8.  Throws an 'IOError' if the file cannot
-- be read.
readEnvFile
  :: (MonadFileSystem m)
  => OsPath
  -- ^ A path to the env file.
  -> m (Either (NonEmpty TomlError) (Environment, [TomlWarning]))
  -- ^ A decoded 'Environment' with warnings, or error(s).
readEnvFile filePath = do
  content <- readFile filePath
  let decoded = decodeUtf8Lenient content
  return $ readEnvironment $ force decoded


-- | Reads the user-defined @[facts]@ table from an environment file.
readFactsFile
  :: (MonadFileSystem m)
  => OsPath
  -> m (Either (NonEmpty TomlError) (FactMap, [TomlWarning]))
readFactsFile filePath = do
  content <- readFile filePath
  let decoded = decodeUtf8Lenient content
  return $ readFacts $ force decoded


-- | Encodes an 'Environment' into a TOML document.
writeEnvironment :: Environment -> Text
writeEnvironment = showt . FromStringShow . encode


-- | Writes an 'Environment' file to the given path.  Throws an 'IOError' if
-- any occurs while writing the file.
writeEnvFile
  :: (MonadFileSystem m)
  => Environment
  -- ^ The 'Environment' to write.
  -> OsPath
  -- ^ The path to write the 'Environment' to.
  -> m ()
writeEnvFile env filePath =
  writeFile filePath $ encodeUtf8 $ writeEnvironment env
