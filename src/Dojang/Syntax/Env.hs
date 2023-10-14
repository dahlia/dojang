{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dojang.Syntax.Env
  ( TomlError
  , TomlWarning
  , readEnvFile
  , readEnvironment
  , writeEnvFile
  , writeEnvironment
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString (fromString))
import Prelude hiding (readFile, writeFile)

import Control.DeepSeq (force)
import Data.ByteString (readFile, writeFile)
import Data.CaseInsensitive (CI (original))
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.OsPath (OsPath, decodeFS)
import TextShow (FromStringShow (FromStringShow), TextShow (showt))
import Toml (decode, encode)
import Toml.FromValue
  ( FromValue (fromValue)
  , Result (Failure, Success)
  , parseTableFromValue
  , reqKey
  )
import Toml.ToValue
  ( ToTable (toTable)
  , ToValue (toValue)
  , defaultTableToValue
  , table
  , (.=)
  )

import Dojang.Types.Environment
  ( Architecture
  , Environment (..)
  , OperatingSystem
  )


instance FromValue OperatingSystem where
  fromValue os = fromString <$> fromValue os


instance ToValue OperatingSystem where
  toValue = toValue . original . (.identifier)


instance FromValue Architecture where
  fromValue arch = fromString <$> fromValue arch


instance ToValue Architecture where
  toValue = toValue . original . (.identifier)


instance FromValue Environment where
  fromValue =
    parseTableFromValue
      $ Environment
      <$> reqKey "os"
      <*> reqKey "arch"


instance ToValue Environment where
  toValue = defaultTableToValue


instance ToTable Environment where
  toTable env =
    table
      [ "os" .= env.operatingSystem
      , "arch" .= env.architecture
      ]


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


-- | Reads a TOML-encoded 'Environment' from the given file path.  It assumes
-- that the file is encoded in UTF-8.
readEnvFile
  :: (MonadIO m)
  => OsPath
  -- ^ A path to the env file.
  -> m (Either (NonEmpty TomlError) (Environment, [TomlWarning]))
  -- ^ A decoded 'Environment' with warnings, or a list of errors.
readEnvFile osPath = liftIO $ do
  filePath <- decodeFS osPath
  content <- readFile filePath
  let decoded = decodeUtf8Lenient content
  return $ readEnvironment $ force decoded


-- | Encodes an 'Environment' into a TOML document.
writeEnvironment :: Environment -> Text
writeEnvironment = showt . FromStringShow . encode


-- | Writes an 'Environment' file to the given path.
writeEnvFile :: (MonadIO m) => Environment -> OsPath -> m ()
writeEnvFile env osPath = liftIO $ do
  filePath <- decodeFS osPath
  writeFile filePath $ encodeUtf8 $ writeEnvironment env
