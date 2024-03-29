{-# LANGUAGE InstanceSigs #-}
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

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString (fromString))
import Prelude hiding (readFile, writeFile)

import Control.DeepSeq (force)
import Data.CaseInsensitive (CI (original))
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.OsPath (OsPath)
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

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Environment
  ( Architecture
  , Environment (..)
  , Kernel (..)
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


instance FromValue Kernel where
  fromValue =
    parseTableFromValue
      $ Kernel
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
  fromValue =
    parseTableFromValue
      $ Environment
      <$> reqKey "os"
      <*> reqKey "arch"
      <*> reqKey "kernel"


instance ToValue Environment where
  toValue = defaultTableToValue


instance ToTable Environment where
  toTable env =
    table
      [ "os" .= env.operatingSystem
      , "arch" .= env.architecture
      , "kernel" .= env.kernel
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
