{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides the registry type and functions for reading and
-- writing the registry file. The registry file stores the path to the
-- repository so that commands like @dojang edit@ can be run from any
-- directory.
module Dojang.Types.Registry
  ( Registry (..)
  , readRegistry
  , readRegistryStrict
  , registryFilename
  , writeRegistry
  ) where

import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import System.IO.Unsafe (unsafePerformIO)

import System.OsPath (OsPath, decodeFS, encodeFS)
import TextShow (FromStringShow (FromStringShow), showt)
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


-- | The registry stores the path to the repository.
data Registry = Registry
  { repositoryPath :: OsPath
  -- ^ The path to the repository.
  }
  deriving (Eq, Show)


-- | The filename for the registry file.
registryFilename :: OsPath
registryFilename = unsafePerformIO $ encodeFS ".dojang"
{-# NOINLINE registryFilename #-}


instance FromValue Registry where
  fromValue =
    parseTableFromValue $
      Registry
        <$> (textToOsPath <$> reqKey "repository")
   where
    textToOsPath :: Text -> OsPath
    textToOsPath t = unsafePerformIO $ encodeFS $ unpack t


instance ToValue Registry where
  toValue = defaultTableToValue


instance ToTable Registry where
  toTable reg =
    table
      [ "repository" .= osPathToText reg.repositoryPath
      ]
   where
    osPathToText :: OsPath -> Text
    osPathToText p = pack $ unsafePerformIO $ decodeFS p


-- | Read the registry from a file.
readRegistry
  :: (MonadFileSystem m)
  => OsPath
  -- ^ The path to the registry file.
  -> m (Maybe Registry)
readRegistry path = do
  result <- readRegistryStrict path
  return $ either (const Nothing) id result


-- | Reads the registry without confusing malformed contents with absence.
readRegistryStrict
  :: (MonadFileSystem m)
  => OsPath
  -- ^ The path to the registry file.
  -> m (Either [Text] (Maybe Registry))
  -- ^ The decoded registry, its absence, or decoding errors.
readRegistryStrict path = do
  fileExists <- exists path
  symbolicLink <- isSymlink path
  if symbolicLink
    then return $ Left ["The legacy registry must not be a symbolic link."]
    else
      if not fileExists
        then return $ Right Nothing
        else do
          regularFile <- isRegularFile path
          if not regularFile
            then return $ Left ["The legacy registry must be a regular file."]
            else decodeRegistry <$> Dojang.MonadFileSystem.readFile path
 where
  decodeRegistry contents = case decodeUtf8' contents of
    Left err -> Left [pack $ show err]
    Right textContents -> case decode $ unpack textContents of
      Success _ reg -> Right $ Just reg
      Failure errors -> Left $ pack <$> errors


-- | Write the registry to a file.
writeRegistry
  :: (MonadFileSystem m)
  => OsPath
  -- ^ The path to the registry file.
  -> Registry
  -- ^ The registry to write.
  -> m ()
writeRegistry path registry = do
  let contents = showt $ FromStringShow $ encode registry
  Dojang.MonadFileSystem.writeFile path (encodeUtf8 contents)
