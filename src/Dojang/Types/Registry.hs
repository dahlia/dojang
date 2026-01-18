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
  , registryFilename
  , writeRegistry
  ) where

import Control.DeepSeq (force)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
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
  :: (MonadFileSystem m, MonadIO m)
  => OsPath
  -- ^ The path to the registry file.
  -> m (Maybe Registry)
readRegistry path = do
  fileExists <- exists path
  if not fileExists
    then return Nothing
    else do
      contents <- Dojang.MonadFileSystem.readFile path
      let textContents = decodeUtf8Lenient contents
      case decode $ unpack $ force textContents of
        Success _ reg -> return $ Just reg
        Failure _ -> return Nothing


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
