{-# LANGUAGE LambdaCase #-}

module Dojang.TestUtils
  ( Entry (..)
  , Tree
  , makeFixtureTree
  , requireNonUtf8FileNames
  , supportsNonUtf8FileNames
  , withHome
  , withTempDir
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, void)
import Data.Char (chr)
import GHC.IO.Exception (IOErrorType (InvalidArgument))

import qualified Data.ByteString as ByteString
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Error (ioeGetErrorType, tryIOError)
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (OsPath, decodeFS, encodeFS, (</>))
import Test.Hspec (pendingWith)

import Data.ByteString (ByteString)
import Dojang.MonadFileSystem (MonadFileSystem (..))


withTempDir :: (OsPath -> FilePath -> IO a) -> IO a
withTempDir action = do
  withSystemTempDirectory "dojang-spec-" $ \tmpDir -> do
    tmpDir' <- encodeFS tmpDir
    action tmpDir' tmpDir


-- | Marks a test pending when its temporary filesystem rejects filenames that
-- are not valid UTF-8.
requireNonUtf8FileNames :: OsPath -> IO ()
requireNonUtf8FileNames parent =
  supportsNonUtf8FileNames parent >>= \supported ->
    unless supported $
      pendingWith
        "The filesystem rejects filenames that are not valid UTF-8."


-- | Tests whether a directory's filesystem accepts filenames that are not
-- valid UTF-8.
supportsNonUtf8FileNames :: OsPath -> IO Bool
supportsNonUtf8FileNames parent = do
  probeName <- encodeFS [chr 0xdc80]
  let probe = parent </> probeName
  tryIOError (Dojang.MonadFileSystem.writeFile probe ByteString.empty) >>= \case
    Left err
      | ioeGetErrorType err == InvalidArgument -> return False
      | otherwise -> ioError err
    Right () -> removeFile probe >> return True


-- | Runs an action with both home-directory environment variables set to the
-- supplied path, restoring their previous values afterward.
withHome :: OsPath -> IO a -> IO a
withHome home action =
  bracket (lookupEnv "HOME") (restore "HOME") $ \_ ->
    bracket (lookupEnv "USERPROFILE") (restore "USERPROFILE") $ \_ -> do
      home' <- decodeFS home
      setEnv "HOME" home'
      setEnv "USERPROFILE" home'
      action
 where
  restore name Nothing = unsetEnv name
  restore name (Just previous) = setEnv name previous


data Entry = F ByteString | D Tree


type Tree = [(OsPath, Entry)]


makeFixtureTree
  :: (MonadFileSystem m) => OsPath -> Tree -> m ()
makeFixtureTree path tree = do
  pathExists <- exists path
  unless pathExists $ void $ createDirectories path
  forM_ tree $ \case
    (p, F bytes) -> Dojang.MonadFileSystem.writeFile (path </> p) bytes
    (p, D tree') -> makeFixtureTree (path </> p) tree'
