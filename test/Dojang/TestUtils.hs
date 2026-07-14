{-# LANGUAGE LambdaCase #-}

module Dojang.TestUtils
  ( Entry (..)
  , Tree
  , makeFixtureTree
  , withHome
  , withTempDir
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, void)

import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (OsPath, decodeFS, encodeFS, (</>))

import Data.ByteString (ByteString)
import Dojang.MonadFileSystem (MonadFileSystem (..))


withTempDir :: (OsPath -> FilePath -> IO a) -> IO a
withTempDir action = do
  withSystemTempDirectory "dojang-spec-" $ \tmpDir -> do
    tmpDir' <- encodeFS tmpDir
    action tmpDir' tmpDir


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
