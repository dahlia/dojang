{-# LANGUAGE LambdaCase #-}

module Dojang.TestUtils (Entry (..), Tree, makeFixtureTree, withTempDir) where

import Control.Monad (forM_, unless, void)

import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (OsPath, encodeFS, (</>))

import Data.ByteString (ByteString)
import Dojang.MonadFileSystem (MonadFileSystem (..))


withTempDir :: (OsPath -> FilePath -> IO a) -> IO a
withTempDir action = do
  withSystemTempDirectory "dojang-spec-" $ \tmpDir -> do
    tmpDir' <- encodeFS tmpDir
    action tmpDir' tmpDir


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
