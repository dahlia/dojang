module Dojang.TestUtils (withTempDir) where

import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (OsPath, encodeFS)


withTempDir :: (OsPath -> FilePath -> IO a) -> IO a
withTempDir action = do
  withSystemTempDirectory "dojang-spec-" $ \tmpDir -> do
    tmpDir' <- encodeFS tmpDir
    action tmpDir' tmpDir
