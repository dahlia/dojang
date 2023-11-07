{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Repository (Repository (..)) where

import System.OsPath (OsPath)

import Dojang.Types.Manifest (Manifest (..))


-- | A repository, which is a directory containing a manifest file and dotfiles.
data Repository = Repository
  { sourcePath :: OsPath
  -- ^ The path to the repository.
  , intermediatePath :: OsPath
  -- ^ The path to the intermediate directory, which is managed by Dojang and
  -- contains the post-processed files.
  , manifest :: Manifest
  -- ^ The manifest of the repository.
  }
