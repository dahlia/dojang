{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Dojang.Version (toString, toText, version) where


#if defined(DOJANG_DEV_BUILD)
import Data.Maybe (fromJust)
#endif

import Data.Version qualified

import Data.SemVer hiding (version)
import Data.SemVer qualified (version)

import Paths_dojang qualified


version :: Version
version =
  let (Data.Version.Version [major', minor', patch'] _) = Paths_dojang.version
  in Data.SemVer.version
      major'
      minor'
      patch'
#if defined(DOJANG_DEV_BUILD)
      [fromJust $ textual "dev", numeric (DOJANG_DEV_BUILD)]
#else
      []
#endif
      []
