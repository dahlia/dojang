module Dojang.Types.FileRouteMap (FileRouteMap) where

import Data.Map.Strict (Map)
import System.OsPath (OsPath)

import Dojang.Types.FileRoute (FileRoute)


-- | A map of directories and their routes.
type FileRouteMap = Map OsPath FileRoute
