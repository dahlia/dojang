{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.FileRouteMap.EvaluateSpec (spec) where

import Data.Map.Strict (Map)
import System.OsPath (OsPath, encodeFS)
import Test.Hspec (Spec, runIO, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)

import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (Environment)
  , OperatingSystem (..)
  )
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning (..))
import Dojang.Types.FilePathExpression (FilePathExpression (..))
import Dojang.Types.FileRoute (FileRoute, FileType (..), fileRoute)
import Dojang.Types.FileRouteMap.Evaluate (evaluateRoutes)
import Dojang.Types.FileRouteSpec (monikerMap)
import Dojang.Types.MonikerName (parseMonikerName)


spec :: Spec
spec = do
  foo <- runIO $ encodeFS "foo"
  bar <- runIO $ encodeFS "bar"
  baz <- runIO $ encodeFS "baz"

  let Right posix = parseMonikerName "posix"
  let Right undefined' = parseMonikerName "undefined"
  let Right windows = parseMonikerName "windows"

  let route = fileRoute monikerMap
  let routes =
        [
          ( foo
          , route
              [(posix, Just $ Substitution "FOO"), (windows, Nothing)]
              Directory
          )
        ,
          ( bar
          , route
              [ (posix, Just $ Substitution "BAR")
              , (windows, Just $ Root $ Just 'B')
              ]
              Directory
          )
        ,
          ( baz
          , route
              [ (undefined', Just $ BareComponent "baz")
              , (windows, Just $ Substitution "BAZ")
              ]
              File
          )
        ]
          :: Map OsPath FileRoute

  specify "evaluateRoutes" $ do
    evaluateRoutes [] (Environment Linux X86_64) `shouldBe` ([], [])
    evaluateRoutes routes (Environment Windows X86_64)
      `shouldBe` (
                   [ (bar, Root $ Just 'B')
                   , (baz, Substitution "BAZ")
                   ]
                 , [UndefinedMoniker undefined']
                 )
    evaluateRoutes routes (Environment Linux AArch64)
      `shouldBe` (
                   [ (foo, Substitution "FOO")
                   , (bar, Substitution "BAR")
                   ]
                 , []
                 )
