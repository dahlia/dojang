{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.FileRouteMap.EvaluateSpec (spec) where

import Data.Map.Strict (Map)
import System.OsPath (OsPath, encodeFS)
import Test.Hspec (Spec, runIO, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)

import Dojang.MonadFileSystem (FileType (..))
import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (Environment)
  , Kernel (..)
  , OperatingSystem (..)
  )
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning (..))
import Dojang.Types.FilePathExpression (FilePathExpression (..))
import Dojang.Types.FileRoute (FileRoute, fileRoute)
import Dojang.Types.FileRouteMap.Evaluate
  ( evaluateRoutes
  , evaluateRoutesWithFileTypes
  )
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
    evaluateRoutes [] (Environment Linux X86_64 $ Kernel "Linux" "5.10.0-8")
      `shouldBe` ([], [])
    evaluateRoutes
      routes
      ( Environment Windows X86_64
          $ Kernel "Microsoft Windows" "10.0.23585.1001"
      )
      `shouldBe` (
                   [ (bar, Root $ Just 'B')
                   , (baz, Substitution "BAZ")
                   ]
                 , [UndefinedMoniker undefined']
                 )
    evaluateRoutes
      routes
      (Environment Linux AArch64 $ Kernel "Linux" "6.5.9-300.fc35.aarch64")
      `shouldBe` (
                   [ (foo, Substitution "FOO")
                   , (bar, Substitution "BAR")
                   ]
                 , []
                 )

  specify "evaluateRoutesWithFileTypes" $ do
    let eval = evaluateRoutesWithFileTypes
    eval [] (Environment Linux X86_64 $ Kernel "Linux" "5.10.0-8")
      `shouldBe` ([], [])
    eval
      routes
      ( Environment Windows X86_64
          $ Kernel "Microsoft Windows" "10.0.23585.1001"
      )
      `shouldBe` (
                   [ (bar, (Root $ Just 'B', Directory))
                   , (baz, (Substitution "BAZ", File))
                   ]
                 , [UndefinedMoniker undefined']
                 )
    eval
      routes
      (Environment Linux AArch64 $ Kernel "Linux" "6.5.9-300.fc35.aarch64")
      `shouldBe` (
                   [ (foo, (Substitution "FOO", Directory))
                   , (bar, (Substitution "BAR", Directory))
                   ]
                 , []
                 )
