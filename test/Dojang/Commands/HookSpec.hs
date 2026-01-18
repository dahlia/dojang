{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.HookSpec (spec) where

import System.IO.Unsafe (unsafePerformIO)

import Data.HashMap.Strict (empty)
import System.OsPath (OsPath, encodeFS)
import Test.Hspec (Spec, describe, it, shouldBe)

import Dojang.Commands.Hook (shouldRunHook)
import Dojang.Types.Environment (Environment (..), Kernel (..))
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.Hook (Hook (..))
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (parseMonikerName)


encodePath :: String -> OsPath
encodePath = unsafePerformIO . encodeFS


spec :: Spec
spec = do
  describe "shouldRunHook" $ do
    let env = Environment "linux" "x86_64" $ Kernel "Linux" "6.0.0"

    it "returns True for Always condition" $ do
      let hook =
            Hook
              { command = encodePath "/bin/echo"
              , args = ["hello"]
              , condition = Always
              , workingDirectory = Nothing
              , ignoreFailure = False
              }
      shouldRunHook empty env hook `shouldBe` True

    it "returns True when OS matches" $ do
      let hook =
            Hook
              { command = encodePath "/bin/echo"
              , args = []
              , condition = OperatingSystem "linux"
              , workingDirectory = Nothing
              , ignoreFailure = False
              }
      shouldRunHook empty env hook `shouldBe` True

    it "returns False when OS does not match" $ do
      let hook =
            Hook
              { command = encodePath "/bin/echo"
              , args = []
              , condition = OperatingSystem "windows"
              , workingDirectory = Nothing
              , ignoreFailure = False
              }
      shouldRunHook empty env hook `shouldBe` False

    it "returns True when moniker matches" $ do
      let Right monikerName = parseMonikerName "linux"
      let monikers = [(monikerName, OperatingSystem "linux")] :: MonikerMap
      let hook =
            Hook
              { command = encodePath "/bin/echo"
              , args = []
              , condition = Moniker monikerName
              , workingDirectory = Nothing
              , ignoreFailure = False
              }
      shouldRunHook monikers env hook `shouldBe` True

    it "returns False when moniker does not match" $ do
      let Right linuxMoniker = parseMonikerName "linux"
      let Right windowsMoniker = parseMonikerName "windows"
      let monikers =
            [ (linuxMoniker, OperatingSystem "linux")
            , (windowsMoniker, OperatingSystem "windows")
            ]
              :: MonikerMap
      let hook =
            Hook
              { command = encodePath "/bin/echo"
              , args = []
              , condition = Moniker windowsMoniker
              , workingDirectory = Nothing
              , ignoreFailure = False
              }
      shouldRunHook monikers env hook `shouldBe` False
