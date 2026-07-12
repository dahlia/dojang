{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.HookSpec (spec) where

import Control.Exception (bracket_)
import Control.Monad (when)
import System.IO.Unsafe (unsafePerformIO)

import Data.HashMap.Strict (empty)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import System.Environment
  ( getArgs
  , getExecutablePath
  , lookupEnv
  , setEnv
  , unsetEnv
  )
import System.OsPath (OsPath, encodeFS, (</>))
import Test.Hspec (Spec, describe, it, sequential, shouldBe)

import Dojang.App (AppEnv (..), runAppWithoutLogging)
import Dojang.Commands.Hook
  ( HookEnv (..)
  , executeHooks
  , mergeHookEnvironment
  , shouldRunHook
  )
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Context (Context (..))
import Dojang.Types.Environment (Environment (..), Kernel (..))
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.Hook (Hook (..), HookType (PreApply))
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.Repository (Repository (..))


encodePath :: String -> OsPath
encodePath = unsafePerformIO . encodeFS


spec :: Spec
spec = sequential $ do
  describe "mergeHookEnvironment" $ do
    let dojangEnv = [("DOJANG_OS", "linux")]
    let parentEnv = [("Dojang_OS", "stale"), ("PATH", "/bin")]

    it "replaces differently cased variable names on Windows" $ do
      mergeHookEnvironment "mingw32" dojangEnv parentEnv
        `shouldBe` [("DOJANG_OS", "linux"), ("PATH", "/bin")]

    it "preserves differently cased variable names on POSIX" $ do
      mergeHookEnvironment "linux" dojangEnv parentEnv
        `shouldBe` dojangEnv ++ parentEnv

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

  describe "executeHooks" $ do
    it "inherits the parent environment and overrides Dojang variables" $
      withTempDir $ \tmpDir _ -> do
        command <- hookCommand
        manifestFilename <- encodeFS "dojang.toml"
        intermediateDir <- encodeFS ".dojang"
        envFilename <- encodeFS "dojang-env.toml"
        let hook =
              Hook
                { command = fst command
                , args = snd command
                , condition = Always
                , workingDirectory = Nothing
                , ignoreFailure = False
                }
        let manifest' =
              Manifest empty Map.empty Map.empty $ Map.singleton PreApply [hook]
        let repository = Repository tmpDir (tmpDir </> intermediateDir) manifest'
        let environment = Environment "linux" "x86_64" $ Kernel "Linux" "6.0.0"
        let context = Context repository environment (const $ pure Nothing)
        let appEnv =
              AppEnv
                tmpDir
                intermediateDir
                manifestFilename
                envFilename
                False
                False
        let hookEnv =
              HookEnv
                tmpDir
                (tmpDir </> manifestFilename)
                False
                "hook-test-os"
                "x86_64"
        withEnvVars
          [ ("DOJANG_TEST_PARENT", Just "inherited")
          , ("DOJANG_OS", Just "stale")
          ]
          $ runAppWithoutLogging appEnv
          $ executeHooks hookEnv context PreApply

    it "hook environment probe" $ do
      arguments <- getArgs
      when (probePattern `elem` arguments && probeSeed `elem` arguments) $ do
        lookupEnv "DOJANG_TEST_PARENT" >>= (`shouldBe` Just "inherited")
        lookupEnv "DOJANG_OS" >>= (`shouldBe` Just "hook-test-os")


hookCommand :: IO (OsPath, [Text])
hookCommand = do
  command <- getExecutablePath >>= encodeFS
  pure
    ( command
    ,
      [ "--match"
      , pack probePattern
      , "--seed"
      , pack probeSeed
      ]
    )


probePattern :: String
probePattern = "/Dojang.Commands.Hook/executeHooks/hook environment probe/"


probeSeed :: String
probeSeed = "250025"


withEnvVars :: [(String, Maybe String)] -> IO a -> IO a
withEnvVars [] action = action
withEnvVars ((name, value) : rest) action = do
  oldValue <- lookupEnv name
  bracket_
    (setOrUnset value)
    (setOrUnset oldValue)
    (withEnvVars rest action)
 where
  setOrUnset (Just value') = setEnv name value'
  setOrUnset Nothing = unsetEnv name
