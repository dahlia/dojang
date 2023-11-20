{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.Environment.CurrentSpec (spec) where

import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (..)
  , Kernel (..)
  , OperatingSystem (..)
  )
import Dojang.Types.Environment.Current
  ( currentArchitecture
  , currentEnvironment
  , currentKernel
  , currentOperatingSystem
  , parseArchitecture
  , parseOperatingSystem
  )

import Data.CaseInsensitive (original)
import Data.String (IsString (fromString))
import Data.Text (Text)
import System.Info (arch, os)
import Test.Hspec (Spec, specify)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)
import Text.Regex.TDFA ((=~))


spec :: Spec
spec = do
  specify "currentOperatingSystem" $ do
    os' <- currentOperatingSystem
    os' `shouldBe` expectedOS

  specify "currentArchitecture" $ do
    arch' <- currentArchitecture
    arch' `shouldBe` expectedArch

  specify "currentKernel" $ do
    kernel <- currentKernel
    expectKernel kernel

  specify "currentEnvironment" $ do
    env <- currentEnvironment
    env.operatingSystem `shouldBe` expectedOS
    env.architecture `shouldBe` expectedArch
    expectKernel env.kernel

  specify "parseOperatingSystem" $ do
    parseOperatingSystem "linux-android" `shouldBe` Android
    parseOperatingSystem "darwin" `shouldBe` MacOS
    parseOperatingSystem "freebsd" `shouldBe` FreeBSD
    parseOperatingSystem "linux" `shouldBe` Linux
    parseOperatingSystem "netbsd" `shouldBe` NetBSD
    parseOperatingSystem "openbsd" `shouldBe` OpenBSD
    parseOperatingSystem "mingw32" `shouldBe` Windows
    parseOperatingSystem "other-os" `shouldBe` OtherOS "other-os"

  specify "parseArchitecture" $ do
    parseArchitecture "aarch64" `shouldBe` AArch64
    parseArchitecture "i386" `shouldBe` X86
    parseArchitecture "x86_64" `shouldBe` X86_64
    parseArchitecture "other-arch" `shouldBe` Etc "other-arch"


expectedOS :: OperatingSystem
expectedOS = case os of
  "darwin" -> MacOS
  "linux" -> Linux
  "mingw32" -> Windows
  "freebsd" -> FreeBSD
  os' -> OtherOS $ fromString os'


expectedArch :: Architecture
expectedArch = case arch of
  "aarch64" -> AArch64
  "x86_64" -> X86_64
  arch' -> Etc $ fromString arch'


expectKernel :: Kernel -> IO ()
expectKernel = case os of
  "darwin" -> expectMacOSKernel
  "linux" -> expectLinuxKernel
  "mingw32" -> expectWindowsKernel
  _ -> \_ -> return ()


expectMacOSKernel :: Kernel -> IO ()
expectMacOSKernel kernel = do
  original kernel.name `shouldBe` "Darwin"
  original kernel.release
    `shouldSatisfy` (=~ ("^[0-9]+\\.[0-9]+\\.[0-9]+$" :: Text))


expectLinuxKernel :: Kernel -> IO ()
expectLinuxKernel kernel = do
  original kernel.name `shouldBe` "Linux"
  original kernel.release
    `shouldSatisfy` (=~ ("^[0-9]+\\.[0-9]+\\.?[0-9]+[.-][0-9]+(-|$)" :: Text))


expectWindowsKernel :: Kernel -> IO ()
expectWindowsKernel kernel = do
  original kernel.name `shouldBe` "Microsoft Windows"
  original kernel.release
    `shouldSatisfy` (=~ ("^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$" :: Text))
