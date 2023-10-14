{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.Environment.CurrentSpec (spec) where

import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (..)
  , OperatingSystem (..)
  )
import Dojang.Types.Environment.Current
  ( currentArchitecture
  , currentEnvironment
  , currentOperatingSystem
  , parseArchitecture
  , parseOperatingSystem
  )

import Data.String (IsString (fromString))
import System.Info (arch, os)
import Test.Hspec (Spec, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)


spec :: Spec
spec = do
  specify "currentOperatingSystem" $ do
    os' <- currentOperatingSystem
    os' `shouldBe` expectedOS

  specify "currentArchitecture" $ do
    arch' <- currentArchitecture
    arch' `shouldBe` expectedArch

  specify "currentEnvironment" $ do
    env <- currentEnvironment
    env
      `shouldBe` Environment
        { operatingSystem = expectedOS
        , architecture = expectedArch
        }

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
  os' -> OtherOS $ fromString os'


expectedArch :: Architecture
expectedArch = case arch of
  "aarch64" -> AArch64
  "x86_64" -> X86_64
  arch' -> Etc $ fromString arch'
