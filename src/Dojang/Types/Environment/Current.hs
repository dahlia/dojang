{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Dojang.Types.Environment.Current
  ( MonadArchitecture (..)
  , MonadEnvironment (..)
  , MonadHostname (..)
  , MonadOperatingSystem (..)
  , MonadKernel (..)
  , parseArchitecture
  , parseOperatingSystem
  ) where

import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.String (IsString (fromString))
import System.Info (arch, os)
import Prelude hiding (break)

import Data.CaseInsensitive (mk)
import Data.Text (Text, break, dropAround, pack, strip, stripEnd, unpack)
import Network.HostName (getHostName)
import System.Process (readCreateProcess, readProcess, shell)

import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (..)
  , Kernel (..)
  , OperatingSystem (..)
  , withFacts
  )


-- | Turns a string returned by 'System.Info.os' into an 'OperatingSystem'.
parseOperatingSystem :: String -> OperatingSystem
parseOperatingSystem = \case
  "linux-android" -> Android
  "darwin" -> MacOS
  "freebsd" -> FreeBSD
  "linux" -> Linux
  "netbsd" -> NetBSD
  "openbsd" -> OpenBSD
  "mingw32" -> Windows
  os' -> OtherOS $ fromString os'


-- | Turns a string returned by 'System.Info.arch' into an 'Architecture'.
parseArchitecture :: String -> Architecture
parseArchitecture = \case
  "aarch64" -> AArch64
  "i386" -> X86
  "x86_64" -> X86_64
  arch' -> Etc $ fromString arch'


-- | A monad that can get the current 'Environment'.
class (Monad m) => MonadOperatingSystem m where
  -- | Gets the current 'OperatingSystem'.
  currentOperatingSystem :: m OperatingSystem


instance MonadOperatingSystem IO where
  currentOperatingSystem = return $ parseOperatingSystem os


-- | A monad that can get the current 'Architecture'.
class (Monad m) => MonadArchitecture m where
  -- | Gets the current 'Architecture'.
  currentArchitecture :: m Architecture


instance MonadArchitecture IO where
  currentArchitecture = return $ parseArchitecture arch


-- | A monad that can get the current 'Kernel' information.
class (Monad m) => MonadKernel m where
  -- | Gets the current 'Kernel' information.
  currentKernel :: m Kernel


instance MonadKernel IO where
  currentKernel
    | os == "mingw32" = do
        ver <- strip . pack <$> readCreateProcess (shell "cmd /c ver") ""
        let (name, rest) = break (== '[') ver
        let (_, release) = break (isDigit) $ dropAround (`elem` "[]") rest
        return $ Kernel (mk $ stripEnd name) (mk $ strip release)
    | otherwise = do
        name <- strip . pack <$> readProcess "uname" ["-s"] ""
        release <- strip . pack <$> readProcess "uname" ["-r"] ""
        return $ Kernel (mk name) (mk release)


-- | A monad that can get the current machine hostname.
class (Monad m) => MonadHostname m where
  -- | Gets the current machine hostname.
  currentHostname :: m Text


instance MonadHostname IO where
  currentHostname = pack <$> getHostName


-- | A monad that can get the current 'Environment'.
class (Monad m) => MonadEnvironment m where
  -- | Gets the current 'Environment'.
  currentEnvironment :: m Environment


instance
  ( Monad m
  , MonadOperatingSystem m
  , MonadArchitecture m
  , MonadKernel m
  , MonadHostname m
  )
  => MonadEnvironment m
  where
  currentEnvironment = do
    os' <- currentOperatingSystem
    arch' <- currentArchitecture
    kernel' <- currentKernel
    hostname <- currentHostname
    return
      $ withFacts
        (Map.singleton (fromString "hostname") $ fromString $ unpack hostname)
      $ Environment os' arch' kernel'
