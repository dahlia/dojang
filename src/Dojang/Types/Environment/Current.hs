{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Dojang.Types.Environment.Current
  ( MonadArchitecture (..)
  , MonadEnvironment (..)
  , MonadOperatingSystem (..)
  , parseArchitecture
  , parseOperatingSystem
  ) where

import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (..)
  , OperatingSystem (..)
  )

import Data.String (IsString (fromString))
import System.Info (arch, os)


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


-- | A monad that can get the current 'Environment'.
class MonadEnvironment m where
  -- | Gets the current 'Environment'.
  currentEnvironment :: m Environment


instance
  ( MonadOperatingSystem m
  , MonadArchitecture m
  )
  => MonadEnvironment m
  where
  currentEnvironment = do
    os' <- currentOperatingSystem
    arch' <- currentArchitecture
    return
      Environment
        { operatingSystem = os'
        , architecture = arch'
        }
