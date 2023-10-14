{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Environment
  ( Architecture (..)
  , Environment (..)
  , OperatingSystem (..)
  ) where

import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import GHC.Records (HasField (getField))

import Data.CaseInsensitive (CI, mk)
import Data.HashMap.Strict (HashMap, fromList, toList, (!), (!?))
import Data.Hashable (Hashable (hashWithSalt))
import Data.Text (Text, pack)


-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> :seti -XOverloadedRecordDot


-- | An operating system identifier.
data OperatingSystem
  = Android
  | FreeBSD
  | Linux
  | MacOS
  | NetBSD
  | OpenBSD
  | Windows
  | -- | An operating system that is not one of the above.
    OtherOS (CI Text)
  deriving (Eq, Ord, Read, Show)


operatingSystems :: HashMap OperatingSystem (CI Text)
operatingSystems =
  [ (Android, "android")
  , (FreeBSD, "freebsd")
  , (Linux, "linux")
  , (MacOS, "macos")
  , (NetBSD, "netbsd")
  , (OpenBSD, "openbsd")
  , (Windows, "windows")
  ]


operatingSystemIdentifiers :: HashMap (CI Text) OperatingSystem
operatingSystemIdentifiers =
  fromList $! [(ident, os') | (os', ident) <- toList operatingSystems]


-- | Gets an 'OperatingSystem' from an identifier.
--
-- >>> fromString "linux" :: OperatingSystem
-- Linux
-- >>> fromString "windows" :: OperatingSystem
-- Windows
-- >>> fromString "amigaos" :: OperatingSystem
-- OtherOS "amigaos"
instance IsString OperatingSystem where
  fromString s =
    fromMaybe (OtherOS ident) (operatingSystemIdentifiers !? ident)
   where
    ident :: CI Text
    ident = mk $ pack s


-- | Gets the identifier for an 'OperatingSystem'.
--
-- >>> (Linux).identifier
-- "linux"
-- >>> (Windows).identifier
-- "windows"
--
-- If the 'OperatingSystem' is 'OtherOS', then the identifier is returned
-- unchanged.
--
-- >>> (OtherOS "amigaos").identifier
-- "amigaos"
instance HasField "identifier" OperatingSystem (CI Text) where
  getField (OtherOS os') = os'
  getField os' = operatingSystems ! os'


instance Hashable OperatingSystem where
  hashWithSalt salt = hashWithSalt salt . show


-- | An architecture identifier.
data Architecture
  = -- | ARM64
    AArch64
  | -- | x86 (i.e., i386)
    X86
  | -- | x86-64 (i.e., amd64)
    X86_64
  | -- | An architecture that is not one of the above.
    Etc (CI Text)
  deriving (Eq, Ord, Read, Show)


architectures :: HashMap Architecture (CI Text)
architectures =
  [ (AArch64, "aarch64")
  , (X86, "x86")
  , (X86_64, "x86_64")
  ]


architectureIdentifiers :: HashMap (CI Text) Architecture
architectureIdentifiers =
  fromList $! [(ident, arch') | (arch', ident) <- toList architectures]


-- | Gets an 'Architecture' from an identifier.
--
-- >>> fromString "x86_64" :: Architecture
-- X86_64
-- >>> fromString "aarch64" :: Architecture
-- AArch64
-- >>> fromString "alpha" :: Architecture
-- Etc "alpha"
instance IsString Architecture where
  fromString s =
    fromMaybe (Etc ident) (architectureIdentifiers !? ident)
   where
    ident :: CI Text
    ident = mk $ pack s


-- | Gets the identifier for an 'Architecture'.
--
-- >>> (X86_64).identifier
-- "x86_64"
-- >>> (AArch64).identifier
-- "aarch64"
--
-- If the 'Architecture' is 'Etc', then the identifier is returned unchanged.
--
-- >>> (Etc "alpha").identifier
-- "alpha"
instance HasField "identifier" Architecture (CI Text) where
  getField (Etc arch') = arch'
  getField arch' = architectures ! arch'


instance Hashable Architecture where
  hashWithSalt salt = hashWithSalt salt . show


-- | An environment.
data Environment = Environment
  { operatingSystem :: OperatingSystem
  -- ^ The operating system (e.g. 'Linux', 'MacOS').
  , architecture :: Architecture
  -- ^ The architecture (e.g. 'X86_64', 'AArch64').
  }
  deriving (Eq, Ord, Read, Show)


instance Hashable Environment where
  hashWithSalt salt (Environment os' arch') =
    salt `hashWithSalt` os' `hashWithSalt` arch'
