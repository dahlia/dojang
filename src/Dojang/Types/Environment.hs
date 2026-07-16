{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Environment
  ( Architecture (..)
  , Environment (Environment, architecture, kernel, operatingSystem)
  , FactKey
  , FactMap
  , FactValue
  , Kernel (..)
  , OperatingSystem (..)
  , environmentFacts
  , additionalFacts
  , factKeyText
  , factValueText
  , isBuiltInFact
  , lookupFact
  , parseFactKey
  , withFacts
  ) where

import Data.Char (isAlpha, isAlphaNum, isAscii)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import GHC.Records (HasField (getField))

import Data.CaseInsensitive (CI, mk, original)
import Data.HashMap.Strict (HashMap, fromList, toList, (!), (!?))
import Data.Hashable (Hashable (hashWithSalt))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, splitOn)
import qualified Data.Text


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


-- | A kernel information.  Equivalent to @uname -sr@.
data Kernel = Kernel
  { name :: CI Text
  -- ^ The kernel name.  Equivalent to @uname -s@.
  , release :: CI Text
  -- ^ The kernel release.  Equivalent to @uname -r@.
  }
  deriving (Eq, Ord, Read, Show)


instance Hashable Kernel where
  hashWithSalt salt (Kernel name' release') =
    salt `hashWithSalt` name' `hashWithSalt` release'


-- | An environment.
data Environment = Environment'
  { operatingSystem' :: OperatingSystem
  -- ^ The operating system (e.g. 'Linux', 'MacOS').
  , architecture' :: Architecture
  -- ^ The architecture (e.g. 'X86_64', 'AArch64').
  , kernel' :: Kernel
  -- ^ The kernel information.  Equivalent to @uname -sr@.
  , facts' :: FactMap
  -- ^ Additional named facts about the machine.
  }
  deriving (Eq, Ord, Read, Show)


-- | Builds an environment without any additional named facts.
--
-- This pattern keeps the original three-argument constructor source-compatible.
pattern Environment :: OperatingSystem -> Architecture -> Kernel -> Environment
pattern Environment{operatingSystem, architecture, kernel} <-
  Environment' operatingSystem architecture kernel _
 where
  Environment operatingSystem architecture kernel =
    Environment' operatingSystem architecture kernel Map.empty


{-# COMPLETE Environment #-}


instance HasField "operatingSystem" Environment OperatingSystem where
  getField (Environment' os' _ _ _) = os'


instance HasField "architecture" Environment Architecture where
  getField (Environment' _ arch' _ _) = arch'


instance HasField "kernel" Environment Kernel where
  getField (Environment' _ _ kernel' _) = kernel'


-- | A case-insensitive machine fact key.
newtype FactKey = FactKey (CI Text)
  deriving (Eq, Ord, Read, Show)


instance Hashable FactKey where
  hashWithSalt salt (FactKey key) = hashWithSalt salt key


instance IsString FactKey where
  fromString value = case parseFactKey $ pack value of
    Left message -> error $ show message
    Right key -> key


-- | A case-insensitive machine fact value.
newtype FactValue = FactValue (CI Text)
  deriving (Eq, Ord, Read, Show)


instance Hashable FactValue where
  hashWithSalt salt (FactValue value) = hashWithSalt salt value


instance IsString FactValue where
  fromString = FactValue . mk . pack


-- | A map of user-defined machine facts.
type FactMap = Map FactKey FactValue


-- | Parses a dotted machine fact key.
--
-- Each segment starts with an ASCII letter and may contain ASCII letters,
-- digits, hyphens, and underscores.  Keys compare case-insensitively.
parseFactKey :: Text -> Either Text FactKey
parseFactKey key
  | null segments = invalid
  | all validSegment segments = Right $ FactKey $ mk key
  | otherwise = invalid
 where
  segments :: [Text]
  segments = splitOn "." key
  validSegment segment = case Data.Text.uncons segment of
    Nothing -> False
    Just (first, rest) ->
      isAscii first
        && isAlpha first
        && Data.Text.all validRest rest
  validRest character =
    isAscii character
      && (isAlphaNum character || character == '-' || character == '_')
  invalid = Left $ "Invalid machine fact key: " <> key <> "."


-- | Gets the canonical text of a machine fact key.
factKeyText :: FactKey -> Text
factKeyText (FactKey key) = original key


-- | Gets the text of a machine fact value.
factValueText :: FactValue -> Text
factValueText (FactValue value) = original value


-- | Determines whether a key names a built-in machine fact.
isBuiltInFact :: FactKey -> Bool
isBuiltInFact key = key `elem` builtInFactKeys


builtInFactKeys :: [FactKey]
builtInFactKeys = ["os", "arch", "kernel", "kernel-release", "hostname"]


-- | Replaces the additional named facts in an environment.
--
-- Typed built-in keys in the supplied map are ignored.  The hostname remains
-- replaceable so an environment file can simulate another machine.
withFacts :: FactMap -> Environment -> Environment
withFacts facts' (Environment' os' arch' kernel' _) =
  Environment' os' arch' kernel' $ Map.filterWithKey keep facts'
 where
  keep key _ = not $ isBuiltInFact key && key /= "hostname"


-- | Gets all built-in and additional facts in an environment.
environmentFacts :: Environment -> FactMap
environmentFacts (Environment' os' arch' kernel' facts') =
  Map.fromList
    [ ("os", FactValue os'.identifier)
    , ("arch", FactValue arch'.identifier)
    , ("kernel", FactValue kernel'.name)
    , ("kernel-release", FactValue kernel'.release)
    ]
    <> facts'


-- | Gets hostname and user-defined facts without the typed built-ins.
additionalFacts :: Environment -> FactMap
additionalFacts (Environment' _ _ _ facts') = facts'


-- | Looks up a named machine fact.
lookupFact :: FactKey -> Environment -> Maybe FactValue
lookupFact key = Map.lookup key . environmentFacts


instance Hashable Environment where
  hashWithSalt salt (Environment' os' arch' kernel' facts') =
    foldl
      hashFact
      (salt `hashWithSalt` os' `hashWithSalt` arch' `hashWithSalt` kernel')
      (sortOn fst $ Map.toList facts')
   where
    hashFact hash' (key, value) = hash' `hashWithSalt` key `hashWithSalt` value
