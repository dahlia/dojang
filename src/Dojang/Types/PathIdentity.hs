-- | Native filesystem identity for destination paths.
--
-- Two lexically different paths can still name the same filesystem entry
-- on platforms whose paths compare case-insensitively.  This module holds
-- the canonical form used whenever destination paths are compared or
-- keyed by their native identity.
module Dojang.Types.PathIdentity
  ( destinationPathIdentity
  , equalDestinationPath
  , pathIdentityComponents
  ) where

import Data.Char (ord, toLower)
import Data.Word (Word32)
import System.Info (os)
import System.OsPath (OsPath, normalise, splitDirectories, toChar, unpack)


-- | Produces the normalized code units used for native destination identity.
--
-- Windows path identity is case-insensitive.  POSIX path identity preserves
-- case and every surrogate-escaped filesystem byte.
destinationPathIdentity :: OsPath -> [Word32]
destinationPathIdentity = fmap canonicalUnit . unpack . normalise
 where
  canonicalUnit value =
    fromIntegral $
      ord $
        if os == "mingw32"
          then toLower $ toChar value
          else toChar value


-- | Compares two destination paths using the host platform's native semantics.
equalDestinationPath :: OsPath -> OsPath -> Bool
equalDestinationPath left right =
  destinationPathIdentity left == destinationPathIdentity right


-- | Splits a normalized path into components carrying their native
-- identity, for containment tests between destination paths.
pathIdentityComponents :: OsPath -> [[Word32]]
pathIdentityComponents path =
  destinationPathIdentity <$> splitDirectories (normalise path)
