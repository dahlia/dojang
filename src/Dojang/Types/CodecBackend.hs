{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Reusable external backends for sensitive route codecs.
module Dojang.Types.CodecBackend
  ( CodecBackend (..)
  , CodecBackendMap
  , CodecBackendOptions (..)
  , defaultCodecBackendTimeoutSeconds
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)

import Dojang.Types.Codec (CodecValue)
import Dojang.Types.FilePathExpression (FilePathExpression)


-- | Backend-specific, non-secret protocol options.
newtype CodecBackendOptions = CodecBackendOptions (Map Text CodecValue)
  deriving (Eq, Show)


-- | A command implementing the codec backend protocol.
data CodecBackend = CodecBackend
  { command :: FilePathExpression
  -- ^ Executable path expression.  Arguments are intentionally unsupported.
  , version :: Text
  -- ^ Stable implementation version included in cache identities.
  , timeoutSeconds :: Int
  -- ^ Maximum execution time in seconds.
  , options :: CodecBackendOptions
  -- ^ Non-secret configuration passed in the protocol header.
  }
  deriving (Eq, Show)


-- | Codec backends keyed by their manifest-local names.
type CodecBackendMap = Map Text CodecBackend


-- | Default backend timeout when a declaration omits @timeout-seconds@.
defaultCodecBackendTimeoutSeconds :: Int
defaultCodecBackendTimeoutSeconds = 30
