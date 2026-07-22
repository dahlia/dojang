{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Registry containing every codec shipped with Dojang.
module Dojang.Types.Codec.BuiltIn
  ( builtInCodecRuntime
  ) where

import Data.Map.Strict qualified as Map

import Dojang.Types.Codec (CodecDefinition (CodecDefinition))
import Dojang.Types.Codec.Evaluate
  ( CodecImplementation (definition)
  , CodecRuntime (..)
  , EvaluationMode
  , identityCodecRuntime
  )
import Dojang.Types.Codec.Template (templateCodecImplementation)


-- | Runtime containing the identity and template codecs.
builtInCodecRuntime :: (Applicative m) => EvaluationMode -> CodecRuntime m
builtInCodecRuntime mode =
  let runtime = identityCodecRuntime mode
      implementation = templateCodecImplementation
      CodecDefinition name _ _ = implementation.definition
  in runtime{registry = Map.insert name implementation runtime.registry}
