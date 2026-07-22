{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Registry containing every codec shipped with Dojang.
module Dojang.Types.Codec.BuiltIn
  ( builtInCodecRuntime
  ) where

import Data.Map.Strict qualified as Map

import Dojang.Types.Codec
  ( CodecDefinition (CodecDefinition)
  , CodecName
  )
import Dojang.Types.Codec.Encrypted
  ( encryptedCodecImplementation
  , encryptedReAddCodecImplementation
  )
import Dojang.Types.Codec.Evaluate
  ( CodecImplementation (definition)
  , CodecRuntime (..)
  , EvaluationMode
  , identityCodecRuntime
  )
import Dojang.Types.Codec.SecretTemplate
  ( secretTemplateCodecImplementation
  )
import Dojang.Types.Codec.Template (templateCodecImplementation)


-- | Runtime containing every codec shipped with Dojang.  Sensitive codecs
-- remain inert until a surrounding context supplies backend resolution.
builtInCodecRuntime :: (Applicative m) => EvaluationMode -> CodecRuntime m
builtInCodecRuntime mode =
  let runtime = identityCodecRuntime mode
      implementations =
        [ templateCodecImplementation
        , secretTemplateCodecImplementation
        , encryptedCodecImplementation
        , encryptedReAddCodecImplementation
        ]
  in runtime{registry = foldr insertImplementation runtime.registry implementations}
 where
  insertImplementation
    :: CodecImplementation
    -> Map.Map CodecName CodecImplementation
    -> Map.Map CodecName CodecImplementation
  insertImplementation implementation registry =
    let CodecDefinition name _ _ = implementation.definition
    in Map.insert name implementation registry
