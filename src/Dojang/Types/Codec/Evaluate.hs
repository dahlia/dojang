{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Controlled evaluation of declarative route codecs.
module Dojang.Types.Codec.Evaluate
  ( CacheScope (..)
  , CodecCacheEntry (..)
  , CodecDryRunPolicy (..)
  , CodecError
  , CodecEvaluationRequest (..)
  , CodecFailure (..)
  , CodecFailureCategory (..)
  , CodecImplementation
    ( CodecImplementation
    , cacheScope
    , definition
    , dryRunPolicy
    , forward
    , requirementsForSource
    , reverse
    , validateConfiguration
    )
  , CodecInputPresence (..)
  , CodecInputSelection (..)
  , CodecInputs (..)
  , CodecRequirements (..)
  , CodecRuntime (..)
  , CodecSourcePosition (..)
  , EvaluationMode (..)
  , EvaluatedCodec (..)
  , ExternalInput (..)
  , ExternalInputRequest (..)
  , OpaqueBytes
  , codecRegistry
  , codecConfigurationRequirements
  , codecImplementationWithSourceRequirements
  , codecRequirements
  , codecReflectPolicy
  , codecSourceTypeError
  , evaluateCodec
  , formatCodecError
  , identityCodecRuntime
  , noCodecInputs
  , opaqueBytes
  , reevaluateCodec
  , reflectCodec
  , reflectCodecWithoutSourceWithEvaluation
  , reflectCodecWithoutSourceWithEvaluationBy
  , reflectCodecWithEvaluation
  , reflectEvaluatedCodec
  , reflectEvaluatedCodecWithEvaluation
  , requiredCodecInputs
  , revealBytes
  ) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.CaseInsensitive (foldCase)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Numeric (showHex)

import Dojang.Types.Codec
  ( CodecConfiguration (CodecConfiguration)
  , CodecDefinition (CodecDefinition)
  , CodecDependency (CodecDependency)
  , CodecName
  , CodecSpec (CodecSpec)
  , ReflectPolicy (ReflectIdentity, ReflectReAdd, ReflectReject)
  , codecCacheKey
  , identityCodecDefinition
  , identityCodecSpec
  , renderCodecName
  )


-- | Bytes whose 'Show' instance never reveals their contents.
newtype OpaqueBytes = OpaqueBytes ByteString
  deriving (Eq, Ord)


instance Show OpaqueBytes where
  show (OpaqueBytes bytes) = "<redacted bytes: " <> show (ByteString.length bytes) <> ">"


-- | Wraps bytes for safe storage in plans and diagnostic values.
opaqueBytes :: ByteString -> OpaqueBytes
opaqueBytes = OpaqueBytes


-- | Extracts bytes at a filesystem or codec boundary.
revealBytes :: OpaqueBytes -> ByteString
revealBytes (OpaqueBytes bytes) = bytes


-- | Whether a codec result may be represented by machine-state metadata.
data CacheScope = PersistentCache | CommandCacheOnly
  deriving (Eq, Ord, Show)


-- | Whether an uncached codec may run during a dry-run.
data CodecDryRunPolicy = EvaluatePurely | CachedOnly
  deriving (Eq, Ord, Show)


-- | Command evaluation mode.
data EvaluationMode = NormalEvaluation | DryRunEvaluation
  deriving (Eq, Ord, Show)


-- | One-based location in a codec's repository source.
data CodecSourcePosition = CodecSourcePosition
  { line :: Int
  -- ^ One-based line number.
  , column :: Int
  -- ^ One-based column number.
  }
  deriving (Eq, Ord, Show)


-- | Safe category for a source-aware codec failure.
data CodecFailureCategory
  = InvalidSourceEncoding
  | InvalidSourceSyntax
  | UnsupportedSourceSyntax
  | MissingSourceInput Text
  | SourceEvaluationFailure
  deriving (Eq, Ord, Show)


-- | Failure returned by a codec implementation.
--
-- Opaque details are retained for equality and debugging boundaries but never
-- rendered by 'formatCodecError'.  A structured failure exposes only a safe
-- category and source position.
data CodecFailure
  = OpaqueCodecFailure Text
  | CodecFailureAt CodecFailureCategory CodecSourcePosition
  | CodecInputEncodingFailure Text
  deriving (Eq)


instance Show CodecFailure where
  show (OpaqueCodecFailure _) = "OpaqueCodecFailure <redacted>"
  show (CodecFailureAt category position) =
    "CodecFailureAt " <> show category <> " " <> show position
  show (CodecInputEncodingFailure _) =
    "CodecInputEncodingFailure <redacted>"


instance IsString CodecFailure where
  fromString = OpaqueCodecFailure . Text.pack


-- | A controlled external input declared by a codec.
newtype ExternalInputRequest = ExternalInputRequest Text
  deriving (Eq, Ord, Show)


-- | A resolved external input and its cache fingerprint.
data ExternalInput = ExternalInput
  { value :: OpaqueBytes
  -- ^ Input value, redacted by 'Show'.
  , fingerprint :: Text
  -- ^ Stable fingerprint used in the codec cache key.
  }
  deriving (Eq, Show)


-- | How a named codec input handles absence.
data CodecInputPresence
  = RequiredInput
  | DeferredInput
  deriving (Eq, Ord, Show)


-- | Named inputs and optional whole-namespace selection for one namespace.
data CodecInputSelection = CodecInputSelection
  { namedInputs :: Map Text CodecInputPresence
  -- ^ Statically referenced names and their missing-input behavior.
  , includeAllInputs :: Bool
  -- ^ Whether every currently available member belongs to the dependency set.
  , manifestInputsOnly :: Bool
  -- ^ Whether variables must come from active manifest declarations.
  }
  deriving (Eq, Show)


-- | Selects no values from a codec input namespace.
noCodecInputs :: CodecInputSelection
noCodecInputs = CodecInputSelection Map.empty False False


-- | Selects named inputs and requires every selected value to be present.
requiredCodecInputs :: [Text] -> CodecInputSelection
requiredCodecInputs names =
  CodecInputSelection
    (Map.fromList [(name, RequiredInput) | name <- names])
    False
    False


-- | Inputs a codec declares before it is evaluated.
data CodecRequirements = CodecRequirements
  { facts :: CodecInputSelection
  -- ^ Machine facts the codec reads.
  , variables :: CodecInputSelection
  -- ^ Manifest variables the codec reads.
  , externalInputs :: [ExternalInputRequest]
  -- ^ External inputs resolved through 'CodecRuntime'.
  }
  deriving (Eq, Show)


-- | Validated configuration and fully resolved inputs passed to a codec
-- implementation.
data CodecInputs = CodecInputs
  { rawSource :: OpaqueBytes
  -- ^ Raw repository representation.
  , configuration :: CodecConfiguration
  -- ^ Route configuration accepted by 'validateConfiguration'.
  , facts :: Map Text ByteString
  -- ^ Only the facts declared by the codec.
  , variables :: Map Text ByteString
  -- ^ Only the variables declared by the codec.
  , externalInputs :: Map ExternalInputRequest ExternalInput
  -- ^ Only the external inputs declared by the codec.
  }
  deriving (Eq)


instance Show CodecInputs where
  show inputs =
    "CodecInputs { rawSource = "
      <> show inputs.rawSource
      <> ", configuration = <redacted>"
      <> ", facts = <redacted: "
      <> show (Map.size inputs.facts)
      <> ">, variables = <redacted: "
      <> show (Map.size inputs.variables)
      <> ">, externalInputs = <redacted: "
      <> show (Map.size inputs.externalInputs)
      <> "> }"


-- | A pure codec transformation registered by the application or a test.
--
-- Implementations receive the validated route configuration and only resolved,
-- declared inputs.  Effects belong to 'CodecRuntime.resolveExternalInput', so
-- transformations cannot bypass the runtime's capability boundary.
data CodecImplementation = CodecImplementationWithSourceRequirements
  { definition :: CodecDefinition
  -- ^ Stable identity, version, and reflect policy.
  , validateConfiguration
      :: CodecConfiguration -> Either CodecFailure CodecRequirements
  -- ^ Validates configuration and declares every allowed input.
  , requirementsForSource
      :: CodecConfiguration -> OpaqueBytes -> Either CodecFailure CodecRequirements
  -- ^ Validates raw source and declares inputs referenced by that source.
  , forward :: CodecInputs -> Either CodecFailure ByteString
  -- ^ Renders repository representation into deployed bytes.
  , reverse :: Maybe (CodecInputs -> OpaqueBytes -> Either CodecFailure ByteString)
  -- ^ Reconstructs repository representation for a re-add codec.
  , cacheScope :: CacheScope
  -- ^ Whether machine state may persist cache metadata.
  , dryRunPolicy :: CodecDryRunPolicy
  -- ^ Whether an uncached dry-run may evaluate this codec.
  }


-- | Constructs a codec whose input requirements depend only on configuration.
--
-- This compatibility constructor supplies no source-derived requirements.
pattern CodecImplementation
  :: CodecDefinition
  -- ^ Stable codec identity, version, and reflection policy.
  -> (CodecConfiguration -> Either CodecFailure CodecRequirements)
  -- ^ Validates configuration and declares configuration-derived inputs.
  -> (CodecInputs -> Either CodecFailure ByteString)
  -- ^ Renders repository bytes with the declared inputs.
  -> Maybe (CodecInputs -> OpaqueBytes -> Either CodecFailure ByteString)
  -- ^ Optionally reconstructs repository bytes from deployed bytes.
  -> CacheScope
  -- ^ Whether rendered results may be persisted in machine state.
  -> CodecDryRunPolicy
  -- ^ Whether an uncached dry-run may execute the transformations.
  -> CodecImplementation
  -- ^ A codec implementation with no source-derived requirements.
pattern CodecImplementation
  definition
  validateConfiguration
  forward
  reverseImplementation
  cacheScope
  dryRunPolicy <-
  CodecImplementationWithSourceRequirements
    definition
    validateConfiguration
    _
    forward
    reverseImplementation
    cacheScope
    dryRunPolicy
 where
  CodecImplementation definition validateConfiguration forward reverseImplementation cacheScope dryRunPolicy =
    CodecImplementationWithSourceRequirements
      definition
      validateConfiguration
      (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
      forward
      reverseImplementation
      cacheScope
      dryRunPolicy


{-# COMPLETE CodecImplementation #-}


-- | Constructs a codec with configuration- and source-derived requirements.
codecImplementationWithSourceRequirements
  :: CodecDefinition
  -- ^ Stable codec identity, version, and reflection policy.
  -> (CodecConfiguration -> Either CodecFailure CodecRequirements)
  -- ^ Validates configuration and declares configuration-derived inputs.
  -> (CodecConfiguration -> OpaqueBytes -> Either CodecFailure CodecRequirements)
  -- ^ Validates source bytes and declares inputs referenced by that source.
  -> (CodecInputs -> Either CodecFailure ByteString)
  -- ^ Renders repository bytes with the combined declared inputs.
  -> Maybe (CodecInputs -> OpaqueBytes -> Either CodecFailure ByteString)
  -- ^ Optionally reconstructs repository bytes from deployed bytes.
  -> CacheScope
  -- ^ Whether rendered results may be persisted in machine state.
  -> CodecDryRunPolicy
  -- ^ Whether an uncached dry-run may execute the transformations.
  -> CodecImplementation
  -- ^ A codec implementation with configuration- and source-derived inputs.
codecImplementationWithSourceRequirements = CodecImplementationWithSourceRequirements


-- | Runtime capabilities supplied explicitly to codec evaluation.
data CodecRuntime m = CodecRuntime
  { registry :: Map CodecName CodecImplementation
  -- ^ Registered implementations, indexed by exact codec name.
  , mode :: EvaluationMode
  -- ^ Normal or dry-run execution.
  , resolveExternalInput
      :: ExternalInputRequest -> m (Either Text ExternalInput)
  -- ^ Controlled resolver for declared external inputs.
  }


-- | One request to render a selected route entry.
data CodecEvaluationRequest = CodecEvaluationRequest
  { routeName :: Text
  -- ^ Redacted route identity used in errors.
  , codec :: CodecSpec
  -- ^ Declarative route codec.
  , rawSource :: OpaqueBytes
  -- ^ Raw repository representation.
  , facts :: Map Text Text
  -- ^ Facts available to the selected route.
  , variables :: Map Text ByteString
  -- ^ Manifest variables available to the selected route as native bytes.
  }
  deriving (Eq)


instance Show CodecEvaluationRequest where
  show request =
    "CodecEvaluationRequest { routeName = "
      <> show request.routeName
      <> ", codec = "
      <> show request.codec
      <> ", rawSource = "
      <> show request.rawSource
      <> ", facts = <redacted: "
      <> show (Map.size request.facts)
      <> ">, variables = <redacted: "
      <> show (Map.size request.variables)
      <> "> }"


-- | A cache record whose bytes come from the current intermediate snapshot.
data CodecCacheEntry = CodecCacheEntry
  { cacheKey :: ByteString
  , renderedBytes :: OpaqueBytes
  }
  deriving (Eq, Show)


-- | A successful codec evaluation.
data EvaluatedCodec = EvaluatedCodec
  { renderedBytes :: OpaqueBytes
  -- ^ Deployed bytes, redacted by 'Show'.
  , cacheKey :: ByteString
  -- ^ Deterministic key for this stable input set.
  , dependencies :: [CodecDependency]
  -- ^ Redacted dependency identities and fingerprints.
  , definition :: CodecDefinition
  -- ^ Registered implementation metadata.
  , cacheScope :: CacheScope
  -- ^ Permitted cache lifetime.
  , resolvedInputs :: CodecInputs
  -- ^ Exact command-scoped inputs used to produce 'renderedBytes'.
  , requirements :: CodecRequirements
  -- ^ Source-derived input selection used for this evaluation.
  }
  deriving (Eq)


instance Show EvaluatedCodec where
  show evaluated =
    "EvaluatedCodec { renderedBytes = "
      <> show evaluated.renderedBytes
      <> ", cacheKey = "
      <> show evaluated.cacheKey
      <> ", dependencies = "
      <> show evaluated.dependencies
      <> ", definition = "
      <> show evaluated.definition
      <> ", cacheScope = "
      <> show evaluated.cacheScope
      <> ", resolvedInputs = <redacted>, requirements = "
      <> show evaluated.requirements
      <> " }"


data CodecErrorKind
  = UnknownCodec
  | InvalidConfiguration CodecFailure
  | InvalidSource CodecFailure
  | MissingInput Text
  | ExternalInputFailure Text Text
  | EvaluationFailure CodecFailure
  | DryRunCacheRequired
  | UnsupportedSourceType Text
  | ReflectionRejected
  | ReverseUnavailable
  | ReverseFailure CodecFailure
  | ReverseValidationFailure
  deriving (Eq, Show)


-- | A typed codec failure that contains no source or rendered bytes.
data CodecError = CodecError Text CodecName CodecErrorKind
  deriving (Eq)


instance Show CodecError where
  show = Text.unpack . formatCodecError


-- | Builds a registry.  Later duplicate definitions replace earlier ones.
codecRegistry
  :: [CodecImplementation] -> Map CodecName CodecImplementation
codecRegistry = Map.fromList . fmap entry
 where
  entry :: CodecImplementation -> (CodecName, CodecImplementation)
  entry implementation =
    let CodecDefinition name _ _ = implementation.definition
    in (name, implementation)


-- | Runtime containing only the built-in identity codec.
identityCodecRuntime
  :: (Applicative m)
  => EvaluationMode
  -- ^ Whether transformations may run or must obey dry-run policy.
  -> CodecRuntime m
  -- ^ Runtime with the identity codec and no external-input resolver.
identityCodecRuntime mode =
  CodecRuntime
    { registry = codecRegistry [identityImplementation]
    , mode = mode
    , resolveExternalInput = \_ -> pure $ Left "identity declares no external input"
    }
 where
  identityImplementation =
    codecImplementationWithSourceRequirements
      identityCodecDefinition
      ( \configuration ->
          if configuration == CodecConfiguration Map.empty
            then Right $ CodecRequirements noCodecInputs noCodecInputs []
            else Left "identity does not accept configuration"
      )
      (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
      (Right . revealBytes . (.rawSource))
      (Just $ \_ deployed -> Right $ revealBytes deployed)
      PersistentCache
      EvaluatePurely


requirementsForImplementation
  :: CodecEvaluationRequest
  -> CodecImplementation
  -> Either CodecError CodecRequirements
requirementsForImplementation request implementation = do
  base <- case implementation.validateConfiguration configuration of
    Left failure ->
      Left $ CodecError request.routeName codecName $ InvalidConfiguration failure
    Right requirements -> Right requirements
  source <- case implementation.requirementsForSource configuration request.rawSource of
    Left failure -> Left $ CodecError request.routeName codecName $ InvalidSource failure
    Right requirements -> Right requirements
  return $ mergeCodecRequirements base source
 where
  CodecSpec codecName configuration = request.codec


mergeCodecRequirements
  :: CodecRequirements -> CodecRequirements -> CodecRequirements
mergeCodecRequirements left right =
  CodecRequirements
    { facts = mergeSelection left.facts right.facts
    , variables = mergeSelection left.variables right.variables
    , externalInputs = nub $ left.externalInputs <> right.externalInputs
    }
 where
  mergeSelection
    :: CodecInputSelection -> CodecInputSelection -> CodecInputSelection
  mergeSelection first second =
    CodecInputSelection
      { namedInputs =
          Map.unionWith
            mergePresence
            first.namedInputs
            second.namedInputs
      , includeAllInputs = first.includeAllInputs || second.includeAllInputs
      , -- The stricter source-isolation requirement must win when either side
        -- declares it; an empty selection is neutral during requirement merging.
        manifestInputsOnly = first.manifestInputsOnly || second.manifestInputsOnly
      }
  mergePresence RequiredInput _ = RequiredInput
  mergePresence _ RequiredInput = RequiredInput
  mergePresence DeferredInput DeferredInput = DeferredInput


-- | Evaluates a selected route codec or reuses an exact cache entry.
evaluateCodec
  :: (Monad m)
  => CodecRuntime m
  -- ^ Registry, evaluation mode, and external-input resolver to use.
  -> CodecEvaluationRequest
  -- ^ Route, codec specification, source bytes, and available inputs.
  -> Maybe CodecCacheEntry
  -- ^ Previously rendered entry to reuse when its key still matches.
  -> m (Either CodecError EvaluatedCodec)
  -- ^ A redacted codec error or the rendered bytes and cache metadata.
evaluateCodec runtime request cached = case Map.lookup codecName runtime.registry of
  Nothing -> return $ Left $ CodecError request.routeName codecName UnknownCodec
  Just implementation -> case requirementsForImplementation request implementation of
    Left err -> return $ Left err
    Right _
      | runtime.mode == DryRunEvaluation
      , implementation.dryRunPolicy == CachedOnly
      , Nothing <- cached ->
          return $
            Left $
              CodecError request.routeName codecName DryRunCacheRequired
    Right requirements
      | runtime.mode == DryRunEvaluation
      , implementation.dryRunPolicy == CachedOnly
      , not $ null requirements.externalInputs ->
          return $
            Left $
              CodecError request.routeName codecName DryRunCacheRequired
    Right requirements -> do
      resolved <- resolveInputs runtime request requirements
      case resolved of
        Left err -> return $ Left err
        Right (inputs, dependencies) -> do
          let CodecDefinition _ version _ = implementation.definition
              key = codecCacheKey request.codec version rawSource dependencies
              makeResult bytes =
                EvaluatedCodec
                  bytes
                  key
                  dependencies
                  implementation.definition
                  implementation.cacheScope
                  inputs
                  requirements
          case cached of
            Just entry | entry.cacheKey == key -> return $ Right $ makeResult entry.renderedBytes
            _
              | runtime.mode == DryRunEvaluation
              , implementation.dryRunPolicy == CachedOnly ->
                  return $
                    Left $
                      CodecError request.routeName codecName DryRunCacheRequired
              | otherwise ->
                  return $ case implementation.forward inputs of
                    Left reason ->
                      Left $
                        CodecError request.routeName codecName $
                          EvaluationFailure reason
                    Right bytes -> Right $ makeResult $ opaqueBytes bytes
 where
  CodecSpec codecName _ = request.codec
  rawSource = revealBytes request.rawSource


-- | Evaluates a codec with the resolved inputs captured by an earlier
-- evaluation, replacing only its raw repository source.
--
-- This is used after re-add reflection writes a reconstructed source.  It
-- recomputes rendered bytes and the cache key without resolving variables or
-- external inputs again.
reevaluateCodec
  :: (Monad m)
  => CodecRuntime m
  -> CodecEvaluationRequest
  -> EvaluatedCodec
  -> m (Either CodecError EvaluatedCodec)
reevaluateCodec runtime request previous =
  case Map.lookup codecName runtime.registry of
    Nothing -> return $ Left $ CodecError request.routeName codecName UnknownCodec
    Just implementation ->
      case requirementsForImplementation request implementation of
        Left err -> return $ Left err
        Right _
          | runtime.mode == DryRunEvaluation
          , implementation.dryRunPolicy == CachedOnly ->
              return $
                Left $
                  CodecError request.routeName codecName DryRunCacheRequired
        Right requirements
          | implementation.definition /= previous.definition ->
              return $
                Left $
                  CodecError request.routeName codecName $
                    InvalidConfiguration
                      "command-scoped evaluation does not match the codec"
          | previous.resolvedInputs.configuration /= configuration ->
              return $
                Left $
                  CodecError request.routeName codecName $
                    InvalidConfiguration
                      "command-scoped evaluation does not match the configuration"
          | requirements /= previous.requirements ->
              return $
                Left $
                  CodecError request.routeName codecName $
                    InvalidConfiguration
                      "source input requirements changed during evaluation"
          | otherwise ->
              let CodecDefinition _ version _ = implementation.definition
                  previousInputs = previous.resolvedInputs
                  inputs =
                    CodecInputs
                      request.rawSource
                      configuration
                      previousInputs.facts
                      previousInputs.variables
                      previousInputs.externalInputs
                  key =
                    codecCacheKey
                      request.codec
                      version
                      (revealBytes request.rawSource)
                      previous.dependencies
                  makeResult bytes =
                    EvaluatedCodec
                      (opaqueBytes bytes)
                      key
                      previous.dependencies
                      implementation.definition
                      implementation.cacheScope
                      inputs
                      requirements
              in return $ case implementation.forward inputs of
                   Left reason ->
                     Left $
                       CodecError request.routeName codecName $
                         EvaluationFailure reason
                   Right bytes -> Right $ makeResult bytes
 where
  CodecSpec codecName configuration = request.codec


-- | Reconstructs repository bytes according to a codec's reflection policy.
-- Re-add codecs must reproduce the exact deployed bytes when run forward.
reflectCodec
  :: (Monad m)
  => CodecRuntime m
  -> CodecEvaluationRequest
  -> OpaqueBytes
  -> m (Either CodecError OpaqueBytes)
reflectCodec runtime request deployed =
  fmap (fmap fst) $ reflectCodecWithEvaluation runtime request deployed


-- | Reconstructs repository bytes and retains the evaluated re-add input
-- snapshot for command-scoped convergence tracking.
reflectCodecWithEvaluation
  :: (Monad m)
  => CodecRuntime m
  -> CodecEvaluationRequest
  -> OpaqueBytes
  -> m (Either CodecError (OpaqueBytes, Maybe EvaluatedCodec))
reflectCodecWithEvaluation = reflectCodecUsingSourceRequirements


-- | Reconstructs repository bytes when the repository source is missing.
--
-- Only configuration requirements are resolved before reversal.  The
-- reconstructed source is then analyzed and evaluated by the normal
-- round-trip validation, so a missing source is never represented to a codec
-- as a real empty source.
reflectCodecWithoutSourceWithEvaluation
  :: (Monad m)
  => CodecRuntime m
  -> CodecEvaluationRequest
  -> OpaqueBytes
  -> m (Either CodecError (OpaqueBytes, Maybe EvaluatedCodec))
reflectCodecWithoutSourceWithEvaluation runtime request deployed =
  fmap (fmap $ \(source, evaluated, ()) -> (source, evaluated)) $
    reflectCodecWithoutSourceWithEvaluationBy
      runtime
      (const $ return (request.variables, ()))
      request
      deployed


-- | Reconstructs a missing repository source and lets the caller resolve
-- variables selected by the reconstructed source before round-trip validation.
--
-- The callback receives the combined configuration and reconstructed-source
-- requirements.  Its annotation is returned with the reconstructed bytes and
-- evaluation so callers can retain variable-resolution warnings.
reflectCodecWithoutSourceWithEvaluationBy
  :: (Monad m)
  => CodecRuntime m
  -- ^ Registry, mode, and external-input resolver used for both stages.
  -> (CodecRequirements -> m (Map Text ByteString, a))
  -- ^ Resolves variables selected after the source has been reconstructed and
  -- returns caller-defined metadata such as warnings.
  -> CodecEvaluationRequest
  -- ^ Route, codec, configuration inputs, and the missing-source placeholder.
  -> OpaqueBytes
  -- ^ Deployed bytes from which to reconstruct repository bytes.
  -> m (Either CodecError (OpaqueBytes, Maybe EvaluatedCodec, a))
  -- ^ Reconstructed bytes, their evaluated snapshot, and callback metadata.
reflectCodecWithoutSourceWithEvaluationBy
  runtime
  resolveVariables
  request
  deployed = case Map.lookup codecName runtime.registry of
    Nothing -> return $ Left $ CodecError request.routeName codecName UnknownCodec
    Just implementation -> case implementation.validateConfiguration configuration of
      Left reason ->
        return $
          Left $
            CodecError request.routeName codecName $
              InvalidConfiguration reason
      Right baseRequirements -> case implementation.definition of
        CodecDefinition _ _ ReflectIdentity -> do
          (_, annotation) <- resolveVariables baseRequirements
          return $ Right (deployed, Nothing, annotation)
        CodecDefinition _ _ ReflectReject ->
          return $ Left $ CodecError request.routeName codecName ReflectionRejected
        CodecDefinition _ _ ReflectReAdd
          | runtime.mode == DryRunEvaluation
          , implementation.dryRunPolicy == CachedOnly ->
              return $ Left $ CodecError request.routeName codecName DryRunCacheRequired
        CodecDefinition _ _ ReflectReAdd -> do
          resolved <- resolveInputs runtime request baseRequirements
          case resolved of
            Left err -> return $ Left err
            Right (inputs, _) ->
              fmap
                ( fmap $ \((source, evaluated), annotation) ->
                    (source, Just evaluated, annotation)
                )
                $ reverseWithoutSourceWithInputs
                  runtime
                  resolveVariables
                  request
                  implementation
                  inputs
                  deployed
   where
    CodecSpec codecName configuration = request.codec


reflectCodecUsingSourceRequirements
  :: (Monad m)
  => CodecRuntime m
  -> CodecEvaluationRequest
  -> OpaqueBytes
  -> m (Either CodecError (OpaqueBytes, Maybe EvaluatedCodec))
reflectCodecUsingSourceRequirements runtime request deployed = case Map.lookup codecName runtime.registry of
  Nothing -> return $ Left $ CodecError request.routeName codecName UnknownCodec
  Just implementation -> case implementation.validateConfiguration configuration of
    Left reason ->
      return $
        Left $
          CodecError request.routeName codecName $
            InvalidConfiguration reason
    Right baseRequirements -> case implementation.definition of
      CodecDefinition _ _ ReflectIdentity -> return $ Right (deployed, Nothing)
      CodecDefinition _ _ ReflectReject ->
        return $ Left $ CodecError request.routeName codecName ReflectionRejected
      CodecDefinition _ _ ReflectReAdd
        | runtime.mode == DryRunEvaluation
        , implementation.dryRunPolicy == CachedOnly ->
            return $ Left $ CodecError request.routeName codecName DryRunCacheRequired
      CodecDefinition _ _ ReflectReAdd -> do
        case implementation.requirementsForSource
          configuration
          request.rawSource of
          Left failure ->
            return $ Left $ CodecError request.routeName codecName $ InvalidSource failure
          Right sourceRequirements -> do
            let requirements =
                  mergeCodecRequirements baseRequirements sourceRequirements
            resolved <- resolveInputs runtime request requirements
            case resolved of
              Left err -> return $ Left err
              Right (inputs, dependencies) ->
                return $
                  fmap (\(source, evaluated) -> (source, Just evaluated)) $
                    reverseWithInputs
                      request
                      implementation
                      requirements
                      inputs
                      dependencies
                      deployed
 where
  CodecSpec codecName configuration = request.codec


-- | Reconstructs repository bytes using the exact inputs captured by a prior
-- successful evaluation in this command.
reflectEvaluatedCodec
  :: (Monad m)
  => CodecRuntime m
  -> CodecEvaluationRequest
  -> EvaluatedCodec
  -> OpaqueBytes
  -> m (Either CodecError OpaqueBytes)
reflectEvaluatedCodec runtime request evaluated deployed =
  fmap (fmap fst) $
    reflectEvaluatedCodecWithEvaluation runtime request evaluated deployed


-- | Reconstructs repository bytes with captured inputs and returns the
-- post-reflection evaluation produced by the round-trip validation.
reflectEvaluatedCodecWithEvaluation
  :: (Monad m)
  => CodecRuntime m
  -> CodecEvaluationRequest
  -> EvaluatedCodec
  -> OpaqueBytes
  -> m (Either CodecError (OpaqueBytes, Maybe EvaluatedCodec))
reflectEvaluatedCodecWithEvaluation runtime request evaluated deployed =
  case Map.lookup codecName runtime.registry of
    Nothing -> return $ Left $ CodecError request.routeName codecName UnknownCodec
    Just implementation -> case implementation.validateConfiguration configuration of
      Left reason ->
        return $
          Left $
            CodecError request.routeName codecName $
              InvalidConfiguration reason
      Right _ -> case implementation.definition of
        CodecDefinition _ _ ReflectIdentity -> return $ Right (deployed, Nothing)
        CodecDefinition _ _ ReflectReject ->
          return $ Left $ CodecError request.routeName codecName ReflectionRejected
        CodecDefinition _ _ ReflectReAdd
          | runtime.mode == DryRunEvaluation
          , implementation.dryRunPolicy == CachedOnly ->
              return $ Left $ CodecError request.routeName codecName DryRunCacheRequired
        CodecDefinition _ _ ReflectReAdd ->
          case requirementsForImplementation request implementation of
            Left err -> return $ Left err
            Right requirements
              | evaluated.resolvedInputs.configuration /= configuration ->
                  return $
                    Left $
                      CodecError request.routeName codecName $
                        InvalidConfiguration
                          "command-scoped evaluation does not match the configuration"
              | requirements /= evaluated.requirements ->
                  return $
                    Left $
                      CodecError request.routeName codecName $
                        InvalidConfiguration
                          "source input requirements changed during evaluation"
              | otherwise ->
                  return $
                    fmap (\(source, result) -> (source, Just result)) $
                      reverseWithInputs
                        request
                        implementation
                        requirements
                        evaluated.resolvedInputs
                        evaluated.dependencies
                        deployed
 where
  CodecSpec codecName configuration = request.codec


reverseWithInputs
  :: CodecEvaluationRequest
  -> CodecImplementation
  -> CodecRequirements
  -> CodecInputs
  -> [CodecDependency]
  -> OpaqueBytes
  -> Either CodecError (OpaqueBytes, EvaluatedCodec)
reverseWithInputs request implementation requirements inputs dependencies deployed =
  do
    source <- reverseSource request implementation inputs deployed
    let source' = opaqueBytes source
        candidateInputs =
          CodecInputs
            source'
            inputs.configuration
            inputs.facts
            inputs.variables
            inputs.externalInputs
        candidateRequest =
          CodecEvaluationRequest
            request.routeName
            request.codec
            source'
            request.facts
            request.variables
    candidateRequirements <-
      requirementsForImplementation candidateRequest implementation
    if candidateRequirements /= requirements
      then
        Left $
          CodecError request.routeName codecName $
            InvalidConfiguration
              "source input requirements changed during reverse validation"
      else
        validateReversedSource
          request
          implementation
          candidateRequirements
          candidateInputs
          dependencies
          deployed
          source
 where
  CodecSpec codecName _ = request.codec


reverseWithoutSourceWithInputs
  :: (Monad m)
  => CodecRuntime m
  -> (CodecRequirements -> m (Map Text ByteString, a))
  -> CodecEvaluationRequest
  -> CodecImplementation
  -> CodecInputs
  -> OpaqueBytes
  -> m (Either CodecError ((OpaqueBytes, EvaluatedCodec), a))
reverseWithoutSourceWithInputs
  runtime
  resolveVariables
  request
  implementation
  inputs
  deployed =
    case reverseSource request implementation inputs deployed of
      Left err -> return $ Left err
      Right source -> do
        let source' = opaqueBytes source
            candidateRequest =
              CodecEvaluationRequest
                request.routeName
                request.codec
                source'
                request.facts
                request.variables
        case requirementsForImplementation candidateRequest implementation of
          Left err -> return $ Left err
          Right candidateRequirements -> do
            (variables, annotation) <- resolveVariables candidateRequirements
            let resolvedRequest =
                  CodecEvaluationRequest
                    request.routeName
                    request.codec
                    source'
                    request.facts
                    variables
            resolved <-
              resolveInputs
                (reuseExternalInputs runtime inputs.externalInputs)
                resolvedRequest
                candidateRequirements
            return $ do
              (candidateInputs, dependencies) <- resolved
              evaluated <-
                validateReversedSource
                  request
                  implementation
                  candidateRequirements
                  candidateInputs
                  dependencies
                  deployed
                  source
              Right (evaluated, annotation)


reuseExternalInputs
  :: (Monad m)
  => CodecRuntime m
  -> Map ExternalInputRequest ExternalInput
  -> CodecRuntime m
reuseExternalInputs runtime existing =
  runtime
    { resolveExternalInput = \externalRequest ->
        case Map.lookup externalRequest existing of
          Just input -> return $ Right input
          Nothing -> runtime.resolveExternalInput externalRequest
    }


reverseSource
  :: CodecEvaluationRequest
  -> CodecImplementation
  -> CodecInputs
  -> OpaqueBytes
  -> Either CodecError ByteString
reverseSource request implementation inputs deployed =
  case implementation.reverse of
    Nothing -> Left $ CodecError request.routeName codecName ReverseUnavailable
    Just reverseCodec -> case reverseCodec inputs deployed of
      Left reason ->
        Left $
          CodecError request.routeName codecName $
            ReverseFailure reason
      Right source -> Right source
 where
  CodecSpec codecName _ = request.codec


validateReversedSource
  :: CodecEvaluationRequest
  -> CodecImplementation
  -> CodecRequirements
  -> CodecInputs
  -> [CodecDependency]
  -> OpaqueBytes
  -> ByteString
  -> Either CodecError (OpaqueBytes, EvaluatedCodec)
validateReversedSource
  request
  implementation
  requirements
  inputs
  dependencies
  deployed
  source =
    case implementation.forward inputs of
      Left reason ->
        Left $
          CodecError request.routeName codecName $
            EvaluationFailure reason
      Right bytes
        | bytes == revealBytes deployed ->
            let CodecDefinition _ version _ = implementation.definition
                key =
                  codecCacheKey
                    request.codec
                    version
                    source
                    dependencies
            in Right
                 ( inputs.rawSource
                 , EvaluatedCodec
                     deployed
                     key
                     dependencies
                     implementation.definition
                     implementation.cacheScope
                     inputs
                     requirements
                 )
        | otherwise ->
            Left $
              CodecError
                request.routeName
                codecName
                ReverseValidationFailure
   where
    CodecSpec codecName _ = request.codec


-- | Validates a codec configuration and returns its configuration-declared
-- inputs without inspecting source bytes or resolving any input.
codecConfigurationRequirements
  :: CodecRuntime m
  -- ^ Runtime containing the codec registry.
  -> Text
  -- ^ Route name used to scope any validation error.
  -> CodecSpec
  -- ^ Codec name and configuration to validate.
  -> Either CodecError CodecRequirements
  -- ^ A redacted validation error or the configuration-declared inputs.
codecConfigurationRequirements runtime routeName (CodecSpec name configuration) =
  case Map.lookup name runtime.registry of
    Nothing -> Left $ CodecError routeName name UnknownCodec
    Just implementation ->
      case implementation.validateConfiguration configuration of
        Left failure ->
          Left $ CodecError routeName name $ InvalidConfiguration failure
        Right requirements -> Right requirements


-- | Validates a codec configuration and source, returning all declared inputs
-- without resolving or evaluating any of them.
codecRequirements
  :: CodecRuntime m
  -- ^ Runtime containing the codec registry.
  -> Text
  -- ^ Route name used to scope any validation error.
  -> CodecSpec
  -- ^ Codec name and configuration to validate.
  -> OpaqueBytes
  -- ^ Raw source whose referenced inputs should be discovered.
  -> Either CodecError CodecRequirements
  -- ^ A redacted validation error or the codec's declared inputs.
codecRequirements runtime routeName spec rawSource = case Map.lookup name runtime.registry of
  Nothing -> Left $ CodecError routeName name UnknownCodec
  Just implementation ->
    requirementsForImplementation
      (CodecEvaluationRequest routeName spec rawSource Map.empty Map.empty)
      implementation
 where
  CodecSpec name _ = spec


-- | Returns the validated reflection policy for a registered codec.
codecReflectPolicy
  :: CodecRuntime m
  -- ^ Runtime containing the codec registry.
  -> Text
  -- ^ Route name used to scope any validation error.
  -> CodecSpec
  -- ^ Codec name and configuration whose policy is requested.
  -> Either CodecError ReflectPolicy
  -- ^ A redacted validation error or the codec's reflection policy.
codecReflectPolicy _ _ spec
  | spec == identityCodecSpec = Right ReflectIdentity
codecReflectPolicy runtime routeName (CodecSpec name configuration) =
  case Map.lookup name runtime.registry of
    Nothing -> Left $ CodecError routeName name UnknownCodec
    Just implementation -> case implementation.validateConfiguration configuration of
      Left failure ->
        Left $ CodecError routeName name $ InvalidConfiguration failure
      Right _ -> case implementation.definition of
        CodecDefinition _ _ policy -> Right policy


-- | Constructs a redacted error for a source entry a codec cannot render.
codecSourceTypeError
  :: Text
  -- ^ Route name used to scope the error.
  -> CodecSpec
  -- ^ Codec specification associated with the route.
  -> Text
  -- ^ Human-readable name of the unsupported source entry type.
  -> CodecError
  -- ^ A redacted unsupported-source error.
codecSourceTypeError routeName (CodecSpec name _) sourceType =
  CodecError routeName name $ UnsupportedSourceType sourceType


resolveInputs
  :: (Monad m)
  => CodecRuntime m
  -> CodecEvaluationRequest
  -> CodecRequirements
  -> m (Either CodecError (CodecInputs, [CodecDependency]))
resolveInputs runtime request requirements = case selectTextInputs of
  Left err -> return $ Left err
  Right (facts, variables, dependencies) -> do
    externalResult <- resolveExternals requirements.externalInputs
    return $ do
      resolvedExternal <- externalResult
      let externalMap = Map.fromList resolvedExternal
          externalDependencies =
            [ CodecDependency
                ("external:" <> requestName externalRequest)
                input.fingerprint
            | (externalRequest, input) <- resolvedExternal
            ]
      Right
        ( CodecInputs
            request.rawSource
            configuration
            facts
            variables
            externalMap
        , dependencies <> externalDependencies
        )
 where
  CodecSpec codecName configuration = request.codec
  selectTextInputs = do
    (facts, factDependencies) <-
      selectInputs
        "fact"
        foldCase
        codecName
        request.routeName
        requirements.facts
        (encodeUtf8 <$> request.facts)
    (variables, variableDependencies) <-
      selectInputs
        "variable"
        id
        codecName
        request.routeName
        requirements.variables
        request.variables
    Right (facts, variables, factDependencies <> variableDependencies)
  resolveExternal externalRequest = do
    result <- runtime.resolveExternalInput externalRequest
    return $ case result of
      Left reason ->
        Left $
          CodecError request.routeName codecName $
            ExternalInputFailure (requestName externalRequest) reason
      Right input -> Right (externalRequest, input)
  resolveExternals [] = return $ Right []
  resolveExternals (externalRequest : externalRequests) = do
    result <- resolveExternal externalRequest
    case result of
      Left err -> return $ Left err
      Right resolved ->
        fmap (resolved :) <$> resolveExternals externalRequests


selectInputs
  :: Text
  -> (Text -> Text)
  -> CodecName
  -> Text
  -> CodecInputSelection
  -> Map Text ByteString
  -> Either CodecError (Map Text ByteString, [CodecDependency])
selectInputs namespace canonicalize codecName routeName selection available = do
  selected <- traverse selectNamed $ Map.toAscList selection.namedInputs
  let namedValues =
        Map.fromList
          [ (name, bytes)
          | (name, Just bytes, _) <- selected
          ]
      namedDependencies =
        Map.fromList
          [ (identity, dependency)
          | (_, _, dependency@(CodecDependency identity _)) <- selected
          ]
      allValues = if selection.includeAllInputs then canonicalAvailable else Map.empty
      allDependencies =
        if selection.includeAllInputs
          then Map.mapWithKey makeDependency canonicalAvailable
          else Map.empty
  return
    ( Map.union namedValues allValues
    , Map.elems $ Map.union namedDependencies allDependencies
    )
 where
  canonicalAvailable = Map.mapKeys canonicalize available
  selectNamed (name, presence) =
    let canonicalName = canonicalize name
        dependency value = CodecDependency (namespace <> ":" <> canonicalName) value
    in case Map.lookup canonicalName canonicalAvailable of
         Nothing ->
           case presence of
             RequiredInput ->
               Left $
                 CodecError routeName codecName $
                   MissingInput $
                     namespace <> ":" <> name
             DeferredInput ->
               Right
                 ( name
                 , Nothing
                 , dependency "missing"
                 )
         Just bytes ->
           Right
             ( name
             , Just bytes
             , dependency $ digestText bytes
             )
  makeDependency name bytes =
    CodecDependency (namespace <> ":" <> name) $ digestText bytes


requestName :: ExternalInputRequest -> Text
requestName (ExternalInputRequest name) = name


-- | Formats a redacted, route-scoped codec error.
formatCodecError
  :: CodecError
  -- ^ Typed codec error whose sensitive details must remain hidden.
  -> Text
  -- ^ User-facing error text safe to log or display.
formatCodecError (CodecError routeName codecName kind) =
  "Route " <> routeName <> " codec " <> renderCodecName codecName <> case kind of
    UnknownCodec -> " is not registered."
    InvalidConfiguration _ -> " has invalid configuration."
    InvalidSource failure -> formatSourceFailure failure
    MissingInput dependency -> " is missing declared input " <> dependency <> "."
    ExternalInputFailure dependency _ ->
      " could not resolve declared input " <> dependency <> "."
    EvaluationFailure failure -> formatEvaluationFailure failure
    DryRunCacheRequired -> " cannot run during dry-run without a valid cache."
    UnsupportedSourceType sourceType ->
      " cannot render source entry type " <> sourceType <> "."
    ReflectionRejected -> " rejects reflection."
    ReverseUnavailable -> " has no reverse implementation."
    ReverseFailure _ -> " failed while reversing."
    ReverseValidationFailure -> " failed reverse validation."
 where
  at :: CodecSourcePosition -> Text
  at position =
    " at line "
      <> Text.pack (show position.line)
      <> ", column "
      <> Text.pack (show position.column)
      <> "."
  formatSourceFailure (OpaqueCodecFailure _) = " has invalid source."
  formatSourceFailure (CodecInputEncodingFailure dependency) =
    " has template input " <> dependency <> " with invalid platform text."
  formatSourceFailure (CodecFailureAt category position) = case category of
    InvalidSourceEncoding -> " has source that is not valid UTF-8" <> at position
    InvalidSourceSyntax -> " has invalid source syntax" <> at position
    UnsupportedSourceSyntax -> " uses unsupported source syntax" <> at position
    MissingSourceInput dependency ->
      " is missing template input " <> dependency <> at position
    SourceEvaluationFailure -> " failed while validating source" <> at position
  formatEvaluationFailure (OpaqueCodecFailure _) = " failed while rendering."
  formatEvaluationFailure (CodecInputEncodingFailure dependency) =
    " has template input " <> dependency <> " with invalid platform text."
  formatEvaluationFailure (CodecFailureAt category position) = case category of
    MissingSourceInput dependency ->
      " is missing template input " <> dependency <> at position
    _ -> " failed while rendering" <> at position


digestText :: ByteString -> Text
digestText = Text.pack . concatMap byteHex . ByteString.unpack . SHA256.hash
 where
  byteHex byte = case showHex byte "" of
    [digit] -> ['0', digit]
    digits -> digits
