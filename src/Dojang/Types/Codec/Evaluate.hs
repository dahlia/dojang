{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Controlled evaluation of declarative route codecs.
module Dojang.Types.Codec.Evaluate
  ( CacheScope (..)
  , CodecCacheEntry (..)
  , CodecDryRunPolicy (..)
  , CodecError
  , CodecEvaluationRequest (..)
  , CodecImplementation (..)
  , CodecInputs (..)
  , CodecRequirements (..)
  , CodecRuntime (..)
  , EvaluationMode (..)
  , EvaluatedCodec (..)
  , ExternalInput (..)
  , ExternalInputRequest (..)
  , OpaqueBytes
  , codecRegistry
  , codecRequirements
  , codecReflectPolicy
  , codecSourceTypeError
  , evaluateCodec
  , formatCodecError
  , identityCodecRuntime
  , opaqueBytes
  , reevaluateCodec
  , reflectCodec
  , reflectCodecWithEvaluation
  , reflectEvaluatedCodec
  , reflectEvaluatedCodecWithEvaluation
  , revealBytes
  ) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.CaseInsensitive (foldCase)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
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


-- | Inputs a codec declares before it is evaluated.
data CodecRequirements = CodecRequirements
  { facts :: Set Text
  -- ^ Machine facts the codec reads.
  , variables :: Set Text
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
data CodecImplementation = CodecImplementation
  { definition :: CodecDefinition
  -- ^ Stable identity, version, and reflect policy.
  , validateConfiguration
      :: CodecConfiguration -> Either Text CodecRequirements
  -- ^ Validates configuration and declares every allowed input.
  , forward :: CodecInputs -> Either Text ByteString
  -- ^ Renders repository representation into deployed bytes.
  , reverse :: Maybe (CodecInputs -> OpaqueBytes -> Either Text ByteString)
  -- ^ Reconstructs repository representation for a re-add codec.
  , cacheScope :: CacheScope
  -- ^ Whether machine state may persist cache metadata.
  , dryRunPolicy :: CodecDryRunPolicy
  -- ^ Whether an uncached dry-run may evaluate this codec.
  }


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
      <> ", resolvedInputs = <redacted> }"


data CodecErrorKind
  = UnknownCodec
  | InvalidConfiguration Text
  | MissingInput Text
  | ExternalInputFailure Text Text
  | EvaluationFailure Text
  | DryRunCacheRequired
  | UnsupportedSourceType Text
  | ReflectionRejected
  | ReverseUnavailable
  | ReverseFailure Text
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
  entry implementation@(CodecImplementation (CodecDefinition name _ _) _ _ _ _ _) =
    (name, implementation)


-- | Runtime containing only the built-in identity codec.
identityCodecRuntime :: (Applicative m) => EvaluationMode -> CodecRuntime m
identityCodecRuntime mode =
  CodecRuntime
    { registry = codecRegistry [identityImplementation]
    , mode = mode
    , resolveExternalInput = \_ -> pure $ Left "identity declares no external input"
    }
 where
  identityImplementation =
    CodecImplementation
      { definition = identityCodecDefinition
      , validateConfiguration = \configuration ->
          if configuration == CodecConfiguration Map.empty
            then Right $ CodecRequirements Set.empty Set.empty []
            else Left "identity does not accept configuration"
      , forward = Right . revealBytes . (.rawSource)
      , reverse = Just $ \_ deployed -> Right $ revealBytes deployed
      , cacheScope = PersistentCache
      , dryRunPolicy = EvaluatePurely
      }


-- | Evaluates a selected route codec or reuses an exact cache entry.
evaluateCodec
  :: (Monad m)
  => CodecRuntime m
  -> CodecEvaluationRequest
  -> Maybe CodecCacheEntry
  -> m (Either CodecError EvaluatedCodec)
evaluateCodec runtime request cached = case Map.lookup codecName runtime.registry of
  Nothing -> return $ Left $ CodecError request.routeName codecName UnknownCodec
  Just implementation -> case implementation.validateConfiguration configuration of
    Left reason ->
      return $
        Left $
          CodecError request.routeName codecName $
            InvalidConfiguration reason
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
  CodecSpec codecName configuration = request.codec
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
      case implementation.validateConfiguration configuration of
        Left reason ->
          return $
            Left $
              CodecError request.routeName codecName $
                InvalidConfiguration reason
        Right _
          | runtime.mode == DryRunEvaluation
          , implementation.dryRunPolicy == CachedOnly ->
              return $
                Left $
                  CodecError request.routeName codecName DryRunCacheRequired
        Right _
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
reflectCodecWithEvaluation runtime request deployed = case Map.lookup codecName runtime.registry of
  Nothing -> return $ Left $ CodecError request.routeName codecName UnknownCodec
  Just implementation -> case implementation.validateConfiguration configuration of
    Left reason ->
      return $
        Left $
          CodecError request.routeName codecName $
            InvalidConfiguration reason
    Right requirements -> case implementation.definition of
      CodecDefinition _ _ ReflectIdentity -> return $ Right (deployed, Nothing)
      CodecDefinition _ _ ReflectReject ->
        return $ Left $ CodecError request.routeName codecName ReflectionRejected
      CodecDefinition _ _ ReflectReAdd
        | runtime.mode == DryRunEvaluation
        , implementation.dryRunPolicy == CachedOnly ->
            return $ Left $ CodecError request.routeName codecName DryRunCacheRequired
      CodecDefinition _ _ ReflectReAdd -> do
        resolved <- resolveInputs runtime request requirements
        case resolved of
          Left err -> return $ Left err
          Right (inputs, dependencies) ->
            return $
              fmap (\(source, evaluated) -> (source, Just evaluated)) $
                reverseWithInputs
                  request
                  implementation
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
          if evaluated.resolvedInputs.configuration /= configuration
            then
              return $
                Left $
                  CodecError request.routeName codecName $
                    InvalidConfiguration
                      "command-scoped evaluation does not match the configuration"
            else
              return $
                fmap (\(source, result) -> (source, Just result)) $
                  reverseWithInputs
                    request
                    implementation
                    evaluated.resolvedInputs
                    evaluated.dependencies
                    deployed
 where
  CodecSpec codecName configuration = request.codec


reverseWithInputs
  :: CodecEvaluationRequest
  -> CodecImplementation
  -> CodecInputs
  -> [CodecDependency]
  -> OpaqueBytes
  -> Either CodecError (OpaqueBytes, EvaluatedCodec)
reverseWithInputs request implementation inputs dependencies deployed =
  case implementation.reverse of
    Nothing -> Left $ CodecError request.routeName codecName ReverseUnavailable
    Just reverseCodec -> case reverseCodec inputs deployed of
      Left reason ->
        Left $
          CodecError request.routeName codecName $
            ReverseFailure reason
      Right source ->
        let source' = opaqueBytes source
            candidateInputs :: CodecInputs
            candidateInputs =
              CodecInputs
                source'
                inputs.configuration
                inputs.facts
                inputs.variables
                inputs.externalInputs
        in case implementation.forward candidateInputs of
             Left reason ->
               Left $ CodecError request.routeName codecName $ EvaluationFailure reason
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
                        ( source'
                        , EvaluatedCodec
                            deployed
                            key
                            dependencies
                            implementation.definition
                            implementation.cacheScope
                            candidateInputs
                        )
               | otherwise ->
                   Left $
                     CodecError
                       request.routeName
                       codecName
                       ReverseValidationFailure
 where
  CodecSpec codecName _ = request.codec


-- | Validates a codec configuration and returns its declared inputs without
-- resolving or evaluating any of them.
codecRequirements
  :: CodecRuntime m
  -> Text
  -> CodecSpec
  -> Either CodecError CodecRequirements
codecRequirements runtime routeName spec = case Map.lookup name runtime.registry of
  Nothing -> Left $ CodecError routeName name UnknownCodec
  Just implementation -> case implementation.validateConfiguration configuration of
    Left reason ->
      Left $ CodecError routeName name $ InvalidConfiguration reason
    Right requirements -> Right requirements
 where
  CodecSpec name configuration = spec


-- | Returns the validated reflection policy for a registered codec.
codecReflectPolicy
  :: CodecRuntime m
  -> Text
  -> CodecSpec
  -> Either CodecError ReflectPolicy
codecReflectPolicy _ _ spec
  | spec == identityCodecSpec = Right ReflectIdentity
codecReflectPolicy runtime routeName spec@(CodecSpec name _) = do
  _ <- codecRequirements runtime routeName spec
  case Map.lookup name runtime.registry of
    Nothing -> Left $ CodecError routeName name UnknownCodec
    Just implementation -> case implementation.definition of
      CodecDefinition _ _ policy -> Right policy


-- | Constructs a redacted error for a source entry a codec cannot render.
codecSourceTypeError :: Text -> CodecSpec -> Text -> CodecError
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
      selectInputs "fact" codecName request.routeName requirements.facts request.facts
    (variables, variableDependencies) <-
      selectByteInputs
        "variable"
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
  -> CodecName
  -> Text
  -> Set Text
  -> Map Text Text
  -> Either CodecError (Map Text ByteString, [CodecDependency])
selectInputs namespace codecName routeName required available = do
  pairs <- traverse select $ Set.toAscList required
  return (Map.fromList $ fst <$> pairs, snd <$> pairs)
 where
  canonicalAvailable = Map.mapKeys foldCase available
  select name = case Map.lookup (foldCase name) canonicalAvailable of
    Nothing ->
      Left $
        CodecError routeName codecName $
          MissingInput $
            namespace <> ":" <> name
    Just value ->
      let bytes = encodeUtf8 value
      in Right
           ( (name, bytes)
           , CodecDependency (namespace <> ":" <> name) $ digestText bytes
           )


selectByteInputs
  :: Text
  -> CodecName
  -> Text
  -> Set Text
  -> Map Text ByteString
  -> Either CodecError (Map Text ByteString, [CodecDependency])
selectByteInputs namespace codecName routeName required available = do
  pairs <- traverse select $ Set.toAscList required
  return (Map.fromList $ fst <$> pairs, snd <$> pairs)
 where
  select name = case Map.lookup name available of
    Nothing ->
      Left $
        CodecError routeName codecName $
          MissingInput $
            namespace <> ":" <> name
    Just bytes ->
      Right
        ( (name, bytes)
        , CodecDependency (namespace <> ":" <> name) $ digestText bytes
        )


requestName :: ExternalInputRequest -> Text
requestName (ExternalInputRequest name) = name


-- | Formats a redacted, route-scoped codec error.
formatCodecError :: CodecError -> Text
formatCodecError (CodecError routeName codecName kind) =
  "Route " <> routeName <> " codec " <> renderCodecName codecName <> case kind of
    UnknownCodec -> " is not registered."
    InvalidConfiguration _ -> " has invalid configuration."
    MissingInput dependency -> " is missing declared input " <> dependency <> "."
    ExternalInputFailure dependency _ ->
      " could not resolve declared input " <> dependency <> "."
    EvaluationFailure _ -> " failed while rendering."
    DryRunCacheRequired -> " cannot run during dry-run without a valid cache."
    UnsupportedSourceType sourceType ->
      " cannot render source entry type " <> sourceType <> "."
    ReflectionRejected -> " rejects reflection."
    ReverseUnavailable -> " has no reverse implementation."
    ReverseFailure _ -> " failed while reversing."
    ReverseValidationFailure -> " failed reverse validation."


digestText :: ByteString -> Text
digestText = Text.pack . concatMap byteHex . ByteString.unpack . SHA256.hash
 where
  byteHex byte = case showHex byte "" of
    [digit] -> ['0', digit]
    digits -> digits
