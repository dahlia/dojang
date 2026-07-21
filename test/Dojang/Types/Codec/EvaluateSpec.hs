{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Codec.EvaluateSpec (spec) where

import Control.Monad.State.Strict (State, get, put, runState)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldNotBe
  , shouldNotContain
  )
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.Types.Codec
  ( CodecConfiguration (CodecConfiguration)
  , CodecDefinition (CodecDefinition)
  , CodecName
  , CodecSpec (CodecSpec)
  , CodecValue (CodecBoolean)
  , ReflectPolicy (ReflectReAdd, ReflectReject)
  , identityCodecSpec
  , parseCodecName
  )
import Dojang.Types.Codec.Evaluate
  ( CacheScope (PersistentCache)
  , CodecCacheEntry (CodecCacheEntry)
  , CodecDryRunPolicy (CachedOnly, EvaluatePurely)
  , CodecEvaluationRequest (CodecEvaluationRequest)
  , CodecImplementation (CodecImplementation)
  , CodecInputs (..)
  , CodecRequirements (CodecRequirements)
  , CodecRuntime (CodecRuntime)
  , EvaluatedCodec (..)
  , EvaluationMode (DryRunEvaluation, NormalEvaluation)
  , ExternalInput
  , ExternalInputRequest (ExternalInputRequest)
  , codecRegistry
  , evaluateCodec
  , formatCodecError
  , identityCodecRuntime
  , opaqueBytes
  , reevaluateCodec
  , reflectCodec
  , revealBytes
  )


spec :: Spec
spec = do
  describe "evaluateCodec" $ do
    specify "identity preserves arbitrary binary input" $ hedgehog $ do
      source <- forAll $ Gen.bytes $ Range.linear 0 4096
      let request = evaluationRequest identityCodecSpec source Map.empty
          result =
            runIdentity $
              evaluateCodec (identityCodecRuntime NormalEvaluation) request Nothing
      case result of
        Left err -> fail $ Text.unpack $ formatCodecError err
        Right evaluated -> revealBytes evaluated.renderedBytes === source

    specify "uses only declared facts in the cache key" $ do
      let runtime = testRuntime EvaluatePurely NormalEvaluation
          personal = Map.fromList [("class", "personal"), ("unused", "first")]
          changedUnused = Map.insert "unused" "second" personal
          work = Map.insert "class" "work" personal
          evaluate facts =
            fst $
              runState
                (evaluateCodec runtime (evaluationRequest testSpec "source" facts) Nothing)
                0
          Right first = evaluate personal
          Right second = evaluate changedUnused
          Right third = evaluate work
      first.cacheKey `shouldBe` second.cacheKey
      first.cacheKey `shouldNotBe` third.cacheKey

    specify "matches declared fact keys case-insensitively" $ hedgehog $ do
      requiredName <-
        forAll $ Gen.element ["class", "Class", "CLASS", "cLaSs"]
      availableName <-
        forAll $ Gen.element ["class", "Class", "CLASS", "cLaSs"]
      let implementation =
            CodecImplementation
              (CodecDefinition testName "1" ReflectReject)
              ( const $
                  Right $
                    CodecRequirements (Set.singleton requiredName) Set.empty []
              )
              ( \inputs -> case Map.lookup requiredName inputs.facts of
                  Just value -> Right value
                  Nothing -> Left "missing case-insensitive fact"
              )
              Nothing
              PersistentCache
              EvaluatePurely
          runtime =
            CodecRuntime
              (codecRegistry [implementation])
              NormalEvaluation
              (const $ pure $ Left "unexpected external input")
          request =
            evaluationRequest
              testSpec
              "source"
              (Map.singleton availableName "work")
          result = runIdentity $ evaluateCodec runtime request Nothing
      case result of
        Left err -> fail $ Text.unpack $ formatCodecError err
        Right evaluated -> revealBytes evaluated.renderedBytes === "work"

    specify "preserves arbitrary declared variable bytes" $ hedgehog $ do
      variable <- forAll $ Gen.bytes $ Range.linear 0 4096
      let request =
            CodecEvaluationRequest
              "route"
              testSpec
              (opaqueBytes "source")
              Map.empty
              (Map.singleton "native" variable)
          result = runIdentity $ evaluateCodec variableRuntime request Nothing
      case result of
        Left err -> fail $ Text.unpack $ formatCodecError err
        Right evaluated -> revealBytes evaluated.renderedBytes === variable

    specify "passes validated configuration to both transformations" $ hedgehog $ do
      enabled <- forAll Gen.bool
      source <- forAll $ Gen.bytes $ Range.linear 0 4096
      let codecSpec = configuredSpec enabled
          request = evaluationRequest codecSpec source Map.empty
          evaluatedResult =
            runIdentity $ evaluateCodec configuredRuntime request Nothing
          deployed = configurationMarker enabled <> source
          reflectedResult =
            runIdentity $
              reflectCodec configuredRuntime request $
                opaqueBytes deployed
      case evaluatedResult of
        Left err -> fail $ Text.unpack $ formatCodecError err
        Right evaluated -> revealBytes evaluated.renderedBytes === deployed
      (revealBytes <$> reflectedResult) === Right source

    specify "reuses a valid cache entry without rendering again" $ do
      let runtime = testRuntime EvaluatePurely NormalEvaluation
          request = evaluationRequest testSpec "source" $ Map.singleton "class" "work"
          action = do
            first <- evaluateCodec runtime request Nothing
            case first of
              Left err -> return $ Left err
              Right evaluated ->
                evaluateCodec
                  runtime
                  request
                  (Just $ CodecCacheEntry evaluated.cacheKey evaluated.renderedBytes)
          (result, _resolverCount) = runState action 0
      revealBytes . (.renderedBytes) <$> result `shouldBe` Right "source:work"

    specify "requires a cache for cached-only dry runs" $ do
      let runtime = testRuntime CachedOnly DryRunEvaluation
          request = evaluationRequest testSpec "source" $ Map.singleton "class" "work"
          (result, renderCount) = runState (evaluateCodec runtime request Nothing) 0
      either formatCodecError (const "unexpected success") result
        `shouldBe` "Route route codec test cannot run during dry-run without a valid cache."
      renderCount `shouldBe` 0

    specify "reuses a dry-run cache when no external input is declared" $ do
      let request =
            evaluationRequest testSpec "source" $
              Map.singleton "class" "work"
          (normalResult, _) =
            runState
              ( evaluateCodec
                  (testRuntime CachedOnly NormalEvaluation)
                  request
                  Nothing
              )
              0
      evaluated <- case normalResult of
        Left err -> fail $ Text.unpack $ formatCodecError err
        Right value -> return value
      let cache = CodecCacheEntry evaluated.cacheKey evaluated.renderedBytes
          (dryRunResult, _) =
            runState
              ( evaluateCodec
                  (testRuntime CachedOnly DryRunEvaluation)
                  request
                  (Just cache)
              )
              0
      revealBytes . (.renderedBytes) <$> dryRunResult
        `shouldBe` Right "source:work"

    specify "does not resolve external inputs without a dry-run cache" $ do
      let request = evaluationRequest reversibleSpec "source" Map.empty
          (result, resolutionCount) =
            runState
              (evaluateCodec cachedOnlyReversibleRuntime request Nothing)
              0
      either formatCodecError (const "unexpected success") result
        `shouldBe` "Route route codec reversible cannot run during dry-run without a valid cache."
      resolutionCount `shouldBe` 0

    specify "does not resolve external inputs to validate a dry-run cache" $ hedgehog $ do
      cacheKey <- forAll $ Gen.bytes $ Range.linear 0 256
      rendered <- forAll $ Gen.bytes $ Range.linear 0 4096
      let request = evaluationRequest reversibleSpec "source" Map.empty
          staleCache = CodecCacheEntry cacheKey $ opaqueBytes rendered
          (result, resolutionCount) =
            runState
              ( evaluateCodec
                  cachedOnlyReversibleRuntime
                  request
                  (Just staleCache)
              )
              0
      either formatCodecError (const "unexpected success") result
        === "Route route codec reversible cannot run during dry-run without a valid cache."
      resolutionCount === 0

    specify "does not reevaluate a cached-only codec during a dry run" $ do
      let originalRequest =
            evaluationRequest testSpec "source" $
              Map.singleton "class" "work"
          changedRequest =
            evaluationRequest testSpec "changed" $
              Map.singleton "class" "work"
          (originalResult, _) =
            runState
              ( evaluateCodec
                  (testRuntime CachedOnly NormalEvaluation)
                  originalRequest
                  Nothing
              )
              0
      original <- case originalResult of
        Left err -> fail $ Text.unpack $ formatCodecError err
        Right evaluated -> return evaluated
      let (result, _) =
            runState
              ( reevaluateCodec
                  (testRuntime CachedOnly DryRunEvaluation)
                  changedRequest
                  original
              )
              0
      either formatCodecError (const "unexpected success") result
        `shouldBe` "Route route codec test cannot run during dry-run without a valid cache."

    specify "redacts source and rendered bytes from diagnostics and Show" $ do
      let secret = "do-not-print-this"
          request = evaluationRequest missingSpec secret Map.empty
          result =
            runIdentity $
              evaluateCodec (identityCodecRuntime NormalEvaluation) request Nothing
          failingResult =
            runIdentity $
              evaluateCodec
                (failingRuntime $ Text.pack $ ByteString.Char8.unpack secret)
                (evaluationRequest testSpec secret $ Map.singleton "class" "work")
                Nothing
          requestWithInputs =
            CodecEvaluationRequest
              "route"
              testSpec
              (opaqueBytes secret)
              (Map.singleton "sensitive-fact" $ Text.pack $ ByteString.Char8.unpack secret)
              (Map.singleton "SENSITIVE_VARIABLE" secret)
          rendered = show $ opaqueBytes secret
      show requestWithInputs `shouldNotContain` ByteString.Char8.unpack secret
      show result `shouldNotContain` ByteString.Char8.unpack secret
      show failingResult `shouldNotContain` ByteString.Char8.unpack secret
      rendered `shouldNotContain` ByteString.Char8.unpack secret

    specify "rejects reflection according to codec policy" $ do
      let runtime = testRuntime EvaluatePurely NormalEvaluation
          request = evaluationRequest testSpec "source" $ Map.singleton "class" "work"
          result = runState (reflectCodec runtime request $ opaqueBytes "deployed") 0
      either formatCodecError (const "unexpected success") (fst result)
        `shouldBe` "Route route codec test rejects reflection."

    specify "re-adds and validates arbitrary deployed bytes" $ hedgehog $ do
      deployed <- forAll $ Gen.bytes $ Range.linear 0 4096
      let runtime = reversibleRuntime
          request = evaluationRequest reversibleSpec "old" Map.empty
          result = runIdentity $ reflectCodec runtime request $ opaqueBytes deployed
      (revealBytes <$> result) === Right deployed

    specify "rejects a reverse result that does not round trip" $ do
      let runtime = brokenReversibleRuntime
          request = evaluationRequest reversibleSpec "old" Map.empty
          result = runIdentity $ reflectCodec runtime request $ opaqueBytes "deployed"
      either formatCodecError (const "unexpected success") result
        `shouldBe` "Route route codec reversible failed reverse validation."

    specify "does not run cached-only re-add reflection during a dry run" $ do
      let runtime = cachedOnlyReversibleRuntime
          request = evaluationRequest reversibleSpec "old" Map.empty
          (result, resolutionCount) =
            runState (reflectCodec runtime request $ opaqueBytes "deployed") 0
      either formatCodecError (const "unexpected success") result
        `shouldBe` "Route route codec reversible cannot run during dry-run without a valid cache."
      resolutionCount `shouldBe` 0


evaluationRequest
  :: CodecSpec -> ByteString -> Map.Map Text Text -> CodecEvaluationRequest
evaluationRequest spec' source facts =
  CodecEvaluationRequest "route" spec' (opaqueBytes source) facts Map.empty


testSpec :: CodecSpec
testSpec = CodecSpec testName $ CodecConfiguration Map.empty


missingSpec :: CodecSpec
missingSpec =
  CodecSpec
    (case parseCodecName "missing" of Just name -> name; Nothing -> testName)
    (CodecConfiguration Map.empty)


testName :: CodecName
testName = case parseCodecName "test" of
  Just name -> name
  Nothing -> error "The test codec name is valid."


reversibleName :: CodecName
reversibleName = case parseCodecName "reversible" of
  Just name -> name
  Nothing -> error "The reversible codec name is valid."


reversibleSpec :: CodecSpec
reversibleSpec = CodecSpec reversibleName $ CodecConfiguration Map.empty


configuredName :: CodecName
configuredName = case parseCodecName "configured" of
  Just name -> name
  Nothing -> error "The configured codec name is valid."


configuredSpec :: Bool -> CodecSpec
configuredSpec enabled =
  CodecSpec configuredName $
    CodecConfiguration $
      Map.singleton "enabled" $
        CodecBoolean enabled


configurationMarker :: Bool -> ByteString
configurationMarker enabled = if enabled then "enabled:" else "disabled:"


configuredRuntime :: CodecRuntime Identity
configuredRuntime =
  CodecRuntime
    (codecRegistry [implementation])
    NormalEvaluation
    (const $ pure $ Left "unexpected external input")
 where
  implementation =
    CodecImplementation
      (CodecDefinition configuredName "1" ReflectReAdd)
      ( \configuration -> case configuration of
          CodecConfiguration values -> case Map.lookup "enabled" values of
            Just (CodecBoolean _) ->
              Right $ CodecRequirements Set.empty Set.empty []
            _ -> Left "enabled must be a boolean"
      )
      ( \inputs -> do
          enabled <- configuredEnabled inputs.configuration
          Right $ configurationMarker enabled <> revealBytes inputs.rawSource
      )
      ( Just $ \inputs deployed -> do
          enabled <- configuredEnabled inputs.configuration
          let marker = configurationMarker enabled
              bytes = revealBytes deployed
          maybe
            (Left "deployed bytes use another configuration")
            Right
            (ByteString.stripPrefix marker bytes)
      )
      PersistentCache
      EvaluatePurely
  configuredEnabled (CodecConfiguration values) = case Map.lookup "enabled" values of
    Just (CodecBoolean enabled) -> Right enabled
    _ -> Left "validated configuration is missing"


reversibleRuntime :: CodecRuntime Identity
reversibleRuntime = reversibleRuntimeWith id


brokenReversibleRuntime :: CodecRuntime Identity
brokenReversibleRuntime = reversibleRuntimeWith (<> ":changed")


reversibleRuntimeWith :: (ByteString -> ByteString) -> CodecRuntime Identity
reversibleRuntimeWith =
  reversibleRuntimeWithPolicy EvaluatePurely NormalEvaluation


reversibleRuntimeWithPolicy
  :: CodecDryRunPolicy
  -> EvaluationMode
  -> (ByteString -> ByteString)
  -> CodecRuntime Identity
reversibleRuntimeWithPolicy dryRunPolicy mode render =
  CodecRuntime
    (codecRegistry [implementation])
    mode
    (const $ pure $ Left "unexpected external input")
 where
  implementation =
    CodecImplementation
      (CodecDefinition reversibleName "1" ReflectReAdd)
      (const $ Right $ CodecRequirements Set.empty Set.empty [])
      (Right . render . revealBytes . (.rawSource))
      (Just $ \_ deployed -> Right $ revealBytes deployed)
      PersistentCache
      dryRunPolicy


cachedOnlyReversibleRuntime :: CodecRuntime (State Int)
cachedOnlyReversibleRuntime =
  CodecRuntime
    (codecRegistry [implementation])
    DryRunEvaluation
    resolveExternalInput
 where
  implementation =
    CodecImplementation
      (CodecDefinition reversibleName "1" ReflectReAdd)
      ( const $
          Right $
            CodecRequirements
              Set.empty
              Set.empty
              [ExternalInputRequest "secret"]
      )
      (Right . revealBytes . (.rawSource))
      (Just $ \_ deployed -> Right $ revealBytes deployed)
      PersistentCache
      CachedOnly
  resolveExternalInput _ = do
    count <- get
    put $ count + 1
    return $ Left "unexpected external input"


testRuntime
  :: CodecDryRunPolicy -> EvaluationMode -> CodecRuntime (State Int)
testRuntime dryRunPolicy mode =
  CodecRuntime
    (codecRegistry [implementation])
    mode
    unexpectedExternalInput
 where
  implementation =
    CodecImplementation
      (CodecDefinition testName "1" ReflectReject)
      ( \configuration ->
          if configuration == CodecConfiguration Map.empty
            then Right $ CodecRequirements (Set.singleton "class") Set.empty []
            else Left "configuration is not empty"
      )
      ( \inputs ->
          let fact = Map.findWithDefault "" "class" inputs.facts
          in Right $ revealBytes inputs.rawSource <> ":" <> fact
      )
      Nothing
      PersistentCache
      dryRunPolicy
  unexpectedExternalInput
    :: ExternalInputRequest -> State Int (Either Text ExternalInput)
  unexpectedExternalInput _ = do
    count <- get
    return $ Left $ "unexpected external input after " <> Text.pack (show count)


variableRuntime :: CodecRuntime Identity
variableRuntime =
  CodecRuntime
    (codecRegistry [implementation])
    NormalEvaluation
    (const $ pure $ Left "unexpected external input")
 where
  implementation =
    CodecImplementation
      (CodecDefinition testName "1" ReflectReject)
      (const $ Right $ CodecRequirements Set.empty (Set.singleton "native") [])
      ( \inputs -> case Map.lookup "native" inputs.variables of
          Just value -> Right value
          Nothing -> Left "missing native variable"
      )
      Nothing
      PersistentCache
      EvaluatePurely


failingRuntime :: Text -> CodecRuntime Identity
failingRuntime reason =
  CodecRuntime
    (codecRegistry [implementation])
    NormalEvaluation
    (const $ pure $ Left "unexpected external input")
 where
  implementation =
    CodecImplementation
      (CodecDefinition testName "1" ReflectReject)
      ( const $
          Right $
            CodecRequirements (Set.singleton "class") Set.empty []
      )
      (const $ Left reason)
      Nothing
      PersistentCache
      EvaluatePurely
