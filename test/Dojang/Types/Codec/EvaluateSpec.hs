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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
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
  , codecCacheKey
  , identityCodecSpec
  , parseCodecName
  )
import Dojang.Types.Codec.Evaluate
  ( CacheScope (CommandCacheOnly, PersistentCache)
  , CodecCacheEntry (CodecCacheEntry)
  , CodecDryRunPolicy (CachedOnly, EvaluatePurely)
  , CodecEvaluationRequest (CodecEvaluationRequest)
  , CodecFailure (OpaqueCodecFailure)
  , CodecImplementation (CodecImplementation)
  , CodecInputPresence (DeferredInput)
  , CodecInputSelection (..)
  , CodecInputs (..)
  , CodecProgram (..)
  , CodecRequirements (..)
  , CodecRuntime (CodecRuntime)
  , EvaluatedCodec (..)
  , EvaluationMode (DryRunEvaluation, NormalEvaluation)
  , ExternalInput (..)
  , ExternalInputRequest (ExternalInputRequest)
  , codecImplementationWithEffects
  , codecImplementationWithSourceRequirements
  , codecRegistry
  , codecRequirements
  , evaluateCodec
  , evaluateCodecWithRequirements
  , formatCodecError
  , identityCodecRuntime
  , noCodecInputs
  , opaqueBytes
  , reevaluateCodec
  , reflectCodec
  , reflectCodecWithoutSourceWithEvaluation
  , reflectEvaluatedCodecWithEvaluation
  , requiredCodecInputs
  , revealBytes
  , validateCodecProtectedStorage
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
                    CodecRequirements (requiredCodecInputs [requiredName]) noCodecInputs []
              )
              ( \inputs -> case Map.lookup requiredName inputs.facts of
                  Just value -> Right value
                  Nothing -> Left "missing case-insensitive fact"
              )
              Nothing
              PersistentCache
              EvaluatePurely
          runtime :: CodecRuntime Identity
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

    specify "rejects an absent required fact" $ hedgehog $ do
      name <- forAll $ Gen.text (Range.linear 1 32) Gen.alphaNum
      let runtime = factSelectionRuntime $ requiredCodecInputs [name]
          request = evaluationRequest testSpec "source" Map.empty
          result = runIdentity $ evaluateCodec runtime request Nothing
      either formatCodecError (const "unexpected success") result
        === "Route route codec test is missing declared input fact:"
        <> name
        <> "."

    specify "allows an absent deferred fact" $ hedgehog $ do
      name <- forAll $ Gen.text (Range.linear 1 32) Gen.alphaNum
      let selection =
            CodecInputSelection
              (Map.singleton name DeferredInput)
              False
              False
          runtime = factSelectionRuntime selection
          request = evaluationRequest testSpec "source" Map.empty
          result = runIdentity $ evaluateCodec runtime request Nothing
      case result of
        Left err -> fail $ Text.unpack $ formatCodecError err
        Right evaluated -> revealBytes evaluated.renderedBytes === "absent"

    specify "still rejects a missing required fact when all inputs are included" $ do
      let required = requiredCodecInputs ["missing"]
          selection = required{includeAllInputs = True}
          runtime = factSelectionRuntime selection
          request =
            evaluationRequest
              testSpec
              "source"
              (Map.singleton "available" "value")
          result = runIdentity $ evaluateCodec runtime request Nothing
      either formatCodecError (const "unexpected success") result
        `shouldBe` "Route route codec test is missing declared input fact:missing."

    specify "selects folded fact-key collisions deterministically" $ hedgehog $ do
      earlierValue <- forAll $ Gen.text (Range.linear 0 1024) Gen.unicode
      laterValue <- forAll $ Gen.text (Range.linear 0 1024) Gen.unicode
      let runtime = factSelectionRuntime $ requiredCodecInputs ["class"]
          request =
            evaluationRequest
              testSpec
              "source"
              ( Map.fromList
                  [ ("Class", earlierValue)
                  , ("class", laterValue)
                  ]
              )
          result = runIdentity $ evaluateCodec runtime request Nothing
      case result of
        Left err -> fail $ Text.unpack $ formatCodecError err
        Right evaluated -> revealBytes evaluated.renderedBytes === encodeUtf8 laterValue

    specify "uses supplied requirements without deriving them again" $ do
      let implementation =
            codecImplementationWithSourceRequirements
              (CodecDefinition testName "1" ReflectReject)
              (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (\_ _ -> Left "source requirements were derived twice")
              (Right . revealBytes . (.rawSource))
              Nothing
              PersistentCache
              EvaluatePurely
          runtime :: CodecRuntime Identity
          runtime =
            CodecRuntime
              (codecRegistry [implementation])
              NormalEvaluation
              (const $ pure $ Left "unexpected external input")
          request = evaluationRequest testSpec "source" Map.empty
          requirements = CodecRequirements noCodecInputs noCodecInputs []
          result =
            runIdentity $
              evaluateCodecWithRequirements
                runtime
                request
                requirements
                Nothing
      case result of
        Left err -> fail $ Text.unpack $ formatCodecError err
        Right evaluated -> revealBytes evaluated.renderedBytes `shouldBe` "source"

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

    specify "preserves manifest-only restrictions across source requirements" $
      hedgehog $ do
        source <- forAll $ Gen.bytes $ Range.linear 0 4096
        let selection =
              CodecInputSelection
                (Map.singleton "declared" DeferredInput)
                False
                True
            implementation =
              codecImplementationWithSourceRequirements
                (CodecDefinition testName "1" ReflectReject)
                ( const $
                    Right $
                      CodecRequirements noCodecInputs selection []
                )
                (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
                (Right . revealBytes . (.rawSource))
                Nothing
                PersistentCache
                EvaluatePurely
            runtime :: CodecRuntime Identity
            runtime =
              CodecRuntime
                (codecRegistry [implementation])
                NormalEvaluation
                (const $ pure $ Left "unexpected external input")
        case codecRequirements runtime "route" testSpec $ opaqueBytes source of
          Left err -> fail $ Text.unpack $ formatCodecError err
          Right requirements -> requirements.variables.manifestInputsOnly === True

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

    specify "reuses a valid cache entry without rendering again" $ hedgehog $ do
      source <- forAll $ Gen.bytes $ Range.linear 0 4096
      cachedSuffix <- forAll $ Gen.bytes $ Range.linear 0 4096
      let cachedBytes = "cached:" <> cachedSuffix
          implementation =
            CodecImplementation
              (CodecDefinition testName "1" ReflectReject)
              (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (const $ Left "renderer ran on a valid cache hit")
              Nothing
              PersistentCache
              EvaluatePurely
          runtime :: CodecRuntime Identity
          runtime =
            CodecRuntime
              (codecRegistry [implementation])
              NormalEvaluation
              (const $ pure $ Left "unexpected external input")
          request = evaluationRequest testSpec source Map.empty
          cache =
            CodecCacheEntry
              (codecCacheKey testSpec "1" source [])
              (opaqueBytes cachedBytes)
          result = runIdentity $ evaluateCodec runtime request $ Just cache
      (revealBytes . (.renderedBytes) <$> result) === Right cachedBytes

    specify "requires a cache for cached-only dry runs" $ do
      let runtime = testRuntime CachedOnly DryRunEvaluation
          request = evaluationRequest testSpec "source" $ Map.singleton "class" "work"
          (result, renderCount) = runState (evaluateCodec runtime request Nothing) 0
      either formatCodecError (const "unexpected success") result
        `shouldBe` "Route route codec test cannot run during dry-run without a valid cache."
      renderCount `shouldBe` 0

    specify "reuses a dry-run cache when no external input is declared" $ hedgehog $ do
      cachedSuffix <- forAll $ Gen.bytes $ Range.linear 0 4096
      let cachedBytes = "dry-run-cache:" <> cachedSuffix
          request =
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
      let cache = CodecCacheEntry evaluated.cacheKey $ opaqueBytes cachedBytes
          (dryRunResult, _) =
            runState
              ( evaluateCodec
                  (testRuntime CachedOnly DryRunEvaluation)
                  request
                  (Just cache)
              )
              0
      (revealBytes . (.renderedBytes) <$> dryRunResult) === Right cachedBytes

    specify "does not resolve external inputs without a dry-run cache" $ do
      let request = evaluationRequest reversibleSpec "source" Map.empty
          (result, resolutionCount) =
            runState
              (evaluateCodec cachedOnlyReversibleRuntime request Nothing)
              0
      either formatCodecError (const "unexpected success") result
        `shouldBe` "Route route codec reversible cannot run during dry-run without a valid cache."
      resolutionCount `shouldBe` 0

    specify "stops resolving external inputs after the first failure" $ do
      let first = ExternalInputRequest "first"
          second = ExternalInputRequest "second"
          implementation =
            CodecImplementation
              (CodecDefinition testName "1" ReflectReject)
              ( const $
                  Right $
                    CodecRequirements noCodecInputs noCodecInputs [first, second]
              )
              (Right . revealBytes . (.rawSource))
              Nothing
              PersistentCache
              EvaluatePurely
          resolve
            :: ExternalInputRequest
            -> State [ExternalInputRequest] (Either Text ExternalInput)
          resolve externalRequest = do
            previousRequests <- get
            put $ previousRequests <> [externalRequest]
            return $
              if externalRequest == first
                then Left "unavailable"
                else Right $ ExternalInput (opaqueBytes "value") "stable"
          runtime = CodecRuntime (codecRegistry [implementation]) NormalEvaluation resolve
          request = evaluationRequest testSpec "source" Map.empty
          (result, resolvedRequests) =
            runState (evaluateCodec runtime request Nothing) []
      either formatCodecError (const "unexpected success") result
        `shouldBe` "Route route codec test could not resolve declared input first."
      resolvedRequests `shouldBe` [first]

    specify "resolves effectful forward inputs only when requested" $ do
      let requested = ExternalInputRequest "dynamic"
          unused = ExternalInputRequest "unused"
          implementation =
            codecImplementationWithEffects
              (CodecDefinition testName "1" ReflectReject)
              (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
              ( \_ ->
                  CodecRequest requested $ \input ->
                    CodecDone $ Right $ revealBytes input.value
              )
              Nothing
              EvaluatePurely
          resolve
            :: ExternalInputRequest
            -> State [ExternalInputRequest] (Either Text ExternalInput)
          resolve request = do
            previous <- get
            put $ previous <> [request]
            return $
              Right $
                ExternalInput
                  (opaqueBytes $ if request == unused then "unused" else "rendered")
                  "stable"
          runtime = CodecRuntime (codecRegistry [implementation]) NormalEvaluation resolve
          codecRequest = evaluationRequest testSpec "source" Map.empty
          (result, resolvedRequests) =
            runState (evaluateCodec runtime codecRequest Nothing) []
      (revealBytes . (.renderedBytes) <$> result) `shouldBe` Right "rendered"
      ((.cacheScope) <$> result) `shouldBe` Right CommandCacheOnly
      resolvedRequests `shouldBe` [requested]

    specify "reuses repeated dynamic requests within one evaluation" $
      hedgehog $ do
        firstSuffix <- forAll $ Gen.bytes $ Range.linear 0 4096
        secondSuffix <- forAll $ Gen.bytes $ Range.linear 0 4096
        let firstValue = "first:" <> firstSuffix
            secondValue = "second:" <> secondSuffix
            requested = ExternalInputRequest "repeated"
            implementation =
              codecImplementationWithEffects
                (CodecDefinition testName "1" ReflectReject)
                (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
                (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
                ( \_ ->
                    CodecRequest requested $ \firstInput ->
                      CodecRequest requested $ \secondInput ->
                        CodecDone $
                          Right $
                            revealBytes firstInput.value
                              <> revealBytes secondInput.value
                )
                Nothing
                EvaluatePurely
            resolve
              :: ExternalInputRequest
              -> State [ByteString] (Either Text ExternalInput)
            resolve _ = do
              values <- get
              case values of
                [] -> return $ Left "unexpected extra resolution"
                value : rest -> do
                  put rest
                  return $ Right $ ExternalInput (opaqueBytes value) "stable"
            runtime =
              CodecRuntime (codecRegistry [implementation]) NormalEvaluation resolve
            request = evaluationRequest testSpec "source" Map.empty
            (result, remaining) =
              runState
                (evaluateCodec runtime request Nothing)
                [firstValue, secondValue]
        (revealBytes . (.renderedBytes) <$> result)
          === Right (firstValue <> firstValue)
        remaining === [secondValue]

    specify "ignores persistent bytes for command-scoped codecs" $ hedgehog $ do
      oldSuffix <- forAll $ Gen.bytes $ Range.linear 0 4096
      freshSuffix <- forAll $ Gen.bytes $ Range.linear 0 4096
      let oldBytes = "old:" <> oldSuffix
          freshBytes = "fresh:" <> freshSuffix
          requested = ExternalInputRequest "dynamic"
          implementation =
            codecImplementationWithEffects
              (CodecDefinition testName "1" ReflectReject)
              (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
              ( \_ ->
                  CodecRequest requested $ \input ->
                    CodecDone $ Right $ revealBytes input.value
              )
              Nothing
              EvaluatePurely
          resolve
            :: ExternalInputRequest
            -> State [ByteString] (Either Text ExternalInput)
          resolve _ = do
            values <- get
            case values of
              [] -> return $ Left "unexpected extra resolution"
              value : rest -> do
                put rest
                return $ Right $ ExternalInput (opaqueBytes value) "stable"
          runtime = CodecRuntime (codecRegistry [implementation]) NormalEvaluation resolve
          request = evaluationRequest testSpec "source" Map.empty
          action = do
            first <- evaluateCodec runtime request Nothing
            case first of
              Left err -> return $ Left err
              Right evaluated ->
                evaluateCodec
                  runtime
                  request
                  (Just $ CodecCacheEntry evaluated.cacheKey evaluated.renderedBytes)
          (result, remaining) = runState action [oldBytes, freshBytes]
      (revealBytes . (.renderedBytes) <$> result) === Right freshBytes
      remaining === []

    specify "reuses dynamic inputs during re-evaluation" $ hedgehog $ do
      secret <- forAll $ Gen.bytes $ Range.linear 0 4096
      source <- forAll $ Gen.bytes $ Range.linear 0 4096
      changedSource <- forAll $ Gen.bytes $ Range.linear 0 4096
      let requested = ExternalInputRequest "dynamic"
          implementation =
            codecImplementationWithEffects
              (CodecDefinition testName "1" ReflectReject)
              (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
              ( \inputs ->
                  CodecRequest requested $ \input ->
                    CodecDone $
                      Right $
                        revealBytes input.value <> revealBytes inputs.rawSource
              )
              Nothing
              EvaluatePurely
          resolve
            :: ExternalInputRequest
            -> State Int (Either Text ExternalInput)
          resolve _ = do
            count <- get
            put $ count + 1
            return $
              if count == 0
                then Right $ ExternalInput (opaqueBytes secret) "stable"
                else Left "dynamic input was resolved again"
          runtime = CodecRuntime (codecRegistry [implementation]) NormalEvaluation resolve
          firstRequest = evaluationRequest testSpec source Map.empty
          secondRequest = evaluationRequest testSpec changedSource Map.empty
          action = do
            first <- evaluateCodec runtime firstRequest Nothing
            case first of
              Left err -> return $ Left err
              Right evaluated -> reevaluateCodec runtime secondRequest evaluated
          (result, resolutionCount) = runState action 0
      (revealBytes . (.renderedBytes) <$> result)
        === Right (secret <> changedSource)
      resolutionCount === 1

    specify "retains dynamic inputs through re-add reflection" $ hedgehog $ do
      oldPlaintext <- forAll $ Gen.bytes $ Range.linear 0 4096
      deployed <- forAll $ Gen.bytes $ Range.linear 0 4096
      sourceSuffix <- forAll $ Gen.bytes $ Range.linear 0 4096
      let oldSource = "old-source"
          newSource = "new-source:" <> sourceSuffix
          decryptOld = ExternalInputRequest "decrypt-old"
          encrypt = ExternalInputRequest "encrypt"
          decryptNew = ExternalInputRequest "decrypt-new"
          implementation =
            codecImplementationWithEffects
              (CodecDefinition reversibleName "1" ReflectReAdd)
              (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
              ( \inputs ->
                  let request =
                        if revealBytes inputs.rawSource == oldSource
                          then decryptOld
                          else decryptNew
                  in CodecRequest request $ \input ->
                       CodecDone $ Right $ revealBytes input.value
              )
              ( Just $ \_ _ ->
                  CodecRequest encrypt $ \input ->
                    CodecDone $ Right $ revealBytes input.value
              )
              EvaluatePurely
          resolve
            :: ExternalInputRequest
            -> State
                 [(ExternalInputRequest, ExternalInput)]
                 (Either Text ExternalInput)
          resolve request = do
            queued <- get
            case queued of
              (expected, input) : rest
                | request == expected -> put rest >> return (Right input)
              _ -> return $ Left "dynamic input was resolved out of sequence"
          runtime = CodecRuntime (codecRegistry [implementation]) NormalEvaluation resolve
          firstRequest = evaluationRequest reversibleSpec oldSource Map.empty
          nextRequest = evaluationRequest reversibleSpec newSource Map.empty
          action = do
            first <- evaluateCodec runtime firstRequest Nothing
            case first of
              Left err -> return $ Left $ formatCodecError err
              Right evaluated -> do
                reflected <-
                  reflectEvaluatedCodecWithEvaluation
                    runtime
                    firstRequest
                    evaluated
                    (opaqueBytes deployed)
                case reflected of
                  Left err -> return $ Left $ formatCodecError err
                  Right (_, Nothing) -> return $ Left "missing re-add snapshot"
                  Right (_, Just snapshot) -> do
                    reevaluated <- reevaluateCodec runtime nextRequest snapshot
                    return $ case reevaluated of
                      Left err -> Left $ formatCodecError err
                      Right value -> Right value
          resolutions =
            [ (decryptOld, ExternalInput (opaqueBytes oldPlaintext) "old")
            , (encrypt, ExternalInput (opaqueBytes newSource) "encrypted")
            , (decryptNew, ExternalInput (opaqueBytes deployed) "new")
            ]
          (result, remaining) = runState action resolutions
      (revealBytes . (.renderedBytes) <$> result) === Right deployed
      remaining === []

    specify "requires owner-only storage for effectful codecs" $ do
      let implementation =
            codecImplementationWithEffects
              (CodecDefinition testName "1" ReflectReject)
              (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (\inputs -> CodecDone $ Right $ revealBytes inputs.rawSource)
              Nothing
              EvaluatePurely
          runtime :: CodecRuntime Identity
          runtime =
            CodecRuntime
              (codecRegistry [implementation])
              NormalEvaluation
              (const $ pure $ Left "unexpected external input")
      ( either
          (Left . formatCodecError)
          Right
          (validateCodecProtectedStorage runtime "route" testSpec False)
        )
        `shouldBe` Left "Route route codec test requires an owner-only destination mode."
      validateCodecProtectedStorage runtime "route" testSpec True
        `shouldBe` Right ()

    specify "runs effectful reverse and validates it through effectful forward" $ do
      let decrypt = ExternalInputRequest "decrypt"
          encrypt = ExternalInputRequest "encrypt"
          implementation =
            codecImplementationWithEffects
              (CodecDefinition reversibleName "1" ReflectReAdd)
              (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
              ( \inputs ->
                  CodecRequest decrypt $ \input ->
                    CodecDone $
                      if revealBytes inputs.rawSource == "ciphertext"
                        then Right $ revealBytes input.value
                        else Left "unexpected ciphertext"
              )
              ( Just $ \_ deployed ->
                  CodecRequest encrypt $ \_ ->
                    CodecDone $
                      if revealBytes deployed == "plaintext"
                        then Right "ciphertext"
                        else Left "unexpected plaintext"
              )
              EvaluatePurely
          resolve
            :: ExternalInputRequest
            -> State [ExternalInputRequest] (Either Text ExternalInput)
          resolve request = do
            previous <- get
            put $ previous <> [request]
            return $
              Right $
                ExternalInput
                  (opaqueBytes $ if request == decrypt then "plaintext" else "ignored")
                  "stable"
          runtime = CodecRuntime (codecRegistry [implementation]) NormalEvaluation resolve
          codecRequest = evaluationRequest reversibleSpec "old" Map.empty
          (result, resolvedRequests) =
            runState (reflectCodec runtime codecRequest $ opaqueBytes "plaintext") []
      (revealBytes <$> result) `shouldBe` Right "ciphertext"
      resolvedRequests `shouldBe` [encrypt, decrypt]

    specify "does not run an effectful cached-only codec from a dry-run cache" $ do
      let requested = ExternalInputRequest "dynamic"
          implementation =
            codecImplementationWithEffects
              (CodecDefinition testName "1" ReflectReject)
              (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (\_ -> CodecRequest requested $ const $ CodecDone $ Right "rendered")
              Nothing
              CachedOnly
          resolve
            :: ExternalInputRequest
            -> State Int (Either Text ExternalInput)
          resolve _ = do
            count <- get
            put $ count + 1
            return $ Right $ ExternalInput (opaqueBytes "secret") "stable"
          runtime =
            CodecRuntime
              (codecRegistry [implementation])
              DryRunEvaluation
              resolve
          cache = CodecCacheEntry "untrusted" $ opaqueBytes "cached"
          codecRequest = evaluationRequest testSpec "source" Map.empty
          (result, resolutionCount) =
            runState (evaluateCodec runtime codecRequest $ Just cache) 0
      either formatCodecError (const "unexpected success") result
        `shouldBe` "Route route codec test cannot run during dry-run without a valid cache."
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

    specify "resolves requirements discovered after missing-source reversal" $
      hedgehog $ do
        deployed <- forAll $ Gen.bytes $ Range.linear 0 4096
        let request =
              evaluationRequest
                reversibleSpec
                ""
                (Map.singleton "class" "work")
            (result, resolutionCount) =
              runState
                ( reflectCodecWithoutSourceWithEvaluation
                    sourceAwareReversibleRuntime
                    request
                    (opaqueBytes deployed)
                )
                0
        (revealBytes . fst <$> result) === Right deployed
        resolutionCount === 1

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
              Right $ CodecRequirements noCodecInputs noCodecInputs []
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


sourceAwareReversibleRuntime :: CodecRuntime (State Int)
sourceAwareReversibleRuntime =
  CodecRuntime
    (codecRegistry [implementation])
    NormalEvaluation
    resolveExternalInput
 where
  sourceInput = ExternalInputRequest "source-input"
  implementation =
    codecImplementationWithSourceRequirements
      (CodecDefinition reversibleName "1" ReflectReAdd)
      (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
      ( \_ _ ->
          Right $
            CodecRequirements
              (requiredCodecInputs ["class"])
              noCodecInputs
              [sourceInput]
      )
      ( \inputs -> case ( Map.lookup "class" inputs.facts
                        , Map.lookup sourceInput inputs.externalInputs
                        ) of
          (Just _, Just _) -> Right $ revealBytes inputs.rawSource
          _ -> Left "missing source input"
      )
      (Just $ \_ deployed -> Right $ revealBytes deployed)
      PersistentCache
      EvaluatePurely
  resolveExternalInput request
    | request == sourceInput = do
        count <- get
        put $ count + 1
        return $ Right $ ExternalInput (opaqueBytes "value") "stable"
    | otherwise = return $ Left "unexpected external input"


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
      (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
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
              noCodecInputs
              noCodecInputs
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
            then Right $ CodecRequirements (requiredCodecInputs ["class"]) noCodecInputs []
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


factSelectionRuntime :: CodecInputSelection -> CodecRuntime Identity
factSelectionRuntime selection =
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
            CodecRequirements selection noCodecInputs []
      )
      ( \inputs ->
          Right $ Map.findWithDefault "absent" "class" inputs.facts
      )
      Nothing
      PersistentCache
      EvaluatePurely


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
      ( const $
          Right $
            CodecRequirements noCodecInputs (requiredCodecInputs ["native"]) []
      )
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
            CodecRequirements (requiredCodecInputs ["class"]) noCodecInputs []
      )
      (const $ Left $ OpaqueCodecFailure reason)
      Nothing
      PersistentCache
      EvaluatePurely
