{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Codec evaluation for already selected managed correspondences.
module Dojang.Types.Codec.Context
  ( EvaluatedManagedCorrespondence (..)
  , evaluateManagedCorrespondences
  , evaluateManagedCorrespondencesWithCache
  , evaluationWarnings
  , loadCodecCacheEntries
  , managedCodecStateFor
  , rawSourceDigestFor
  , rawSourceFor
  , reevaluateManagedCorrespondence
  , reflectManagedCorrespondence
  , renderedSourceFor
  , reuseEvaluatedManagedCorrespondence
  ) where

import Control.Monad (foldM, forM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as ByteString
import Data.Char (ord)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8)
import Numeric (showHex)
import System.OsPath (normalise, (</>))
import System.OsString qualified as OsString
import Prelude hiding (readFile)

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.MonadFileSystem qualified as FileSystem
import Dojang.Types.Codec
  ( CodecDefinition (..)
  , CodecDependency (..)
  , codecSpecDigest
  , identityCodecSpec
  , renderCodecName
  )
import Dojang.Types.Codec.Evaluate
  ( CacheScope (PersistentCache)
  , CodecCacheEntry (CodecCacheEntry)
  , CodecError
  , CodecEvaluationRequest (CodecEvaluationRequest)
  , CodecInputSelection (..)
  , CodecInputs (..)
  , CodecRequirements (CodecRequirements)
  , CodecRuntime
  , EvaluatedCodec (..)
  , OpaqueBytes
  , codecConfigurationRequirements
  , codecReflectPolicy
  , codecRequirements
  , codecSourceTypeError
  , evaluateCodec
  , opaqueBytes
  , reevaluateCodec
  , reflectCodecWithEvaluation
  , reflectCodecWithoutSourceWithEvaluationBy
  , reflectEvaluatedCodecWithEvaluation
  , revealBytes
  )
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , ManagedCorrespondence (..)
  )
import Dojang.Types.Environment
  ( environmentFacts
  , factKeyText
  , factValueText
  )
import Dojang.Types.FilePathExpression.Expansion (VariableLookup (..))
import Dojang.Types.MachineState (MachineState (..))
import Dojang.Types.ManagedTarget
  ( ManagedCodecState (..)
  , ManagedTarget (..)
  , TargetFingerprint (FileFingerprint)
  )
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.ManifestVariable
  ( dispatchManifestVariable
  , renderManifestVariableName
  )
import Dojang.Types.Repository
  ( Repository (..)
  , RouteMapWarning (FilePathExpressionWarning)
  , RouteResult (..)
  )
import Dojang.Types.TargetTracking (managedTargetId)


-- | A managed correspondence whose source view has passed through its codec.
data EvaluatedManagedCorrespondence = EvaluatedManagedCorrespondence
  { managed :: ManagedCorrespondence
  -- ^ Correspondence with codec-aware source stat and delta.
  , codecResult :: Maybe EvaluatedCodec
  -- ^ Evaluation result for a regular source file.
  , rawSourceDigest :: Maybe ByteString.ByteString
  -- ^ SHA-256 of the exact raw bytes supplied to the codec.
  , warnings :: [RouteMapWarning]
  -- ^ Warnings produced while resolving declared codec inputs.
  }
  deriving (Eq, Show)


-- | Evaluates selected correspondences without persistent cache entries.
evaluateManagedCorrespondences
  :: (MonadFileSystem m)
  => CodecRuntime m
  -> Context m
  -> [ManagedCorrespondence]
  -> m (Either CodecError [EvaluatedManagedCorrespondence])
evaluateManagedCorrespondences runtime context =
  evaluateManagedCorrespondencesWithCache runtime context Map.empty


-- | Loads validated rendered bytes from managed target snapshots.
--
-- Entries are returned only when the persisted fingerprint still matches the
-- current intermediate file.  The cache never trusts state metadata alone.
loadCodecCacheEntries
  :: forall m
   . (MonadFileSystem m)
  => Context m
  -> MachineState
  -> [ManagedCorrespondence]
  -> m (Map Text.Text CodecCacheEntry)
loadCodecCacheEntries context machineState managed =
  Map.fromList . concat <$> mapM makeEntry managed
 where
  makeEntry
    :: ManagedCorrespondence -> m [(Text.Text, CodecCacheEntry)]
  makeEntry correspondence = do
    identifier <- managedTargetId context.repository correspondence
    case Map.lookup identifier machineState.targetRecords of
      Just target -> case ( target.codecState
                          , target.fingerprint
                          , correspondence.correspondence.intermediate.stat
                          ) of
        (Just state, FileFingerprint size digest, File intermediateSize)
          | size == intermediateSize -> do
              bytes <- readFile correspondence.correspondence.intermediate.path
              return
                [ ( identifier
                  , CodecCacheEntry
                      (encodeUtf8 state.cacheKey)
                      (opaqueBytes bytes)
                  )
                | digestHex bytes == digest
                ]
        _ -> return []
      Nothing -> return []
  digestHex =
    Text.pack
      . concatMap byteHex
      . ByteString.unpack
      . SHA256.hash
  byteHex byte = case showHex byte "" of
    [digit] -> ['0', digit]
    digits -> digits


-- | Evaluates selected correspondences, reusing entries indexed by managed
-- target identity when their codec cache key remains valid.
evaluateManagedCorrespondencesWithCache
  :: (MonadFileSystem m)
  => CodecRuntime m
  -> Context m
  -> Map Text.Text CodecCacheEntry
  -> [ManagedCorrespondence]
  -> m (Either CodecError [EvaluatedManagedCorrespondence])
evaluateManagedCorrespondencesWithCache runtime context cache managed =
  fmap reverse <$> foldM evaluateNext (Right []) managed
 where
  evaluateNext (Left err) _ = return $ Left err
  evaluateNext (Right evaluated) correspondence = do
    result <- evaluateOne correspondence
    return $ (: evaluated) <$> result
  evaluateOne correspondence
    | correspondence.route.codec == identityCodecSpec =
        return $
          Right $
            EvaluatedManagedCorrespondence correspondence Nothing Nothing []
    | otherwise = do
        routeName <- managedRouteName correspondence
        case codecReflectPolicy runtime routeName correspondence.route.codec of
          Left err -> return $ Left err
          Right _ -> case correspondence.correspondence.source.stat of
            File _ -> evaluateFile routeName correspondence
            Directory
              | correspondence.route.fileType == FileSystem.File ->
                  return $
                    Left $
                      codecSourceTypeError
                        routeName
                        correspondence.route.codec
                        "directory"
            Symlink _ ->
              return $
                Left $
                  codecSourceTypeError
                    routeName
                    correspondence.route.codec
                    "symbolic-link"
            _ ->
              return $
                Right $
                  EvaluatedManagedCorrespondence correspondence Nothing Nothing []
  evaluateFile routeName correspondence = do
    source <- readFile correspondence.correspondence.source.path
    case codecRequirements
      runtime
      routeName
      correspondence.route.codec
      (opaqueBytes source) of
      Left err -> return $ Left err
      Right requirements -> do
        (variables, warnings) <- resolveRequiredVariables context requirements
        identifier <- managedTargetId context.repository correspondence
        result <-
          evaluateCodec
            runtime
            ( CodecEvaluationRequest
                routeName
                correspondence.route.codec
                (opaqueBytes source)
                availableFacts
                variables
            )
            (Map.lookup identifier cache)
        case result of
          Left err -> return $ Left err
          Right evaluated ->
            Right
              <$> finishEvaluation
                correspondence
                (Just $ SHA256.hash source)
                warnings
                evaluated
  availableFacts = contextFacts context


-- | Re-evaluates a regular source with inputs captured earlier in the same
-- command.  Only the raw source is read again; manifest variables and external
-- inputs are not resolved again.
reevaluateManagedCorrespondence
  :: (MonadFileSystem m)
  => CodecRuntime m
  -> Context m
  -> ManagedCorrespondence
  -> EvaluatedCodec
  -> m (Either CodecError EvaluatedManagedCorrespondence)
reevaluateManagedCorrespondence runtime context correspondence previous = do
  routeName <- managedRouteName correspondence
  case correspondence.correspondence.source.stat of
    File _ -> do
      source <- readFile correspondence.correspondence.source.path
      result <-
        reevaluateCodec
          runtime
          ( CodecEvaluationRequest
              routeName
              correspondence.route.codec
              (opaqueBytes source)
              (contextFacts context)
              Map.empty
          )
          previous
      case result of
        Left err -> return $ Left err
        Right evaluated ->
          Right
            <$> finishEvaluation
              correspondence
              (Just $ SHA256.hash source)
              []
              evaluated
    Directory ->
      return $
        Left $
          codecSourceTypeError routeName correspondence.route.codec "directory"
    Symlink _ ->
      return $
        Left $
          codecSourceTypeError routeName correspondence.route.codec "symbolic-link"
    Missing ->
      return $
        Left $
          codecSourceTypeError routeName correspondence.route.codec "missing file"


finishEvaluation
  :: (MonadFileSystem m)
  => ManagedCorrespondence
  -> Maybe ByteString.ByteString
  -> [RouteMapWarning]
  -> EvaluatedCodec
  -> m EvaluatedManagedCorrespondence
finishEvaluation correspondence rawDigest warnings evaluated = do
  sourceDelta <-
    calculateRenderedDelta
      evaluated.renderedBytes
      correspondence.correspondence.intermediate
  let renderedStat =
        File $
          fromIntegral $
            ByteString.length $
              revealBytes evaluated.renderedBytes
      updated =
        correspondence
          { correspondence =
              correspondence.correspondence
                { source =
                    correspondence.correspondence.source
                      { stat = renderedStat
                      }
                , sourceDelta = sourceDelta
                }
          }
  return $
    EvaluatedManagedCorrespondence
      updated
      (Just evaluated)
      rawDigest
      warnings


-- | Rebinds a command-scoped codec evaluation to a freshly observed
-- correspondence without resolving or transforming its inputs again.
reuseEvaluatedManagedCorrespondence
  :: (MonadFileSystem m)
  => ManagedCorrespondence
  -> EvaluatedManagedCorrespondence
  -> m EvaluatedManagedCorrespondence
reuseEvaluatedManagedCorrespondence correspondence previous =
  case previous.codecResult of
    Nothing ->
      return previous{managed = correspondence}
    Just evaluated ->
      finishEvaluation
        correspondence
        previous.rawSourceDigest
        previous.warnings
        evaluated


-- | Collects and deduplicates warnings from codec input resolution.
evaluationWarnings
  :: [EvaluatedManagedCorrespondence] -> [RouteMapWarning]
evaluationWarnings = nub . concatMap (.warnings)


-- | Returns codec-produced bytes only for non-identity regular files.
renderedSourceFor :: EvaluatedManagedCorrespondence -> Maybe OpaqueBytes
renderedSourceFor evaluated
  | evaluated.managed.route.codec == identityCodecSpec = Nothing
  | otherwise = (.renderedBytes) <$> evaluated.codecResult


-- | Returns the exact raw bytes used for a non-identity rendered source.
rawSourceFor :: EvaluatedManagedCorrespondence -> Maybe OpaqueBytes
rawSourceFor evaluated
  | renderedSourceFor evaluated == Nothing = Nothing
  | otherwise = (.resolvedInputs.rawSource) <$> evaluated.codecResult


-- | Returns the digest of the raw bytes read for codec evaluation.
rawSourceDigestFor
  :: EvaluatedManagedCorrespondence -> Maybe ByteString.ByteString
rawSourceDigestFor evaluated
  | renderedSourceFor evaluated == Nothing = Nothing
  | otherwise = evaluated.rawSourceDigest


-- | Converts a persistable non-identity evaluation to redacted state metadata.
managedCodecStateFor
  :: EvaluatedManagedCorrespondence -> Maybe ManagedCodecState
managedCodecStateFor evaluated = do
  result <- evaluated.codecResult
  if evaluated.managed.route.codec == identityCodecSpec
    || result.cacheScope /= PersistentCache
    then Nothing
    else
      let CodecDefinition name version _ = result.definition
      in Just $
           ManagedCodecState
             (renderCodecName name)
             version
             (decodeUtf8 $ codecSpecDigest evaluated.managed.route.codec)
             (decodeUtf8 result.cacheKey)
             ( Map.fromList
                 [ (dependency.identity, dependency.fingerprint)
                 | dependency <- result.dependencies
                 ]
             )


calculateRenderedDelta
  :: (MonadFileSystem m) => OpaqueBytes -> FileEntry -> m FileDeltaKind
calculateRenderedDelta content (FileEntry path (File size))
  | fromIntegral (ByteString.length bytes) /= size = return Modified
  | otherwise = do
      intermediate <- readFile path
      return $ if intermediate == bytes then Unchanged else Modified
 where
  bytes = revealBytes content
calculateRenderedDelta _ (FileEntry _ Missing) = return Added
calculateRenderedDelta _ _ = return Modified


-- | Reconstructs raw repository bytes for one selected correspondence.
reflectManagedCorrespondence
  :: (MonadFileSystem m)
  => CodecRuntime m
  -> Context m
  -> ManagedCorrespondence
  -> Maybe EvaluatedCodec
  -- ^ Command-scoped evaluation whose resolved inputs should be reused.
  -> OpaqueBytes
  -> m (Either CodecError (OpaqueBytes, Maybe EvaluatedCodec, [RouteMapWarning]))
reflectManagedCorrespondence runtime context managed evaluated deployed = do
  routeName <- managedRouteName managed
  case managed.correspondence.source.stat of
    Directory ->
      return $
        Left $
          codecSourceTypeError routeName managed.route.codec "directory"
    Symlink _ ->
      return $
        Left $
          codecSourceTypeError routeName managed.route.codec "symbolic-link"
    sourceStat -> case evaluated of
      Just snapshot ->
        fmap (\(raw, reflected) -> (raw, reflected, []))
          <$> reflectEvaluatedCodecWithEvaluation
            runtime
            ( CodecEvaluationRequest
                routeName
                managed.route.codec
                snapshot.resolvedInputs.rawSource
                (contextFacts context)
                Map.empty
            )
            snapshot
            deployed
      Nothing -> do
        source <- case sourceStat of
          File _ -> readFile managed.correspondence.source.path
          Missing -> return ByteString.empty
        let requirementsResult = case sourceStat of
              File _ ->
                codecRequirements
                  runtime
                  routeName
                  managed.route.codec
                  (opaqueBytes source)
              Missing ->
                codecConfigurationRequirements
                  runtime
                  routeName
                  managed.route.codec
        case requirementsResult of
          Left err -> return $ Left err
          Right requirements -> do
            (variables, warnings) <- resolveRequiredVariables context requirements
            let request =
                  CodecEvaluationRequest
                    routeName
                    managed.route.codec
                    (opaqueBytes source)
                    (contextFacts context)
                    variables
            case sourceStat of
              File _ ->
                fmap (\(raw, snapshot) -> (raw, snapshot, warnings))
                  <$> reflectCodecWithEvaluation runtime request deployed
              Missing ->
                fmap
                  ( \(raw, snapshot, candidateWarnings) ->
                      (raw, snapshot, nub $ warnings <> candidateWarnings)
                  )
                  <$> reflectCodecWithoutSourceWithEvaluationBy
                    runtime
                    (resolveRequiredVariables context)
                    request
                    deployed


resolveRequiredVariables
  :: (MonadFileSystem m)
  => Context m
  -> CodecRequirements
  -> m (Map Text.Text ByteString.ByteString, [RouteMapWarning])
resolveRequiredVariables context (CodecRequirements _ selection _) = do
  resolved <- forM (Set.toAscList requestedNames) $ \name ->
    if selection.manifestInputsOnly && not (name `Set.member` activeNames)
      then return (Nothing, [])
      else do
        lookupResult <- context.variableGetter name
        let value = osStringBytes <$> lookupResult.value
        return
          ( (,) name <$> value
          , FilePathExpressionWarning <$> lookupResult.warnings
          )
  return
    ( Map.fromList [pair | (Just pair, _) <- resolved]
    , nub $ concatMap snd resolved
    )
 where
  declaredVariables = context.repository.manifest.variables
  activeNames =
    Set.fromList
      [ renderManifestVariableName name
      | (name, variable) <- Map.toAscList declaredVariables
      , isJust $ fst $ dispatchManifestVariable context.environment variable
      ]
  requestedNames =
    Map.keysSet selection.namedInputs
      <> if selection.includeAllInputs then activeNames else Set.empty


osStringBytes :: OsString.OsString -> ByteString.ByteString
osStringBytes = ByteString.pack . concatMap codeUnitBytes . OsString.unpack


codeUnitBytes :: OsString.OsChar -> [Word8]
#ifdef mingw32_HOST_OS
codeUnitBytes character =
  let value = ord $ OsString.toChar character
  in [fromIntegral value, fromIntegral $ value `div` 0x100]
#else
codeUnitBytes = (: []) . fromIntegral . ord . OsString.toChar
#endif


contextFacts
  :: (MonadFileSystem m) => Context m -> Map Text.Text Text.Text
contextFacts context =
  Map.fromList
    [ (factKeyText key, factValueText value)
    | (key, value) <- Map.toAscList $ environmentFacts context.environment
    ]


managedRouteName
  :: (MonadFileSystem m) => ManagedCorrespondence -> m Text.Text
managedRouteName managed =
  Text.pack
    <$> decodePath
      (normalise $ managed.route.routeName </> managed.relativePath)
