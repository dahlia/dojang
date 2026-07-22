{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.DiffSpec (spec) where

import Control.Exception (bracket, bracket_, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (findExecutable)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (SeekMode (AbsoluteSeek), hClose, hFlush, hSeek, stdout)
import System.IO.Temp (withSystemTempFile)
import System.OsPath (OsPath, encodeFS, (</>))
import Test.Hspec (Spec, describe, it, runIO, sequential, xit)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldReturn, shouldThrow)
import Prelude hiding (writeFile)

import Dojang.App (App, AppEnv (..), runAppWithoutLogging)
import Dojang.Commands.Apply (applyWithCodecRuntime)
import Dojang.Commands.Diff
  ( DiffMode (Both, Destination, Source)
  , diffWithCodecRuntime
  )
import Dojang.MonadFileSystem
  ( FileType (File)
  , MonadFileSystem (createDirectories, decodePath, writeFile)
  )
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Codec
  ( CodecConfiguration (CodecConfiguration)
  , CodecDefinition (CodecDefinition)
  , CodecSpec (CodecSpec)
  , ReflectPolicy (ReflectReject)
  , identityCodecSpec
  , parseCodecName
  )
import Dojang.Types.Codec.Evaluate
  ( CacheScope (PersistentCache)
  , CodecDryRunPolicy (CachedOnly, EvaluatePurely)
  , CodecImplementation (CodecImplementation)
  , CodecInputs (..)
  , CodecProgram (CodecDone)
  , CodecRequirements (CodecRequirements)
  , CodecRuntime (CodecRuntime)
  , EvaluationMode (DryRunEvaluation, NormalEvaluation)
  , codecImplementationWithEffects
  , codecRegistry
  , identityCodecRuntime
  , noCodecInputs
  , revealBytes
  )
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression (FilePathExpression (Substitution))
import Dojang.Types.FileRoute
  ( FileRoute
  , RouteKind (CopyRoute)
  , RouteMode (DefaultMode, Private)
  , RouteTarget (RouteTarget)
  , fileRoutePreservingOrder
  )
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.Manifest qualified as Manifest
import Dojang.Types.RepositoryId (parseRepositoryId)


spec :: Spec
spec = sequential $ describe "diff" $ do
  echoProgram <- runIO $ findExecutable "echo"
  it "filters requested files before evaluating codecs" $
    withSelectedIdentityRoute $ \appEnv selectedSource ->
      runAppWithoutLogging
        appEnv
        ( diffWithCodecRuntime
            (identityCodecRuntime NormalEvaluation)
            Source
            Nothing
            [selectedSource]
        )
        `shouldReturn` ExitSuccess

  it "handles arbitrary binary rendered output in the builtin diff" $
    withCodecFile "intermediate" $ \appEnv source _ codecSpec -> do
      let runtime =
            testRuntime EvaluatePurely NormalEvaluation codecSpec $
              const $
                ByteString.pack [0xff, 0x00]
      runAppWithoutLogging
        appEnv
        (diffWithCodecRuntime runtime Source Nothing [source])
        `shouldReturn` ExitSuccess

  it "suppresses rendered plaintext for a sensitive codec" $
    withSensitiveCodecFile $ \appEnv source codecSpec -> do
      let CodecSpec name _ = codecSpec
          implementation =
            codecImplementationWithEffects
              (CodecDefinition name "sensitive-1" ReflectReject)
              (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (const $ CodecDone $ Right "decrypted-secret")
              Nothing
              EvaluatePurely
          runtime =
            CodecRuntime
              (codecRegistry [implementation])
              NormalEvaluation
              (const $ return $ Left "unexpected external input")
      (output, result) <-
        captureStdout $
          runAppWithoutLogging
            appEnv
            (diffWithCodecRuntime runtime Source Nothing [source])
      result `shouldBe` ExitSuccess
      ByteString.isInfixOf "decrypted-secret" output `shouldBe` False
      output `shouldBe` "Sensitive codec content differs; diff suppressed.\n"

  it "rejects an unprotected sensitive destination-only diff" $
    withCodecFile "previous-plaintext" $ \appEnv _ destination codecSpec -> do
      let CodecSpec name _ = codecSpec
          implementation =
            codecImplementationWithEffects
              (CodecDefinition name "sensitive-1" ReflectReject)
              (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
              (const $ CodecDone $ Right "decrypted-secret")
              Nothing
              EvaluatePurely
          runtime =
            CodecRuntime
              (codecRegistry [implementation])
              NormalEvaluation
              (const $ return $ Left "unexpected external input")
      runAppWithoutLogging
        appEnv
        (diffWithCodecRuntime runtime Destination Nothing [destination])
        `shouldThrow` (== ExitFailure 39)

  it "reuses a persisted codec cache in dry-run diff" $
    withCodecFile "" $ \appEnv source _ codecSpec -> do
      let render (TestInputs rawSourceBytes) = rawSourceBytes <> ":rendered"
          normalRuntime =
            testRuntime EvaluatePurely NormalEvaluation codecSpec render
          cachedRuntime =
            testRuntime CachedOnly DryRunEvaluation codecSpec render
      runAppWithoutLogging appEnv (applyWithCodecRuntime normalRuntime True [])
        `shouldReturn` ExitSuccess
      runAppWithoutLogging
        appEnv{dryRun = True}
        (diffWithCodecRuntime cachedRuntime Source Nothing [source])
        `shouldReturn` ExitSuccess

  it "does not evaluate codecs for a destination-only diff" $
    withCodecFile "deployed" $ \appEnv _ destination _ ->
      runAppWithoutLogging
        appEnv
        ( diffWithCodecRuntime
            (identityCodecRuntime NormalEvaluation)
            Destination
            Nothing
            [destination]
        )
        `shouldReturn` ExitSuccess

  it "does not report equal binary source and intermediate files" $
    withEqualBinarySource $ \appEnv source -> do
      (output, result) <-
        captureStdout $
          runAppWithoutLogging
            appEnv
            ( diffWithCodecRuntime
                (identityCodecRuntime NormalEvaluation)
                Source
                Nothing
                [source]
            )
      result `shouldBe` ExitSuccess
      output `shouldBe` ""

  it "compares equal binary source and destination contents" $
    withEqualBinaryReplicas $ \appEnv source -> do
      (output, result) <-
        captureStdout $
          runAppWithoutLogging
            appEnv
            ( diffWithCodecRuntime
                (identityCodecRuntime NormalEvaluation)
                Both
                Nothing
                [source]
            )
      result `shouldBe` ExitSuccess
      output `shouldBe` ""

  let externalIt = maybe xit (const it) echoProgram
  externalIt "rejects an external diff before producing partial output" $
    withMixedCodecFiles $ \appEnv codecSpec -> do
      let Just program = echoProgram
          runtime =
            testRuntime EvaluatePurely NormalEvaluation codecSpec $
              \(TestInputs rawSourceBytes) -> rawSourceBytes <> ":rendered"
      programPath <- encodeFS program
      (output, result) <-
        captureStdout $
          try $
            runAppWithoutLogging
              appEnv
              (diffWithCodecRuntime runtime Source (Just programPath) [])
      result `shouldBe` Left (ExitFailure 39)
      output `shouldBe` ""


newtype TestInputs = TestInputs ByteString


testRuntime
  :: CodecDryRunPolicy
  -> EvaluationMode
  -> CodecSpec
  -> (TestInputs -> ByteString)
  -> CodecRuntime (App IO)
testRuntime dryRunPolicy mode codecSpec render =
  CodecRuntime
    (codecRegistry [implementation])
    mode
    (const $ return $ Left "unexpected external input")
 where
  CodecSpec name _ = codecSpec
  implementation =
    CodecImplementation
      (CodecDefinition name "test-1" ReflectReject)
      (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
      (\inputs -> Right $ render $ TestInputs $ revealBytes inputs.rawSource)
      Nothing
      PersistentCache
      dryRunPolicy


withCodecFile
  :: ByteString
  -> (AppEnv -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withCodecFile intermediateContents action =
  withRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "encoded-file"
    destinationName <- encodeFS "destination"
    let source = repository </> routeName
        intermediate = repository </> intermediateDir </> routeName
        destination = tmpDir </> destinationName
        Just codecName = parseCodecName "test-codec"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        route = fileRoute routeName "DEST" codecSpec
        manifest' = baseManifest $ Map.singleton routeName route
    writeManifestFile manifest' manifestPath
    writeFile source "raw"
    writeFile intermediate intermediateContents
    writeFile destination intermediateContents
    withEnvVar "DEST" (Just destination) $
      action appEnv source destination codecSpec


withSensitiveCodecFile
  :: (AppEnv -> OsPath -> CodecSpec -> IO a)
  -> IO a
withSensitiveCodecFile action =
  withRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "sensitive-file"
    destinationName <- encodeFS "sensitive-destination"
    let source = repository </> routeName
        intermediate = repository </> intermediateDir </> routeName
        destination = tmpDir </> destinationName
        Just codecName = parseCodecName "sensitive-codec"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        route = fileRouteWithMode Private routeName "DEST" codecSpec
        manifest' = baseManifest $ Map.singleton routeName route
    writeManifestFile manifest' manifestPath
    writeFile source "encrypted-source"
    writeFile intermediate "previous-plaintext"
    writeFile destination "previous-plaintext"
    withEnvVar "DEST" (Just destination) $
      action appEnv source codecSpec


withEqualBinarySource :: (AppEnv -> OsPath -> IO a) -> IO a
withEqualBinarySource action =
  withRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "binary-file"
    destinationName <- encodeFS "destination"
    let source = repository </> routeName
        intermediate = repository </> intermediateDir </> routeName
        destination = tmpDir </> destinationName
        route = fileRoute routeName "DEST" identityCodecSpec
        bytes = ByteString.pack [0xff, 0x00]
    writeManifestFile (baseManifest $ Map.singleton routeName route) manifestPath
    writeFile source bytes
    writeFile intermediate bytes
    writeFile destination "changed destination"
    withEnvVar "DEST" (Just destination) $ action appEnv source


withEqualBinaryReplicas :: (AppEnv -> OsPath -> IO a) -> IO a
withEqualBinaryReplicas action =
  withRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "binary-file"
    destinationName <- encodeFS "destination"
    let source = repository </> routeName
        intermediate = repository </> intermediateDir </> routeName
        destination = tmpDir </> destinationName
        route = fileRoute routeName "DEST" identityCodecSpec
        bytes = ByteString.pack [0xff, 0x00]
    writeManifestFile (baseManifest $ Map.singleton routeName route) manifestPath
    writeFile source bytes
    writeFile intermediate "old intermediate"
    writeFile destination bytes
    withEnvVar "DEST" (Just destination) $ action appEnv source


withSelectedIdentityRoute :: (AppEnv -> OsPath -> IO a) -> IO a
withSelectedIdentityRoute action =
  withRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    selectedName <- encodeFS "selected"
    unrelatedName <- encodeFS "unrelated"
    selectedDestinationName <- encodeFS "selected-destination"
    unrelatedDestinationName <- encodeFS "unrelated-destination"
    let selectedSource = repository </> selectedName
        unrelatedSource = repository </> unrelatedName
        selectedIntermediate = repository </> intermediateDir </> selectedName
        unrelatedIntermediate = repository </> intermediateDir </> unrelatedName
        selectedDestination = tmpDir </> selectedDestinationName
        unrelatedDestination = tmpDir </> unrelatedDestinationName
        Just unknownName = parseCodecName "unknown"
        routes =
          Map.fromList
            [ (selectedName, fileRoute selectedName "SELECTED_DEST" identityCodecSpec)
            ,
              ( unrelatedName
              , fileRoute
                  unrelatedName
                  "UNRELATED_DEST"
                  (CodecSpec unknownName $ CodecConfiguration mempty)
              )
            ]
    writeManifestFile (baseManifest routes) manifestPath
    writeFile selectedSource "selected source"
    writeFile selectedIntermediate "selected intermediate"
    writeFile selectedDestination "selected intermediate"
    writeFile unrelatedSource "unrelated"
    writeFile unrelatedIntermediate "unrelated"
    writeFile unrelatedDestination "unrelated"
    withEnvVar "SELECTED_DEST" (Just selectedDestination) $
      withEnvVar "UNRELATED_DEST" (Just unrelatedDestination) $
        action appEnv selectedSource


withMixedCodecFiles :: (AppEnv -> CodecSpec -> IO a) -> IO a
withMixedCodecFiles action =
  withRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    identityName <- encodeFS "a-identity"
    codecName <- encodeFS "z-codec"
    identityDestinationName <- encodeFS "a-identity-destination"
    codecDestinationName <- encodeFS "z-codec-destination"
    let identitySource = repository </> identityName
        codecSource = repository </> codecName
        identityIntermediate = repository </> intermediateDir </> identityName
        codecIntermediate = repository </> intermediateDir </> codecName
        identityDestination = tmpDir </> identityDestinationName
        codecDestination = tmpDir </> codecDestinationName
        Just implementationName = parseCodecName "test-codec"
        codecSpec = CodecSpec implementationName $ CodecConfiguration mempty
        routes =
          Map.fromList
            [ (identityName, fileRoute identityName "IDENTITY_DEST" identityCodecSpec)
            , (codecName, fileRoute codecName "CODEC_DEST" codecSpec)
            ]
    writeManifestFile (baseManifest routes) manifestPath
    writeFile identitySource "identity source"
    writeFile identityIntermediate "old identity"
    writeFile identityDestination "old identity"
    writeFile codecSource "codec source"
    writeFile codecIntermediate "old codec"
    writeFile codecDestination "old codec"
    withEnvVar "IDENTITY_DEST" (Just identityDestination) $
      withEnvVar "CODEC_DEST" (Just codecDestination) $
        action appEnv codecSpec


fileRoute :: OsPath -> Text -> CodecSpec -> FileRoute
fileRoute = fileRouteWithMode DefaultMode


fileRouteWithMode :: RouteMode -> OsPath -> Text -> CodecSpec -> FileRoute
fileRouteWithMode mode _ variable codecSpec =
  fileRoutePreservingOrder
    (const Nothing)
    [
      ( Always
      , Just $
          RouteTarget
            (Substitution variable)
            mode
            CopyRoute
            codecSpec
      )
    ]
    File


baseManifest :: Map.Map OsPath FileRoute -> Manifest
baseManifest routes =
  ManifestWithCodecBackends
    { Manifest.repositoryId =
        either (const Nothing) Just $
          parseRepositoryId "823e4567-e89b-42d3-a456-426614174000"
    , monikers = mempty
    , variables = mempty
    , fileRoutes = routes
    , ignorePatterns = mempty
    , codecBackends = mempty
    , hooks = mempty
    }


withRepository
  :: (OsPath -> OsPath -> OsPath -> OsPath -> AppEnv -> IO a)
  -> IO a
withRepository action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  let repository = tmpDir </> sourceDir
      manifestPath = repository </> manifestFilename
      appEnv =
        AppEnv
          repository
          False
          (Just intermediateDir)
          (tmpDir </> stateDir)
          manifestFilename
          envFilename
          False
          False
  createDirectories $ repository </> intermediateDir
  action tmpDir repository intermediateDir manifestPath appEnv


withEnvVar :: String -> Maybe OsPath -> IO a -> IO a
withEnvVar name value action = do
  oldValue <- lookupEnv name
  bracket_
    (setOrUnset value)
    (maybe (unsetEnv name) (setEnv name) oldValue)
    action
 where
  setOrUnset Nothing = unsetEnv name
  setOrUnset (Just value') = decodePath value' >>= setEnv name


captureStdout :: IO a -> IO (ByteString, a)
captureStdout action =
  withSystemTempFile "dojang-diff-spec-stdout" $ \_ captureHandle ->
    bracket (hDuplicate stdout) restore $ \_ -> do
      hDuplicateTo captureHandle stdout
      result <- action
      hFlush stdout
      hSeek captureHandle AbsoluteSeek 0
      output <- ByteString.hGetContents captureHandle
      return (output, result)
 where
  restore originalHandle = do
    hDuplicateTo originalHandle stdout
    hClose originalHandle
