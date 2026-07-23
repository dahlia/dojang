{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.ApplySpec (spec) where

import Control.Exception (bracket, bracket_)
import Control.Monad.Except (catchError)
import Data.ByteString qualified as ByteString
import Data.HashMap.Strict (singleton)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (SeekMode (AbsoluteSeek), hClose, hFlush, hSeek, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Info (os)
import System.OsPath (OsPath, encodeFS, (</>))


#ifndef mingw32_HOST_OS
import System.Posix.Env.ByteString qualified as PosixEnv
#endif
import Test.Hspec
  ( Spec
  , anyIOException
  , describe
  , it
  , pendingWith
  , runIO
  , sequential
  , xit
  )
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldReturn
  , shouldSatisfy
  , shouldThrow
  )
import Prelude hiding (readFile, writeFile)

import Dojang.App (App, AppEnv (..), liftApp, runAppWithoutLogging)
import Dojang.Commands.Apply (apply, applyWithCodecRuntime)
import Dojang.Commands.Status (defaultStatusOptions, statusWithCodecRuntime)
import Dojang.ExitCodes (codecError, conflictError, manifestReadError)
import Dojang.MonadFileSystem
  ( FileType (Directory, File)
  , MonadFileSystem (..)
  , dryRunIO
  )
import Dojang.Syntax.Env (writeEnvFile)
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Codec
  ( CodecConfiguration (CodecConfiguration)
  , CodecDefinition (CodecDefinition)
  , CodecSpec (CodecSpec)
  , CodecValue (CodecBoolean)
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
  , ExternalInput (ExternalInput)
  , ExternalInputRequest (ExternalInputRequest)
  , codecImplementationWithEffects
  , codecRegistry
  , noCodecInputs
  , opaqueBytes
  , requiredCodecInputs
  , revealBytes
  )
import Dojang.Types.Codec.Template (templateCodecSpec)
import Dojang.Types.Environment
  ( Architecture (X86_64)
  , Kernel (Kernel)
  , OperatingSystem (Linux)
  , emptyEnvironment
  , withFacts
  )
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression
  ( FilePathExpression (BareComponent, Substitution)
  )
import Dojang.Types.FileRoute
  ( RouteKind (CopyRoute, SymlinkRoute)
  , RouteMode (DefaultMode, Private)
  , RouteTarget (RouteTarget)
  , fileRoutePreservingOrder
  )
import Dojang.Types.MachineState
  ( MachineState (..)
  , readMachineId
  , readRepositoryState
  , updateManagedTargets
  )
import Dojang.Types.ManagedTarget
  ( ManagedCodecState (..)
  , ManagedTarget (..)
  , TargetFingerprint (FileFingerprint, SymlinkFingerprint)
  )
import Dojang.Types.Manifest (Manifest (..), manifest)
import Dojang.Types.Manifest qualified as Manifest
import Dojang.Types.ManifestVariable
  ( ManifestVariableMap
  , manifestVariable
  , parseManifestVariableName
  )
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.RepositoryId (parseRepositoryId)
import Dojang.Types.RouteMetadata
  ( PortableMode (..)
  , portableModeFromBits
  )


spec :: Spec
spec = sequential $ do
  symlinkAvailable <- runIO $ withTempDir $ \tmpDir _ -> do
    probeTarget <- encodeFS "probe-target"
    probeLink <- encodeFS "probe-link"
    writeFile (tmpDir </> probeTarget) ""
    ( do
        createSymbolicLink
          (tmpDir </> probeTarget)
          (tmpDir </> probeLink)
          File
        return True
      )
      `catchError` const (return False)
  let symIt = if symlinkAvailable then it else xit

  describe "apply" $ do
    it "writes rendered bytes through the intermediate snapshot" $
      withCodecFile $ \appEnv source intermediate destination spec' -> do
        let CodecSpec name _ = spec'
            implementation =
              CodecImplementation
                (CodecDefinition name "test-1" ReflectReject)
                (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
                (\inputs -> Right $ revealBytes inputs.rawSource <> ":rendered")
                Nothing
                PersistentCache
                EvaluatePurely
            runtime =
              CodecRuntime
                (codecRegistry [implementation])
                NormalEvaluation
                (const $ return $ Left "unexpected external input")
        result <-
          runAppWithoutLogging appEnv $
            applyWithCodecRuntime runtime True []
        result `shouldBe` ExitSuccess
        readFile source `shouldReturn` "raw"
        readFile intermediate `shouldReturn` "raw:rendered"
        readFile destination `shouldReturn` "raw:rendered"
        Right (Just machineId) <- readMachineId appEnv.stateDirectory
        let Right repositoryId' =
              parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        Right (Just state) <-
          readRepositoryState appEnv.stateDirectory repositoryId' machineId
        let [target] = Map.elems state.targetRecords
        ((.name) <$> target.codecState) `shouldBe` Just "test-codec"
        runAppWithoutLogging appEnv (applyWithCodecRuntime runtime True [])
          `shouldReturn` ExitSuccess

    it "aborts before applying stale codec output" $
      withCodecFile $ \appEnv source intermediate destination spec' -> do
        let CodecSpec name _ = spec'
            inputRequest = ExternalInputRequest "rotate-source"
            implementation =
              CodecImplementation
                (CodecDefinition name "test-1" ReflectReject)
                ( const $
                    Right $
                      CodecRequirements noCodecInputs noCodecInputs [inputRequest]
                )
                (\inputs -> Right $ revealBytes inputs.rawSource <> ":rendered")
                Nothing
                PersistentCache
                EvaluatePurely
            resolve request
              | request == inputRequest = do
                  writeFile source "changed"
                  return $ Right $ ExternalInput (opaqueBytes "") "stable"
              | otherwise = return $ Left "unexpected external input"
            runtime =
              CodecRuntime
                (codecRegistry [implementation])
                NormalEvaluation
                resolve
        runAppWithoutLogging appEnv (applyWithCodecRuntime runtime True [])
          `shouldThrow` anyIOException
        readFile source `shouldReturn` "changed"
        exists intermediate `shouldReturn` False
        exists destination `shouldReturn` False

    it "exposes machine facts under their canonical keys" $
      withCodecFile $ \appEnv _ _ destination spec' -> do
        let CodecSpec name _ = spec'
            implementation =
              CodecImplementation
                (CodecDefinition name "test-1" ReflectReject)
                ( const $
                    Right $
                      CodecRequirements
                        (requiredCodecInputs ["os", "arch", "kernel", "kernel-release"])
                        noCodecInputs
                        []
                )
                (\inputs -> Right $ revealBytes inputs.rawSource)
                Nothing
                PersistentCache
                EvaluatePurely
            runtime =
              CodecRuntime
                (codecRegistry [implementation])
                NormalEvaluation
                (const $ return $ Left "unexpected external input")
        runAppWithoutLogging appEnv (applyWithCodecRuntime runtime True [])
          `shouldReturn` ExitSuccess
        readFile destination `shouldReturn` "raw"

    it "looks up codec machine facts case-insensitively" $
      withCodecFile $ \appEnv _ _ destination spec' -> do
        let CodecSpec name _ = spec'
            environment =
              withFacts
                (Map.singleton "Team" "platform")
                (emptyEnvironment Linux X86_64 $ Kernel "Linux" "6.0")
            implementation =
              CodecImplementation
                (CodecDefinition name "test-1" ReflectReject)
                ( const $
                    Right $
                      CodecRequirements (requiredCodecInputs ["team"]) noCodecInputs []
                )
                ( \inputs -> case Map.lookup "team" inputs.facts of
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
                (const $ return $ Left "unexpected external input")
        writeEnvFile
          environment
          (appEnv.sourceDirectory </> appEnv.envFile)
        runAppWithoutLogging appEnv (applyWithCodecRuntime runtime True [])
          `shouldReturn` ExitSuccess
        readFile destination `shouldReturn` "platform"

    it "does not evaluate the default identity codec" $
      withTwoManagedFiles $ \appEnv sourceA destinationA _ -> do
        let runtime =
              CodecRuntime
                mempty
                NormalEvaluation
                (const $ return $ Left "unexpected external input")
        runAppWithoutLogging
          appEnv
          (applyWithCodecRuntime runtime True [sourceA])
          `shouldReturn` ExitSuccess
        readFile destinationA `shouldReturn` "source a"

    it "validates an unknown codec before deleting a missing source" $
      withCodecFile $ \appEnv source _ destination _ -> do
        let runtime =
              CodecRuntime
                mempty
                NormalEvaluation
                (const $ return $ Left "unexpected external input")
        removeFile source
        writeFile destination "preserve"
        runAppWithoutLogging appEnv (applyWithCodecRuntime runtime True [])
          `shouldThrow` (== codecError)
        readFile destination `shouldReturn` "preserve"

    it "validates codec configuration before deleting a missing source" $ do
      let Just codecName = parseCodecName "test-codec"
          invalidSpec =
            CodecSpec codecName $
              CodecConfiguration $
                Map.singleton "unexpected" $
                  CodecBoolean True
      withCodecFileWithSpec mempty invalidSpec $
        \appEnv source _ destination _ -> do
          let implementation =
                CodecImplementation
                  (CodecDefinition codecName "test-1" ReflectReject)
                  ( \configuration ->
                      if configuration == CodecConfiguration mempty
                        then Right $ CodecRequirements noCodecInputs noCodecInputs []
                        else Left "unexpected configuration"
                  )
                  (Right . revealBytes . (.rawSource))
                  Nothing
                  PersistentCache
                  EvaluatePurely
              runtime =
                CodecRuntime
                  (codecRegistry [implementation])
                  NormalEvaluation
                  (const $ return $ Left "unexpected external input")
          removeFile source
          writeFile destination "preserve"
          runAppWithoutLogging appEnv (applyWithCodecRuntime runtime True [])
            `shouldThrow` (== codecError)
          readFile destination `shouldReturn` "preserve"

    it "requires private storage before evaluating a sensitive codec" $
      withCodecFile $ \appEnv _ intermediate destination spec' -> do
        let CodecSpec name _ = spec'
            implementation =
              codecImplementationWithEffects
                (CodecDefinition name "sensitive-1" ReflectReject)
                (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
                (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
                (const $ CodecDone $ Right "sentinel-secret")
                Nothing
                CachedOnly
            runtime =
              CodecRuntime
                (codecRegistry [implementation])
                NormalEvaluation
                (const $ return $ Left "unexpected external input")
        runAppWithoutLogging appEnv (applyWithCodecRuntime runtime True [])
          `shouldThrow` (== codecError)
        exists intermediate `shouldReturn` False
        exists destination `shouldReturn` False

    it "protects sensitive intermediate and destination files" $ do
      let Just codecName = parseCodecName "sensitive-codec"
          spec' = CodecSpec codecName $ CodecConfiguration mempty
      withCodecFileWithMode Private mempty spec' $
        \appEnv _ intermediate destination _ -> do
          let implementation =
                codecImplementationWithEffects
                  (CodecDefinition codecName "sensitive-1" ReflectReject)
                  (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
                  (\_ _ -> Right $ CodecRequirements noCodecInputs noCodecInputs [])
                  (const $ CodecDone $ Right "sentinel-secret")
                  Nothing
                  CachedOnly
              runtime =
                CodecRuntime
                  (codecRegistry [implementation])
                  NormalEvaluation
                  (const $ return $ Left "unexpected external input")
          runAppWithoutLogging appEnv (applyWithCodecRuntime runtime True [])
            `shouldReturn` ExitSuccess
          readFile intermediate `shouldReturn` "sentinel-secret"
          readFile destination `shouldReturn` "sentinel-secret"
          getPortableMode intermediate
            `shouldReturn` expectedPrivateFileMode
          getPortableMode destination
            `shouldReturn` expectedPrivateFileMode
          Right (Just machineId) <- readMachineId appEnv.stateDirectory
          let Right repositoryId' =
                parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
          Right (Just state) <-
            readRepositoryState appEnv.stateDirectory repositoryId' machineId
          let [target] = Map.elems state.targetRecords
          target.codecState `shouldBe` Nothing
          getPortableMode target.snapshotPath
            `shouldReturn` expectedPrivateFileMode

    it "renders the built-in template codec with manifest variables" $ do
      let Right variableName = parseManifestVariableName "NAME"
          variables =
            Map.singleton variableName $
              manifestVariable mempty $
                BareComponent "Ada"
      withTemplateFile variables $ \appEnv source intermediate destination _ -> do
        writeFile source "name = {{ vars.NAME }}\n"
        runAppWithoutLogging appEnv (apply True []) `shouldReturn` ExitSuccess
        readFile intermediate `shouldReturn` "name = Ada\n"
        readFile destination `shouldReturn` "name = Ada\n"

    it "does not expose inherited environment variables to templates" $
      withTemplateFile mempty $ \appEnv source intermediate destination _ -> do
        inherited <- encodeFS "private"
        writeFile source "{{ vars.INHERITED_ONLY }}"
        withEnvVars [("INHERITED_ONLY", Just inherited)] $
          runAppWithoutLogging appEnv (apply True [])
            `shouldThrow` (== codecError)
        exists intermediate `shouldReturn` False
        exists destination `shouldReturn` False

    it "preserves native non-UTF-8 variable bytes for codecs" $
      if os == "mingw32"
        then pendingWith "POSIX environment values are native byte strings."
        else withCodecFile $ \appEnv _ _ destination spec' -> do
          let CodecSpec name _ = spec'
              variableName = "CODEC_NATIVE_VALUE"
              rawValue = ByteString.pack [0xff, 0xfe]
              implementation =
                CodecImplementation
                  (CodecDefinition name "test-1" ReflectReject)
                  ( const $
                      Right $
                        CodecRequirements noCodecInputs (requiredCodecInputs [variableName]) []
                  )
                  ( \inputs -> case Map.lookup variableName inputs.variables of
                      Just value -> Right value
                      Nothing -> Left "missing native variable"
                  )
                  Nothing
                  PersistentCache
                  EvaluatePurely
              runtime =
                CodecRuntime
                  (codecRegistry [implementation])
                  NormalEvaluation
                  (const $ return $ Left "unexpected external input")
          withRawEnvVar "CODEC_NATIVE_VALUE" rawValue $
            runAppWithoutLogging appEnv (applyWithCodecRuntime runtime True [])
              `shouldReturn` ExitSuccess
          readFile destination `shouldReturn` rawValue

    it "prints warnings from codec manifest variables" $ do
      let variableName = "TOKEN"
          Right declaredName = parseManifestVariableName variableName
          variables =
            Map.singleton declaredName $
              manifestVariable mempty $
                Substitution "MISSING_TOKEN"
      withEnvVars [("MISSING_TOKEN", Nothing)] $
        withCodecFileWithVariables variables $ \appEnv _ _ _ spec' -> do
          let CodecSpec name _ = spec'
              implementation =
                CodecImplementation
                  (CodecDefinition name "test-1" ReflectReject)
                  ( const $
                      Right $
                        CodecRequirements noCodecInputs (requiredCodecInputs [variableName]) []
                  )
                  ( \inputs ->
                      Right $
                        Map.findWithDefault
                          mempty
                          variableName
                          inputs.variables
                  )
                  Nothing
                  PersistentCache
                  EvaluatePurely
              runtime =
                CodecRuntime
                  (codecRegistry [implementation])
                  NormalEvaluation
                  (const $ return $ Left "unexpected external input")
          (output, result) <-
            captureStderr $
              runAppWithoutLogging appEnv $
                applyWithCodecRuntime runtime True []
          result `shouldBe` ExitSuccess
          output
            `shouldSatisfy` ByteString.isInfixOf
              "Reference to an undefined environment variable: MISSING_TOKEN."

    it "reuses a persisted codec cache in dry-run status" $
      withCodecFile $ \appEnv _ _ _ spec' -> do
        let normalRuntime = testCodecRuntime EvaluatePurely NormalEvaluation spec'
            cachedRuntime = testCodecRuntime CachedOnly DryRunEvaluation spec'
            dryRunEnv = appEnv{dryRun = True}
        runAppWithoutLogging appEnv (applyWithCodecRuntime normalRuntime True [])
          `shouldReturn` ExitSuccess
        runAppWithoutLogging
          dryRunEnv
          (statusWithCodecRuntime cachedRuntime defaultStatusOptions)
          `shouldReturn` ExitSuccess

    it "loads the active cache record when an orphan shares its source" $
      withCodecFile $ \appEnv _ _ _ spec' -> do
        let normalRuntime = testCodecRuntime EvaluatePurely NormalEvaluation spec'
            cachedRuntime = testCodecRuntime CachedOnly DryRunEvaluation spec'
        runAppWithoutLogging appEnv (applyWithCodecRuntime normalRuntime True [])
          `shouldReturn` ExitSuccess
        Right (Just machineId) <- readMachineId appEnv.stateDirectory
        let Right repositoryId =
              parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        Right (Just state) <-
          readRepositoryState appEnv.stateDirectory repositoryId machineId
        active <- case Map.elems state.targetRecords of
          [target] -> return target
          targets -> fail $ "Unexpected managed targets: " <> show targets
        let FileFingerprint size _ = active.fingerprint
            orphan =
              active
                { targetId = "000-orphan"
                , fingerprint = FileFingerprint size "invalid"
                }
        now <- getCurrentTime
        Right _ <-
          updateManagedTargets
            appEnv.stateDirectory
            now
            state
            (Map.insert orphan.targetId orphan)
        runAppWithoutLogging
          appEnv{dryRun = True}
          (statusWithCodecRuntime cachedRuntime defaultStatusOptions)
          `shouldReturn` ExitSuccess

    it "keeps separate cache entries for correspondences sharing a source" $
      withSharedSourceCodecRoutes $ \appEnv specs -> do
        let normalRuntime = sharedSourceRuntime EvaluatePurely NormalEvaluation specs
            cachedRuntime = sharedSourceRuntime CachedOnly DryRunEvaluation specs
        runAppWithoutLogging appEnv (applyWithCodecRuntime normalRuntime True [])
          `shouldReturn` ExitSuccess
        runAppWithoutLogging
          appEnv{dryRun = True}
          (statusWithCodecRuntime cachedRuntime defaultStatusOptions)
          `shouldReturn` ExitSuccess

    it "rejects directory drift for a codec-backed file route" $
      withCodecFile $ \appEnv source _ _ spec' -> do
        removeFile source
        createDirectories source
        let runtime = testCodecRuntime EvaluatePurely NormalEvaluation spec'
        runAppWithoutLogging appEnv (applyWithCodecRuntime runtime True [])
          `shouldThrow` (== codecError)

    it "does not update destinations outside the selected paths" $
      withTwoManagedFiles $ \appEnv sourceA destinationA destinationB -> do
        (result, destinationAContents, destinationBContents, targets) <- dryRunIO $ do
          result <- runAppWithoutLogging appEnv (apply True [sourceA])
          destinationAContents <- readFile destinationA
          destinationBContents <- readFile destinationB
          Right (Just machineId) <- readMachineId appEnv.stateDirectory
          let Right repositoryId' =
                parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
          Right (Just state) <-
            readRepositoryState appEnv.stateDirectory repositoryId' machineId
          return
            ( result
            , destinationAContents
            , destinationBContents
            , Map.elems state.targetRecords
            )
        result `shouldBe` ExitSuccess
        destinationAContents `shouldBe` "source a"
        destinationBContents `shouldBe` "destination b"
        (.destinationPath) <$> targets `shouldBe` [destinationA]
        exists appEnv.stateDirectory `shouldReturn` False

    it "does not expand targeted codec evaluation in debug mode" $
      withDebugTargetedCodecFiles $
        \appEnv sourceA destinationA destinationB codecSpec -> do
          resolutionCount <- newIORef (0 :: Int)
          runAppWithoutLogging
            appEnv
            ( applyWithCodecRuntime
                (countingApplyRuntime resolutionCount codecSpec)
                True
                [sourceA]
            )
            `shouldReturn` ExitSuccess
          readFile destinationA `shouldReturn` "source a"
          readFile destinationB `shouldReturn` "destination b"
          readIORef resolutionCount `shouldReturn` 1

    it "keeps tracked files managed when they match an ignore pattern" $
      withTrackedIgnoredFile $ \appEnv intermediate destination -> do
        (result, intermediateContents, destinationContents) <- dryRunIO $ do
          result <- runAppWithoutLogging appEnv (apply False [])
          intermediateContents <- readFile intermediate
          destinationContents <- readFile destination
          return (result, intermediateContents, destinationContents)
        result `shouldBe` ExitSuccess
        intermediateContents `shouldBe` "source contents"
        destinationContents `shouldBe` "source contents"

    it "reclaims superseded target-snapshot transaction roots" $
      withTwoManagedFiles $ \appEnv _ _ _ -> do
        runAppWithoutLogging appEnv (apply True []) `shouldReturn` ExitSuccess
        runAppWithoutLogging appEnv (apply True []) `shouldReturn` ExitSuccess
        Right (Just machineId) <- readMachineId appEnv.stateDirectory
        let Right repositoryId' =
              parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        Right (Just state) <-
          readRepositoryState appEnv.stateDirectory repositoryId' machineId
        transactions <- listDirectory state.targetSnapshotRoot
        length transactions `shouldBe` 1

    it "reclaims baselines when a route becomes a link" $
      withKindSwitchRoute $ \appEnv source manifestPath linkManifest -> do
        let Right repositoryId' =
              parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        (result, baselineExisted, result', fingerprints, baselineRemains) <-
          dryRunIO $ do
            result <- runAppWithoutLogging appEnv (apply True [])
            Right (Just machineId) <- readMachineId appEnv.stateDirectory
            Right (Just state) <-
              readRepositoryState appEnv.stateDirectory repositoryId' machineId
            let [copyRecord] = Map.elems state.targetRecords
            baselineExisted <- exists copyRecord.snapshotPath
            -- The same route switches from a copied kind to a deployment
            -- link; its materialized baseline must not leak:
            () <- writeManifestFile linkManifest manifestPath
            result' <- runAppWithoutLogging appEnv (apply True [])
            Right (Just state') <-
              readRepositoryState appEnv.stateDirectory repositoryId' machineId
            baselineRemains <- exists copyRecord.snapshotPath
            return
              ( result
              , baselineExisted
              , result'
              , (.fingerprint) <$> Map.elems state'.targetRecords
              , baselineRemains
              )
        result `shouldBe` ExitSuccess
        baselineExisted `shouldBe` True
        result' `shouldBe` ExitSuccess
        fingerprints `shouldBe` [SymlinkFingerprint source]
        baselineRemains `shouldBe` False

    it "deploys and repairs symbolic-link routes" $
      withSymlinkRoute $ \appEnv source destination -> do
        (result, isLink, target) <- dryRunIO $ do
          result <- runAppWithoutLogging appEnv (apply True [])
          isLink <- isSymlink destination
          target <- readSymlinkTarget destination
          return (result, isLink, target)
        result `shouldBe` ExitSuccess
        isLink `shouldBe` True
        target `shouldBe` source
        -- The deployed link is recorded in machine state with its stored
        -- target as the snapshot:
        records <- dryRunIO $ do
          _ <- runAppWithoutLogging appEnv (apply True [])
          Right (Just machineId) <- readMachineId appEnv.stateDirectory
          let Right repositoryId' =
                parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
          Right (Just state) <-
            readRepositoryState appEnv.stateDirectory repositoryId' machineId
          return $ Map.elems state.targetRecords
        [record.fingerprint | record <- records]
          `shouldBe` [SymlinkFingerprint source]
        -- A link with the wrong target is repaired even without force,
        -- since replacing a drifted link destroys no content:
        (result', target') <- dryRunIO $ do
          wrongName <- encodePath "wrong-target"
          () <- writeFile (appEnv.sourceDirectory </> wrongName) "wrong"
          () <-
            createSymbolicLink
              (appEnv.sourceDirectory </> wrongName)
              destination
              File
          result' <- runAppWithoutLogging appEnv (apply False [])
          target' <- readSymlinkTarget destination
          return (result', target')
        result' `shouldBe` ExitSuccess
        target' `shouldBe` source

    it "refuses to replace an entry with a link without force" $
      withSymlinkRoute $ \appEnv _ destination -> do
        dryRunIO
          ( do
              () <- writeFile destination "existing"
              runAppWithoutLogging appEnv (apply False [])
          )
          `shouldThrow` (== conflictError)
        -- With force, the entry is replaced by the link:
        (result', isLink) <- dryRunIO $ do
          () <- writeFile destination "existing"
          result' <- runAppWithoutLogging appEnv (apply True [])
          isLink <- isSymlink destination
          return (result', isLink)
        result' `shouldBe` ExitSuccess
        isLink `shouldBe` True

    it "agrees between dry-run and real mode application" $ do
      let observe appEnv destination = do
            result <- runAppWithoutLogging appEnv (apply True [])
            contents <- readFile destination
            mode <- getPortableMode destination
            return (result, contents, mode.writable)
      dryObserved <-
        withPrivateModeFile $ \appEnv destination ->
          dryRunIO $ observe appEnv destination
      realObserved <-
        withPrivateModeFile $ \appEnv destination ->
          observe appEnv destination
      dryObserved `shouldBe` realObserved
      dryObserved `shouldBe` (ExitSuccess, "private contents", True)

    symIt "agrees between dry-run and real link deployment" $ do
      let observe appEnv source destination = do
            result <- runAppWithoutLogging appEnv (apply True [])
            isLink <- isSymlink destination
            target <- readSymlinkTarget destination
            return (result, isLink, target == source)
      dryObserved <-
        withSymlinkRoute $ \appEnv source destination ->
          dryRunIO $ observe appEnv source destination
      realObserved <-
        withSymlinkRoute $ \appEnv source destination ->
          observe appEnv source destination
      dryObserved `shouldBe` realObserved
      dryObserved `shouldBe` (ExitSuccess, True, True)

    it "applies a declared private mode to recreated destinations" $
      withPrivateModeFile $ \appEnv destination -> do
        (result, contents, observedMode) <- dryRunIO $ do
          result <- runAppWithoutLogging appEnv (apply True [])
          contents <- readFile destination
          observedMode <- getPortableMode destination
          return (result, contents, observedMode)
        result `shouldBe` ExitSuccess
        contents `shouldBe` "private contents"
        observedMode `shouldBe` portableModeFromBits 0o600

    it "applies a declared directory mode to the route root" $
      withPrivateModeDirectory $ \appEnv destinationDir destinationFile -> do
        (result, dirMode, fileMode) <- dryRunIO $ do
          result <- runAppWithoutLogging appEnv (apply True [])
          dirMode <- getPortableMode destinationDir
          fileMode <- getPortableMode destinationFile
          return (result, dirMode, fileMode)
        result `shouldBe` ExitSuccess
        dirMode `shouldBe` portableModeFromBits 0o700
        fileMode `shouldBe` portableModeFromBits 0o600

    it "reconciles metadata-only drift toward the declared mode" $
      withPrivateModeFile $ \appEnv destination -> do
        (result, observedMode) <- dryRunIO $ do
          -- Converge contents first so only the mode drifts:
          () <- writeFile destination "private contents"
          result <- runAppWithoutLogging appEnv (apply True [])
          observedMode <- getPortableMode destination
          return (result, observedMode)
        result `shouldBe` ExitSuccess
        observedMode `shouldBe` expectedPrivateFileMode

    it "rejects traversing route names before mutating destinations" $
      withTempDir $ \tmpDir _ -> do
        repositoryName <- encodeFS "repository"
        manifestName <- encodeFS "dojang.toml"
        envName <- encodeFS "dojang-env.toml"
        stateName <- encodeFS "state"
        outsideName <- encodeFS "outside"
        destinationName <- encodeFS "destination"
        homeName <- encodeFS "home"
        let repository = tmpDir </> repositoryName
        let manifestPath = repository </> manifestName
        let outside = tmpDir </> outsideName
        let destination = tmpDir </> destinationName
        let home = tmpDir </> homeName
        let stateRoot = tmpDir </> stateName
        let appEnv =
              AppEnv
                repository
                False
                Nothing
                stateRoot
                manifestName
                envName
                False
                False
        createDirectories repository
        createDirectories home
        writeFile outside "outside source"
        writeFile destination "original destination"
        writeFile
          manifestPath
          "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n[dirs]\n[files]\n\"../outside\" = [{ when = \"always\", path = \"$DEST\" }]\n[ignores]\n[monikers]\n"
        withEnvVars
          [ ("DEST", Just destination)
          , ("HOME", Just home)
          , ("USERPROFILE", Just home)
          ]
          $ runAppWithoutLogging appEnv (apply True [])
            `shouldThrow` (== manifestReadError)
        readFile destination `shouldReturn` "original destination"
        exists stateRoot `shouldReturn` False


withTwoManagedFiles
  :: (AppEnv -> OsPath -> OsPath -> OsPath -> IO a)
  -> IO a
withTwoManagedFiles action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeA <- encodeFS "managed-a"
  routeB <- encodeFS "managed-b"
  destinationAName <- encodeFS "destination-a"
  destinationBName <- encodeFS "destination-b"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let intermediateA = repository </> intermediateDir </> routeA
  let intermediateB = repository </> intermediateDir </> routeB
  let sourceA = repository </> routeA
  let sourceB = repository </> routeB
  let destinationA = tmpDir </> destinationAName
  let destinationB = tmpDir </> destinationBName
  let home = tmpDir </> homeName
  let Right always = parseMonikerName "always"
  let Right repositoryId' =
        parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
  let manifest' =
        ( ( manifest
              (singleton always Always)
              ( Map.fromList
                  [ (routeA, [(always, Just $ Substitution "DEST_A")])
                  , (routeB, [(always, Just $ Substitution "DEST_B")])
                  ]
              )
              mempty
              mempty
              mempty
          )
            :: Manifest
        )
          { Manifest.repositoryId = Just repositoryId'
          }
  let appEnv =
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
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile sourceA "source a"
  writeFile intermediateA "base a"
  writeFile destinationA "base a"
  writeFile sourceB "base b"
  writeFile intermediateB "base b"
  writeFile destinationB "destination b"

  withEnvVars
    [ ("DEST_A", Just destinationA)
    , ("DEST_B", Just destinationB)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv sourceA destinationA destinationB


withDebugTargetedCodecFiles
  :: (AppEnv -> OsPath -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withDebugTargetedCodecFiles action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeA <- encodeFS "managed-a"
  routeB <- encodeFS "managed-b"
  destinationAName <- encodeFS "destination-a"
  destinationBName <- encodeFS "destination-b"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
      sourceA = repository </> routeA
      sourceB = repository </> routeB
      intermediateA = repository </> intermediateDir </> routeA
      intermediateB = repository </> intermediateDir </> routeB
      destinationA = tmpDir </> destinationAName
      destinationB = tmpDir </> destinationBName
      home = tmpDir </> homeName
      Just selectedName = parseCodecName "selected"
      Just unrelatedName = parseCodecName "unregistered"
      selectedSpec = CodecSpec selectedName $ CodecConfiguration mempty
      unrelatedSpec = CodecSpec unrelatedName $ CodecConfiguration mempty
      Right repositoryId' =
        parseRepositoryId "a33e4567-e89b-42d3-a456-426614174000"
      route destinationVariable codecSpec =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $
                RouteTarget
                  (Substitution destinationVariable)
                  DefaultMode
                  CopyRoute
                  codecSpec
            )
          ]
          File
      manifest' =
        ManifestWithCodecBackends
          { Manifest.repositoryId = Just repositoryId'
          , monikers = mempty
          , variables = mempty
          , fileRoutes =
              Map.fromList
                [ (routeA, route "DEST_A" selectedSpec)
                , (routeB, route "DEST_B" unrelatedSpec)
                ]
          , ignorePatterns = mempty
          , codecBackends = mempty
          , hooks = mempty
          }
      appEnv =
        AppEnv
          repository
          False
          (Just intermediateDir)
          (tmpDir </> stateDir)
          manifestFilename
          envFilename
          False
          True
  createDirectories $ repository </> intermediateDir
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile sourceA "source a"
  writeFile intermediateA "base a"
  writeFile destinationA "base a"
  writeFile sourceB "source b"
  writeFile intermediateB "base b"
  writeFile destinationB "destination b"
  withEnvVars
    [ ("DEST_A", Just destinationA)
    , ("DEST_B", Just destinationB)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv sourceA destinationA destinationB selectedSpec


countingApplyRuntime
  :: IORef Int -> CodecSpec -> CodecRuntime (App IO)
countingApplyRuntime resolutionCount spec' =
  CodecRuntime
    (codecRegistry [implementation])
    NormalEvaluation
    resolve
 where
  CodecSpec name _ = spec'
  counterInput = ExternalInputRequest "counter"
  implementation =
    CodecImplementation
      (CodecDefinition name "test-1" ReflectReject)
      ( const $
          Right $
            CodecRequirements noCodecInputs noCodecInputs [counterInput]
      )
      (Right . revealBytes . (.rawSource))
      Nothing
      PersistentCache
      EvaluatePurely
  resolve request
    | request == counterInput = do
        liftApp $ modifyIORef' resolutionCount (+ 1)
        return $ Right $ ExternalInput (opaqueBytes "") "stable"
    | otherwise = return $ Left "unexpected external input"


withSharedSourceCodecRoutes
  :: (AppEnv -> [CodecSpec] -> IO a)
  -> IO a
withSharedSourceCodecRoutes action = withTempDir $ \root _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestName <- encodeFS "dojang.toml"
  envName <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  treeRoute <- encodeFS "tree"
  childName <- encodeFS "child"
  childRoute <- encodeFS "tree/child"
  treeDestinationName <- encodeFS "tree-destination"
  childDestinationName <- encodeFS "child-destination"
  homeName <- encodeFS "home"
  let repository = root </> sourceDir
      sourceTree = repository </> treeRoute
      sourceChild = sourceTree </> childName
      treeDestination = root </> treeDestinationName
      childDestination = root </> childDestinationName
      home = root </> homeName
      Just treeCodecName = parseCodecName "tree-codec"
      Just childCodecName = parseCodecName "child-codec"
      treeCodec = CodecSpec treeCodecName $ CodecConfiguration mempty
      childCodec = CodecSpec childCodecName $ CodecConfiguration mempty
      Right repositoryId =
        parseRepositoryId "b33e4567-e89b-42d3-a456-426614174000"
      route destinationVariable codecSpec fileType =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $
                RouteTarget
                  (Substitution destinationVariable)
                  DefaultMode
                  CopyRoute
                  codecSpec
            )
          ]
          fileType
      manifest' =
        ManifestWithCodecBackends
          { Manifest.repositoryId = Just repositoryId
          , monikers = mempty
          , variables = mempty
          , fileRoutes =
              Map.fromList
                [ (treeRoute, route "TREE_DEST" treeCodec Directory)
                , (childRoute, route "CHILD_DEST" childCodec File)
                ]
          , ignorePatterns = mempty
          , codecBackends = mempty
          , hooks = mempty
          }
      appEnv =
        AppEnv
          repository
          False
          (Just intermediateDir)
          (root </> stateDir)
          manifestName
          envName
          False
          False
  createDirectories $ repository </> intermediateDir
  createDirectories sourceTree
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestName
  writeFile sourceChild "shared"
  withEnvVars
    [ ("TREE_DEST", Just treeDestination)
    , ("CHILD_DEST", Just childDestination)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv [treeCodec, childCodec]


sharedSourceRuntime
  :: CodecDryRunPolicy
  -> EvaluationMode
  -> [CodecSpec]
  -> CodecRuntime (App IO)
sharedSourceRuntime dryRunPolicy mode specs =
  CodecRuntime
    (codecRegistry $ implementation <$> specs)
    mode
    (const $ return $ Left "unexpected external input")
 where
  implementation spec' =
    let CodecSpec name _ = spec'
    in CodecImplementation
         (CodecDefinition name "test-1" ReflectReject)
         (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
         (Right . revealBytes . (.rawSource))
         Nothing
         PersistentCache
         dryRunPolicy


withCodecFile
  :: (AppEnv -> OsPath -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withCodecFile = withCodecFileWithVariables mempty


withCodecFileWithVariables
  :: ManifestVariableMap
  -> (AppEnv -> OsPath -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withCodecFileWithVariables manifestVariables =
  withCodecFileWithSpec manifestVariables testCodecSpec
 where
  Just codecName = parseCodecName "test-codec"
  testCodecSpec = CodecSpec codecName $ CodecConfiguration mempty


withTemplateFile
  :: ManifestVariableMap
  -> (AppEnv -> OsPath -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withTemplateFile manifestVariables =
  withCodecFileWithSpec manifestVariables templateCodecSpec


withCodecFileWithSpec
  :: ManifestVariableMap
  -> CodecSpec
  -> (AppEnv -> OsPath -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withCodecFileWithSpec = withCodecFileWithMode DefaultMode


withCodecFileWithMode
  :: RouteMode
  -> ManifestVariableMap
  -> CodecSpec
  -> (AppEnv -> OsPath -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withCodecFileWithMode routeMode manifestVariables codecSpec action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "encoded-file"
  destinationName <- encodeFS "destination"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
      source = repository </> routeName
      intermediate = repository </> intermediateDir </> routeName
      destination = tmpDir </> destinationName
      home = tmpDir </> homeName
      Right repositoryId' =
        parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      route =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $
                RouteTarget
                  (Substitution "DEST_CODEC")
                  routeMode
                  CopyRoute
                  codecSpec
            )
          ]
          File
      manifest' =
        ManifestWithCodecBackends
          { Manifest.repositoryId = Just repositoryId'
          , monikers = mempty
          , variables = manifestVariables
          , fileRoutes = Map.singleton routeName route
          , ignorePatterns = mempty
          , codecBackends = mempty
          , hooks = mempty
          }
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
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile source "raw"
  withEnvVars
    [ ("DEST_CODEC", Just destination)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv source intermediate destination codecSpec


captureStderr :: IO a -> IO (ByteString.ByteString, a)
captureStderr action =
  withSystemTempFile "dojang-apply-spec-stderr" $ \_ captureHandle ->
    bracket (hDuplicate stderr) restore $ \_ -> do
      hDuplicateTo captureHandle stderr
      result <- action
      hFlush stderr
      hSeek captureHandle AbsoluteSeek 0
      output <- ByteString.hGetContents captureHandle
      return (output, result)
 where
  restore originalHandle = do
    hDuplicateTo originalHandle stderr
    hClose originalHandle


testCodecRuntime
  :: CodecDryRunPolicy
  -> EvaluationMode
  -> CodecSpec
  -> CodecRuntime (App IO)
testCodecRuntime dryRunPolicy mode spec' =
  CodecRuntime
    (codecRegistry [implementation])
    mode
    (const $ return $ Left "unexpected external input")
 where
  CodecSpec name _ = spec'
  implementation =
    CodecImplementation
      (CodecDefinition name "test-1" ReflectReject)
      (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
      (\inputs -> Right $ revealBytes inputs.rawSource <> ":rendered")
      Nothing
      PersistentCache
      dryRunPolicy


withPrivateModeFile
  :: (AppEnv -> OsPath -> IO a)
  -> IO a
withPrivateModeFile action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "private-file"
  destinationName <- encodeFS "destination"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let source = repository </> routeName
  let intermediate = repository </> intermediateDir </> routeName
  let destination = tmpDir </> destinationName
  let home = tmpDir </> homeName
  let Right repositoryId' =
        parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
  let route =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $
                RouteTarget
                  (Substitution "DEST_PRIVATE")
                  Private
                  CopyRoute
                  identityCodecSpec
            )
          ]
          File
  let manifest' =
        ManifestWithCodecBackends
          { Manifest.repositoryId = Just repositoryId'
          , monikers = mempty
          , variables = mempty
          , fileRoutes = Map.fromList [(routeName, route)]
          , ignorePatterns = mempty
          , codecBackends = mempty
          , hooks = mempty
          }
  let appEnv =
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
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile source "private contents"
  writeFile intermediate "base"
  writeFile destination "base"

  withEnvVars
    [ ("DEST_PRIVATE", Just destination)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv destination


withKindSwitchRoute
  :: (AppEnv -> OsPath -> OsPath -> Manifest -> IO a)
  -> IO a
withKindSwitchRoute action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "switched-file"
  destinationName <- encodeFS "destination"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let source = repository </> routeName
  let destination = tmpDir </> destinationName
  let home = tmpDir </> homeName
  let Right repositoryId' =
        parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
  let routeOf kind' =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $
                RouteTarget
                  (Substitution "DEST_FILE")
                  DefaultMode
                  kind'
                  identityCodecSpec
            )
          ]
          File
  let manifestOf kind' =
        ManifestWithCodecBackends
          { Manifest.repositoryId = Just repositoryId'
          , monikers = mempty
          , variables = mempty
          , fileRoutes = Map.fromList [(routeName, routeOf kind')]
          , ignorePatterns = mempty
          , codecBackends = mempty
          , hooks = mempty
          }
  let appEnv =
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
  createDirectories home
  writeManifestFile (manifestOf CopyRoute) $ repository </> manifestFilename
  writeFile source "switched contents"

  withEnvVars
    [ ("DEST_FILE", Just destination)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action
      appEnv
      source
      (repository </> manifestFilename)
      (manifestOf SymlinkRoute)


withSymlinkRoute
  :: (AppEnv -> OsPath -> OsPath -> IO a)
  -> IO a
withSymlinkRoute action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "linked-file"
  destinationName <- encodeFS "destination"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let source = repository </> routeName
  let destination = tmpDir </> destinationName
  let home = tmpDir </> homeName
  let Right repositoryId' =
        parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
  let route =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $
                RouteTarget
                  (Substitution "DEST_LINK")
                  DefaultMode
                  SymlinkRoute
                  identityCodecSpec
            )
          ]
          File
  let manifest' =
        ManifestWithCodecBackends
          { Manifest.repositoryId = Just repositoryId'
          , monikers = mempty
          , variables = mempty
          , fileRoutes = Map.fromList [(routeName, route)]
          , ignorePatterns = mempty
          , codecBackends = mempty
          , hooks = mempty
          }
  let appEnv =
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
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile source "linked contents"

  withEnvVars
    [ ("DEST_LINK", Just destination)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv source destination


withPrivateModeDirectory
  :: (AppEnv -> OsPath -> OsPath -> IO a)
  -> IO a
withPrivateModeDirectory action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "private-dir"
  fileName' <- encodeFS "inner-file"
  destinationName <- encodeFS "destination-dir"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let source = repository </> routeName
  let destination = tmpDir </> destinationName
  let home = tmpDir </> homeName
  let Right repositoryId' =
        parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
  let route =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $
                RouteTarget
                  (Substitution "DEST_DIR")
                  Private
                  CopyRoute
                  identityCodecSpec
            )
          ]
          Directory
  let manifest' =
        ManifestWithCodecBackends
          { Manifest.repositoryId = Just repositoryId'
          , monikers = mempty
          , variables = mempty
          , fileRoutes = Map.fromList [(routeName, route)]
          , ignorePatterns = mempty
          , codecBackends = mempty
          , hooks = mempty
          }
  let appEnv =
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
  createDirectories source
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile (source </> fileName') "inner contents"

  withEnvVars
    [ ("DEST_DIR", Just destination)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv destination (destination </> fileName')


withTrackedIgnoredFile
  :: (AppEnv -> OsPath -> OsPath -> IO a)
  -> IO a
withTrackedIgnoredFile action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "managed-directory"
  filename <- encodeFS "tracked-file"
  destinationDir <- encodeFS "destination"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let sourceRoot = repository </> routeName
  let intermediateRoot = repository </> intermediateDir </> routeName
  let destinationRoot = tmpDir </> destinationDir
  let source = sourceRoot </> filename
  let intermediate = intermediateRoot </> filename
  let destination = destinationRoot </> filename
  let home = tmpDir </> homeName
  let Right always = parseMonikerName "always"
  let Right repositoryId' =
        parseRepositoryId "223e4567-e89b-42d3-a456-426614174000"
  let manifest' =
        ( ( manifest
              (singleton always Always)
              mempty
              ( Map.singleton
                  routeName
                  [(always, Just $ Substitution "DEST_DIR")]
              )
              (Map.singleton routeName ["*"])
              mempty
          )
            :: Manifest
        )
          { Manifest.repositoryId = Just repositoryId'
          }
  let appEnv =
        AppEnv
          repository
          False
          (Just intermediateDir)
          (tmpDir </> stateDir)
          manifestFilename
          envFilename
          False
          False

  createDirectories sourceRoot
  createDirectories intermediateRoot
  createDirectories destinationRoot
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile source "source contents"
  writeFile intermediate "base contents"
  writeFile destination "base contents"

  withEnvVars
    [ ("DEST_DIR", Just destinationRoot)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv intermediate destination

-- Windows cannot observe POSIX permission bits, so private files expose
-- only the owner-writable bit included in mode 0600.
#ifdef mingw32_HOST_OS
expectedPrivateFileMode :: PortableMode
expectedPrivateFileMode = PortableMode Nothing True
#else
expectedPrivateFileMode :: PortableMode
expectedPrivateFileMode = portableModeFromBits 0o600
#endif


withEnvVars :: [(String, Maybe OsPath)] -> IO a -> IO a
withEnvVars [] action = action
withEnvVars ((name, value) : rest) action = do
  oldValue <- lookupEnv name
  bracket_
    (setOrUnset value)
    (maybe (unsetEnv name) (setEnv name) oldValue)
    (withEnvVars rest action)
 where
  setOrUnset Nothing = unsetEnv name
  setOrUnset (Just value') = decodePath value' >>= setEnv name
#ifndef mingw32_HOST_OS
withRawEnvVar :: ByteString.ByteString -> ByteString.ByteString -> IO a -> IO a
withRawEnvVar name value action = do
  oldValue <- PosixEnv.getEnv name
  bracket_
    (PosixEnv.setEnv name value True)
    ( case oldValue of
        Nothing -> PosixEnv.unsetEnv name
        Just old -> PosixEnv.setEnv name old True
    )
    action
#else
withRawEnvVar :: ByteString.ByteString -> ByteString.ByteString -> IO a -> IO a
withRawEnvVar _ _ action = action
#endif
