{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.ReflectSpec (spec) where

import Control.Exception (bracket, bracket_)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.HashMap.Strict (singleton)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (SeekMode (AbsoluteSeek), hClose, hFlush, hSeek, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Info (os)
import System.OsPath (OsPath, encodeFS, takeDirectory, (</>))
import Test.Hspec (Spec, describe, it, pendingWith, runIO, sequential, xit)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldReturn
  , shouldSatisfy
  , shouldThrow
  )
import Prelude hiding (readFile, writeFile)

import Dojang.App (App, AppEnv (..), runAppWithoutLogging)
import Dojang.Commands.Apply (applyWithCodecRuntime)
import Dojang.Commands.Reflect (reflect, reflectWithCodecRuntime)
import Dojang.ExitCodes (codecError, conflictError, ignoredFileError)
import Dojang.MonadFileSystem
  ( FileType (Directory, File)
  , MonadFileSystem (..)
  )
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Codec
  ( CodecConfiguration (CodecConfiguration)
  , CodecDefinition (CodecDefinition)
  , CodecSpec (CodecSpec)
  , ReflectPolicy (ReflectIdentity, ReflectReAdd, ReflectReject)
  , identityCodecSpec
  , parseCodecName
  )
import Dojang.Types.Codec.Evaluate
  ( CacheScope (PersistentCache)
  , CodecDryRunPolicy (CachedOnly, EvaluatePurely)
  , CodecFailure (OpaqueCodecFailure)
  , CodecImplementation (CodecImplementation)
  , CodecInputs (..)
  , CodecRequirements (CodecRequirements)
  , CodecRuntime (CodecRuntime)
  , EvaluationMode (DryRunEvaluation, NormalEvaluation)
  , ExternalInput (..)
  , ExternalInputRequest (ExternalInputRequest)
  , codecImplementationWithSourceRequirements
  , codecRegistry
  , noCodecInputs
  , opaqueBytes
  , requiredCodecInputs
  , revealBytes
  )
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression
  ( FilePathExpression (BareComponent, Substitution)
  )
import Dojang.Types.FileRoute
  ( RouteKind (CopyRoute, SymlinkRoute)
  , RouteMode (DefaultMode)
  , RouteTarget (RouteTarget)
  , fileRoutePreservingOrder
  )
import Dojang.Types.MachineState
  ( MachineState (..)
  , encodeMachineState
  , readMachineId
  , readRepositoryState
  )
import Dojang.Types.ManagedTarget (ManagedTarget (..))
import Dojang.Types.Manifest (Manifest (..), manifest)
import qualified Dojang.Types.Manifest as Manifest
import Dojang.Types.ManifestVariable
  ( ManifestVariableMap
  , manifestVariable
  , parseManifestVariableName
  )
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.RepositoryId (parseRepositoryId)


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
  describe "reflect" $ do
    it "reflects a destination-side deletion" $
      withDeletedDestination $
        \appEnv source intermediate _ -> do
          runAppWithoutLogging appEnv (reflect False True False Nothing [])
            `shouldReturn` ExitSuccess
          exists source `shouldReturn` False
          exists intermediate `shouldReturn` False

    it "reflects an explicitly named deleted destination" $
      withDeletedDestination $ \appEnv source intermediate destination -> do
        runAppWithoutLogging
          appEnv
          (reflect False True False Nothing [destination])
          `shouldReturn` ExitSuccess
        exists source `shouldReturn` False
        exists intermediate `shouldReturn` False

    it "deduplicates repeated destination selections" $
      withManagedFile "managed" "managed" (Just "changed") $
        \appEnv source intermediate destination -> do
          runAppWithoutLogging
            appEnv
            (reflect False True False Nothing [destination, destination])
            `shouldReturn` ExitSuccess
          readFile source `shouldReturn` "changed"
          readFile intermediate `shouldReturn` "changed"

    it "deduplicates a file selected directly and through its directory" $
      withManagedFile "managed" "managed" (Just "changed") $
        \appEnv source intermediate destination -> do
          runAppWithoutLogging
            appEnv
            ( reflect
                False
                True
                False
                Nothing
                [takeDirectory destination, destination]
            )
            `shouldReturn` ExitSuccess
          readFile source `shouldReturn` "changed"
          readFile intermediate `shouldReturn` "changed"

    it "never reflects a deployment link" $
      withSymlinkRouteDestinationFile $
        \appEnv source destination -> do
          runAppWithoutLogging appEnv (reflect False True False Nothing [])
            `shouldReturn` ExitSuccess
          readFile source `shouldReturn` "source contents"
          -- Even a forced, explicitly named destination is refused:
          runAppWithoutLogging
            appEnv
            (reflect True True False Nothing [destination])
            `shouldReturn` ExitSuccess
          readFile source `shouldReturn` "source contents"
          readFile destination `shouldReturn` "destination contents"

    it "accepts a source-only change" $
      withSourceOnlyChange $ \appEnv source intermediate destination -> do
        runAppWithoutLogging appEnv (reflect False True False Nothing [])
          `shouldReturn` ExitSuccess
        readFile source `shouldReturn` "managed contents"
        readFile intermediate `shouldReturn` "managed contents"
        readFile destination `shouldReturn` "managed contents"

    it "reflects identity routes with a custom-only runtime" $
      withManagedFile "managed" "managed" (Just "changed") $
        \appEnv source intermediate destination -> do
          let runtime =
                CodecRuntime
                  mempty
                  NormalEvaluation
                  (const $ return $ Left "unexpected external input")
          runAppWithoutLogging
            appEnv
            ( reflectWithCodecRuntime
                runtime
                False
                True
                False
                Nothing
                [destination]
            )
            `shouldReturn` ExitSuccess
          readFile source `shouldReturn` "changed"
          readFile intermediate `shouldReturn` "changed"

    it "records already converged files on a no-op reflection" $
      withManagedFile "managed" "managed" (Just "managed") $
        \appEnv _ _ _ -> do
          runAppWithoutLogging appEnv (reflect False True False Nothing [])
            `shouldReturn` ExitSuccess
          let AppEnv _ _ _ stateDirectory' _ _ _ _ = appEnv
          Right (Just machineId) <- readMachineId stateDirectory'
          let Right repositoryId =
                parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
          Right (Just state) <-
            readRepositoryState stateDirectory' repositoryId machineId
          encodeMachineState state `shouldSatisfy` Text.isInfixOf "[targets."

    it "records converged files alongside changed files in a full reflection" $
      withTwoManagedFiles $ \appEnv convergedDestination -> do
        runAppWithoutLogging appEnv (reflect False True False Nothing [])
          `shouldReturn` ExitSuccess
        let AppEnv _ _ _ stateDirectory' _ _ _ _ = appEnv
        Right (Just machineId) <- readMachineId stateDirectory'
        let Right repositoryId =
              parseRepositoryId "323e4567-e89b-42d3-a456-426614174000"
        Right (Just state) <-
          readRepositoryState stateDirectory' repositoryId machineId
        Map.size state.targetRecords `shouldBe` 2
        any
          ((== convergedDestination) . (.destinationPath))
          (Map.elems state.targetRecords)
          `shouldBe` True

    it "cleans the baseline after reflecting a managed deletion" $
      withManagedFile "managed" "managed" (Just "managed") $
        \appEnv _ _ destination -> do
          runAppWithoutLogging appEnv (reflect False True False Nothing [])
            `shouldReturn` ExitSuccess
          let AppEnv _ _ _ stateDirectory' _ _ _ _ = appEnv
          Right (Just machineId) <- readMachineId stateDirectory'
          let Right repositoryId =
                parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
          Right (Just before) <-
            readRepositoryState stateDirectory' repositoryId machineId
          target <- case Map.elems before.targetRecords of
            [one] -> return one
            records -> fail $ "Unexpected managed targets: " <> show records
          removeFile destination
          runAppWithoutLogging appEnv (reflect False True False Nothing [])
            `shouldReturn` ExitSuccess
          Right (Just after) <-
            readRepositoryState stateDirectory' repositoryId machineId
          after.targetRecords `shouldBe` Map.empty
          exists target.snapshotPath `shouldReturn` False

    it "reflects an explicitly forced ignored file" $
      withIgnoredDestination $ \appEnv source intermediate destination -> do
        runAppWithoutLogging
          appEnv
          (reflect False True False Nothing [destination])
          `shouldThrow` (== ignoredFileError)
        runAppWithoutLogging
          appEnv
          (reflect True True False Nothing [destination])
          `shouldReturn` ExitSuccess
        readFile source `shouldReturn` "ignored contents"
        readFile intermediate `shouldReturn` "ignored contents"
        let AppEnv _ _ _ stateDirectory' _ _ _ _ = appEnv
        Right (Just machineId) <- readMachineId stateDirectory'
        let Right repositoryId =
              parseRepositoryId "223e4567-e89b-42d3-a456-426614174000"
        Right (Just state) <-
          readRepositoryState stateDirectory' repositoryId machineId
        case Map.elems state.targetRecords of
          [target] -> target.destinationPath `shouldBe` destination
          records -> fail $ "Unexpected managed targets: " <> show records

    it "evaluates a forced ignored codec before selecting changes" $
      withConvergedIgnoredCodecFile $ \appEnv runtime -> do
        (output, result) <-
          captureStderr $
            runAppWithoutLogging
              appEnv
              ( reflectWithCodecRuntime
                  runtime
                  True
                  True
                  False
                  Nothing
                  []
              )
        result `shouldBe` ExitSuccess
        output
          `shouldSatisfy` ByteString.isInfixOf "No changed files to reflect."

    it "reuses codec evaluation across Windows path casing" $
      if os /= "mingw32"
        then pendingWith "Windows paths use case-insensitive native identity."
        else withCaseVariantRotatingReAddFile $
          \appEnv source selectedDestination codecSpec -> do
            resolutionCount <- newIORef (0 :: Int)
            runAppWithoutLogging
              appEnv
              ( reflectWithCodecRuntime
                  (rotatingReAddRuntime resolutionCount codecSpec)
                  False
                  True
                  False
                  Nothing
                  [selectedDestination]
              )
              `shouldReturn` ExitSuccess
            readFile source `shouldReturn` "new"
            readIORef resolutionCount `shouldReturn` 1

    it "records only the explicitly reflected overlapping route" $
      withOverlappingManagedFiles $ \appEnv selectedDestination -> do
        runAppWithoutLogging
          appEnv
          (reflect False True False Nothing [selectedDestination])
          `shouldReturn` ExitSuccess
        let AppEnv _ _ _ stateDirectory' _ _ _ _ = appEnv
        Right (Just machineId) <- readMachineId stateDirectory'
        let Right repositoryId =
              parseRepositoryId "423e4567-e89b-42d3-a456-426614174000"
        Right (Just state) <-
          readRepositoryState stateDirectory' repositoryId machineId
        case Map.elems state.targetRecords of
          [target] -> target.destinationPath `shouldBe` selectedDestination
          records -> fail $ "Unexpected managed targets: " <> show records

    it "records an explicit route whose source path needs normalization" $
      withNoncanonicalManagedFile $ \appEnv destination -> do
        runAppWithoutLogging
          appEnv
          (reflect False True False Nothing [destination])
          `shouldReturn` ExitSuccess
        let AppEnv _ _ _ stateDirectory' _ _ _ _ = appEnv
        Right (Just machineId) <- readMachineId stateDirectory'
        let Right repositoryId =
              parseRepositoryId "523e4567-e89b-42d3-a456-426614174000"
        Right (Just state) <-
          readRepositoryState stateDirectory' repositoryId machineId
        case Map.elems state.targetRecords of
          [target] -> target.destinationPath `shouldBe` destination
          records -> fail $ "Unexpected managed targets: " <> show records

    it "reflects the selected child of a directory route" $
      withDirectoryChildren $ \appEnv sourceA sourceB destinationB -> do
        runAppWithoutLogging
          appEnv
          (reflect False True False Nothing [destinationB])
          `shouldReturn` ExitSuccess
        readFile sourceA `shouldReturn` "first"
        readFile sourceB `shouldReturn` "changed second"

    it "cleans the baseline after deleting a directory-route child" $
      withDirectoryChildren $ \appEnv _ _ destinationB -> do
        runAppWithoutLogging
          appEnv
          (reflect False True False Nothing [destinationB])
          `shouldReturn` ExitSuccess
        let AppEnv _ _ _ stateDirectory' _ _ _ _ = appEnv
        Right (Just machineId) <- readMachineId stateDirectory'
        let Right repositoryId =
              parseRepositoryId "623e4567-e89b-42d3-a456-426614174000"
        Right (Just before) <-
          readRepositoryState stateDirectory' repositoryId machineId
        target <- case Map.elems before.targetRecords of
          [one] -> return one
          records -> fail $ "Unexpected managed targets: " <> show records
        removeFile destinationB
        runAppWithoutLogging
          appEnv
          (reflect False True False Nothing [destinationB])
          `shouldReturn` ExitSuccess
        Right (Just after) <-
          readRepositoryState stateDirectory' repositoryId machineId
        after.targetRecords `shouldBe` Map.empty
        exists target.snapshotPath `shouldReturn` False

    it "preserves codec policy for an ownerless ignored destination" $
      withOwnerlessIgnoredRejectDestination $
        \appEnv source destination codecSpec -> do
          runAppWithoutLogging
            appEnv
            ( reflectWithCodecRuntime
                (rejectRuntime codecSpec)
                True
                True
                False
                Nothing
                [destination]
            )
            `shouldThrow` (== codecError)
          exists source `shouldReturn` False

    it "compares a changed destination with newly rendered source bytes" $
      withRenderedEquivalentChanges $
        \appEnv source intermediate codecSpec -> do
          runAppWithoutLogging
            appEnv
            ( reflectWithCodecRuntime
                (reAddRuntime codecSpec)
                False
                True
                False
                Nothing
                []
            )
            `shouldReturn` ExitSuccess
          readFile source `shouldReturn` "new"
          readFile intermediate `shouldReturn` "new:rendered"

    it "reuses directory-route codec evaluations" $
      withChangedRejectDirectory $ \appEnv destinationRoot codecSpec -> do
        count <- newIORef (0 :: Int)
        runAppWithoutLogging
          appEnv
          ( reflectWithCodecRuntime
              (countingRejectRuntime count codecSpec)
              False
              True
              False
              Nothing
              [destinationRoot]
          )
          `shouldThrow` (== codecError)
        readIORef count `shouldReturn` 1

    it "rejects a directory destination before re-add reflection" $
      withReAddDirectoryDestination $ \appEnv source destination codecSpec -> do
        runAppWithoutLogging
          appEnv
          ( reflectWithCodecRuntime
              (reAddRuntime codecSpec)
              True
              True
              False
              Nothing
              [destination]
          )
          `shouldThrow` (== codecError)
        readFile source `shouldReturn` "raw"

    it "rejects a directory before identity reflection mutates the source" $
      withIdentityCodecDirectoryDestination $
        \appEnv source destination codecSpec -> do
          runAppWithoutLogging
            appEnv
            ( reflectWithCodecRuntime
                (identityReflectRuntime codecSpec)
                True
                True
                False
                Nothing
                [destination]
            )
            `shouldThrow` (== codecError)
          readFile source `shouldReturn` "raw"

    symIt "rejects a symbolic-link destination for a codec-backed file" $
      withIdentityCodecSymlinkDestination $
        \appEnv source destination codecSpec -> do
          runAppWithoutLogging
            appEnv
            ( reflectWithCodecRuntime
                (identityReflectRuntime codecSpec)
                True
                True
                False
                Nothing
                [destination]
            )
            `shouldThrow` (== codecError)
          readFile source `shouldReturn` "raw"

    it "reuses a persisted codec cache in dry-run reflection" $
      withReAddFile $ \appEnv _ _ codecSpec -> do
        runAppWithoutLogging
          appEnv
          (applyWithCodecRuntime (reAddRuntime codecSpec) True [])
          `shouldReturn` ExitSuccess
        runAppWithoutLogging
          appEnv{dryRun = True}
          ( reflectWithCodecRuntime
              (reAddRuntimeWith CachedOnly DryRunEvaluation codecSpec)
              False
              True
              False
              Nothing
              []
          )
          `shouldReturn` ExitSuccess

    it "evaluates an unchanged codec route once per reflection command" $
      withConvergedReAddFile $ \appEnv codecSpec -> do
        count <- newIORef (0 :: Int)
        runAppWithoutLogging
          appEnv
          ( reflectWithCodecRuntime
              (countingReAddRuntime count codecSpec)
              False
              True
              False
              Nothing
              []
          )
          `shouldReturn` ExitSuccess
        readIORef count `shouldReturn` 1

    it "reuses evaluated external inputs for re-add reflection" $
      withRotatingReAddFile "B:new" $ \appEnv source destination codecSpec -> do
        resolutionCount <- newIORef (0 :: Int)
        runAppWithoutLogging
          appEnv
          ( reflectWithCodecRuntime
              (rotatingReAddRuntime resolutionCount codecSpec)
              False
              True
              False
              Nothing
              [destination]
          )
          `shouldThrow` (== codecError)
        readFile source `shouldReturn` "raw"
        readIORef resolutionCount `shouldReturn` 1

    it "keeps re-add inputs stable through convergence tracking" $
      withRotatingReAddFile "A:new" $ \appEnv source destination codecSpec -> do
        resolutionCount <- newIORef (0 :: Int)
        runAppWithoutLogging
          appEnv
          ( reflectWithCodecRuntime
              (rotatingReAddRuntime resolutionCount codecSpec)
              False
              True
              False
              Nothing
              [destination]
          )
          `shouldReturn` ExitSuccess
        readFile source `shouldReturn` "new"
        readIORef resolutionCount `shouldReturn` 1

    it "retains re-add inputs when reflection creates a missing source" $
      withMissingRotatingReAddFile $
        \appEnv source destination codecSpec -> do
          resolutionCount <- newIORef (0 :: Int)
          runAppWithoutLogging
            appEnv
            ( reflectWithCodecRuntime
                (rotatingReAddRuntime resolutionCount codecSpec)
                False
                True
                False
                Nothing
                [destination]
            )
            `shouldReturn` ExitSuccess
          readFile source `shouldReturn` "new"
          readIORef resolutionCount `shouldReturn` 1

    it "analyzes a reconstructed source after missing-source re-add" $
      withMissingRotatingReAddFile $
        \appEnv source destination codecSpec -> do
          runAppWithoutLogging
            appEnv
            ( reflectWithCodecRuntime
                (sourceAwareReAddRuntime codecSpec)
                False
                True
                False
                Nothing
                [destination]
            )
            `shouldReturn` ExitSuccess
          readFile source `shouldReturn` "new"

    it "resolves variables discovered after missing-source re-add" $ do
      let Right variableName = parseManifestVariableName "NAME"
          variables =
            Map.singleton variableName $
              manifestVariable mempty $
                BareComponent "Ada"
      withMissingReAddFileWithVariables variables $
        \appEnv source destination codecSpec -> do
          runAppWithoutLogging
            appEnv
            ( reflectWithCodecRuntime
                (sourceVariableReAddRuntime codecSpec)
                False
                True
                False
                Nothing
                [destination]
            )
            `shouldReturn` ExitSuccess
          readFile source `shouldReturn` "new"

    it "rejects ambiguous reflection of routes sharing a source" $
      withSharedSourceReAddRoutes $
        \appEnv source destinations codecSpec -> do
          runAppWithoutLogging
            appEnv
            ( reflectWithCodecRuntime
                (reAddRuntime codecSpec)
                False
                True
                False
                Nothing
                destinations
            )
            `shouldThrow` (== conflictError)
          readFile source `shouldReturn` "raw"


withSymlinkRouteDestinationFile
  :: (AppEnv -> OsPath -> OsPath -> IO a) -> IO a
withSymlinkRouteDestinationFile action = withTempDir $ \tmpDir _ -> do
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
        Manifest
          { Manifest.repositoryId = Just repositoryId'
          , monikers = mempty
          , variables = mempty
          , fileRoutes = Map.fromList [(routeName, route)]
          , ignorePatterns = mempty
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
  writeFile source "source contents"
  writeFile destination "destination contents"

  withEnvVar "DEST_LINK" (Just destination) $
    withEnvVar "HOME" (Just home) $
      withEnvVar "USERPROFILE" (Just home) $
        action appEnv source destination


withDeletedDestination :: (AppEnv -> OsPath -> OsPath -> OsPath -> IO a) -> IO a
withDeletedDestination =
  withManagedFile "managed contents" "managed contents" Nothing


withSourceOnlyChange :: (AppEnv -> OsPath -> OsPath -> OsPath -> IO a) -> IO a
withSourceOnlyChange =
  withManagedFile
    "source contents"
    "managed contents"
    (Just "managed contents")


withManagedFile
  :: ByteString
  -> ByteString
  -> Maybe ByteString
  -> (AppEnv -> OsPath -> OsPath -> OsPath -> IO a)
  -> IO a
withManagedFile sourceContents intermediateContents destinationContents action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "managed-file"
    destination <- encodeFS "destination"
    let intermediate = repository </> intermediateDir </> routeName
    let source = repository </> routeName
    let destinationPath = tmpDir </> destination
    let Right always = parseMonikerName "always"
    let Right repositoryId' =
          parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
    let manifest' =
          ( manifest
              (singleton always Always)
              (Map.singleton routeName [(always, Just $ Substitution "DEST")])
              mempty
              mempty
              mempty
          )
            { Manifest.repositoryId = Just repositoryId'
            }

    writeManifestFile manifest' manifestPath
    writeFile source sourceContents
    writeFile intermediate intermediateContents
    maybe (return ()) (writeFile destinationPath) destinationContents

    withEnvVar "DEST" (Just destinationPath) $
      action appEnv source intermediate destinationPath


withTwoManagedFiles :: (AppEnv -> OsPath -> IO a) -> IO a
withTwoManagedFiles action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    changedRoute <- encodeFS "changed-file"
    convergedRoute <- encodeFS "converged-file"
    changedDestinationName <- encodeFS "changed-destination"
    convergedDestinationName <- encodeFS "converged-destination"
    let changedSource = repository </> changedRoute
    let convergedSource = repository </> convergedRoute
    let changedIntermediate = repository </> intermediateDir </> changedRoute
    let convergedIntermediate = repository </> intermediateDir </> convergedRoute
    let changedDestination = tmpDir </> changedDestinationName
    let convergedDestination = tmpDir </> convergedDestinationName
    let Right always = parseMonikerName "always"
    let Right repositoryId =
          parseRepositoryId "323e4567-e89b-42d3-a456-426614174000"
    let manifest' =
          ( manifest
              (singleton always Always)
              ( Map.fromList
                  [ (changedRoute, [(always, Just $ Substitution "CHANGED_DEST")])
                  ,
                    ( convergedRoute
                    , [(always, Just $ Substitution "CONVERGED_DEST")]
                    )
                  ]
              )
              mempty
              mempty
              mempty
          )
            { Manifest.repositoryId = Just repositoryId
            }
    writeManifestFile manifest' manifestPath
    writeFile changedSource "managed"
    writeFile changedIntermediate "managed"
    writeFile changedDestination "changed"
    writeFile convergedSource "converged"
    writeFile convergedIntermediate "converged"
    writeFile convergedDestination "converged"
    withEnvVar "CHANGED_DEST" (Just changedDestination) $
      withEnvVar "CONVERGED_DEST" (Just convergedDestination) $
        action appEnv convergedDestination


withIgnoredDestination
  :: (AppEnv -> OsPath -> OsPath -> OsPath -> IO a)
  -> IO a
withIgnoredDestination action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "managed-directory"
    filename <- encodeFS "ignored-file"
    destinationDir <- encodeFS "destination"
    let intermediate = repository </> intermediateDir </> routeName </> filename
    let source = repository </> routeName </> filename
    let destinationRoot = tmpDir </> destinationDir
    let destination = destinationRoot </> filename
    let Right always = parseMonikerName "always"
    let Right repositoryId' =
          parseRepositoryId "223e4567-e89b-42d3-a456-426614174000"
    let manifest' =
          ( manifest
              (singleton always Always)
              mempty
              ( Map.singleton
                  routeName
                  [(always, Just $ Substitution "DEST_DIR")]
              )
              (Map.singleton routeName ["*"])
              mempty
          )
            { Manifest.repositoryId = Just repositoryId'
            }

    createDirectories destinationRoot
    writeManifestFile manifest' manifestPath
    writeFile destination "ignored contents"

    withEnvVar "DEST_DIR" (Just destinationRoot) $
      action appEnv source intermediate destination


withConvergedIgnoredCodecFile
  :: (AppEnv -> CodecRuntime (App IO) -> IO a)
  -> IO a
withConvergedIgnoredCodecFile action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "tree"
    filename <- encodeFS "ignored-file"
    destinationName <- encodeFS "destination"
    let sourceRoot = repository </> routeName
        source = sourceRoot </> filename
        intermediateRoot = repository </> intermediateDir </> routeName
        intermediate = intermediateRoot </> filename
        destinationRoot = tmpDir </> destinationName
        destination = destinationRoot </> filename
        Just codecName = parseCodecName "ignored-codec"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "323e4567-e89b-42d3-a456-426614174001"
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [
              ( Always
              , Just $
                  RouteTarget
                    (Substitution "DEST_DIR")
                    DefaultMode
                    CopyRoute
                    codecSpec
              )
            ]
            Directory
        manifest' =
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = mempty
            , fileRoutes = Map.singleton routeName route
            , ignorePatterns = Map.singleton routeName ["*"]
            , hooks = mempty
            }
        implementation =
          CodecImplementation
            (CodecDefinition codecName "test-1" ReflectReject)
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
    createDirectories sourceRoot
    createDirectories intermediateRoot
    createDirectories destinationRoot
    writeManifestFile manifest' manifestPath
    writeFile source "raw"
    writeFile intermediate "raw:rendered"
    writeFile destination "raw:rendered"
    withEnvVar "DEST_DIR" (Just destinationRoot) $
      action appEnv runtime


withOverlappingManagedFiles :: (AppEnv -> OsPath -> IO a) -> IO a
withOverlappingManagedFiles action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    directoryRoute <- encodeFS "tree"
    fileName <- encodeFS "file"
    fileRoute <- encodeFS "tree/file"
    directoryDestinationName <- encodeFS "directory-destination"
    fileDestinationName <- encodeFS "file-destination"
    let source = repository </> directoryRoute </> fileName
    let intermediate = repository </> intermediateDir </> fileRoute
    let directoryDestination = tmpDir </> directoryDestinationName
    let overlappingDestination = directoryDestination </> fileName
    let selectedDestination = tmpDir </> fileDestinationName
    let Right always = parseMonikerName "always"
    let Right repositoryId =
          parseRepositoryId "423e4567-e89b-42d3-a456-426614174000"
    let manifest' =
          ( manifest
              (singleton always Always)
              ( Map.singleton
                  fileRoute
                  [(always, Just $ Substitution "FILE_DEST")]
              )
              ( Map.singleton
                  directoryRoute
                  [(always, Just $ Substitution "DIRECTORY_DEST")]
              )
              mempty
              mempty
          )
            { Manifest.repositoryId = Just repositoryId
            }
    createDirectories directoryDestination
    createDirectories $ repository </> directoryRoute
    createDirectories $ repository </> intermediateDir </> directoryRoute
    writeManifestFile manifest' manifestPath
    writeFile source "managed"
    writeFile intermediate "managed"
    writeFile overlappingDestination "managed"
    writeFile selectedDestination "managed"
    withEnvVar "DIRECTORY_DEST" (Just directoryDestination) $
      withEnvVar "FILE_DEST" (Just selectedDestination) $
        action appEnv selectedDestination


withNoncanonicalManagedFile :: (AppEnv -> OsPath -> IO a) -> IO a
withNoncanonicalManagedFile action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "managed-file"
    destinationName <- encodeFS "destination"
    let source = repository </> routeName
    let intermediate = repository </> intermediateDir </> routeName
    let destination = tmpDir </> destinationName
    writeFile
      manifestPath
      "repository-id = \"523e4567-e89b-42d3-a456-426614174000\"\n[dirs]\n[files]\n\"./managed-file\" = [{ when = \"always\", path = \"$DEST\" }]\n[ignores]\n[monikers]\n"
    writeFile source "managed"
    writeFile intermediate "managed"
    writeFile destination "managed"
    withEnvVar "DEST" (Just destination) $ action appEnv destination


withDirectoryChildren
  :: (AppEnv -> OsPath -> OsPath -> OsPath -> IO a)
  -> IO a
withDirectoryChildren action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "tree"
    firstName <- encodeFS "first"
    secondName <- encodeFS "second"
    destinationName <- encodeFS "destination"
    let sourceRoot = repository </> routeName
        sourceA = sourceRoot </> firstName
        sourceB = sourceRoot </> secondName
        intermediateRoot = repository </> intermediateDir </> routeName
        destinationRoot = tmpDir </> destinationName
        destinationA = destinationRoot </> firstName
        destinationB = destinationRoot </> secondName
        Right always = parseMonikerName "always"
        Right repositoryId =
          parseRepositoryId "623e4567-e89b-42d3-a456-426614174000"
        manifest' =
          ( manifest
              (singleton always Always)
              mempty
              (Map.singleton routeName [(always, Just $ Substitution "DEST_DIR")])
              mempty
              mempty
          )
            { Manifest.repositoryId = Just repositoryId
            }
    createDirectories sourceRoot
    createDirectories intermediateRoot
    createDirectories destinationRoot
    writeManifestFile manifest' manifestPath
    writeFile sourceA "first"
    writeFile sourceB "second"
    writeFile (intermediateRoot </> firstName) "first"
    writeFile (intermediateRoot </> secondName) "second"
    writeFile destinationA "first"
    writeFile destinationB "changed second"
    withEnvVar "DEST_DIR" (Just destinationRoot) $
      action appEnv sourceA sourceB destinationB


withOwnerlessIgnoredRejectDestination
  :: (AppEnv -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withOwnerlessIgnoredRejectDestination action =
  withReflectRepository $ \tmpDir repository _ manifestPath appEnv -> do
    routeName <- encodeFS "tree"
    filename <- encodeFS "ignored-file"
    destinationName <- encodeFS "destination"
    let source = repository </> routeName </> filename
        destinationRoot = tmpDir </> destinationName
        destination = destinationRoot </> filename
        Just codecName = parseCodecName "reject"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "b23e4567-e89b-42d3-a456-426614174000"
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [
              ( Always
              , Just $
                  RouteTarget
                    (Substitution "DEST_DIR")
                    DefaultMode
                    CopyRoute
                    codecSpec
              )
            ]
            Directory
        manifest' =
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = mempty
            , fileRoutes = Map.singleton routeName route
            , ignorePatterns = Map.singleton routeName ["*"]
            , hooks = mempty
            }
    createDirectories destinationRoot
    writeManifestFile manifest' manifestPath
    writeFile destination "deployed"
    withEnvVar "DEST_DIR" (Just destinationRoot) $
      action appEnv source destination codecSpec


withRenderedEquivalentChanges
  :: (AppEnv -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withRenderedEquivalentChanges action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "encoded-file"
    destinationName <- encodeFS "destination"
    let source = repository </> routeName
        intermediate = repository </> intermediateDir </> routeName
        destination = tmpDir </> destinationName
        Just codecName = parseCodecName "re-add"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "c23e4567-e89b-42d3-a456-426614174000"
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [
              ( Always
              , Just $
                  RouteTarget
                    (Substitution "DEST")
                    DefaultMode
                    CopyRoute
                    codecSpec
              )
            ]
            File
        manifest' =
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = mempty
            , fileRoutes = Map.singleton routeName route
            , ignorePatterns = mempty
            , hooks = mempty
            }
    writeManifestFile manifest' manifestPath
    writeFile source "new"
    writeFile intermediate "old:rendered"
    writeFile destination "new:rendered"
    withEnvVar "DEST" (Just destination) $
      action appEnv source intermediate codecSpec


withChangedRejectDirectory
  :: (AppEnv -> OsPath -> CodecSpec -> IO a)
  -> IO a
withChangedRejectDirectory action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "tree"
    filename <- encodeFS "file"
    destinationName <- encodeFS "destination"
    let sourceRoot = repository </> routeName
        intermediateRoot = repository </> intermediateDir </> routeName
        destinationRoot = tmpDir </> destinationName
        Just codecName = parseCodecName "reject"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "d23e4567-e89b-42d3-a456-426614174000"
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [
              ( Always
              , Just $
                  RouteTarget
                    (Substitution "DEST_DIR")
                    DefaultMode
                    CopyRoute
                    codecSpec
              )
            ]
            Directory
        manifest' =
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = mempty
            , fileRoutes = Map.singleton routeName route
            , ignorePatterns = mempty
            , hooks = mempty
            }
    createDirectories sourceRoot
    createDirectories intermediateRoot
    createDirectories destinationRoot
    writeManifestFile manifest' manifestPath
    writeFile (sourceRoot </> filename) "raw"
    writeFile (intermediateRoot </> filename) "raw:rendered"
    writeFile (destinationRoot </> filename) "changed"
    withEnvVar "DEST_DIR" (Just destinationRoot) $
      action appEnv destinationRoot codecSpec


withReAddDirectoryDestination
  :: (AppEnv -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withReAddDirectoryDestination action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "encoded-file"
    destinationName <- encodeFS "destination"
    let source = repository </> routeName
        intermediate = repository </> intermediateDir </> routeName
        destination = tmpDir </> destinationName
        Just codecName = parseCodecName "re-add"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "723e4567-e89b-42d3-a456-426614174000"
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [
              ( Always
              , Just $
                  RouteTarget
                    (Substitution "DEST")
                    DefaultMode
                    CopyRoute
                    codecSpec
              )
            ]
            File
        manifest' =
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = mempty
            , fileRoutes = Map.singleton routeName route
            , ignorePatterns = mempty
            , hooks = mempty
            }
    writeManifestFile manifest' manifestPath
    writeFile source "raw"
    writeFile intermediate "raw:rendered"
    createDirectories destination
    withEnvVar "DEST" (Just destination) $
      action appEnv source destination codecSpec


withIdentityCodecDirectoryDestination
  :: (AppEnv -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withIdentityCodecDirectoryDestination action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "encoded-file"
    destinationName <- encodeFS "destination"
    let source = repository </> routeName
        intermediate = repository </> intermediateDir </> routeName
        destination = tmpDir </> destinationName
        Just codecName = parseCodecName "identity-reflect"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "e23e4567-e89b-42d3-a456-426614174000"
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [
              ( Always
              , Just $
                  RouteTarget
                    (Substitution "DEST")
                    DefaultMode
                    CopyRoute
                    codecSpec
              )
            ]
            File
        manifest' =
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = mempty
            , fileRoutes = Map.singleton routeName route
            , ignorePatterns = mempty
            , hooks = mempty
            }
    writeManifestFile manifest' manifestPath
    writeFile source "raw"
    writeFile intermediate "raw:rendered"
    createDirectories destination
    withEnvVar "DEST" (Just destination) $
      action appEnv source destination codecSpec


withIdentityCodecSymlinkDestination
  :: (AppEnv -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withIdentityCodecSymlinkDestination action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "encoded-file"
    destinationName <- encodeFS "destination"
    linkTargetName <- encodeFS "link-target"
    let source = repository </> routeName
        intermediate = repository </> intermediateDir </> routeName
        destination = tmpDir </> destinationName
        linkTarget = tmpDir </> linkTargetName
        Just codecName = parseCodecName "identity-reflect"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "f33e4567-e89b-42d3-a456-426614174000"
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [
              ( Always
              , Just $
                  RouteTarget
                    (Substitution "DEST")
                    DefaultMode
                    CopyRoute
                    codecSpec
              )
            ]
            File
        manifest' =
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = mempty
            , fileRoutes = Map.singleton routeName route
            , ignorePatterns = mempty
            , hooks = mempty
            }
    writeManifestFile manifest' manifestPath
    writeFile source "raw"
    writeFile intermediate "raw:rendered"
    writeFile linkTarget "deployed"
    createSymbolicLink linkTarget destination File
    withEnvVar "DEST" (Just destination) $
      action appEnv source destination codecSpec


withReAddFile
  :: (AppEnv -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withReAddFile action =
  withReflectRepository $ \tmpDir repository _ manifestPath appEnv -> do
    routeName <- encodeFS "encoded-file"
    destinationName <- encodeFS "destination"
    let source = repository </> routeName
        destination = tmpDir </> destinationName
        Just codecName = parseCodecName "re-add"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "923e4567-e89b-42d3-a456-426614174000"
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [
              ( Always
              , Just $
                  RouteTarget
                    (Substitution "DEST")
                    DefaultMode
                    CopyRoute
                    codecSpec
              )
            ]
            File
        manifest' =
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = mempty
            , fileRoutes = Map.singleton routeName route
            , ignorePatterns = mempty
            , hooks = mempty
            }
    writeManifestFile manifest' manifestPath
    writeFile source "raw"
    withEnvVar "DEST" (Just destination) $
      action appEnv source destination codecSpec


withConvergedReAddFile :: (AppEnv -> CodecSpec -> IO a) -> IO a
withConvergedReAddFile action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "encoded-file"
    destinationName <- encodeFS "destination"
    let source = repository </> routeName
        intermediate = repository </> intermediateDir </> routeName
        destination = tmpDir </> destinationName
        Just codecName = parseCodecName "re-add"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "a23e4567-e89b-42d3-a456-426614174000"
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [
              ( Always
              , Just $
                  RouteTarget
                    (Substitution "DEST")
                    DefaultMode
                    CopyRoute
                    codecSpec
              )
            ]
            File
        manifest' =
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = mempty
            , fileRoutes = Map.singleton routeName route
            , ignorePatterns = mempty
            , hooks = mempty
            }
    writeManifestFile manifest' manifestPath
    writeFile source "raw"
    writeFile intermediate "raw:rendered"
    writeFile destination "raw:rendered"
    withEnvVar "DEST" (Just destination) $ action appEnv codecSpec


withRotatingReAddFile
  :: ByteString
  -> (AppEnv -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withRotatingReAddFile deployed action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "encoded-file"
    destinationName <- encodeFS "destination"
    let source = repository </> routeName
        intermediate = repository </> intermediateDir </> routeName
        destination = tmpDir </> destinationName
        Just codecName = parseCodecName "rotating-re-add"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "f23e4567-e89b-42d3-a456-426614174000"
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [
              ( Always
              , Just $
                  RouteTarget
                    (Substitution "DEST")
                    DefaultMode
                    CopyRoute
                    codecSpec
              )
            ]
            File
        manifest' =
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = mempty
            , fileRoutes = Map.singleton routeName route
            , ignorePatterns = mempty
            , hooks = mempty
            }
    writeManifestFile manifest' manifestPath
    writeFile source "raw"
    writeFile intermediate "A:raw"
    writeFile destination deployed
    withEnvVar "DEST" (Just destination) $
      action appEnv source destination codecSpec


withCaseVariantRotatingReAddFile
  :: (AppEnv -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withCaseVariantRotatingReAddFile action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    routeName <- encodeFS "encoded-file"
    destinationName <- encodeFS "Destination"
    selectedDestinationName <- encodeFS "destination"
    let source = repository </> routeName
        intermediate = repository </> intermediateDir </> routeName
        destination = tmpDir </> destinationName
        selectedDestination = tmpDir </> selectedDestinationName
        Just codecName = parseCodecName "rotating-re-add"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "f23e4567-e89b-42d3-a456-426614174001"
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [
              ( Always
              , Just $
                  RouteTarget
                    (Substitution "DEST")
                    DefaultMode
                    CopyRoute
                    codecSpec
              )
            ]
            File
        manifest' =
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = mempty
            , fileRoutes = Map.singleton routeName route
            , ignorePatterns = mempty
            , hooks = mempty
            }
    writeManifestFile manifest' manifestPath
    writeFile source "raw"
    writeFile intermediate "A:raw"
    writeFile destination "A:new"
    withEnvVar "DEST" (Just destination) $
      action appEnv source selectedDestination codecSpec


withMissingRotatingReAddFile
  :: (AppEnv -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withMissingRotatingReAddFile = withMissingReAddFileWithVariables mempty


withMissingReAddFileWithVariables
  :: ManifestVariableMap
  -> (AppEnv -> OsPath -> OsPath -> CodecSpec -> IO a)
  -> IO a
withMissingReAddFileWithVariables variables action =
  withReflectRepository $ \tmpDir repository _ manifestPath appEnv -> do
    routeName <- encodeFS "encoded-file"
    destinationName <- encodeFS "destination"
    let source = repository </> routeName
        destination = tmpDir </> destinationName
        Just codecName = parseCodecName "rotating-re-add"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "013e4567-e89b-42d3-a456-426614174000"
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [
              ( Always
              , Just $
                  RouteTarget
                    (Substitution "DEST")
                    DefaultMode
                    CopyRoute
                    codecSpec
              )
            ]
            File
        manifest' =
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = variables
            , fileRoutes = Map.singleton routeName route
            , ignorePatterns = mempty
            , hooks = mempty
            }
    writeManifestFile manifest' manifestPath
    writeFile destination "A:new"
    withEnvVar "DEST" (Just destination) $
      action appEnv source destination codecSpec


withSharedSourceReAddRoutes
  :: (AppEnv -> OsPath -> [OsPath] -> CodecSpec -> IO a)
  -> IO a
withSharedSourceReAddRoutes action =
  withReflectRepository $ \tmpDir repository intermediateDir manifestPath appEnv -> do
    treeRoute <- encodeFS "tree"
    childName <- encodeFS "child"
    childRoute <- encodeFS "tree/child"
    treeDestinationName <- encodeFS "tree-destination"
    childDestinationName <- encodeFS "child-destination"
    let sourceTree = repository </> treeRoute
        source = sourceTree </> childName
        intermediate = repository </> intermediateDir </> childRoute
        treeDestination = tmpDir </> treeDestinationName
        treeChildDestination = treeDestination </> childName
        childDestination = tmpDir </> childDestinationName
        Just codecName = parseCodecName "re-add"
        codecSpec = CodecSpec codecName $ CodecConfiguration mempty
        Right repositoryId =
          parseRepositoryId "113e4567-e89b-42d3-a456-426614174000"
        route destinationVariable fileType =
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
          Manifest
            { Manifest.repositoryId = Just repositoryId
            , monikers = mempty
            , variables = mempty
            , fileRoutes =
                Map.fromList
                  [ (treeRoute, route "TREE_DEST" Directory)
                  , (childRoute, route "CHILD_DEST" File)
                  ]
            , ignorePatterns = mempty
            , hooks = mempty
            }
    createDirectories sourceTree
    createDirectories $ repository </> intermediateDir </> treeRoute
    createDirectories treeDestination
    writeManifestFile manifest' manifestPath
    writeFile source "raw"
    writeFile intermediate "raw:rendered"
    writeFile treeChildDestination "changed:rendered"
    writeFile childDestination "raw:rendered"
    withEnvVar "TREE_DEST" (Just treeDestination) $
      withEnvVar "CHILD_DEST" (Just childDestination) $
        action
          appEnv
          source
          [treeChildDestination, childDestination]
          codecSpec


reAddRuntime :: CodecSpec -> CodecRuntime (App IO)
reAddRuntime = reAddRuntimeWith EvaluatePurely NormalEvaluation


reAddRuntimeWith
  :: CodecDryRunPolicy
  -> EvaluationMode
  -> CodecSpec
  -> CodecRuntime (App IO)
reAddRuntimeWith dryRunPolicy mode spec' =
  CodecRuntime
    (codecRegistry [implementation])
    mode
    (const $ return $ Left "unexpected external input")
 where
  CodecSpec name _ = spec'
  implementation =
    CodecImplementation
      (CodecDefinition name "test-1" ReflectReAdd)
      (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
      (\inputs -> Right $ revealBytes inputs.rawSource <> ":rendered")
      ( Just $ \_ deployed ->
          maybe
            (Left "invalid rendered bytes")
            Right
            (ByteString.stripSuffix ":rendered" $ revealBytes deployed)
      )
      PersistentCache
      dryRunPolicy


rejectRuntime :: CodecSpec -> CodecRuntime (App IO)
rejectRuntime spec' =
  CodecRuntime
    (codecRegistry [implementation])
    NormalEvaluation
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
      EvaluatePurely


identityReflectRuntime :: CodecSpec -> CodecRuntime (App IO)
identityReflectRuntime spec' =
  CodecRuntime
    (codecRegistry [implementation])
    NormalEvaluation
    (const $ return $ Left "unexpected external input")
 where
  CodecSpec name _ = spec'
  implementation =
    CodecImplementation
      (CodecDefinition name "test-1" ReflectIdentity)
      (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
      (\inputs -> Right $ revealBytes inputs.rawSource <> ":rendered")
      Nothing
      PersistentCache
      EvaluatePurely


countingRejectRuntime :: IORef Int -> CodecSpec -> CodecRuntime (App IO)
countingRejectRuntime count spec' =
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
      (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [counterInput])
      (\inputs -> Right $ revealBytes inputs.rawSource <> ":rendered")
      Nothing
      PersistentCache
      EvaluatePurely
  resolve request
    | request == counterInput = do
        liftIO $ modifyIORef' count (+ 1)
        return $ Right $ ExternalInput (opaqueBytes "") "stable"
    | otherwise = return $ Left "unexpected external input"


countingReAddRuntime :: IORef Int -> CodecSpec -> CodecRuntime (App IO)
countingReAddRuntime count spec' =
  CodecRuntime
    (codecRegistry [implementation])
    NormalEvaluation
    resolve
 where
  CodecSpec name _ = spec'
  counterInput = ExternalInputRequest "counter"
  implementation =
    CodecImplementation
      (CodecDefinition name "test-1" ReflectReAdd)
      (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [counterInput])
      (\inputs -> Right $ revealBytes inputs.rawSource <> ":rendered")
      (Just $ \_ deployed -> Right $ revealBytes deployed)
      PersistentCache
      EvaluatePurely
  resolve request
    | request == counterInput = do
        liftIO $ modifyIORef' count (+ 1)
        return $ Right $ ExternalInput (opaqueBytes "") "stable"
    | otherwise = return $ Left "unexpected external input"


rotatingReAddRuntime :: IORef Int -> CodecSpec -> CodecRuntime (App IO)
rotatingReAddRuntime resolutionCount spec' =
  CodecRuntime
    (codecRegistry [implementation])
    NormalEvaluation
    resolve
 where
  CodecSpec name _ = spec'
  keyInput = ExternalInputRequest "key"
  implementation =
    CodecImplementation
      (CodecDefinition name "test-1" ReflectReAdd)
      ( const $
          Right $
            CodecRequirements (requiredCodecInputs ["os"]) noCodecInputs [keyInput]
      )
      ( \inputs -> do
          key <- inputKey inputs
          Right $ key <> ":" <> revealBytes inputs.rawSource
      )
      ( Just $ \inputs deployed -> do
          key <- inputKey inputs
          maybe
            (Left "deployed bytes use another key")
            Right
            (ByteString.stripPrefix (key <> ":") $ revealBytes deployed)
      )
      PersistentCache
      EvaluatePurely
  inputKey :: CodecInputs -> Either CodecFailure ByteString
  inputKey inputs =
    case Map.lookup keyInput inputs.externalInputs of
      Just input -> Right $ revealBytes input.value
      Nothing -> Left $ OpaqueCodecFailure "missing key"
  resolve request
    | request == keyInput = do
        count <- liftIO $ readIORef resolutionCount
        liftIO $ modifyIORef' resolutionCount (+ 1)
        let key = if count == 0 then "A" else "B"
        return $ Right $ ExternalInput (opaqueBytes key) (Text.pack $ show key)
    | otherwise = return $ Left "unexpected external input"


sourceAwareReAddRuntime :: CodecSpec -> CodecRuntime (App IO)
sourceAwareReAddRuntime spec' =
  CodecRuntime
    (codecRegistry [implementation])
    NormalEvaluation
    (const $ return $ Left "unexpected external input")
 where
  CodecSpec name _ = spec'
  implementation =
    codecImplementationWithSourceRequirements
      (CodecDefinition name "test-1" ReflectReAdd)
      (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
      ( \_ source ->
          if revealBytes source == ""
            then Left "source must not be empty"
            else Right $ CodecRequirements noCodecInputs noCodecInputs []
      )
      (\inputs -> Right $ "A:" <> revealBytes inputs.rawSource)
      ( Just $ \_ deployed ->
          maybe
            (Left "deployed bytes use another key")
            Right
            (ByteString.stripPrefix "A:" $ revealBytes deployed)
      )
      PersistentCache
      EvaluatePurely


sourceVariableReAddRuntime :: CodecSpec -> CodecRuntime (App IO)
sourceVariableReAddRuntime spec' =
  CodecRuntime
    (codecRegistry [implementation])
    NormalEvaluation
    (const $ return $ Left "unexpected external input")
 where
  CodecSpec name _ = spec'
  implementation =
    codecImplementationWithSourceRequirements
      (CodecDefinition name "test-1" ReflectReAdd)
      (const $ Right $ CodecRequirements noCodecInputs noCodecInputs [])
      ( \_ _ ->
          Right $
            CodecRequirements
              noCodecInputs
              (requiredCodecInputs ["NAME"])
              []
      )
      ( \inputs -> case Map.lookup "NAME" inputs.variables of
          Just "Ada" -> Right $ "A:" <> revealBytes inputs.rawSource
          _ -> Left "missing NAME"
      )
      ( Just $ \_ deployed ->
          maybe
            (Left "deployed bytes use another key")
            Right
            (ByteString.stripPrefix "A:" $ revealBytes deployed)
      )
      PersistentCache
      EvaluatePurely


withReflectRepository
  :: (OsPath -> OsPath -> OsPath -> OsPath -> AppEnv -> IO a)
  -> IO a
withReflectRepository action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  let repository = tmpDir </> sourceDir
  let manifestPath = repository </> manifestFilename
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


captureStderr :: IO a -> IO (ByteString, a)
captureStderr action =
  withSystemTempFile "dojang-reflect-spec-stderr" $ \_ captureHandle ->
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
