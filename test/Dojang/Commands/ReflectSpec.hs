{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.ReflectSpec (spec) where

import Control.Exception (bracket_)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (singleton)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath (OsPath, encodeFS, (</>))
import Test.Hspec (Spec, describe, it, sequential)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldReturn
  , shouldSatisfy
  , shouldThrow
  )
import Prelude hiding (readFile, writeFile)

import Dojang.App (AppEnv (..), runAppWithoutLogging)
import Dojang.Commands.Reflect (reflect)
import Dojang.ExitCodes (ignoredFileError)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression (FilePathExpression (Substitution))
import Dojang.Types.MachineState
  ( MachineState (..)
  , encodeMachineState
  , readMachineId
  , readRepositoryState
  )
import Dojang.Types.ManagedTarget (ManagedTarget (..))
import Dojang.Types.Manifest (manifest)
import qualified Dojang.Types.Manifest as Manifest
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.RepositoryId (parseRepositoryId)


spec :: Spec
spec = sequential $ do
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

    it "accepts a source-only change" $
      withSourceOnlyChange $ \appEnv source intermediate destination -> do
        runAppWithoutLogging appEnv (reflect False True False Nothing [])
          `shouldReturn` ExitSuccess
        readFile source `shouldReturn` "managed contents"
        readFile intermediate `shouldReturn` "managed contents"
        readFile destination `shouldReturn` "managed contents"

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
