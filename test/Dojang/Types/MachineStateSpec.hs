{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.MachineStateSpec (spec) where

import Control.Concurrent
  ( forkIO
  , newEmptyMVar
  , putMVar
  , readMVar
  , takeMVar
  )
import Control.Exception qualified as Exception
import Control.Monad (replicateM, when)
import Control.Monad.Except
  ( ExceptT
  , MonadError (catchError, throwError)
  , runExceptT
  , tryError
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..), runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (chr)
import Data.Either (isRight)
import Data.List (isPrefixOf, nub)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (linear)
import System.Directory.OsPath qualified


#ifndef mingw32_HOST_OS
import System.Posix.Files qualified as Posix
#endif

import System.OsPath
  ( OsPath
  , decodeFS
  , encodeFS
  , isAbsolute
  , normalise
  , takeDirectory
  , (</>)
  )
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, runIO, shouldSatisfy, xit)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))
import Prelude hiding (readFile, writeFile)

import Dojang.MonadFileSystem
  ( MonadFileSystem (..)
  , dryRunIO
  )
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Environment (OperatingSystem (..))
import Dojang.Types.MachineState
  ( MachineId
  , MachineState (..)
  , MigrationResult (..)
  , RepositorySelection (..)
  , StateError (..)
  , StateRootInputs (..)
  , canonicalizeStateRoot
  , decodeMachineState
  , defaultIntermediatePath
  , encodeMachineState
  , ensureMachineId
  , listRepositoryStates
  , markFirstApplied
  , migrationMarkerPath
  , parseMachineId
  , prepareRepositoryState
  , prepareRepositoryStateWithLegacyHistory
  , prepareRepositoryStateWithOwnership
  , readMachineId
  , readRepositoryState
  , repositoryStateDirectory
  , repositoryStatePath
  , resolveStateRoot
  , selectRepositoryState
  , validateMachineStateStore
  , validateMigrationStateRoot
  )
import Dojang.Types.RepositoryId
  ( RepositoryId
  , parseRepositoryId
  , repositoryIdText
  )


spec :: Spec
spec = do
  symlinkAvailable <- runIO $ withTempDir $ \tmp _ -> do
    target <- encodeFS "target"
    link <- encodeFS "link"
    createDirectory $ tmp </> target
    (System.Directory.OsPath.createDirectoryLink target (tmp </> link) >> pure True)
      `catchError` const (pure False)
  let symlinkIt = if symlinkAvailable then it else xit

  describe "resolveStateRoot" $ do
    home <- runIO $ encodeFS "/home/tester"
    xdg <- runIO $ encodeFS "/data"
    local <- runIO $ encodeFS "/local"
    appSupport <- runIO $ encodeFS "Library/Application Support/dojang"
    linuxFallback <- runIO $ encodeFS ".local/share/dojang"
    windowsFallback <- runIO $ encodeFS "AppData/Local/dojang"
    dojang <- runIO $ encodeFS "dojang"

    it "uses the native data directory on every supported platform" $ do
      resolveStateRoot Linux (StateRootInputs home (Just xdg) Nothing)
        `shouldBe` normalise (xdg </> dojang)
      resolveStateRoot MacOS (StateRootInputs home (Just xdg) Nothing)
        `shouldBe` normalise (home </> appSupport)
      resolveStateRoot Windows (StateRootInputs home Nothing (Just local))
        `shouldBe` normalise (local </> dojang)

    it "has documented Linux and Windows fallbacks" $ do
      resolveStateRoot Linux (StateRootInputs home Nothing Nothing)
        `shouldBe` normalise (home </> linuxFallback)
      resolveStateRoot Windows (StateRootInputs home Nothing Nothing)
        `shouldBe` normalise (home </> windowsFallback)

    it "always returns an absolute path for absolute native inputs" $ hedgehog $ do
      suffix <- forAll $ Gen.text (linear 1 20) Gen.alphaNum
      suffixPath <- liftIO $ encodeFS $ Text.unpack suffix
      let root =
            resolveStateRoot
              Linux
              (StateRootInputs home (Just (xdg </> suffixPath)) Nothing)
      isAbsolute root === True

    it "ignores arbitrary relative XDG data directories" $ hedgehog $ do
      suffix <- forAll $ Gen.text (linear 0 20) Gen.alphaNum
      relative <- liftIO $ encodeFS $ Text.unpack suffix
      let root =
            resolveStateRoot Linux (StateRootInputs home (Just relative) Nothing)
      root === normalise (home </> linuxFallback)

    it "ignores arbitrary relative local app-data directories" $ hedgehog $ do
      suffix <- forAll $ Gen.text (linear 0 20) Gen.alphaNum
      relative <- liftIO $ encodeFS $ Text.unpack suffix
      let root =
            resolveStateRoot Windows (StateRootInputs home Nothing (Just relative))
      root === normalise (home </> windowsFallback)

    symlinkIt "resolves a symlinked native data directory before creating state" $
      withTempDir $ \tmp _ -> do
        targetName <- encodeFS "data"
        aliasName <- encodeFS "data-alias"
        stateName <- encodeFS "dojang"
        let target = tmp </> targetName
        let alias = tmp </> aliasName
        createDirectories target
        System.Directory.OsPath.createDirectoryLink target alias
        expected <-
          System.Directory.OsPath.canonicalizePath $ target </> stateName
        resolved <- canonicalizeStateRoot $ alias </> stateName
        resolved `shouldBe` Right expected

  describe "machine-state codec" $ do
    it "round-trips repository records" $ do
      state <- fixtureState
      decodeMachineState state.repositoryId state.machineId (encodeMachineState state)
        `shouldBe` Right state

    it "round-trips arbitrary checkout metadata" $ hedgehog $ do
      suffix <- forAll $ Gen.text (linear 1 30) Gen.alphaNum
      applied <- forAll Gen.bool
      state <- liftIO fixtureState
      suffixPath <- liftIO $ encodeFS $ Text.unpack suffix
      let checkout = state.checkoutPath </> suffixPath
      let generated =
            state
              { checkoutPath = checkout
              , firstApplied = applied
              }
      decodeMachineState
        generated.repositoryId
        generated.machineId
        (encodeMachineState generated)
        === Right generated

    it "round-trips arbitrary surrogate-escaped filesystem paths" $ hedgehog $ do
      byte <- forAll $ Gen.word8 (linear 0x80 0xff)
      state <- liftIO fixtureState
      checkout <-
        liftIO $
          encodeFS $
            [chr $ 0xdc00 + fromIntegral byte]
      intermediate <-
        liftIO $
          encodeFS $
            [chr $ 0xdc00 + fromIntegral byte]
      let generated =
            state
              { checkoutPath = state.checkoutPath </> checkout
              , manifestPath = state.manifestPath </> checkout
              , intermediatePath = state.intermediatePath </> intermediate
              }
      decodeMachineState
        generated.repositoryId
        generated.machineId
        (encodeMachineState generated)
        === Right generated

    it "rejects arbitrary relative persisted paths" $ hedgehog $ do
      suffix <- forAll $ Gen.text (linear 1 30) Gen.alphaNum
      state <- liftIO fixtureState
      let relative = "relative/" <> suffix
      let encoded = encodeMachineState state
      let relativeCheckout = replacePathField "checkout-path" relative encoded
      let relativeManifest = replacePathField "manifest-path" relative encoded
      let relativeIntermediate =
            replacePathField "intermediate-path" relative encoded
      isMalformed
        (decodeMachineState state.repositoryId state.machineId relativeCheckout)
        === True
      isMalformed
        (decodeMachineState state.repositoryId state.machineId relativeManifest)
        === True
      isMalformed
        (decodeMachineState state.repositoryId state.machineId relativeIntermediate)
        === True

    it "rejects invalid UTF-8 in a repository state record" $ hedgehog $ do
      invalidByte <- forAll $ Gen.word8 (linear 0x80 0xbf)
      malformed <- liftIO $ withTempDir $ \root _ -> do
        state <- fixtureState
        createDirectories $ repositoryStateDirectory root state.repositoryId
        let encoded = encodeUtf8 $ encodeMachineState state
        let marker = "checkout-path = \""
        let (prefix, checkout) = ByteString.breakSubstring marker encoded
        let checkoutPath = ByteString.drop (ByteString.length marker) checkout
        let corrupted =
              prefix
                <> marker
                <> ByteString.cons invalidByte (ByteString.drop 1 checkoutPath)
        writeFile
          (repositoryStatePath root state.repositoryId)
          corrupted
        result <- readRepositoryState root state.repositoryId state.machineId
        return $ isMalformed result
      malformed === True

    it "distinguishes unsupported, malformed, mismatched, and foreign state" $ do
      state <- fixtureState
      anotherRepository <- repositoryId "223e4567-e89b-42d3-a456-426614174000"
      anotherMachine <- machineId "423e4567-e89b-42d3-a456-426614174000"
      let encoded = encodeMachineState state
      decodeMachineState state.repositoryId state.machineId "not toml"
        `shouldSatisfy` isMalformed
      ( decodeMachineState state.repositoryId state.machineId $
          Text.replace "schema-version = 1" "schema-version = 2" encoded
        )
        `shouldBe` Left (UnsupportedSchemaVersion 2)
      decodeMachineState
        state.repositoryId
        state.machineId
        "schema-version = 2\n"
        `shouldBe` Left (UnsupportedSchemaVersion 2)
      decodeMachineState anotherRepository state.machineId encoded
        `shouldBe` Left (RepositoryIdentityMismatch anotherRepository state.repositoryId)
      decodeMachineState state.repositoryId anotherMachine encoded
        `shouldBe` Left (MachineProvenanceMismatch anotherMachine state.machineId)

  describe "selectRepositoryState" $ do
    it "keeps every list with multiple records ambiguous" $ hedgehog $ do
      additional <- forAll $ Gen.int (linear 0 30)
      state <- liftIO fixtureState
      selectRepositoryState (replicate (additional + 2) state)
        === AmbiguousRepositoryStates

  describe "listRepositoryStates" $ do
    it "rejects arbitrary non-directory repository-store roots" $ hedgehog $ do
      payload <- forAll $ Gen.bytes (linear 0 256)
      liftIO $ withTempDir $ \root _ -> do
        repositoriesName <- encodeFS "repositories"
        state <- fixtureState
        writeFile (root </> repositoriesName) payload
        result <- listRepositoryStates root state.machineId
        result `shouldSatisfy` isMalformed

    symlinkIt "rejects a symlinked repository-store root" $
      withTempDir $ \root _ -> do
        repositoriesName <- encodeFS "repositories"
        externalName <- encodeFS "external-repositories"
        state <- fixtureState
        let external = root </> externalName
        createDirectories external
        System.Directory.OsPath.createDirectoryLink
          external
          (root </> repositoriesName)
        result <- listRepositoryStates root state.machineId
        result `shouldSatisfy` isMalformed

    symlinkIt "rejects a symlinked repository-ID directory" $
      withTempDir $ \root _ -> do
        state <- fixtureState
        externalName <- encodeFS "external-repository-state"
        repositoriesName <- encodeFS "repositories"
        let external = root </> externalName
        let repositoryDirectory =
              repositoryStateDirectory root state.repositoryId
        createDirectories external
        createDirectories $ root </> repositoriesName
        System.Directory.OsPath.createDirectoryLink
          external
          repositoryDirectory
        result <- listRepositoryStates root state.machineId
        result `shouldSatisfy` isMalformed

    it "rejects noncanonical repository-ID directory names" $
      withTempDir $ \root _ -> do
        state <- fixtureState
        repositoriesName <- encodeFS "repositories"
        stateName <- encodeFS "state.toml"
        entry <-
          encodeFS $
            Text.unpack $
              Text.toUpper $
                repositoryIdText state.repositoryId
        let entryDirectory = root </> repositoriesName </> entry
        createDirectories entryDirectory
        writeFile
          (entryDirectory </> stateName)
          (encodeUtf8 $ encodeMachineState state)
        result <- listRepositoryStates root state.machineId
        result `shouldSatisfy` isMalformed

    it "rejects a repository state entry that is not a regular file" $
      withTempDir $ \root _ -> do
        state <- fixtureState
        createDirectories $ repositoryStatePath root state.repositoryId
        result <- listRepositoryStates root state.machineId
        result `shouldSatisfy` isMalformed

    it "rejects nonempty repository entries without state records" $ hedgehog $ do
      payload <- forAll $ Gen.bytes (linear 0 256)
      liftIO $ withTempDir $ \root _ -> do
        state <- fixtureState
        createDirectories $ repositoryStateDirectory root state.repositoryId
        writeFile
          (migrationMarkerPath root state.repositoryId)
          payload
        result <- listRepositoryStates root state.machineId
        result `shouldSatisfy` isMalformed

    it "continues to ignore an otherwise empty repository entry after locking" $
      withTempDir $ \root _ -> do
        state <- fixtureState
        createDirectories $ repositoryStateDirectory root state.repositoryId
        first <- listRepositoryStates root state.machineId
        second <- listRepositoryStates root state.machineId
        first `shouldBe` Right []
        second `shouldBe` Right []

    it "rejects nonempty entries with malformed repository IDs" $ hedgehog $ do
      suffix <- forAll $ Gen.text (linear 1 20) Gen.alphaNum
      payload <- forAll $ Gen.bytes (linear 0 256)
      liftIO $ withTempDir $ \root _ -> do
        state <- fixtureState
        createDirectories $ repositoryStateDirectory root state.repositoryId
        writeFile
          (repositoryStatePath root state.repositoryId)
          (encodeUtf8 $ encodeMachineState state)
        entry <- encodeFS $ "invalid-" <> Text.unpack suffix
        repositoriesName <- encodeFS "repositories"
        stateName <- encodeFS "state.toml"
        let entryDirectory = root </> repositoriesName </> entry
        createDirectories entryDirectory
        writeFile (entryDirectory </> stateName) payload
        result <- listRepositoryStates root state.machineId
        result `shouldSatisfy` isMalformed

  describe "validateMachineStateStore" $ do
    it "returns a state error when the state root cannot be traversed" $
      withTempDir $ \tmp _ -> do
        rootName <- encodeFS "state"
        let root = tmp </> rootName
        writeFile root "not a directory"
        result <- tryError $ validateMachineStateStore root
        result `shouldSatisfy` isStateIOErrorResult

    it "rejects arbitrary non-directory repository-store roots" $ hedgehog $ do
      payload <- forAll $ Gen.bytes (linear 0 256)
      liftIO $ withTempDir $ \root _ -> do
        repositoriesName <- encodeFS "repositories"
        _ <- ensureMachineId root
        writeFile (root </> repositoriesName) payload
        result <- validateMachineStateStore root
        result `shouldSatisfy` isMalformed

    symlinkIt "rejects a symlinked repository-store root" $
      withTempDir $ \root _ -> do
        repositoriesName <- encodeFS "repositories"
        externalName <- encodeFS "external-repositories"
        let external = root </> externalName
        _ <- ensureMachineId root
        createDirectories external
        System.Directory.OsPath.createDirectoryLink
          external
          (root </> repositoriesName)
        result <- validateMachineStateStore root
        result `shouldSatisfy` isMalformed

  describe "validateMigrationStateRoot" $ do
    it "rejects state roots at arbitrary depths under the legacy snapshot" $
      hedgehog $ do
        depth <- forAll $ Gen.int (linear 0 20)
        liftIO $ withTempDir $ \tmp _ -> do
          paths <- migrationPaths tmp
          stateName <- encodeFS "state"
          let nested = iterate (</> stateName) paths.legacy !! depth
          result <- validateMigrationStateRoot paths.checkout nested
          result `shouldSatisfy` isOverlappingStateRoot

    symlinkIt "rejects a state root whose parent resolves inside the snapshot" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        aliasName <- encodeFS "legacy-alias"
        stateName <- encodeFS "state"
        let alias = tmp </> aliasName
        createDirectories paths.legacy
        System.Directory.OsPath.createDirectoryLink paths.legacy alias
        result <-
          validateMigrationStateRoot paths.checkout (alias </> stateName)
        result `shouldSatisfy` isOverlappingStateRoot

  describe "machine-state documents" $ do
    it "returns a state error when the machine document cannot be inspected" $
      withTempDir $ \tmp _ -> do
        rootName <- encodeFS "state"
        let root = tmp </> rootName
        writeFile root "not a directory"
        result <- tryError $ readMachineId root
        result `shouldSatisfy` isStateIOErrorResult

    it "rejects a repository state path that is not a regular file" $
      withTempDir $ \root _ -> do
        state <- fixtureState
        createDirectories $ repositoryStatePath root state.repositoryId
        result <- readRepositoryState root state.repositoryId state.machineId
        result `shouldSatisfy` isMalformed

    symlinkIt "rejects a symlinked repository state document" $
      withTempDir $ \root _ -> do
        state <- fixtureState
        targetName <- encodeFS "target-state.toml"
        let target = root </> targetName
        createDirectories $ repositoryStateDirectory root state.repositoryId
        writeFile target $ encodeUtf8 $ encodeMachineState state
        System.Directory.OsPath.createFileLink
          target
          (repositoryStatePath root state.repositoryId)
        result <- readRepositoryState root state.repositoryId state.machineId
        result `shouldSatisfy` isMalformed

    it "rejects a machine identity path that is not a regular file" $
      withTempDir $ \root _ -> do
        machineName <- encodeFS "machine.toml"
        createDirectories $ root </> machineName
        result <- readMachineId root
        result `shouldSatisfy` isMalformed

    symlinkIt "rejects a symlinked machine identity document" $
      withTempDir $ \root _ -> do
        machineName <- encodeFS "machine.toml"
        targetName <- encodeFS "target-machine.toml"
        let target = root </> targetName
        writeFile target "machine-id = \"323e4567-e89b-42d3-a456-426614174000\"\n"
        System.Directory.OsPath.createFileLink target (root </> machineName)
        result <- readMachineId root
        result `shouldSatisfy` isMalformed

    posixStateDocumentSpec

    it "does not replace a missing machine identity when records remain" $
      withTempDir $ \root _ -> do
        state <- fixtureState
        createDirectories $ repositoryStateDirectory root state.repositoryId
        writeFile
          (repositoryStatePath root state.repositoryId)
          (encodeUtf8 $ encodeMachineState state)
        readMachineId root >>= (`shouldSatisfy` isMalformed)
        result <- ensureMachineId root
        result `shouldSatisfy` isMalformed
        machineName <- encodeFS "machine.toml"
        exists (root </> machineName) >>= (`shouldBe` False)

  describe "prepareRepositoryState" $ do
    it "isolates two repositories on one machine" $ do
      root <- encodeFS "/state"
      repositoryA <- repositoryId "123e4567-e89b-42d3-a456-426614174000"
      repositoryB <- repositoryId "223e4567-e89b-42d3-a456-426614174000"
      repositoryStateDirectory root repositoryA
        `shouldSatisfy` (/= repositoryStateDirectory root repositoryB)

    it "updates the checkout path after a repository is moved" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        movedName <- encodeFS "moved-checkout"
        let moved = tmp </> movedName
        createDirectories paths.checkout
        first <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        first `shouldSatisfy` isRightState
        removeDirectory paths.checkout
        createDirectories moved
        second <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            moved
            Nothing
            fixtureTime
        case second of
          Right (state, MovedRepositoryCheckout) ->
            state.checkoutPath `shouldBe` moved
          _ -> fail $ "Unexpected result: " <> show second

    it "updates a moved checkout when the old path contains unrelated data" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        movedName <- encodeFS "moved-checkout"
        unrelatedName <- encodeFS "unrelated"
        let moved = tmp </> movedName
        createDirectories paths.checkout
        first <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        first `shouldSatisfy` isRightState
        removeDirectory paths.checkout
        createDirectories paths.checkout
        writeFile (paths.checkout </> unrelatedName) "unrelated"
        createDirectories moved
        second <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            moved
            Nothing
            fixtureTime
        case second of
          Right (state, MovedRepositoryCheckout) ->
            state.checkoutPath `shouldBe` moved
          _ -> fail $ "Unexpected result: " <> show second

    it "rejects a duplicate identity while the first checkout still exists" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        duplicateName <- encodeFS "duplicate-checkout"
        let duplicate = tmp </> duplicateName
        createDirectories paths.checkout
        createDirectories duplicate
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        second <-
          prepareRepositoryStateWithOwnership
            paths.root
            paths.repositoryId
            paths.machineId
            duplicate
            (duplicate </> paths.file)
            Nothing
            (\_ _ -> return True)
            False
            fixtureTime
        second
          `shouldBe` Left
            ( DuplicateRepositoryIdentity
                paths.repositoryId
                paths.checkout
                duplicate
            )

    symlinkIt "recognizes a symlink alias of the same checkout" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        aliasName <- encodeFS "checkout-alias"
        let alias = tmp </> aliasName
        createDirectories paths.checkout
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        createDirectories paths.external
        System.Directory.OsPath.createDirectoryLink paths.checkout alias
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            alias
            Nothing
            fixtureTime
        result `shouldSatisfy` isRightState

    symlinkIt "keeps associated paths valid when reusing a checkout alias" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        checkoutAliasName <- encodeFS "checkout-alias"
        snapshotContainerName <- encodeFS "snapshot-container"
        snapshotAliasName <- encodeFS "snapshot-alias"
        currentName <- encodeFS "current"
        manifestName <- encodeFS "custom.toml"
        let checkoutAlias = tmp </> checkoutAliasName
        let snapshotContainer = tmp </> snapshotContainerName
        let snapshotAlias = tmp </> snapshotAliasName
        let current = snapshotContainer </> currentName
        let aliasedCurrent = snapshotAlias </> currentName
        let manifest = paths.checkout </> manifestName
        let aliasedManifest = checkoutAlias </> manifestName
        createDirectories paths.checkout
        writeFile manifest "repository-id = \"fixture\"\n"
        first <-
          prepareRepositoryStateWithOwnership
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            manifest
            (Just current)
            (\_ _ -> return False)
            False
            fixtureTime
        first `shouldSatisfy` isRightState
        System.Directory.OsPath.createDirectoryLink paths.checkout checkoutAlias
        System.Directory.OsPath.createDirectoryLink snapshotContainer snapshotAlias
        second <-
          prepareRepositoryStateWithOwnership
            paths.root
            paths.repositoryId
            paths.machineId
            checkoutAlias
            aliasedManifest
            (Just aliasedCurrent)
            (\_ _ -> return False)
            False
            fixtureTime
        stored <- case second of
          Right (state, ReusedRepositoryState) -> return state
          _ -> fail $ "Unexpected result: " <> show second
        stored.checkoutPath `shouldBe` paths.checkout
        stored.manifestPath `shouldBe` manifest
        stored.intermediatePath `shouldBe` current
        removeFile checkoutAlias
        removeFile snapshotAlias
        exists stored.checkoutPath >>= (`shouldBe` True)
        exists stored.manifestPath >>= (`shouldBe` True)
        exists stored.intermediatePath >>= (`shouldBe` True)

    it "migrates a local mirror and persists the external snapshot" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        createDirectories paths.checkout
        createDirectories paths.legacy
        writeFile (paths.legacy </> paths.file) "base"
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        case result of
          Left err -> fail $ show err
          Right (state, MigratedLegacySnapshot) -> do
            state.intermediatePath `shouldBe` paths.external
            readFile (paths.external </> paths.file) >>= (`shouldBe` "base")
            exists paths.legacy >>= (`shouldBe` False)
          Right (_, migration) -> fail $ "Unexpected migration: " <> show migration

    symlinkIt "rejects a symlinked legacy mirror without touching its target" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        targetName <- encodeFS "legacy-target"
        let target = tmp </> targetName
        createDirectories paths.checkout
        createDirectories target
        writeFile (target </> paths.file) "recovery"
        System.Directory.OsPath.createDirectoryLink target paths.legacy
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result `shouldBe` Left (UnsupportedSnapshotSymlink paths.legacy)
        readFile (target </> paths.file) >>= (`shouldBe` "recovery")
        isSymlink paths.legacy >>= (`shouldBe` True)

    symlinkIt "rejects a symlinked private snapshot ancestor" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        snapshotsName <- encodeFS "snapshots"
        currentName <- encodeFS "current"
        targetName <- encodeFS "snapshot-target"
        let stateDirectory =
              repositoryStateDirectory paths.root paths.repositoryId
        let snapshots = stateDirectory </> snapshotsName
        let target = tmp </> targetName
        createDirectories stateDirectory
        createDirectories $ target </> currentName
        System.Directory.OsPath.createDirectoryLink target snapshots
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result `shouldBe` Left (UnsupportedSnapshotSymlink snapshots)
        isSymlink snapshots >>= (`shouldBe` True)
        isDirectory (target </> currentName) >>= (`shouldBe` True)

    symlinkIt "rejects matching snapshots that contain nested symlinks" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        targetName <- encodeFS "target"
        linkName <- encodeFS "link"
        createDirectories paths.legacy
        createDirectories paths.external
        writeFile (paths.legacy </> targetName) "same"
        writeFile (paths.external </> targetName) "same"
        System.Directory.OsPath.createFileLink
          targetName
          (paths.legacy </> linkName)
        System.Directory.OsPath.createFileLink
          targetName
          (paths.external </> linkName)
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result
          `shouldBe` Left
            (UnsupportedSnapshotSymlink $ paths.legacy </> linkName)
        exists paths.legacy >>= (`shouldBe` True)
        exists paths.external >>= (`shouldBe` True)

    posixSpecialFileSpec

    it "rejects an intermediate path nested inside the legacy mirror" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        copyName <- encodeFS "copy"
        let nested = paths.legacy </> copyName
        createDirectories paths.legacy
        writeFile (paths.legacy </> paths.file) "recovery"
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just nested)
            fixtureTime
        result `shouldBe` Left (SnapshotInsideLegacy paths.legacy nested)
        readFile (paths.legacy </> paths.file) >>= (`shouldBe` "recovery")
        exists nested >>= (`shouldBe` False)

    it "refuses different old and external mirrors without mutating either" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        createDirectories paths.legacy
        createDirectories paths.external
        writeFile (paths.legacy </> paths.file) "old"
        writeFile (paths.external </> paths.file) "new"
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result `shouldBe` Left (ConflictingSnapshots paths.legacy paths.external)
        readFile (paths.legacy </> paths.file) >>= (`shouldBe` "old")
        readFile (paths.external </> paths.file) >>= (`shouldBe` "new")

    it "does not publish a migration marker when atomic replacement fails" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        createDirectories paths.legacy
        writeFile (paths.legacy </> paths.file) "ancestor"
        let marker = migrationMarkerPath paths.root paths.repositoryId
        failed <-
          runFailingReplaceIO marker $
            prepareRepositoryState
              paths.root
              paths.repositoryId
              paths.machineId
              paths.checkout
              Nothing
              fixtureTime
        failed `shouldSatisfy` isLeftIOError
        exists marker >>= (`shouldBe` False)
        readFile (paths.legacy </> paths.file) >>= (`shouldBe` "ancestor")
        entries <-
          listDirectory $ repositoryStateDirectory paths.root paths.repositoryId
        filenames <- mapM decodePath entries
        filenames
          `shouldSatisfy` all (not . isPrefixOf "migration-in-progress.tmp.")
        retried <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        retried `shouldSatisfy` isRightState

    it "rejects a migration marker that is not a regular file" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        let marker = migrationMarkerPath paths.root paths.repositoryId
        createDirectories paths.legacy
        writeFile (paths.legacy </> paths.file) "ancestor"
        createDirectories $ repositoryStateDirectory paths.root paths.repositoryId
        createDirectory marker
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result `shouldSatisfy` isMalformedMigrationMarker
        isDirectory marker >>= (`shouldBe` True)
        readFile (paths.legacy </> paths.file) >>= (`shouldBe` "ancestor")

    symlinkIt "rejects a symlinked migration marker without following it" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        targetName <- encodeFS "marker-target"
        let marker = migrationMarkerPath paths.root paths.repositoryId
        let target = tmp </> targetName
        createDirectories paths.legacy
        writeFile (paths.legacy </> paths.file) "ancestor"
        createDirectories $ repositoryStateDirectory paths.root paths.repositoryId
        writeMigrationMarkerFixture target paths.legacy paths.external
        System.Directory.OsPath.createFileLink target marker
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result `shouldSatisfy` isMalformedMigrationMarker
        isSymlink marker >>= (`shouldBe` True)
        exists target >>= (`shouldBe` True)
        readFile (paths.legacy </> paths.file) >>= (`shouldBe` "ancestor")

    symlinkIt "rejects a dangling migration-marker symlink" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        missingTargetName <- encodeFS "missing-marker-target"
        let marker = migrationMarkerPath paths.root paths.repositoryId
        createDirectories paths.legacy
        writeFile (paths.legacy </> paths.file) "ancestor"
        createDirectories $ repositoryStateDirectory paths.root paths.repositoryId
        System.Directory.OsPath.createFileLink missingTargetName marker
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result `shouldSatisfy` isMalformedMigrationMarker
        isSymlink marker >>= (`shouldBe` True)
        readFile (paths.legacy </> paths.file) >>= (`shouldBe` "ancestor")

    it "retries an interrupted partial copy from the complete legacy mirror" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        copiedName <- encodeFS "copied"
        createDirectories paths.legacy
        createDirectories paths.external
        writeFile (paths.legacy </> paths.file) "complete"
        writeFile (paths.legacy </> copiedName) "already copied"
        writeFile (paths.external </> copiedName) "already copied"
        createDirectories $ repositoryStateDirectory paths.root paths.repositoryId
        writeMigrationMarkerFixture
          (migrationMarkerPath paths.root paths.repositoryId)
          paths.legacy
          paths.external
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        case result of
          Right (_, RecoveredInterruptedMigration) ->
            readFile (paths.external </> paths.file) >>= (`shouldBe` "complete")
          _ -> fail $ "Unexpected result: " <> show result

    it "preserves divergent snapshots when retrying legacy migration" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        createDirectories paths.legacy
        createDirectories paths.external
        writeFile (paths.legacy </> paths.file) "legacy"
        writeFile (paths.external </> paths.file) "divergent"
        createDirectories $ repositoryStateDirectory paths.root paths.repositoryId
        writeMigrationMarkerFixture
          (migrationMarkerPath paths.root paths.repositoryId)
          paths.legacy
          paths.external
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result `shouldBe` Left (ConflictingSnapshots paths.legacy paths.external)
        readFile (paths.legacy </> paths.file) >>= (`shouldBe` "legacy")
        readFile (paths.external </> paths.file) >>= (`shouldBe` "divergent")
        exists (migrationMarkerPath paths.root paths.repositoryId)
          >>= (`shouldBe` True)

    it "retains a divergent migration marker and both snapshots" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        createDirectories paths.checkout
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        createDirectories paths.legacy
        createDirectories paths.external
        writeFile (paths.legacy </> paths.file) "changed legacy"
        writeFile (paths.external </> paths.file) "persisted external"
        writeMigrationMarkerFixture
          (migrationMarkerPath paths.root paths.repositoryId)
          paths.legacy
          paths.external
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result `shouldBe` Left (ConflictingSnapshots paths.legacy paths.external)
        exists paths.legacy >>= (`shouldBe` True)
        exists paths.external >>= (`shouldBe` True)
        exists (migrationMarkerPath paths.root paths.repositoryId)
          >>= (`shouldBe` True)

    it "finishes cleanup before removing a recovered migration marker" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        createDirectories paths.checkout
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        createDirectories paths.legacy
        createDirectories paths.external
        writeFile (paths.legacy </> paths.file) "same"
        writeFile (paths.external </> paths.file) "same"
        writeMigrationMarkerFixture
          (migrationMarkerPath paths.root paths.repositoryId)
          paths.legacy
          paths.external
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result `shouldSatisfy` isRightState
        exists paths.legacy >>= (`shouldBe` False)
        exists (migrationMarkerPath paths.root paths.repositoryId)
          >>= (`shouldBe` False)

    it "retains a marker when its recorded legacy snapshot is missing" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        createDirectories paths.legacy
        writeFile (paths.legacy </> paths.file) "recovery"
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just paths.legacy)
            fixtureTime
        removeDirectoryRecursively paths.legacy
        writeMigrationMarkerFixture
          (migrationMarkerPath paths.root paths.repositoryId)
          paths.legacy
          paths.legacy
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just paths.legacy)
            fixtureTime
        result `shouldBe` Left (MissingSnapshot paths.legacy)
        exists (migrationMarkerPath paths.root paths.repositoryId)
          >>= (`shouldBe` True)

    it "does not remove a destination that differs from the recovery marker" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        unrelatedName <- encodeFS "unrelated"
        preciousName <- encodeFS "precious"
        let unrelated = tmp </> unrelatedName
        createDirectories paths.legacy
        createDirectories paths.external
        createDirectories unrelated
        writeFile (paths.legacy </> paths.file) "complete"
        writeFile (paths.external </> paths.file) "partial"
        writeFile (unrelated </> preciousName) "keep"
        createDirectories $ repositoryStateDirectory paths.root paths.repositoryId
        writeMigrationMarkerFixture
          (migrationMarkerPath paths.root paths.repositoryId)
          paths.legacy
          paths.external
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just unrelated)
            fixtureTime
        result
          `shouldBe` Left
            (MigrationDestinationMismatch paths.external unrelated)
        readFile (unrelated </> preciousName) >>= (`shouldBe` "keep")
        readFile (paths.external </> paths.file) >>= (`shouldBe` "partial")
        exists (migrationMarkerPath paths.root paths.repositoryId)
          >>= (`shouldBe` True)

    it "refuses an existing snapshot that has no state or legacy owner" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        createDirectories paths.external
        writeFile (paths.external </> paths.file) "unowned"
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result `shouldBe` Left (UnownedSnapshot paths.external)
        readFile (paths.external </> paths.file) >>= (`shouldBe` "unowned")

    it "retries initial state creation after its atomic write fails" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        failed <-
          runFailingReplaceIO
            (repositoryStatePath paths.root paths.repositoryId)
            $ prepareRepositoryState
              paths.root
              paths.repositoryId
              paths.machineId
              paths.checkout
              Nothing
              fixtureTime
        failed `shouldSatisfy` isLeftIOError
        listRepositoryStates paths.root paths.machineId >>= (`shouldBe` Right [])
        retried <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        case retried of
          Right (state, CreatedRepositoryState) ->
            state.intermediatePath `shouldBe` paths.external
          _ -> fail $ "Unexpected retry result: " <> show retried

    it "adopts an explicitly selected legacy snapshot without changing it" $
      hedgehog $ do
        contents <- forAll $ Gen.bytes $ linear 0 256
        adopted <- liftIO $ withTempDir $ \tmp _ -> do
          paths <- migrationPaths tmp
          customName <- encodeFS "custom-snapshot"
          let custom = tmp </> customName
          createDirectories paths.checkout
          createDirectories custom
          writeFile (custom </> paths.file) contents
          result <-
            prepareRepositoryState
              paths.root
              paths.repositoryId
              paths.machineId
              paths.checkout
              (Just custom)
              fixtureTime
          preserved <- readFile $ custom </> paths.file
          return $ case result of
            Right (state, MigratedLegacySnapshot) ->
              state.intermediatePath == custom && preserved == contents
            _ -> False
        adopted === True

    it "rejects snapshots that contain protected directories" $ hedgehog $ do
      selectStateRoot <- forAll Gen.bool
      contents <- forAll $ Gen.bytes $ linear 0 256
      rejected <- liftIO $ withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        let protected = if selectStateRoot then paths.root else paths.checkout
        createDirectories paths.checkout
        writeFile (paths.checkout </> paths.file) contents
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just protected)
            fixtureTime
        preserved <- readFile $ paths.checkout </> paths.file
        stateExists <- exists $ repositoryStatePath paths.root paths.repositoryId
        return $
          case result of
            Left (ProtectedSnapshotLocation _ _) ->
              preserved == contents && not stateExists
            _ -> False
      rejected === True

    it "does not clean up a stored snapshot that contains the old checkout" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        movedName <- encodeFS "moved-checkout"
        newSnapshotName <- encodeFS "new-snapshot"
        let moved = tmp </> movedName
        let newSnapshot = tmp </> newSnapshotName
        let unsafeState =
              MachineState
                1
                paths.repositoryId
                paths.machineId
                paths.checkout
                (paths.checkout </> paths.file)
                paths.checkout
                fixtureTime
                fixtureTime
                False
        createDirectories paths.checkout
        createDirectories moved
        writeFile (paths.checkout </> paths.file) "repository data"
        createDirectories $
          repositoryStateDirectory paths.root paths.repositoryId
        writeFile
          (repositoryStatePath paths.root paths.repositoryId)
          (encodeUtf8 $ encodeMachineState unsafeState)
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            moved
            (Just newSnapshot)
            fixtureTime
        result `shouldSatisfy` isProtectedSnapshot
        readFile (paths.checkout </> paths.file)
          >>= (`shouldBe` "repository data")
        exists newSnapshot >>= (`shouldBe` False)

    symlinkIt "rejects nested symlinks before adopting a custom snapshot" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        customName <- encodeFS "custom-snapshot"
        targetName <- encodeFS "target"
        linkName <- encodeFS "link"
        let custom = tmp </> customName
        createDirectories paths.checkout
        createDirectories custom
        writeFile (custom </> targetName) "contents"
        System.Directory.OsPath.createFileLink
          targetName
          (custom </> linkName)
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just custom)
            fixtureTime
        result
          `shouldBe` Left (UnsupportedSnapshotSymlink $ custom </> linkName)
        exists (repositoryStatePath paths.root paths.repositoryId)
          >>= (`shouldBe` False)
        isSymlink (custom </> linkName) >>= (`shouldBe` True)

    it "rejects a legacy snapshot path that is a regular file" $ hedgehog $ do
      contents <- forAll $ Gen.bytes $ linear 0 256
      rejected <- liftIO $ withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        createDirectories paths.checkout
        writeFile paths.legacy contents
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        preserved <- readFile paths.legacy
        stateExists <- exists $ repositoryStatePath paths.root paths.repositoryId
        return $
          result == Left (InvalidSnapshotLocation paths.legacy)
            && preserved == contents
            && not stateExists
      rejected === True

    it "rejects an override nested inside the current snapshot" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        oldName <- encodeFS "old-snapshot"
        childName <- encodeFS "nested"
        let old = tmp </> oldName
        let nested = old </> childName
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just old)
            fixtureTime
        createDirectories old
        writeFile (old </> paths.file) "ancestor"
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just nested)
            fixtureTime
        result `shouldBe` Left (OverlappingSnapshots old nested)
        readFile (old </> paths.file) >>= (`shouldBe` "ancestor")
        exists nested >>= (`shouldBe` False)

    it "rejects an override that contains the current snapshot" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        parentName <- encodeFS "snapshot-parent"
        childName <- encodeFS "current"
        let parent = tmp </> parentName
        let current = parent </> childName
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just current)
            fixtureTime
        createDirectories current
        writeFile (current </> paths.file) "ancestor"
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just parent)
            fixtureTime
        result `shouldBe` Left (OverlappingSnapshots current parent)
        readFile (current </> paths.file) >>= (`shouldBe` "ancestor")

    symlinkIt "rejects an override whose resolved path overlaps the snapshot" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        oldName <- encodeFS "current-snapshot"
        aliasName <- encodeFS "snapshot-alias"
        nestedName <- encodeFS "nested"
        let old = tmp </> oldName
        let alias = tmp </> aliasName
        let nested = alias </> nestedName
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just old)
            fixtureTime
        writeFile (old </> paths.file) "ancestor"
        System.Directory.OsPath.createDirectoryLink old alias
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just nested)
            fixtureTime
        result `shouldBe` Left (OverlappingSnapshots old nested)
        readFile (old </> paths.file) >>= (`shouldBe` "ancestor")
        exists (old </> nestedName) >>= (`shouldBe` False)

    symlinkIt "reuses an aliased path to the same snapshot without cleanup" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        containerName <- encodeFS "snapshot-container"
        aliasName <- encodeFS "container-alias"
        currentName <- encodeFS "current"
        let container = tmp </> containerName
        let current = container </> currentName
        let alias = tmp </> aliasName
        let aliasedCurrent = alias </> currentName
        first <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just current)
            fixtureTime
        first `shouldSatisfy` isRightState
        writeFile (current </> paths.file) "ancestor"
        System.Directory.OsPath.createDirectoryLink container alias
        second <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just aliasedCurrent)
            fixtureTime
        second `shouldSatisfy` isRightState
        readFile (current </> paths.file) >>= (`shouldBe` "ancestor")
        readFile (aliasedCurrent </> paths.file) >>= (`shouldBe` "ancestor")

    it "keeps the old snapshot when persisting an override fails" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        oldName <- encodeFS "old-snapshot"
        newName <- encodeFS "new-snapshot"
        let old = tmp </> oldName
        let new = tmp </> newName
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just old)
            fixtureTime
        createDirectories old
        writeFile (old </> paths.file) "ancestor"
        failed <-
          runFailingReplaceIO
            (repositoryStatePath paths.root paths.repositoryId)
            $ prepareRepositoryState
              paths.root
              paths.repositoryId
              paths.machineId
              paths.checkout
              (Just new)
              fixtureTime
        failed `shouldSatisfy` isLeftIOError
        readFile (old </> paths.file) >>= (`shouldBe` "ancestor")
        readFile (new </> paths.file) >>= (`shouldBe` "ancestor")
        loaded <-
          readRepositoryState paths.root paths.repositoryId paths.machineId
        case loaded of
          Right (Just state) -> state.intermediatePath `shouldBe` old
          _ -> fail $ "Unexpected state: " <> show loaded

    it "retries an interrupted intermediate-path override" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        oldName <- encodeFS "old-snapshot"
        newName <- encodeFS "new-snapshot"
        let old = tmp </> oldName
        let new = tmp </> newName
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just old)
            fixtureTime
        writeFile (old </> paths.file) "ancestor"
        failed <-
          runFailingCopyIO (new </> paths.file) $
            prepareRepositoryState
              paths.root
              paths.repositoryId
              paths.machineId
              paths.checkout
              (Just new)
              fixtureTime
        failed `shouldSatisfy` isLeftIOError
        exists (migrationMarkerPath paths.root paths.repositoryId)
          >>= (`shouldBe` True)
        retried <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just new)
            fixtureTime
        case retried of
          Right (state, PersistedIntermediateOverride) ->
            state.intermediatePath `shouldBe` new
          _ -> fail $ "Unexpected retry: " <> show retried
        readFile (new </> paths.file) >>= (`shouldBe` "ancestor")
        exists old >>= (`shouldBe` False)
        exists (migrationMarkerPath paths.root paths.repositoryId)
          >>= (`shouldBe` False)

    it "retries cleanup for a matching pre-existing override" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        oldName <- encodeFS "old-snapshot"
        newName <- encodeFS "new-snapshot"
        let old = tmp </> oldName
        let new = tmp </> newName
        let oldFile = old </> paths.file
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just old)
            fixtureTime
        writeFile oldFile "ancestor"
        createDirectories new
        writeFile (new </> paths.file) "ancestor"
        failed <-
          runFailingRemoveIO oldFile $
            prepareRepositoryState
              paths.root
              paths.repositoryId
              paths.machineId
              paths.checkout
              (Just new)
              fixtureTime
        case failed of
          Right (Left (LegacyCleanupFailed source destination _)) -> do
            source `shouldBe` old
            destination `shouldBe` new
          _ -> fail $ "Unexpected cleanup result: " <> show failed
        exists (migrationMarkerPath paths.root paths.repositoryId)
          >>= (`shouldBe` True)
        loaded <-
          readRepositoryState paths.root paths.repositoryId paths.machineId
        case loaded of
          Right (Just state) -> state.intermediatePath `shouldBe` new
          _ -> fail $ "Unexpected state: " <> show loaded
        retried <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just new)
            fixtureTime
        retried `shouldSatisfy` isRightState
        exists old >>= (`shouldBe` False)
        exists (migrationMarkerPath paths.root paths.repositoryId)
          >>= (`shouldBe` False)

    it "finishes cleanup after the old snapshot was partially removed" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        oldName <- encodeFS "old-snapshot"
        newName <- encodeFS "new-snapshot"
        let old = tmp </> oldName
        let new = tmp </> newName
        let oldFile = old </> paths.file
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just old)
            fixtureTime
        writeFile oldFile "ancestor"
        failed <-
          runFailingRemoveIO old $
            prepareRepositoryState
              paths.root
              paths.repositoryId
              paths.machineId
              paths.checkout
              (Just new)
              fixtureTime
        case failed of
          Right (Left (LegacyCleanupFailed source destination _)) -> do
            source `shouldBe` old
            destination `shouldBe` new
          _ -> fail $ "Unexpected cleanup result: " <> show failed
        exists old >>= (`shouldBe` True)
        exists oldFile >>= (`shouldBe` False)
        exists (migrationMarkerPath paths.root paths.repositoryId)
          >>= (`shouldBe` True)
        retried <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just new)
            fixtureTime
        retried `shouldSatisfy` isRightState
        exists old >>= (`shouldBe` False)
        exists (migrationMarkerPath paths.root paths.repositoryId)
          >>= (`shouldBe` False)

    it "preserves a divergent override destination during retry" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        oldName <- encodeFS "old-snapshot"
        newName <- encodeFS "new-snapshot"
        let old = tmp </> oldName
        let new = tmp </> newName
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just old)
            fixtureTime
        writeFile (old </> paths.file) "ancestor"
        createDirectories new
        writeFile (new </> paths.file) "divergent"
        writeMigrationMarkerFixture
          (migrationMarkerPath paths.root paths.repositoryId)
          old
          new
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just new)
            fixtureTime
        result `shouldBe` Left (ConflictingSnapshots old new)
        readFile (old </> paths.file) >>= (`shouldBe` "ancestor")
        readFile (new </> paths.file) >>= (`shouldBe` "divergent")
        exists (migrationMarkerPath paths.root paths.repositoryId)
          >>= (`shouldBe` True)

    it "preserves known first-apply history when creating migrated state" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        result <-
          prepareRepositoryStateWithLegacyHistory
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            True
            fixtureTime
        case result of
          Right (state, CreatedRepositoryState) ->
            state.firstApplied `shouldBe` True
          _ -> fail $ "Unexpected result: " <> show result

    it "persists an explicit intermediate path" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        explicit <- encodeFS "custom-snapshot"
        createDirectories paths.checkout
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just explicit)
            fixtureTime
        case result of
          Right (state, CreatedRepositoryState) -> do
            state.intermediatePath
              `shouldBe` normalise (paths.checkout </> explicit)
            reloaded <-
              prepareRepositoryState
                paths.root
                paths.repositoryId
                paths.machineId
                paths.checkout
                Nothing
                fixtureTime
            case reloaded of
              Right (stored, ReusedRepositoryState) ->
                stored.intermediatePath `shouldBe` state.intermediatePath
              _ -> fail $ "Unexpected reload: " <> show reloaded
          _ -> fail $ "Unexpected result: " <> show result

    symlinkIt "ignores an unused symlinked default for a stored custom snapshot" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        customName <- encodeFS "custom-snapshot"
        targetName <- encodeFS "unused-default-target"
        let custom = tmp </> customName
        let target = tmp </> targetName
        createDirectories paths.checkout
        first <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just custom)
            fixtureTime
        first `shouldSatisfy` isRightState
        createDirectories target
        createDirectories $ takeDirectory paths.external
        System.Directory.OsPath.createDirectoryLink target paths.external
        reloaded <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        case reloaded of
          Right (state, ReusedRepositoryState) ->
            state.intermediatePath `shouldBe` custom
          _ -> fail $ "Unexpected reload: " <> show reloaded

    it "rejects a missing persisted snapshot instead of reusing it" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        createDirectories paths.checkout
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        snapshotExists <- isDirectory paths.external
        when (not snapshotExists) $ createDirectories paths.external
        removeDirectory paths.external
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result `shouldBe` Left (MissingSnapshot paths.external)

    it "rejects a persisted snapshot replaced by a regular file" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        createDirectories paths.checkout
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        snapshotExists <- isDirectory paths.external
        when snapshotExists $ removeDirectory paths.external
        createDirectories $ takeDirectory paths.external
        writeFile paths.external "not a directory"
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        result `shouldBe` Left (InvalidSnapshotLocation paths.external)

    it "rejects an override when the persisted snapshot is missing" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        replacementName <- encodeFS "replacement"
        let replacement = tmp </> replacementName
        createDirectories paths.checkout
        _ <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        removeDirectory paths.external
        result <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just replacement)
            fixtureTime
        result `shouldBe` Left (MissingSnapshot paths.external)
        exists replacement >>= (`shouldBe` False)
        loaded <-
          readRepositoryState paths.root paths.repositoryId paths.machineId
        case loaded of
          Right (Just state) -> state.intermediatePath `shouldBe` paths.external
          _ -> fail $ "Unexpected state: " <> show loaded

    it "leaves persistent files untouched under dry-run" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        createDirectories paths.legacy
        writeFile (paths.legacy </> paths.file) "base"
        _ <-
          dryRunIO $
            prepareRepositoryState
              paths.root
              paths.repositoryId
              paths.machineId
              paths.checkout
              Nothing
              fixtureTime
        exists paths.legacy >>= (`shouldBe` True)
        exists paths.external >>= (`shouldBe` False)

    it "records first apply per repository" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        prepared <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        localState <- case prepared of
          Right (state, CreatedRepositoryState) -> return state
          _ -> fail $ "Unexpected state: " <> show prepared
        _ <- markFirstApplied paths.root fixtureTime localState
        loaded <-
          readRepositoryState paths.root paths.repositoryId paths.machineId
        case loaded of
          Right (Just stored) -> stored.firstApplied `shouldBe` True
          _ -> fail $ "Unexpected state: " <> show loaded
        entries <-
          listDirectory $ repositoryStateDirectory paths.root paths.repositoryId
        filenames <- mapM decodePath entries
        filenames `shouldSatisfy` all (not . isPrefixOf "state.toml.tmp.")

    it "returns a state error when recording first apply fails" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        prepared <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        localState <- case prepared of
          Right (state, CreatedRepositoryState) -> return state
          _ -> fail $ "Unexpected state: " <> show prepared
        result <-
          runFailingReplaceIO
            (repositoryStatePath paths.root paths.repositoryId)
            $ markFirstApplied paths.root fixtureTime localState
        result `shouldSatisfy` isStateIOErrorResult

    it "reloads state before recording first apply" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        oldName <- encodeFS "old-snapshot"
        newName <- encodeFS "new-snapshot"
        let old = tmp </> oldName
        let new = tmp </> newName
        first <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just old)
            fixtureTime
        stale <- case first of
          Right (state, CreatedRepositoryState) -> return state
          _ -> fail $ "Unexpected state: " <> show first
        writeFile (old </> paths.file) "ancestor"
        migrated <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            (Just new)
            fixtureTime
        migrated `shouldSatisfy` isRightState
        _ <- markFirstApplied paths.root fixtureTime stale
        loaded <-
          readRepositoryState paths.root paths.repositoryId paths.machineId
        case loaded of
          Right (Just stored) -> do
            stored.intermediatePath `shouldBe` new
            stored.firstApplied `shouldBe` True
          _ -> fail $ "Unexpected state: " <> show loaded
        exists old >>= (`shouldBe` False)
        readFile (new </> paths.file) >>= (`shouldBe` "ancestor")

  describe "concurrent state persistence" $ do
    it "allocates a unique temporary path for every simultaneous writer" $
      withTempDir $ \tmp _ -> do
        results <-
          concurrently 32 $
            writeTemporaryFile tmp "state.toml.tmp." "temporary state"
        temporaryPaths <- case sequence results of
          Left err -> fail $ show err
          Right paths -> return paths
        length (nub temporaryPaths) `shouldBe` 32
        mapM_ removeFile temporaryPaths

    it "creates one machine identity for simultaneous first commands" $
      withTempDir $ \tmp _ -> do
        rootName <- encodeFS "state"
        let root = tmp </> rootName
        results <- concurrently 32 $ ensureMachineId root
        identifiers <- mapM expectMachineId results
        identifiers `shouldSatisfy` allEqual
        case identifiers of
          first : _ -> readMachineId root >>= (`shouldBe` Right (Just first))
          [] -> fail "No machine identity was created."

    it "uses independent temporary files for simultaneous state writes" $
      withTempDir $ \tmp _ -> do
        paths <- migrationPaths tmp
        prepared <-
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
        localState <- case prepared of
          Right (state, CreatedRepositoryState) -> return state
          _ -> fail $ "Unexpected state: " <> show prepared
        results <- concurrently 32 $ markFirstApplied paths.root fixtureTime localState
        mapM_ (`shouldSatisfy` isRight) results
        loaded <-
          readRepositoryState paths.root paths.repositoryId paths.machineId
        case loaded of
          Right (Just stored) -> stored.firstApplied `shouldBe` True
          _ -> fail $ "Unexpected state: " <> show loaded

#ifdef mingw32_HOST_OS
posixStateDocumentSpec :: Spec
posixStateDocumentSpec = pure ()
#else
posixStateDocumentSpec :: Spec
posixStateDocumentSpec = do
  it "rejects a FIFO machine identity without trying to read it" $
    withTempDir $ \root _ -> do
      machineName <- encodeFS "machine.toml"
      let machinePath = root </> machineName
      createDirectories root
      machinePath' <- decodeFS machinePath
      Posix.createNamedPipe machinePath' 0o600
      completed <- timeout 1000000 $ readMachineId root
      let expected =
            Left $
              MalformedState $
                "The machine identity "
                  <> Text.pack machinePath'
                  <> " is not a regular file."
      completed `shouldBe` Just expected

  it "rejects a FIFO repository state without trying to read it" $
    withTempDir $ \root _ -> do
      state <- fixtureState
      let statePath = repositoryStatePath root state.repositoryId
      createDirectories $ repositoryStateDirectory root state.repositoryId
      statePath' <- decodeFS statePath
      Posix.createNamedPipe statePath' 0o600
      completed <-
        timeout 1000000 $
          readRepositoryState root state.repositoryId state.machineId
      let expected =
            Left $
              MalformedState $
                "The repository state entry "
                  <> Text.pack statePath'
                  <> " is not a regular file."
      completed `shouldBe` Just expected
#endif

#ifdef mingw32_HOST_OS
posixSpecialFileSpec :: Spec
posixSpecialFileSpec = pure ()
#else
posixSpecialFileSpec :: Spec
posixSpecialFileSpec = do
  it "rejects a FIFO in a legacy snapshot without trying to copy it" $
    withTempDir $ \tmp _ -> do
      paths <- migrationPaths tmp
      fifoName <- encodeFS "named-pipe"
      let fifo = paths.legacy </> fifoName
      createDirectories paths.legacy
      fifo' <- decodeFS fifo
      Posix.createNamedPipe fifo' 0o600
      outcome <-
        timeout 1000000 $
          prepareRepositoryState
            paths.root
            paths.repositoryId
            paths.machineId
            paths.checkout
            Nothing
            fixtureTime
      case outcome of
        Just result -> result `shouldBe` Left (UnsupportedSnapshotEntry fifo)
        Nothing -> fail "Migration blocked while reading a FIFO."
      exists fifo >>= (`shouldBe` True)
      exists paths.external >>= (`shouldBe` False)

  it "rejects a FIFO in an explicitly adopted snapshot" $
    withTempDir $ \tmp _ -> do
      paths <- migrationPaths tmp
      customName <- encodeFS "custom-snapshot"
      fifoName <- encodeFS "named-pipe"
      let custom = tmp </> customName
      let fifo = custom </> fifoName
      createDirectories custom
      fifo' <- decodeFS fifo
      Posix.createNamedPipe fifo' 0o600
      result <-
        prepareRepositoryState
          paths.root
          paths.repositoryId
          paths.machineId
          paths.checkout
          (Just custom)
          fixtureTime
      result `shouldBe` Left (UnsupportedSnapshotEntry fifo)
      exists fifo >>= (`shouldBe` True)
      exists (repositoryStatePath paths.root paths.repositoryId)
        >>= (`shouldBe` False)
#endif


concurrently :: Int -> IO a -> IO [Either IOError a]
concurrently count action = do
  gate <- newEmptyMVar
  completions <- replicateM count $ do
    completion <- newEmptyMVar
    _ <- forkIO $ do
      readMVar gate
      result <- Exception.try action
      putMVar completion result
    return completion
  putMVar gate ()
  mapM takeMVar completions


expectMachineId
  :: Either IOError (Either StateError MachineId)
  -> IO MachineId
expectMachineId (Right (Right identifier)) = return identifier
expectMachineId result = fail $ "Unexpected machine identity result: " <> show result


allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (first : rest) = all (== first) rest


data MigrationPaths = MigrationPaths
  { root :: OsPath
  , checkout :: OsPath
  , legacy :: OsPath
  , external :: OsPath
  , file :: OsPath
  , repositoryId :: RepositoryId
  , machineId :: MachineId
  }


migrationPaths :: OsPath -> IO MigrationPaths
migrationPaths tmp = do
  stateName <- encodeFS "state"
  checkoutName <- encodeFS "checkout"
  legacyName <- encodeFS ".dojang"
  file <- encodeFS "file"
  repositoryId' <- repositoryId "123e4567-e89b-42d3-a456-426614174000"
  machineId' <- machineId "323e4567-e89b-42d3-a456-426614174000"
  let root = tmp </> stateName
  let checkout = tmp </> checkoutName
  return $
    MigrationPaths
      root
      checkout
      (checkout </> legacyName)
      (defaultIntermediatePath root repositoryId')
      file
      repositoryId'
      machineId'


fixtureState :: IO MachineState
fixtureState = do
  repositoryId' <- repositoryId "123e4567-e89b-42d3-a456-426614174000"
  machineId' <- machineId "323e4567-e89b-42d3-a456-426614174000"
  checkoutName <- encodeFS "checkout"
  manifestName <- encodeFS "dojang.toml"
  intermediateName <- encodeFS "state/snapshots/current"
  checkout <- System.Directory.OsPath.makeAbsolute checkoutName
  intermediate <- System.Directory.OsPath.makeAbsolute intermediateName
  return $
    MachineState
      1
      repositoryId'
      machineId'
      checkout
      (checkout </> manifestName)
      intermediate
      fixtureTime
      fixtureTime
      False


replacePathField :: Text.Text -> Text.Text -> Text.Text -> Text.Text
replacePathField field value =
  Text.unlines
    . fmap
      ( \line ->
          if (field <> " = ") `Text.isPrefixOf` line
            then field <> " = \"" <> value <> "\""
            else line
      )
    . Text.lines


fixtureTime :: UTCTime
fixtureTime =
  parseTimeOrError
    True
    defaultTimeLocale
    "%Y-%m-%dT%H:%M:%SZ"
    "2026-07-13T00:00:00Z"


repositoryId :: Text.Text -> IO RepositoryId
repositoryId value = case parseRepositoryId value of
  Right identifier -> return identifier
  Left err -> fail $ Text.unpack err


machineId :: Text.Text -> IO MachineId
machineId value = case parseMachineId value of
  Right identifier -> return identifier
  Left err -> fail $ Text.unpack err


isMalformed :: Either StateError a -> Bool
isMalformed (Left (MalformedState _)) = True
isMalformed _ = False


isStateIOErrorResult :: Either IOError (Either StateError a) -> Bool
isStateIOErrorResult (Right (Left (StateIOError _))) = True
isStateIOErrorResult _ = False


isMalformedMigrationMarker :: Either StateError a -> Bool
isMalformedMigrationMarker (Left (MalformedMigrationMarker _)) = True
isMalformedMigrationMarker _ = False


isOverlappingStateRoot :: Either StateError a -> Bool
isOverlappingStateRoot (Left (OverlappingStateRoot _ _)) = True
isOverlappingStateRoot _ = False


isProtectedSnapshot :: Either StateError a -> Bool
isProtectedSnapshot (Left (ProtectedSnapshotLocation _ _)) = True
isProtectedSnapshot _ = False


writeMigrationMarkerFixture :: OsPath -> OsPath -> OsPath -> IO ()
writeMigrationMarkerFixture marker source destination = do
  source' <- decodePath source
  destination' <- decodePath destination
  let escapedSource =
        Text.replace "\\" "\\\\" $
          Text.replace "\"" "\\\"" $
            Text.pack source'
  let escapedDestination =
        Text.replace "\\" "\\\\" $
          Text.replace "\"" "\\\"" $
            Text.pack destination'
  writeFile marker $
    encodeUtf8 $
      "source-path = \""
        <> escapedSource
        <> "\"\ndestination-path = \""
        <> escapedDestination
        <> "\"\n"


isRightState :: Either StateError (MachineState, MigrationResult) -> Bool
isRightState (Right _) = True
isRightState _ = False


isLeftIOError
  :: Either IOError (Either StateError (MachineState, MigrationResult)) -> Bool
isLeftIOError (Left _) = True
isLeftIOError _ = False


newtype FailingReplaceIO a
  = FailingReplaceIO (ReaderT OsPath (ExceptT IOError IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError IOError
    , MonadReader OsPath
    )


runFailingReplaceIO :: OsPath -> FailingReplaceIO a -> IO (Either IOError a)
runFailingReplaceIO target (FailingReplaceIO action) =
  runExceptT $ runReaderT action target


instance MonadFileSystem FailingReplaceIO where
  encodePath value = liftIO (encodePath value :: IO OsPath)
  decodePath value = liftIO (decodePath value :: IO FilePath)
  exists value = liftIO (exists value :: IO Bool)
  isFile value = liftIO (isFile value :: IO Bool)
  isRegularFile value = liftIO (isRegularFile value :: IO Bool)
  isDirectory value = liftIO (isDirectory value :: IO Bool)
  isSymlink value = liftIO (isSymlink value :: IO Bool)
  readFile value = liftIO (readFile value :: IO ByteString)
  writeFile filename contents = liftIO (writeFile filename contents :: IO ())
  replaceFile source destination = do
    target <- ask
    if destination == target
      then throwError $ userError "injected replace failure"
      else liftIO (replaceFile source destination :: IO ())
  writeTemporaryFile directory template contents =
    liftIO (writeTemporaryFile directory template contents :: IO OsPath)
  withFileLock _ action = action
  canonicalizePath value = liftIO (canonicalizePath value :: IO OsPath)
  readSymlinkTarget value = liftIO (readSymlinkTarget value :: IO OsPath)
  copyFile source destination = liftIO (copyFile source destination :: IO ())
  createDirectory value = liftIO (createDirectory value :: IO ())
  removeFile value = liftIO (removeFile value :: IO ())
  removeDirectory value = liftIO (removeDirectory value :: IO ())
  listDirectory value = liftIO (listDirectory value :: IO [OsPath])
  getFileSize value = liftIO (getFileSize value :: IO Integer)


newtype FailingCopyIO a
  = FailingCopyIO (ReaderT OsPath (ExceptT IOError IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError IOError
    , MonadReader OsPath
    )


runFailingCopyIO :: OsPath -> FailingCopyIO a -> IO (Either IOError a)
runFailingCopyIO target (FailingCopyIO action) =
  runExceptT $ runReaderT action target


instance MonadFileSystem FailingCopyIO where
  encodePath value = liftIO (encodePath value :: IO OsPath)
  decodePath value = liftIO (decodePath value :: IO FilePath)
  exists value = liftIO (exists value :: IO Bool)
  isFile value = liftIO (isFile value :: IO Bool)
  isRegularFile value = liftIO (isRegularFile value :: IO Bool)
  isDirectory value = liftIO (isDirectory value :: IO Bool)
  isSymlink value = liftIO (isSymlink value :: IO Bool)
  readFile value = liftIO (readFile value :: IO ByteString)
  writeFile filename contents = liftIO (writeFile filename contents :: IO ())
  replaceFile source destination =
    liftIO (replaceFile source destination :: IO ())
  writeTemporaryFile directory template contents =
    liftIO (writeTemporaryFile directory template contents :: IO OsPath)
  withFileLock _ action = action
  canonicalizePath value = liftIO (canonicalizePath value :: IO OsPath)
  readSymlinkTarget value = liftIO (readSymlinkTarget value :: IO OsPath)
  copyFile source destination = do
    target <- ask
    if destination == target
      then throwError $ userError "injected copy failure"
      else liftIO (copyFile source destination :: IO ())
  createDirectory value = liftIO (createDirectory value :: IO ())
  removeFile value = liftIO (removeFile value :: IO ())
  removeDirectory value = liftIO (removeDirectory value :: IO ())
  listDirectory value = liftIO (listDirectory value :: IO [OsPath])
  getFileSize value = liftIO (getFileSize value :: IO Integer)


newtype FailingRemoveIO a
  = FailingRemoveIO (ReaderT OsPath (ExceptT IOError IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError IOError
    , MonadReader OsPath
    )


runFailingRemoveIO :: OsPath -> FailingRemoveIO a -> IO (Either IOError a)
runFailingRemoveIO target (FailingRemoveIO action) =
  runExceptT $ runReaderT action target


instance MonadFileSystem FailingRemoveIO where
  encodePath value = liftIO (encodePath value :: IO OsPath)
  decodePath value = liftIO (decodePath value :: IO FilePath)
  exists value = liftIO (exists value :: IO Bool)
  isFile value = liftIO (isFile value :: IO Bool)
  isRegularFile value = liftIO (isRegularFile value :: IO Bool)
  isDirectory value = liftIO (isDirectory value :: IO Bool)
  isSymlink value = liftIO (isSymlink value :: IO Bool)
  readFile value = liftIO (readFile value :: IO ByteString)
  writeFile filename contents = liftIO (writeFile filename contents :: IO ())
  replaceFile source destination =
    liftIO (replaceFile source destination :: IO ())
  writeTemporaryFile directory template contents =
    liftIO (writeTemporaryFile directory template contents :: IO OsPath)
  withFileLock _ action = action
  canonicalizePath value = liftIO (canonicalizePath value :: IO OsPath)
  readSymlinkTarget value = liftIO (readSymlinkTarget value :: IO OsPath)
  copyFile source destination = liftIO (copyFile source destination :: IO ())
  createDirectory value = liftIO (createDirectory value :: IO ())
  removeFile value = do
    target <- ask
    if value == target
      then throwError $ userError "injected remove failure"
      else liftIO (removeFile value :: IO ())
  removeDirectory value = do
    target <- ask
    if value == target
      then throwError $ userError "injected remove failure"
      else liftIO (removeDirectory value :: IO ())
  listDirectory value = liftIO (listDirectory value :: IO [OsPath])
  getFileSize value = liftIO (getFileSize value :: IO Integer)
