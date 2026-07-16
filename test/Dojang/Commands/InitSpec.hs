{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.InitSpec (spec) where

import Control.Concurrent
  ( MVar
  , forkIO
  , newEmptyMVar
  , putMVar
  , readMVar
  , takeMVar
  , tryPutMVar
  )
import Control.Exception (SomeException)
import Control.Exception qualified as Exception
import Control.Monad (replicateM, void, when)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LogLevel (LevelWarn), fromLogStr)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..), runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , modifyIORef'
  , newIORef
  , readIORef
  )
import Data.Map.Strict qualified as Map
import System.Exit (ExitCode (ExitSuccess))
import System.FileLock qualified as FileLock
import System.Info (os)
import System.OsPath (OsPath, decodeFS, encodeFS, (</>))
import System.Timeout (timeout)
import Test.Hspec (Spec, it, sequential, xit)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldReturn, shouldThrow)
import Prelude hiding (init, readFile, writeFile)

import Dojang.App
  ( AppEnv (..)
  , prepareMachineState
  , runAppWithLogging
  , runAppWithoutLogging
  )
import Dojang.Commands.Init qualified as Init
import Dojang.ExitCodes
  ( cliError
  , envFileReadError
  , machineStateError
  , manifestAlreadyExists
  , missingMachineFactError
  )
import Dojang.MonadFileSystem
  ( MonadFileSystem (..)
  , dryRunIO
  )
import Dojang.TestUtils (withHome, withTempDir)
import Dojang.Types.MachineState
  ( MachineState (declaredFacts, firstApplied, updatedTime)
  , listRepositoryStates
  , readMachineId
  , repositoryStatePath
  , updateMachineFacts
  )
import Dojang.Types.Manifest (Manifest (Manifest))
import Dojang.Types.Registry
  ( Registry (Registry)
  , registryFilename
  , writeRegistry
  )
import Dojang.Types.RepositoryId (parseRepositoryId)


spec :: Spec
spec = sequential $ do
  let redirectedHomeIt = if os == "mingw32" then xit else it

  it "creates one repository identity during concurrent initialization" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      repositoriesName <- encodeFS "repositories"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      let manifestPath = checkout </> manifestName
      createDirectories checkout
      createDirectories home
      checkCount <- newIORef 0
      secondCheck <- newEmptyMVar
      let gate = ManifestCheckGate manifestPath checkCount secondCheck
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      results <-
        withHome home $
          concurrently 2 $
            runCoordinatedInitIO gate $
              runAppWithoutLogging appEnv $
                Init.init [Init.Amd64Linux] True
      any isSuccessful results `shouldBe` True
      all
        (\result -> isSuccessful result || isManifestAlreadyExists result)
        results
        `shouldBe` True
      repositoryStates <- listDirectory $ stateRoot </> repositoriesName
      length repositoryStates `shouldBe` 1

  it "does not inherit legacy first-apply history for a new identity" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      writeRegistry (home </> registryFilename) $ Registry checkout
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      result <-
        withHome home $
          runAppWithoutLogging appEnv $
            Init.init [Init.Amd64Linux] True
      result `shouldBe` ExitSuccess
      machineResult <- readMachineId stateRoot
      machineId' <- case machineResult of
        Right (Just identifier) -> return identifier
        unexpected -> fail $ "Unexpected machine identity: " <> show unexpected
      statesResult <- listRepositoryStates stateRoot machineId'
      states <- case statesResult of
        Right values -> return values
        Left err -> fail $ "Unexpected repository state error: " <> show err
      fmap (.firstApplied) states `shouldBe` [False]

  it "validates fact options before publishing a new repository" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      factsName <- encodeFS "facts.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      withHome
        home
        ( runAppWithoutLogging appEnv $
            Init.initWithFacts [Init.Amd64Linux] True Nothing ["invalid"]
        )
        `shouldThrow` (== cliError)
      exists (checkout </> manifestName) >>= (`shouldBe` False)
      exists stateRoot >>= (`shouldBe` False)
      withHome
        home
        ( runAppWithoutLogging appEnv $
            Init.initWithFacts
              [Init.Amd64Linux]
              True
              (Just factsName)
              []
        )
        `shouldThrow` (== envFileReadError)
      exists (checkout </> manifestName) >>= (`shouldBe` False)
      exists stateRoot >>= (`shouldBe` False)

  it "validates fact options before preparing existing repository state" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      factsName <- encodeFS "facts.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      writeFile
        (checkout </> manifestName)
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      withHome
        home
        ( runAppWithoutLogging appEnv $
            Init.initWithFacts [] True Nothing ["invalid"]
        )
        `shouldThrow` (== cliError)
      exists stateRoot >>= (`shouldBe` False)
      withHome
        home
        ( runAppWithoutLogging appEnv $
            Init.initWithFacts [] True (Just factsName) []
        )
        `shouldThrow` (== envFileReadError)
      exists stateRoot >>= (`shouldBe` False)
      writeFile (checkout </> envName) "[facts\n"
      withHome
        home
        (runAppWithoutLogging appEnv $ Init.initWithFacts [] True Nothing [])
        `shouldThrow` (== envFileReadError)
      exists stateRoot >>= (`shouldBe` False)

  it "reports parser warnings from a facts file" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      factsName <- encodeFS "facts.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      writeFile (checkout </> factsName) "[fact]\nclass = \"work\"\n"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      warnings <- newIORef []
      result <-
        withHome home $
          runAppWithLogging
            appEnv
            ( Init.initWithFacts
                [Init.Amd64Linux]
                True
                (Just factsName)
                []
            )
            ( \_ _ level message ->
                when (level == LevelWarn) $
                  modifyIORef' warnings (fromLogStr message :)
            )
      result `shouldBe` ExitSuccess
      messages <- readIORef warnings
      any (ByteString.isInfixOf "unexpected key") messages `shouldBe` True

  it "validates a stored facts file before moving existing state" $
    withTempDir $ \tmp _ -> do
      oldCheckoutName <- encodeFS "old-checkout"
      movedCheckoutName <- encodeFS "moved-checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      factsName <- encodeFS "facts.toml"
      let oldCheckout = tmp </> oldCheckoutName
      let movedCheckout = tmp </> movedCheckoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories oldCheckout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let manifestSource =
            "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      writeFile (oldCheckout </> manifestName) manifestSource
      let oldAppEnv =
            AppEnv
              oldCheckout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
      state <-
        withHome home $
          runAppWithoutLogging oldAppEnv $
            prepareMachineState manifest
      enrolled <-
        updateMachineFacts
          stateRoot
          state.updatedTime
          state
          (Just factsName)
          Map.empty
      case enrolled of
        Left err -> fail $ "Unexpected state error: " <> show err
        Right _ -> return ()
      let statePath = repositoryStatePath stateRoot repositoryId
      before <- readFile statePath
      removeFile $ oldCheckout </> manifestName
      removeDirectory oldCheckout
      createDirectories movedCheckout
      writeFile (movedCheckout </> manifestName) manifestSource
      let movedAppEnv = oldAppEnv{sourceDirectory = movedCheckout}
      withHome
        home
        (runAppWithoutLogging movedAppEnv $ Init.initWithFacts [] True Nothing [])
        `shouldThrow` (== envFileReadError)
      readFile statePath `shouldReturn` before

  it "requires and enrolls facts in an existing repository" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      _ <-
        withHome home $
          runAppWithoutLogging appEnv $
            Init.init [Init.Amd64Linux] True
      manifest <- readFile $ checkout </> manifestName
      writeFile
        (checkout </> manifestName)
        ( manifest
            <> "\n[monikers.everywhere]\n"
            <> "when = \"always\"\n"
            <> "\n[[files.unreachable]]\n"
            <> "when = \"moniker = everywhere || fact.branch = yes\"\n"
            <> "path = \"$HOME/unreachable\"\n"
            <> "\n[[files.unreachable]]\n"
            <> "when = \"fact.unreachable = yes\"\n"
            <> "path = \"$HOME/also-unreachable\"\n"
        )
      _ <-
        withHome home $
          runAppWithoutLogging appEnv $
            Init.initWithFacts [] True Nothing []
      manifestWithUnreachable <- readFile $ checkout </> manifestName
      writeFile
        (checkout </> manifestName)
        ( manifestWithUnreachable
            <> "\n[[hooks.pre-status]]\n"
            <> "command = \"true\"\n"
            <> "when = \"fact.class = work\"\n"
            <> "\n[[hooks.pre-status]]\n"
            <> "command = \"true\"\n"
            <> "when = \"fact.location not in ()\"\n"
        )
      withHome
        home
        (runAppWithoutLogging appEnv $ Init.initWithFacts [] True Nothing [])
        `shouldThrow` (== missingMachineFactError)
      withHome
        home
        ( runAppWithoutLogging appEnv $
            Init.initWithFacts [] True Nothing ["class=work"]
        )
        `shouldThrow` (== missingMachineFactError)
      _ <-
        withHome home $
          runAppWithoutLogging appEnv $
            Init.initWithFacts
              []
              True
              Nothing
              ["class=work", "location=home"]
      machineResult <- readMachineId stateRoot
      machineId' <- case machineResult of
        Right (Just identifier) -> return identifier
        unexpected -> fail $ "Unexpected machine identity: " <> show unexpected
      statesResult <- listRepositoryStates stateRoot machineId'
      states <- case statesResult of
        Right values -> return values
        Left err -> fail $ "Unexpected repository state error: " <> show err
      fmap (Map.lookup "class" . (.declaredFacts)) states
        `shouldBe` [Just "work"]
      fmap (Map.lookup "location" . (.declaredFacts)) states
        `shouldBe` [Just "home"]
      _ <-
        withHome home $
          dryRunIO $
            runAppWithoutLogging appEnv{dryRun = True} $
              Init.initWithFacts [] True Nothing ["class=personal"]
      statesAfterDryRunResult <- listRepositoryStates stateRoot machineId'
      statesAfterDryRun <- case statesAfterDryRunResult of
        Right values -> return values
        Left err -> fail $ "Unexpected repository state error: " <> show err
      fmap (Map.lookup "class" . (.declaredFacts)) statesAfterDryRun
        `shouldBe` [Just "work"]

  it "reports state-root creation failures as machine-state errors" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      blockerName <- encodeFS "state-parent"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let blocker = tmp </> blockerName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      writeFile blocker "not a directory"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              (blocker </> stateName)
              manifestName
              envName
              False
              False
      withHome
        home
        (runAppWithoutLogging appEnv $ Init.init [Init.Amd64Linux] True)
        `shouldThrow` (== machineStateError)
      exists (checkout </> manifestName) >>= (`shouldBe` False)

  it "reports state-lock acquisition failures as machine-state errors" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      lockName <- encodeFS "manifest-identity.lock"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      createDirectories $ stateRoot </> lockName
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      withHome
        home
        (runAppWithoutLogging appEnv $ Init.init [Init.Amd64Linux] True)
        `shouldThrow` (== machineStateError)
      exists (checkout </> manifestName) >>= (`shouldBe` False)

  it "does not save the manifest when intermediate validation fails" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      intermediateName <- encodeFS "intermediate"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      let intermediate = tmp </> intermediateName
      createDirectories checkout
      createDirectories home
      writeFile intermediate "not a directory"
      let appEnv =
            AppEnv
              checkout
              True
              (Just intermediate)
              stateRoot
              manifestName
              envName
              False
              False
      withHome
        home
        (runAppWithoutLogging appEnv $ Init.init [Init.Amd64Linux] True)
        `shouldThrow` (== machineStateError)
      exists (checkout </> manifestName) >>= (`shouldBe` False)
      removeFile intermediate
      retried <-
        withHome
          home
          (runAppWithoutLogging appEnv $ Init.init [Init.Amd64Linux] True)
      retried `shouldBe` ExitSuccess
      exists (checkout </> manifestName) >>= (`shouldBe` True)

  it "does not publish state when checkout output fails" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      homeRouteName <- encodeFS "HOME"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      let blockedRoute = checkout </> homeRouteName
      createDirectories checkout
      createDirectories home
      writeFile blockedRoute "not a directory"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      withHome
        home
        (runAppWithoutLogging appEnv $ Init.init [Init.Amd64Linux] True)
        `shouldThrow` (== machineStateError)
      exists (checkout </> manifestName) >>= (`shouldBe` False)
      machineResult <- readMachineId stateRoot
      machineId' <- case machineResult of
        Right (Just identifier) -> return identifier
        unexpected -> fail $ "Unexpected machine identity: " <> show unexpected
      statesResult <- listRepositoryStates stateRoot machineId'
      statesResult `shouldBe` Right []
      removeFile blockedRoute
      retried <-
        withHome
          home
          (runAppWithoutLogging appEnv $ Init.init [Init.Amd64Linux] True)
      retried `shouldBe` ExitSuccess
      retriedStates <- listRepositoryStates stateRoot machineId'
      fmap length retriedStates `shouldBe` Right 1

  it "refuses to consume a pre-existing legacy snapshot" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      legacyName <- encodeFS ".dojang"
      recoveryName <- encodeFS "recovery"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      let legacy = checkout </> legacyName
      let recovery = legacy </> recoveryName
      createDirectories legacy
      createDirectories home
      writeFile recovery "preserve me"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      withHome
        home
        (runAppWithoutLogging appEnv $ Init.init [Init.Amd64Linux] True)
        `shouldThrow` (== machineStateError)
      readFile recovery >>= (`shouldBe` "preserve me")
      exists (checkout </> manifestName) >>= (`shouldBe` False)

  redirectedHomeIt
    "does not commit initialization output for a malformed legacy registry"
    $ withTempDir
    $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      homeRouteName <- encodeFS "HOME"
      configRouteName <- encodeFS "XDG_CONFIG_HOME"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      writeFile (home </> registryFilename) "invalid toml"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      withHome
        home
        (runAppWithoutLogging appEnv $ Init.init [Init.Amd64Linux] True)
        `shouldThrow` (== machineStateError)
      exists (checkout </> manifestName) >>= (`shouldBe` False)
      exists (checkout </> homeRouteName) >>= (`shouldBe` False)
      exists (checkout </> configRouteName) >>= (`shouldBe` False)
      exists stateRoot >>= (`shouldBe` False)
      removeFile $ home </> registryFilename
      retried <-
        withHome
          home
          (runAppWithoutLogging appEnv $ Init.init [Init.Amd64Linux] True)
      retried `shouldBe` ExitSuccess
      exists (checkout </> manifestName) >>= (`shouldBe` True)


data ManifestCheckGate = ManifestCheckGate
  { manifestPath :: OsPath
  , checkCount :: IORef Int
  , secondCheck :: MVar ()
  }


newtype CoordinatedInitIO a
  = CoordinatedInitIO
      (ReaderT ManifestCheckGate IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError IOError
    , MonadReader ManifestCheckGate
    )


runCoordinatedInitIO
  :: ManifestCheckGate
  -> CoordinatedInitIO a
  -> IO a
runCoordinatedInitIO gate (CoordinatedInitIO action) = runReaderT action gate


instance MonadFileSystem CoordinatedInitIO where
  encodePath value = liftIO (encodePath value :: IO OsPath)
  decodePath value = liftIO (decodePath value :: IO FilePath)
  getCurrentDirectory = liftIO (getCurrentDirectory :: IO OsPath)
  getHomeDirectory = liftIO (getHomeDirectory :: IO OsPath)
  exists value = do
    gate <- ask
    when (value == gate.manifestPath) $ liftIO $ do
      checkNumber <-
        atomicModifyIORef' gate.checkCount $ \count ->
          let next = count + 1 in (next, next)
      case checkNumber of
        1 -> do
          arrived <- timeout 5000000 $ readMVar gate.secondCheck
          case arrived of
            Nothing ->
              Exception.throwIO $
                userError "second manifest check never arrived"
            Just () -> return ()
        2 -> void $ tryPutMVar gate.secondCheck ()
        _ -> return ()
    liftIO (exists value :: IO Bool)
  isFile value = liftIO (isFile value :: IO Bool)
  isRegularFile value = liftIO (isRegularFile value :: IO Bool)
  isDirectory value = liftIO (isDirectory value :: IO Bool)
  isSymlink value = liftIO (isSymlink value :: IO Bool)
  readFile filename = liftIO (readFile filename :: IO ByteString)
  writeFile filename contents = liftIO (writeFile filename contents :: IO ())
  replaceFile source destination =
    liftIO (replaceFile source destination :: IO ())
  copyFileWithMetadata source destination =
    liftIO (copyFileWithMetadata source destination :: IO ())
  copyFilePermissions source destination =
    liftIO (copyFilePermissions source destination :: IO ())
  writeTemporaryFile directory template contents =
    liftIO (writeTemporaryFile directory template contents :: IO OsPath)
  withFileLock lockPath (CoordinatedInitIO action) =
    CoordinatedInitIO $ ReaderT $ \gate -> do
      lockPath' <- decodeFS lockPath
      FileLock.withFileLock lockPath' FileLock.Exclusive $
        const $
          runReaderT action gate
  canonicalizePath value = liftIO (canonicalizePath value :: IO OsPath)
  readSymlinkTarget value = liftIO (readSymlinkTarget value :: IO OsPath)
  copyFile source destination = liftIO (copyFile source destination :: IO ())
  createDirectory value = liftIO (createDirectory value :: IO ())
  removeFile value = liftIO (removeFile value :: IO ())
  removeDirectory value = liftIO (removeDirectory value :: IO ())
  listDirectory value = liftIO (listDirectory value :: IO [OsPath])
  getFileSize value = liftIO (getFileSize value :: IO Integer)


concurrently
  :: Int
  -> IO a
  -> IO [Either SomeException a]
concurrently count action = do
  start <- newEmptyMVar
  results <- replicateM count newEmptyMVar
  mapM_
    ( \result ->
        void $
          forkIO $ do
            readMVar start
            outcome <- Exception.try action
            putMVar result outcome
    )
    results
  putMVar start ()
  mapM takeMVar results


isSuccessful :: Either SomeException ExitCode -> Bool
isSuccessful (Right ExitSuccess) = True
isSuccessful _ = False


isManifestAlreadyExists :: Either SomeException ExitCode -> Bool
isManifestAlreadyExists (Left exception) =
  Exception.fromException exception == Just manifestAlreadyExists
isManifestAlreadyExists _ = False
