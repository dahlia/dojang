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
import Control.Exception (SomeException, bracket)
import Control.Exception qualified as Exception
import Control.Monad (replicateM, void, when)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..), runReaderT)
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FileLock qualified as FileLock
import System.OsPath (OsPath, decodeFS, encodeFS, (</>))
import Test.Hspec (Spec, it, sequential)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldThrow)
import Prelude hiding (init, readFile, writeFile)

import Dojang.App (AppEnv (..), runAppWithoutLogging)
import Dojang.Commands.Init qualified as Init
import Dojang.ExitCodes (machineStateError, manifestAlreadyExists)
import Dojang.MonadFileSystem
  ( MonadFileSystem (..)
  )
import Dojang.TestUtils (withTempDir)
import Dojang.Types.MachineState
  ( MachineState (firstApplied)
  , listRepositoryStates
  , readMachineId
  )
import Dojang.Types.Registry
  ( Registry (Registry)
  , registryFilename
  , writeRegistry
  )


spec :: Spec
spec = sequential $ do
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
      length (filter isSuccessful results) `shouldBe` 1
      length (filter isManifestAlreadyExists results) `shouldBe` 1
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

  it "does not commit initialization output for a malformed legacy registry" $
    withTempDir $ \tmp _ -> do
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


withHome :: OsPath -> IO a -> IO a
withHome home action = bracket (lookupEnv "HOME") restore $ \_ -> do
  home' <- decodeFS home
  setEnv "HOME" home'
  action
 where
  restore Nothing = unsetEnv "HOME"
  restore (Just previous) = setEnv "HOME" previous


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
  exists value = do
    gate <- ask
    when (value == gate.manifestPath) $ liftIO $ do
      checkNumber <-
        atomicModifyIORef' gate.checkCount $ \count ->
          let next = count + 1 in (next, next)
      case checkNumber of
        1 -> readMVar gate.secondCheck
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
