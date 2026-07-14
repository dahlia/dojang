{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.MigrateSpec (spec) where

import Control.Concurrent
  ( MVar
  , forkIO
  , newEmptyMVar
  , putMVar
  , readMVar
  , takeMVar
  , tryPutMVar
  , tryReadMVar
  )
import Control.Exception (SomeException, bracket, bracket_)
import Control.Exception qualified as Exception
import Control.Monad (replicateM, void, when)
import Control.Monad.Except
  ( ExceptT
  , MonadError (throwError)
  , runExceptT
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..), runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString


#ifndef mingw32_HOST_OS
import Data.Bits ((.&.))
#endif
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import System.Directory.OsPath qualified as Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FileLock qualified as FileLock


#ifndef mingw32_HOST_OS
import System.Posix.Files qualified as Posix
#endif
import System.Exit (ExitCode)
import System.OsPath (OsPath, decodeFS, encodeFS, (</>))
import System.Timeout (timeout)
import Test.Hspec
  ( Spec
  , it
  , runIO
  , sequential
  , shouldSatisfy
  , shouldThrow
  , xit
  )
import Test.Hspec.Expectations.Pretty (shouldBe)
import Prelude hiding (readFile, writeFile)

import Dojang.App (AppEnv (..), runAppWithoutLogging)
import Dojang.Commands.Migrate (migrate)
import Dojang.ExitCodes (machineStateError, manifestReadError)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.TestUtils (withTempDir)


spec :: Spec
spec = sequential $ do
  symlinkAvailable <- runIO $ withTempDir $ \tmp _ -> do
    targetName <- encodeFS "target"
    linkName <- encodeFS "link"
    let target = tmp </> targetName
    writeFile target "target"
    result <-
      Exception.try (Directory.createFileLink target (tmp </> linkName))
        :: IO (Either IOError ())
    return $ either (const False) (const True) result
  let symlinkIt = if symlinkAvailable then it else xit

  symlinkIt "rejects a symlinked manifest without replacing it" $
    withTempDir $ \tmp _ -> do
      repositoryName <- encodeFS "repository"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      manifestTargetName <- encodeFS "manifest-target.toml"
      envName <- encodeFS "dojang-env.toml"
      let repository = tmp </> repositoryName
      let manifestPath = repository </> manifestName
      let manifestTarget = tmp </> manifestTargetName
      let home = tmp </> homeName
      let original = "[monikers]\n"
      createDirectories repository
      createDirectories home
      writeFile manifestTarget original
      Directory.createFileLink manifestTarget manifestPath
      let appEnv =
            AppEnv
              repository
              True
              Nothing
              (tmp </> stateName)
              manifestName
              envName
              False
              False
      withHome home (runAppWithoutLogging appEnv migrate)
        `shouldThrow` (== manifestReadError)
      isSymlink manifestPath >>= (`shouldBe` True)
      readFile manifestTarget >>= (`shouldBe` original)

  it "reports state-root creation failures as machine-state errors" $
    withTempDir $ \tmp _ -> do
      repositoryName <- encodeFS "repository"
      blockerName <- encodeFS "state-parent"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let repository = tmp </> repositoryName
      let blocker = tmp </> blockerName
      let home = tmp </> homeName
      createDirectories repository
      createDirectories home
      writeFile blocker "not a directory"
      writeFile (repository </> manifestName) "[monikers]\n"
      let appEnv =
            AppEnv
              repository
              True
              Nothing
              (blocker </> stateName)
              manifestName
              envName
              False
              False
      withHome home (runAppWithoutLogging appEnv migrate)
        `shouldThrow` (== machineStateError)

  it "reports state-lock acquisition failures as machine-state errors" $
    withTempDir $ \tmp _ -> do
      repositoryName <- encodeFS "repository"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      lockName <- encodeFS "manifest-identity.lock"
      let repository = tmp </> repositoryName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories repository
      createDirectories home
      createDirectories $ stateRoot </> lockName
      writeFile (repository </> manifestName) "[monikers]\n"
      let appEnv =
            AppEnv
              repository
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      withHome home (runAppWithoutLogging appEnv migrate)
        `shouldThrow` (== machineStateError)

  it "reports manifest I/O failures as manifest-read errors" $
    withTempDir $ \tmp _ -> do
      repositoryName <- encodeFS "repository"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let repository = tmp </> repositoryName
      let home = tmp </> homeName
      createDirectories $ repository </> manifestName
      createDirectories home
      let appEnv =
            AppEnv
              repository
              True
              Nothing
              (tmp </> stateName)
              manifestName
              envName
              False
              False
      withHome home (runAppWithoutLogging appEnv migrate)
        `shouldThrow` (== manifestReadError)

  it "keeps the original manifest when its replacement cannot be written" $
    withTempDir $ \tmp _ -> do
      repositoryName <- encodeFS "repository"
      stateName <- encodeFS "state"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let repository = tmp </> repositoryName
      let manifestPath = repository </> manifestName
      createDirectories repository
      let original =
            Text.unlines
              [ "[dirs]"
              , "[files]"
              , "[ignores]"
              , "[monikers]"
              ]
      writeFile manifestPath $ encodeUtf8 original
      let appEnv =
            AppEnv
              repository
              True
              Nothing
              (tmp </> stateName)
              manifestName
              envName
              False
              False
      outcome <-
        Exception.try $
          runFailingManifestWrite manifestPath $
            runAppWithoutLogging appEnv migrate
          :: IO (Either ExitCode (Either IOError ExitCode))
      case outcome of
        Left exitCode -> exitCode `shouldBe` manifestReadError
        Right result -> fail $ "Unexpected migration result: " <> show result
      readFile manifestPath >>= (`shouldBe` encodeUtf8 original)

  posixPermissionSpec

  it "replaces a read-only manifest when its directory is writable" $
    withTempDir $ \tmp _ -> do
      repositoryName <- encodeFS "repository"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let repository = tmp </> repositoryName
      let manifestPath = repository </> manifestName
      let home = tmp </> homeName
      createDirectories repository
      createDirectories home
      writeFile manifestPath "[monikers]\n"
      originalPermissions <- Directory.getPermissions manifestPath
      let readOnly = Directory.setOwnerWritable False originalPermissions
      let restoreWritable = do
            current <- Directory.getPermissions manifestPath
            Directory.setPermissions
              manifestPath
              (Directory.setOwnerWritable True current)
      bracket_
        (Directory.setPermissions manifestPath readOnly)
        restoreWritable
        $ do
          let appEnv =
                AppEnv
                  repository
                  True
                  Nothing
                  (tmp </> stateName)
                  manifestName
                  envName
                  False
                  False
          _ <- withHome home $ runAppWithoutLogging appEnv migrate
          migrated <- readFile manifestPath
          migrated `shouldSatisfy` ByteString.isInfixOf "repository-id"
          migratedPermissions <- Directory.getPermissions manifestPath
          Directory.writable migratedPermissions `shouldBe` False

  it "rejects invalid UTF-8 without replacing the manifest" $
    withTempDir $ \tmp _ -> do
      repositoryName <- encodeFS "repository"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let repository = tmp </> repositoryName
      let manifestPath = repository </> manifestName
      let home = tmp </> homeName
      let original =
            "[monikers]\n# invalid comment: "
              <> ByteString.singleton 0xff
              <> "\n"
      createDirectories repository
      createDirectories home
      writeFile manifestPath original
      let appEnv =
            AppEnv
              repository
              True
              Nothing
              (tmp </> stateName)
              manifestName
              envName
              False
              False
      result <-
        Exception.try (withHome home $ runAppWithoutLogging appEnv migrate)
          :: IO (Either ExitCode ExitCode)
      result `shouldBe` Left manifestReadError
      readFile manifestPath >>= (`shouldBe` original)

  it "rejects a machine-state root inside the legacy snapshot before writing" $
    withTempDir $ \tmp _ -> do
      repositoryName <- encodeFS "repository"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      legacyName <- encodeFS ".dojang"
      stateName <- encodeFS "state"
      externalName <- encodeFS "external"
      trackedName <- encodeFS "tracked"
      let repository = tmp </> repositoryName
      let legacy = repository </> legacyName
      let stateRoot = legacy </> stateName
      let external = tmp </> externalName
      let manifestPath = repository </> manifestName
      let home = tmp </> homeName
      let original = "[monikers]\n"
      createDirectories legacy
      createDirectories home
      writeFile manifestPath original
      writeFile (legacy </> trackedName) "ancestor"
      let appEnv =
            AppEnv
              repository
              True
              (Just external)
              stateRoot
              manifestName
              envName
              False
              False
      withHome home (runAppWithoutLogging appEnv migrate)
        `shouldThrow` (== machineStateError)
      readFile manifestPath >>= (`shouldBe` original)
      readFile (legacy </> trackedName) >>= (`shouldBe` "ancestor")
      exists stateRoot >>= (`shouldBe` False)
      exists external >>= (`shouldBe` False)

  it "creates one repository identity during concurrent migration" $
    withTempDir $ \tmp _ -> do
      repositoryName <- encodeFS "repository"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      legacyName <- encodeFS ".dojang"
      fileName <- encodeFS "tracked"
      repositoriesName <- encodeFS "repositories"
      let repository = tmp </> repositoryName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      let manifestPath = repository </> manifestName
      createDirectories $ repository </> legacyName
      createDirectories home
      writeFile manifestPath "[monikers]\n"
      writeFile (repository </> legacyName </> fileName) "ancestor"
      readCount <- newIORef 0
      secondRead <- newEmptyMVar
      lockAttempted <- newEmptyMVar
      overlapped <- newIORef False
      let gate =
            ManifestReadGate
              manifestPath
              readCount
              secondRead
              lockAttempted
              overlapped
      let appEnv =
            AppEnv
              repository
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
            runCoordinatedMigrationIO gate $
              runAppWithoutLogging appEnv migrate
      results `shouldSatisfy` all isRightResult
      readIORef overlapped >>= (`shouldBe` False)
      repositoryStates <- listDirectory $ stateRoot </> repositoriesName
      length repositoryStates `shouldBe` 1
#ifdef mingw32_HOST_OS
posixPermissionSpec :: Spec
posixPermissionSpec = pure ()
#else
posixPermissionSpec :: Spec
posixPermissionSpec = do
  it "preserves executable manifest permissions during replacement" $
    withTempDir $ \tmp _ -> do
      repositoryName <- encodeFS "repository"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let repository = tmp </> repositoryName
      let manifestPath = repository </> manifestName
      let home = tmp </> homeName
      createDirectories repository
      createDirectories home
      writeFile manifestPath "[monikers]\n"
      permissions <- Directory.getPermissions manifestPath
      Directory.setPermissions
        manifestPath
        (Directory.setOwnerExecutable True permissions)
      let appEnv =
            AppEnv
              repository
              True
              Nothing
              (tmp </> stateName)
              manifestName
              envName
              False
              False
      _ <- withHome home $ runAppWithoutLogging appEnv migrate
      migratedPermissions <- Directory.getPermissions manifestPath
      Directory.executable migratedPermissions `shouldBe` True

  it "preserves group and other manifest permission bits" $
    withTempDir $ \tmp _ -> do
      repositoryName <- encodeFS "repository"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let repository = tmp </> repositoryName
      let manifestPath = repository </> manifestName
      let home = tmp </> homeName
      createDirectories repository
      createDirectories home
      writeFile manifestPath "[monikers]\n"
      manifestPath' <- decodeFS manifestPath
      Posix.setFileMode manifestPath' 0o640
      let appEnv =
            AppEnv
              repository
              True
              Nothing
              (tmp </> stateName)
              manifestName
              envName
              False
              False
      _ <- withHome home $ runAppWithoutLogging appEnv migrate
      mode <- Posix.fileMode <$> Posix.getFileStatus manifestPath'
      mode .&. 0o777 `shouldBe` 0o640
#endif


withHome :: OsPath -> IO a -> IO a
withHome home action = bracket (lookupEnv "HOME") restore $ \_ -> do
  home' <- decodeFS home
  setEnv "HOME" home'
  action
 where
  restore Nothing = unsetEnv "HOME"
  restore (Just previous) = setEnv "HOME" previous


data ManifestReadGate = ManifestReadGate
  { manifestPath :: OsPath
  , readCount :: IORef Int
  , secondRead :: MVar ()
  , lockAttempted :: MVar ()
  , overlapped :: IORef Bool
  }


newtype CoordinatedMigrationIO a
  = CoordinatedMigrationIO
      (ReaderT ManifestReadGate IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError IOError
    , MonadReader ManifestReadGate
    )


runCoordinatedMigrationIO
  :: ManifestReadGate
  -> CoordinatedMigrationIO a
  -> IO a
runCoordinatedMigrationIO gate (CoordinatedMigrationIO action) =
  runReaderT action gate


instance MonadFileSystem CoordinatedMigrationIO where
  encodePath value = liftIO (encodePath value :: IO OsPath)
  decodePath value = liftIO (decodePath value :: IO FilePath)
  exists value = liftIO (exists value :: IO Bool)
  isFile value = liftIO (isFile value :: IO Bool)
  isRegularFile value = liftIO (isRegularFile value :: IO Bool)
  isDirectory value = liftIO (isDirectory value :: IO Bool)
  isSymlink value = liftIO (isSymlink value :: IO Bool)
  readFile filename = do
    gate <- ask
    when (filename == gate.manifestPath) $ liftIO $ do
      readNumber <-
        atomicModifyIORef' gate.readCount $ \count ->
          let next = count + 1 in (next, next)
      case readNumber of
        1 -> do
          protected <- tryReadMVar gate.lockAttempted
          case protected of
            Just () -> return ()
            Nothing -> do
              overlap <- timeout 2000000 $ readMVar gate.secondRead
              writeIORef gate.overlapped $ case overlap of
                Just () -> True
                Nothing -> False
        2 -> void $ tryPutMVar gate.secondRead ()
        _ -> return ()
    liftIO (readFile filename :: IO ByteString)
  writeFile filename contents = liftIO (writeFile filename contents :: IO ())
  replaceFile source destination =
    liftIO (replaceFile source destination :: IO ())
  copyFileWithMetadata source destination =
    liftIO (copyFileWithMetadata source destination :: IO ())
  writeTemporaryFile directory template contents =
    liftIO (writeTemporaryFile directory template contents :: IO OsPath)
  withFileLock lockPath (CoordinatedMigrationIO action) =
    CoordinatedMigrationIO $ ReaderT $ \gate -> do
      void $ tryPutMVar gate.lockAttempted ()
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


isRightResult :: Either SomeException a -> Bool
isRightResult (Right _) = True
isRightResult _ = False


newtype FailingManifestWriteIO a
  = FailingManifestWriteIO
      (ReaderT OsPath (ExceptT IOError IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError IOError
    , MonadReader OsPath
    )


runFailingManifestWrite
  :: OsPath
  -> FailingManifestWriteIO a
  -> IO (Either IOError a)
runFailingManifestWrite target (FailingManifestWriteIO action) =
  runExceptT $ runReaderT action target


instance MonadFileSystem FailingManifestWriteIO where
  encodePath value = liftIO (encodePath value :: IO OsPath)
  decodePath value = liftIO (decodePath value :: IO FilePath)
  exists value = liftIO (exists value :: IO Bool)
  isFile value = liftIO (isFile value :: IO Bool)
  isRegularFile value = liftIO (isRegularFile value :: IO Bool)
  isDirectory value = liftIO (isDirectory value :: IO Bool)
  isSymlink value = liftIO (isSymlink value :: IO Bool)
  readFile value = liftIO (readFile value :: IO ByteString)
  writeFile filename contents = do
    target <- ask
    if filename == target
      then do
        liftIO (writeFile filename "partial" :: IO ())
        throwError $ userError "injected manifest write failure"
      else liftIO (writeFile filename contents :: IO ())
  replaceFile source destination =
    liftIO (replaceFile source destination :: IO ())
  writeTemporaryFile directory template _ = do
    temporary <- liftIO $ encodePath $ template <> "injected"
    let filename = directory </> temporary
    liftIO (writeFile filename "partial" :: IO ())
    throwError $ userError "injected temporary write failure"
  withFileLock _ action = action
  canonicalizePath value = liftIO (canonicalizePath value :: IO OsPath)
  readSymlinkTarget value = liftIO (readSymlinkTarget value :: IO OsPath)
  copyFile source destination = liftIO (copyFile source destination :: IO ())
  createDirectory value = liftIO (createDirectory value :: IO ())
  removeFile value = liftIO (removeFile value :: IO ())
  removeDirectory value = liftIO (removeDirectory value :: IO ())
  listDirectory value = liftIO (listDirectory value :: IO [OsPath])
  getFileSize value = liftIO (getFileSize value :: IO Integer)
