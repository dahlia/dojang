{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.TargetTrackingSpec (spec) where

import Control.Exception qualified as Exception
import Control.Monad.Except
  ( ExceptT
  , MonadError (catchError, throwError)
  , runExceptT
  )
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (linear)
import System.Directory.OsPath qualified
import System.Info (os)

import Control.Monad (when)
import Dojang.Types.ManagedTarget
  ( TargetFingerprint (SymlinkFingerprint)
  )


#ifndef mingw32_HOST_OS
import Dojang.Types.ManagedTarget
  ( TargetFingerprint (FileFingerprint)
  )
import System.OsPath (decodeFS)
import System.Posix.Files qualified as Posix
import System.Timeout (timeout)
#endif

import System.OsPath
  ( OsPath
  , encodeFS
  , isAbsolute
  , normalise
  , takeDirectory
  , (</>)
  )
import Test.Hspec (Spec, describe, it, sequential)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldNotBe
  , shouldReturn
  , shouldSatisfy
  , shouldThrow
  )
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))
import Prelude hiding (readFile, writeFile)

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.MonadFileSystem qualified as FileSystem
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Codec (identityCodecSpec)
import Dojang.Types.Codec.Evaluate (opaqueBytes)
import Dojang.Types.Context
  ( FileCorrespondence (..)
  , FileDeltaKind (Unchanged)
  , FileEntry (..)
  , FileStat (Directory, File)
  , ManagedCorrespondence (..)
  )
import Dojang.Types.FileRoute
  ( RouteKind (CopyRoute, SymlinkRoute)
  , RouteMode (DefaultMode, Private, PrivateExecutable)
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (..)
  , OrphanStatus (..)
  , SynchronizationCommand (Applied)
  , TargetFingerprint (DirectoryFingerprint)
  )
import Dojang.Types.Manifest (manifest)
import Dojang.Types.Repository (Repository (..), RouteResult (..))


#ifdef mingw32_HOST_OS
import Dojang.Types.RouteMetadata (PortableMode (..))
#else
import Dojang.Types.RouteMetadata (PortableMode (..), portableModeFromBits)
#endif
import Dojang.Types.TargetTracking
  ( managedTargetId
  , newTargetSnapshotTransaction
  , observeConvergedManagedTarget
  , observeConvergedManagedTargetWithRenderedSource
  , observeManagedTarget
  , observeOrphanStatus
  )


spec :: Spec
spec = do
  symlinkOrphanSpec
  -- These tests observe or mutate the process working directory, so they
  -- must not run concurrently with other working-directory users:
  sequential $ describe "managed-target construction" $ do
    it "persists a relative destination as an absolute path" $
      withTempDir $ \root _ ->
        System.Directory.OsPath.withCurrentDirectory root $ do
          managed <- fixtureManaged "destination"
          snapshotDirectory <- encodeFS "snapshot"
          targetSnapshotsName <- encodeFS "target-snapshots"
          let targetSnapshots = root </> targetSnapshotsName
          createDirectories snapshotDirectory
          writeFile managed.correspondence.destination.path "managed"
          writeFile managed.correspondence.intermediate.path "managed"
          now <- getCurrentTime
          repository <- fixtureRepository
          transaction <- newTargetSnapshotTransaction targetSnapshots
          Just target <-
            observeManagedTarget repository transaction Applied now managed
          isAbsolute target.destinationPath `shouldBe` True

    it "protects target transactions and sensitive file snapshots" $
      withTempDir $ \root _ -> do
        managed <- fixtureManagedAt root
        repository <- fixtureRepositoryAt root
        snapshotRootName <- encodeFS "target-snapshots"
        let route = managed.route
            protected = managed{route = route{mode = Private}}
            intermediate = protected.correspondence.intermediate.path
            destination = protected.correspondence.destination.path
        createDirectories $ takeDirectory intermediate
        writeFile intermediate "managed"
        writeFile destination "managed"
        transaction <-
          newTargetSnapshotTransaction $ root </> snapshotRootName
        getPortableMode transaction
          `shouldReturn` expectedPrivateDirectoryMode
        now <- getCurrentTime
        Just target <-
          observeManagedTarget repository transaction Applied now protected
        getPortableMode target.snapshotPath
          `shouldReturn` expectedPrivateFileMode
        getPortableMode (takeDirectory target.snapshotPath)
          `shouldReturn` expectedPrivateDirectoryMode

    it "keeps private executable file snapshots executable" $
      withTempDir $ \root _ -> do
        managed <- fixtureManagedAt root
        repository <- fixtureRepositoryAt root
        snapshotRootName <- encodeFS "target-snapshots"
        let route = managed.route
            protected = managed{route = route{mode = PrivateExecutable}}
            intermediate = protected.correspondence.intermediate.path
            destination = protected.correspondence.destination.path
        createDirectories $ takeDirectory intermediate
        writeFile intermediate "managed"
        writeFile destination "managed"
        transaction <-
          newTargetSnapshotTransaction $ root </> snapshotRootName
        now <- getCurrentTime
        Just target <-
          observeManagedTarget repository transaction Applied now protected
        getPortableMode target.snapshotPath
          `shouldReturn` expectedPrivateExecutableFileMode
        getPortableMode (takeDirectory target.snapshotPath)
          `shouldReturn` expectedPrivateDirectoryMode

    it "protects sensitive directory snapshots" $
      withTempDir $ \root _ -> do
        managed <- fixtureManagedAt root
        repository <- fixtureRepositoryAt root
        snapshotRootName <- encodeFS "target-snapshots"
        let route = managed.route
            intermediate = managed.correspondence.intermediate.path
            destination = managed.correspondence.destination.path
            directoryEntry path = FileEntry path Directory
            protected =
              managed
                { route =
                    RouteResult
                      route.sourcePath
                      route.routeName
                      route.destinationPath
                      FileSystem.Directory
                      Private
                      route.kind
                      route.routeDefinition
                      route.routeProvenance
                      route.codec
                , correspondence =
                    FileCorrespondence
                      (directoryEntry managed.correspondence.source.path)
                      Unchanged
                      (directoryEntry intermediate)
                      (directoryEntry destination)
                      Unchanged
                }
        createDirectories intermediate
        createDirectories destination
        transaction <-
          newTargetSnapshotTransaction $ root </> snapshotRootName
        now <- getCurrentTime
        Just target <-
          observeManagedTarget repository transaction Applied now protected
        getPortableMode target.snapshotPath
          `shouldReturn` expectedPrivateDirectoryMode

    it "does not publish a private snapshot before its mode is narrowed" $
      withTempDir $ \root _ -> do
        managed <- fixtureManagedAt root
        repository <- fixtureRepositoryAt root
        snapshotRootName <- encodeFS "target-snapshots"
        let route = managed.route
            protected = managed{route = route{mode = Private}}
            intermediate = protected.correspondence.intermediate.path
            destination = protected.correspondence.destination.path
        createDirectories $ takeDirectory intermediate
        writeFile intermediate "managed"
        writeFile destination "managed"
        transaction <-
          newTargetSnapshotTransaction $ root </> snapshotRootName
        now <- getCurrentTime
        outcome <-
          runFailingPrivateModeIO $
            observeManagedTarget
              repository
              transaction
              Applied
              now
              protected
        outcome `shouldSatisfy` isLeft
        entries <- listDirectoryRecursively transaction []
        [entry | (FileSystem.File, entry) <- entries] `shouldBe` []

    it "distinguishes arbitrary surrogate-escaped destination bytes" $
      hedgehog $ do
        first <- forAll $ Gen.word8 (linear 0x80 0xfe)
        second <- forAll $ Gen.word8 (linear (first + 1) 0xff)
        firstPath <- liftIO $ encodeFS [toEnum $ 0xdc00 + fromIntegral first]
        secondPath <- liftIO $ encodeFS [toEnum $ 0xdc00 + fromIntegral second]
        firstManaged <- liftIO $ fixtureManagedPath firstPath
        secondManaged <- liftIO $ fixtureManagedPath secondPath
        repository <- liftIO fixtureRepository
        firstId <- liftIO $ managedTargetId repository firstManaged
        secondId <- liftIO $ managedTargetId repository secondManaged
        (firstId == secondId) === False

    it "normalizes route names before deriving target IDs" $ do
      rawRouteName <- encodeFS "./config"
      managed <- fixtureManaged "destination"
      repository <- fixtureRepository
      let canonicalRoute = managed.route
      let rawRoute :: RouteResult
          rawRoute =
            RouteResult
              canonicalRoute.sourcePath
              rawRouteName
              canonicalRoute.destinationPath
              canonicalRoute.fileType
              canonicalRoute.mode
              canonicalRoute.kind
              canonicalRoute.routeDefinition
              canonicalRoute.routeProvenance
              canonicalRoute.codec
      canonicalId <- managedTargetId repository managed
      rawId <- managedTargetId repository managed{route = rawRoute}
      rawId `shouldBe` canonicalId
      normalise rawRouteName `shouldBe` canonicalRoute.routeName

    it "distinguishes route definitions when deriving target IDs" $ do
      managed <- fixtureManaged "destination"
      repository <- fixtureRepository
      let route = managed.route
      let changedRoute =
            RouteResult
              route.sourcePath
              route.routeName
              route.destinationPath
              route.fileType
              route.mode
              route.kind
              "other-definition"
              route.routeProvenance
              route.codec
      originalId <- managedTargetId repository managed
      changedId <-
        managedTargetId
          repository
          managed{route = changedRoute}
      changedId `shouldNotBe` originalId

    it "distinguishes route types when deriving target IDs" $ do
      managed <- fixtureManaged "destination"
      repository <- fixtureRepository
      let route = managed.route
      let changedRoute =
            RouteResult
              route.sourcePath
              route.routeName
              route.destinationPath
              FileSystem.Directory
              route.mode
              route.kind
              route.routeDefinition
              route.routeProvenance
              route.codec
      originalId <- managedTargetId repository managed
      changedId <-
        managedTargetId
          repository
          managed{route = changedRoute}
      changedId `shouldNotBe` originalId

    it "separates snapshot generations for different route types" $
      withTempDir $ \root _ -> do
        managed <- fixtureManagedAt root
        repository <- fixtureRepositoryAt root
        snapshotRootName <- encodeFS "target-snapshots"
        let intermediate = managed.correspondence.intermediate.path
        let destination = managed.correspondence.destination.path
        createDirectories $ takeDirectory intermediate
        writeFile intermediate "managed"
        writeFile destination "managed"
        transaction <-
          newTargetSnapshotTransaction $ root </> snapshotRootName
        now <- getCurrentTime
        Just fileTarget <-
          observeManagedTarget repository transaction Applied now managed
        removeFile intermediate
        removeFile destination
        createDirectories intermediate
        createDirectories destination
        let route = managed.route
        let directoryEntry path = FileEntry path Directory
        let directoryManaged =
              managed
                { route =
                    RouteResult
                      route.sourcePath
                      route.routeName
                      route.destinationPath
                      FileSystem.Directory
                      route.mode
                      route.kind
                      route.routeDefinition
                      route.routeProvenance
                      route.codec
                , correspondence =
                    FileCorrespondence
                      (directoryEntry managed.correspondence.source.path)
                      Unchanged
                      (directoryEntry intermediate)
                      (directoryEntry destination)
                      Unchanged
                }
        Just directoryTarget <-
          observeManagedTarget
            repository
            transaction
            Applied
            now
            directoryManaged
        directoryTarget.snapshotPath `shouldNotBe` fileTarget.snapshotPath

    it "uses native destination identity when deriving target IDs" $ do
      upper <- fixtureManaged "C:/Users/Alice/App"
      lower <- fixtureManaged "c:\\users\\alice\\app"
      repository <- fixtureRepository
      upperId <- managedTargetId repository upper
      lowerId <- managedTargetId repository lower
      if os == "mingw32"
        then upperId `shouldBe` lowerId
        else upperId `shouldNotBe` lowerId

    it "rejects managed source paths outside the repository" $
      withTempDir $ \root _ -> do
        managed <- fixtureManagedAt root
        repository <- fixtureRepositoryAt root
        outsideName <- encodeFS "outside"
        let route = managed.route
        let escapedRoute =
              RouteResult
                (root </> outsideName)
                route.routeName
                route.destinationPath
                route.fileType
                route.mode
                route.kind
                route.routeDefinition
                route.routeProvenance
                route.codec
        let escaped = managed{route = escapedRoute}
        managedTargetId repository escaped
          `shouldThrow` (const True :: IOError -> Bool)

    it "preserves an existing baseline when a replacement copy fails" $
      withTempDir $ \root _ -> do
        managed <- fixtureManagedAt root
        repository <- fixtureRepositoryAt root
        snapshotRootName <- encodeFS "target-snapshots"
        childName <- encodeFS "not-a-file"
        let intermediate = managed.correspondence.intermediate.path
        let destination = managed.correspondence.destination.path
        let snapshotRoot = root </> snapshotRootName
        createDirectories $ takeDirectory intermediate
        writeFile intermediate "old baseline"
        writeFile destination "old baseline"
        now <- getCurrentTime
        firstTransaction <- newTargetSnapshotTransaction snapshotRoot
        Just old <-
          observeManagedTarget
            repository
            firstTransaction
            Applied
            now
            managed
        removeFile intermediate
        createDirectories $ intermediate </> childName
        secondTransaction <- newTargetSnapshotTransaction snapshotRoot
        outcome <-
          Exception.try
            ( observeManagedTarget
                repository
                secondTransaction
                Applied
                now
                managed
            )
            :: IO (Either IOError (Maybe ManagedTarget))
        outcome `shouldSatisfy` isLeft
        readFile old.snapshotPath `shouldReturn` "old baseline"

    it "does not emit an update for an operation that did not converge" $
      withTempDir $ \root _ -> do
        managed <- fixtureManagedAt root
        repository <- fixtureRepositoryAt root
        snapshotRootName <- encodeFS "target-snapshots"
        let intermediate = managed.correspondence.intermediate.path
        let source = managed.correspondence.source.path
        let destination = managed.correspondence.destination.path
        createDirectories $ takeDirectory intermediate
        createDirectories $ takeDirectory source
        writeFile intermediate "old"
        writeFile source "new"
        writeFile destination "old"
        transaction <-
          newTargetSnapshotTransaction $ root </> snapshotRootName
        now <- getCurrentTime
        observeConvergedManagedTarget
          repository
          transaction
          Applied
          now
          managed
          `shouldReturn` Nothing

    it "does not record rendered convergence after the raw source changes" $
      withTempDir $ \root _ -> do
        managed <- fixtureManagedAt root
        repository <- fixtureRepositoryAt root
        snapshotRootName <- encodeFS "target-snapshots"
        let intermediate = managed.correspondence.intermediate.path
            source = managed.correspondence.source.path
            destination = managed.correspondence.destination.path
            rendered = "rendered"
            originalRaw = "raw"
        createDirectories $ takeDirectory intermediate
        createDirectories $ takeDirectory source
        writeFile intermediate rendered
        writeFile destination rendered
        writeFile source "changed"
        transaction <-
          newTargetSnapshotTransaction $ root </> snapshotRootName
        now <- getCurrentTime
        observeConvergedManagedTargetWithRenderedSource
          repository
          transaction
          Applied
          now
          (Just $ opaqueBytes rendered)
          (Just $ SHA256.hash originalRaw)
          managed
          `shouldReturn` Nothing

    convergedSymlinkSpec

    it "materializes directory baselines one entry at a time" $
      withTempDir $ \root _ -> do
        routeName <- encodeFS "config"
        childName <- encodeFS "child"
        nestedName <- encodeFS "nested"
        sourceRootName <- encodeFS "repository"
        snapshotName <- encodeFS "snapshot"
        destinationName <- encodeFS "destination"
        targetRootName <- encodeFS "target-snapshots"
        let sourceRoot = root </> sourceRootName </> routeName
        let intermediateRoot = root </> snapshotName </> routeName
        let destinationRoot = root </> destinationName
        let childIntermediate = intermediateRoot </> childName
        createDirectories $ childIntermediate </> nestedName
        createDirectories $ destinationRoot </> childName </> nestedName
        writeFile (childIntermediate </> nestedName </> childName) "nested"
        writeFile
          (destinationRoot </> childName </> nestedName </> childName)
          "nested"
        let entry path = FileEntry path Directory
        let managed =
              ManagedCorrespondence
                ( RouteResult
                    sourceRoot
                    routeName
                    destinationRoot
                    FileSystem.Directory
                    DefaultMode
                    CopyRoute
                    "definition"
                    Map.empty
                    identityCodecSpec
                )
                childName
                ( FileCorrespondence
                    (entry $ sourceRoot </> childName)
                    Unchanged
                    (entry childIntermediate)
                    (entry $ destinationRoot </> childName)
                    Unchanged
                )
        let repository =
              Repository
                (root </> sourceRootName)
                (root </> snapshotName)
                (manifest mempty mempty mempty mempty mempty)
        transaction <-
          newTargetSnapshotTransaction $ root </> targetRootName
        now <- getCurrentTime
        Just target <-
          observeManagedTarget repository transaction Applied now managed
        exists (target.snapshotPath </> nestedName </> childName)
          `shouldReturn` False

  describe "observeOrphanStatus" $ do
    it "does not compare descendants through a directory entry record" $
      withTempDir $ \root _ -> do
        destinationName <- encodeFS "destination"
        snapshotName <- encodeFS "snapshot"
        nestedName <- encodeFS "nested"
        fileName <- encodeFS "file"
        routeName <- encodeFS "route"
        let destination = root </> destinationName
        let snapshot = root </> snapshotName
        createDirectories $ destination </> nestedName
        createDirectories $ snapshot </> nestedName
        writeFile (destination </> nestedName </> fileName) "managed"
        writeFile (snapshot </> nestedName </> fileName) "managed"
        now <- getCurrentTime
        let target =
              ManagedTarget
                "target"
                routeName
                routeName
                FileSystem.Directory
                CopyRoute
                DefaultMode
                destination
                snapshot
                "definition"
                Map.empty
                Nothing
                DirectoryFingerprint
                Applied
                now
        observeOrphanStatus target `shouldReturn` OrphanUnchanged
        writeFile (destination </> nestedName </> fileName) "locally modified"
        observeOrphanStatus target `shouldReturn` OrphanUnchanged

    posixOrphanStatusSpec

#ifdef mingw32_HOST_OS
convergedSymlinkSpec :: Spec
convergedSymlinkSpec = pure ()
#else
convergedSymlinkSpec :: Spec
convergedSymlinkSpec =
  it "does not emit a deletion for converged symbolic links" $
    withTempDir $ \root _ -> do
      managed <- fixtureManagedAt root
      repository <- fixtureRepositoryAt root
      targetName <- encodeFS "symlink-target"
      snapshotRootName <- encodeFS "target-snapshots"
      let target = root </> targetName
      let intermediate = managed.correspondence.intermediate.path
      let source = managed.correspondence.source.path
      let destination = managed.correspondence.destination.path
      writeFile target "managed"
      createDirectories $ takeDirectory intermediate
      createDirectories $ takeDirectory source
      System.Directory.OsPath.createFileLink target intermediate
      System.Directory.OsPath.createFileLink target source
      System.Directory.OsPath.createFileLink target destination
      transaction <- newTargetSnapshotTransaction $ root </> snapshotRootName
      now <- getCurrentTime
      observeConvergedManagedTarget
        repository
        transaction
        Applied
        now
        managed
        `shouldReturn` Nothing
#endif

#ifdef mingw32_HOST_OS
posixOrphanStatusSpec :: Spec
posixOrphanStatusSpec = pure ()
#else
posixOrphanStatusSpec :: Spec
posixOrphanStatusSpec = do
  it "classifies a special-file destination without reading it" $
    withTempDir $ \root _ -> do
      target <- fixtureOrphanTarget root
      createDirectories $ takeDirectory target.snapshotPath
      writeFile target.snapshotPath mempty
      destination <- decodeFS target.destinationPath
      Posix.createNamedPipe destination 0o600
      timeout 1000000 (observeOrphanStatus target)
        `shouldReturn` Just OrphanModified

  it "classifies a special-file baseline without reading it" $
    withTempDir $ \root _ -> do
      target <- fixtureOrphanTarget root
      writeFile target.destinationPath mempty
      createDirectories $ takeDirectory target.snapshotPath
      snapshot <- decodeFS target.snapshotPath
      Posix.createNamedPipe snapshot 0o600
      timeout 1000000 (observeOrphanStatus target)
        `shouldReturn` Just OrphanModified
#endif


symlinkOrphanSpec :: Spec
symlinkOrphanSpec =
  describe "observeOrphanStatus (deployment links)" $ do
    it "reports an unchanged link with a relative recorded target" $
      withTempDir $ \root _ -> do
        sourceName <- encodeFS "linked-source"
        destinationName <- encodeFS "linked-destination"
        probeName <- encodeFS "link-probe"
        writeFile (root </> sourceName) "linked"
        symlinkAvailable <-
          ( do
              createSymbolicLink sourceName (root </> probeName) FileSystem.File
              return True
          )
            `catchError` const (return False)
        when symlinkAvailable $ do
          createSymbolicLink
            sourceName
            (root </> destinationName)
            FileSystem.File
          target <- fixtureLinkOrphan root sourceName destinationName
          observeOrphanStatus target `shouldReturn` OrphanUnchanged
          -- A retargeted link is modified, and a removed one missing:
          removeFile $ root </> destinationName
          otherName <- encodeFS "other"
          writeFile (root </> otherName) "other"
          createSymbolicLink
            otherName
            (root </> destinationName)
            FileSystem.File
          target' <- fixtureLinkOrphan root sourceName destinationName
          observeOrphanStatus target' `shouldReturn` OrphanModified
          removeFile $ root </> destinationName
          target'' <- fixtureLinkOrphan root sourceName destinationName
          observeOrphanStatus target'' `shouldReturn` OrphanMissing
 where
  fixtureLinkOrphan :: OsPath -> OsPath -> OsPath -> IO ManagedTarget
  fixtureLinkOrphan root sourceName destinationName = do
    routeName <- encodeFS "linked"
    snapshotName <- encodeFS "snapshot/linked"
    now <- getCurrentTime
    return $
      ManagedTarget
        "link-orphan"
        routeName
        routeName
        FileSystem.File
        SymlinkRoute
        DefaultMode
        (root </> destinationName)
        (root </> snapshotName)
        "definition"
        Map.empty
        Nothing
        (SymlinkFingerprint sourceName)
        Applied
        now

#ifndef mingw32_HOST_OS
fixtureOrphanTarget :: OsPath -> IO ManagedTarget
fixtureOrphanTarget root = do
  destinationName <- encodeFS "destination"
  snapshotName <- encodeFS "snapshots/baseline"
  routeName <- encodeFS "route"
  now <- getCurrentTime
  return $
    ManagedTarget
      "target"
      routeName
      routeName
      FileSystem.File
      CopyRoute
      DefaultMode
      (root </> destinationName)
      (root </> snapshotName)
      "definition"
      Map.empty
      Nothing
      (FileFingerprint 7 "digest")
      Applied
      now
#endif


fixtureManaged :: FilePath -> IO ManagedCorrespondence
fixtureManaged destination = encodeFS destination >>= fixtureManagedPath


fixtureManagedPath :: OsPath -> IO ManagedCorrespondence
fixtureManagedPath destination = do
  routeName <- encodeFS "config"
  source <- encodeFS "repository/config"
  intermediate <- encodeFS "snapshot/config"
  let entry path = FileEntry path (File 7)
  return $
    ManagedCorrespondence
      ( RouteResult
          source
          routeName
          destination
          FileSystem.File
          DefaultMode
          CopyRoute
          "definition"
          Map.empty
          identityCodecSpec
      )
      mempty
      ( FileCorrespondence
          (entry source)
          Unchanged
          (entry intermediate)
          (entry destination)
          Unchanged
      )


fixtureManagedAt :: OsPath -> IO ManagedCorrespondence
fixtureManagedAt root = do
  routeName <- encodeFS "config"
  repositoryName <- encodeFS "repository"
  snapshotName <- encodeFS "snapshot"
  destinationName <- encodeFS "destination"
  let source = root </> repositoryName </> routeName
  let intermediate = root </> snapshotName </> routeName
  let destination = root </> destinationName
  let entry path = FileEntry path (File 12)
  return $
    ManagedCorrespondence
      ( RouteResult
          source
          routeName
          destination
          FileSystem.File
          DefaultMode
          CopyRoute
          "definition"
          Map.empty
          identityCodecSpec
      )
      mempty
      ( FileCorrespondence
          (entry source)
          Unchanged
          (entry intermediate)
          (entry destination)
          Unchanged
      )


fixtureRepository :: IO Repository
fixtureRepository =
  Repository
    <$> encodeFS "repository"
    <*> encodeFS "snapshot"
    <*> pure (manifest mempty mempty mempty mempty mempty)


fixtureRepositoryAt :: OsPath -> IO Repository
fixtureRepositoryAt root = do
  repositoryName <- encodeFS "repository"
  snapshotName <- encodeFS "snapshot"
  return $
    Repository
      (root </> repositoryName)
      (root </> snapshotName)
      (manifest mempty mempty mempty mempty mempty)


newtype FailingPrivateModeIO a
  = FailingPrivateModeIO (ExceptT IOError IO a)
  deriving (Functor, Applicative, Monad, MonadError IOError)


runFailingPrivateModeIO
  :: FailingPrivateModeIO a
  -> IO (Either IOError a)
runFailingPrivateModeIO (FailingPrivateModeIO action) = runExceptT action


instance MonadFileSystem FailingPrivateModeIO where
  encodePath value = FailingPrivateModeIO $ liftIO (encodePath value :: IO OsPath)
  decodePath value = FailingPrivateModeIO $ liftIO (decodePath value :: IO FilePath)
  getCurrentDirectory =
    FailingPrivateModeIO $ liftIO (getCurrentDirectory :: IO OsPath)
  getHomeDirectory = FailingPrivateModeIO $ liftIO (getHomeDirectory :: IO OsPath)
  exists value = FailingPrivateModeIO $ liftIO (exists value :: IO Bool)
  isFile value = FailingPrivateModeIO $ liftIO (isFile value :: IO Bool)
  isRegularFile value =
    FailingPrivateModeIO $ liftIO (isRegularFile value :: IO Bool)
  isDirectory value = FailingPrivateModeIO $ liftIO (isDirectory value :: IO Bool)
  isSymlink value = FailingPrivateModeIO $ liftIO (isSymlink value :: IO Bool)
  readFile value = FailingPrivateModeIO $ liftIO (readFile value)
  writeFile path contents =
    FailingPrivateModeIO $ liftIO (writeFile path contents :: IO ())
  replaceFile source destination =
    FailingPrivateModeIO $
      liftIO (replaceFile source destination :: IO ())
  writeTemporaryFile directory template contents =
    FailingPrivateModeIO $
      liftIO (writeTemporaryFile directory template contents :: IO OsPath)
  withFileLock _ action = action
  canonicalizePath value =
    FailingPrivateModeIO $ liftIO (canonicalizePath value :: IO OsPath)
  readSymlinkTarget value =
    FailingPrivateModeIO $ liftIO (readSymlinkTarget value :: IO OsPath)
  copyFile source destination =
    FailingPrivateModeIO $ liftIO (copyFile source destination :: IO ())
  copyFileWithMetadata source destination =
    FailingPrivateModeIO $
      liftIO (copyFileWithMetadata source destination :: IO ())
  copyFilePermissions source destination =
    FailingPrivateModeIO $
      liftIO (copyFilePermissions source destination :: IO ())
  createDirectory value =
    FailingPrivateModeIO $ liftIO (createDirectory value :: IO ())
  removeFile value = FailingPrivateModeIO $ liftIO (removeFile value :: IO ())
  removeDirectory value =
    FailingPrivateModeIO $ liftIO (removeDirectory value :: IO ())
  listDirectory value =
    FailingPrivateModeIO $ liftIO (listDirectory value :: IO [OsPath])
  getFileSize value =
    FailingPrivateModeIO $ liftIO (getFileSize value :: IO Integer)
  getPortableMode value =
    FailingPrivateModeIO $ liftIO (getPortableMode value)
  setPortableMode path bits
    | bits == 0o600 = throwError $ userError "injected private-mode failure"
    | otherwise =
        FailingPrivateModeIO $ liftIO (setPortableMode path bits :: IO ())
  setPortableWritable path writable =
    FailingPrivateModeIO $
      liftIO (setPortableWritable path writable :: IO ())
  createSymbolicLink target link fileType =
    FailingPrivateModeIO $
      liftIO (createSymbolicLink target link fileType :: IO ())

#ifdef mingw32_HOST_OS
expectedPrivateFileMode :: PortableMode
expectedPrivateFileMode = PortableMode Nothing True
expectedPrivateExecutableFileMode :: PortableMode
expectedPrivateExecutableFileMode = PortableMode Nothing True
expectedPrivateDirectoryMode :: PortableMode
expectedPrivateDirectoryMode = PortableMode Nothing True
#else
expectedPrivateFileMode :: PortableMode
expectedPrivateFileMode = portableModeFromBits 0o600
expectedPrivateExecutableFileMode :: PortableMode
expectedPrivateExecutableFileMode = portableModeFromBits 0o700
expectedPrivateDirectoryMode :: PortableMode
expectedPrivateDirectoryMode = portableModeFromBits 0o700
#endif
