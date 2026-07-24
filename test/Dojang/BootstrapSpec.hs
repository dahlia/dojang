{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.BootstrapSpec (spec) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Archive.Zip qualified as Zip
import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Hedgehog (evalIO, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range


#ifndef mingw32_HOST_OS
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except
  ( ExceptT
  , MonadError (throwError)
  , runExceptT
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bits (shiftL, (.|.))
import Data.Word (Word32)
import System.Directory.OsPath qualified
import System.OsPath (OsPath)
#endif
import System.OsPath (encodeFS, (</>))
import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  , shouldReturn
  , shouldSatisfy
  )
import Test.Hspec.Hedgehog (hedgehog, (===))
import Prelude hiding (readFile, writeFile)

import Dojang.Bootstrap
  ( AcquisitionError (..)
  , ArchiveFormat (..)
  , BuiltinSource (..)
  , archiveFormatFromFilePath
  , detectBuiltinSource
  , normalizeArchiveEntryPath
  , publishStagedDirectory
  , stageBuiltinSource
  )


#ifndef mingw32_HOST_OS
import Dojang.Bootstrap
  ( publishStagedDirectoryWithMetadata
  , stageBuiltinSourceWithMetadata
  )
#endif
import Dojang.MonadFileSystem
  ( MonadFileSystem (..)
  )
import Dojang.TestUtils (withTempDir)


#ifndef mingw32_HOST_OS
import Dojang.Types.RouteMetadata (PortableMode (..), portableModeFromBits)
#endif


spec :: Spec
spec = do
  describe "normalizeArchiveEntryPath" $ do
    it "rejects arbitrary parent traversal" $
      hedgehog $ do
        suffix <-
          forAll $
            Gen.string
              (Range.linear 1 100)
              (Gen.filter (`notElem` ['/', '\\']) Gen.unicode)
        normalizeArchiveEntryPath ("../" <> suffix)
          === Left (UnsafeArchiveEntry ("../" <> suffix))

    it "rejects absolute and Windows-style paths" $ do
      normalizeArchiveEntryPath "/absolute"
        `shouldBe` Left (UnsafeArchiveEntry "/absolute")
      normalizeArchiveEntryPath "C:\\outside"
        `shouldBe` Left (UnsafeArchiveEntry "C:\\outside")
      normalizeArchiveEntryPath "nested\\outside"
        `shouldBe` Left (UnsafeArchiveEntry "nested\\outside")
      normalizeArchiveEntryPath "nested/file:stream"
        `shouldBe` Left (UnsafeArchiveEntry "nested/file:stream")
      normalizeArchiveEntryPath "NUL.txt"
        `shouldBe` Left (UnsafeArchiveEntry "NUL.txt")
      normalizeArchiveEntryPath "nested/COM1"
        `shouldBe` Left (UnsafeArchiveEntry "nested/COM1")
      normalizeArchiveEntryPath "trailing."
        `shouldBe` Left (UnsafeArchiveEntry "trailing.")
      normalizeArchiveEntryPath "trailing "
        `shouldBe` Left (UnsafeArchiveEntry "trailing ")

  describe "archiveFormatFromFilePath" $ do
    it "recognizes every built-in archive format" $ do
      archiveFormatFromFilePath "repo.zip" `shouldBe` Just ZipArchive
      archiveFormatFromFilePath "repo.tar" `shouldBe` Just TarArchive
      archiveFormatFromFilePath "repo.tar.gz" `shouldBe` Just TarGzipArchive
      archiveFormatFromFilePath "repo.tgz" `shouldBe` Just TarGzipArchive
      archiveFormatFromFilePath "repo.rar" `shouldBe` Nothing

  describe "directory acquisition" $ do
    it "stages and atomically publishes paths containing spaces" $
      withTempDir $ \tmpDir _ -> do
        sourceName <- encodeFS "source with spaces"
        stagingName <- encodeFS ".dojang-bootstrap-test"
        destinationName <- encodeFS "destination with spaces"
        manifestName <- encodeFS "dojang.toml"
        let source = tmpDir </> sourceName
            staging = tmpDir </> stagingName
            destination = tmpDir </> destinationName
        createDirectory source
        writeFile (source </> manifestName) "repository-id = \"test\"\n"
        detected <- detectBuiltinSource source
        detected `shouldBe` Right (DirectorySource source)
        Right () <- stageBuiltinSource (DirectorySource source) staging
        publishStagedDirectory staging destination
        isDirectory staging `shouldReturn` False
        readFile (destination </> manifestName)
          `shouldReturn` "repository-id = \"test\"\n"

    it "publishes into an existing empty destination" $
      withTempDir $ \tmpDir _ -> do
        stagingName <- encodeFS "staging"
        destinationName <- encodeFS "destination"
        manifestName <- encodeFS "dojang.toml"
        let staging = tmpDir </> stagingName
            destination = tmpDir </> destinationName
        createDirectory staging
        createDirectory destination
        writeFile (staging </> manifestName) "manifest"
        publishStagedDirectory staging destination
        isDirectory staging `shouldReturn` False
        readFile (destination </> manifestName) `shouldReturn` "manifest"

    symlinkSpecs

    it "reports missing and unsupported local sources" $
      withTempDir $ \tmpDir _ -> do
        missingName <- encodeFS "missing.zip"
        unsupportedName <- encodeFS "repository.rar"
        let missing = tmpDir </> missingName
            unsupported = tmpDir </> unsupportedName
        missingPath <- decodePath missing
        unsupportedPath <- decodePath unsupported
        detectBuiltinSource missing
          `shouldReturn` Left (SourceDoesNotExist missingPath)
        writeFile unsupported "contents"
        detectBuiltinSource unsupported
          `shouldReturn` Left (UnsupportedArchiveFormat unsupportedPath)

  describe "archive acquisition" $ do
    it "extracts zip, tar, tar.gz, and tgz sources" $
      withTempDir $ \tmpDir _ -> do
        archiveName <- encodeFS "repository.zip"
        tarName <- encodeFS "repository.tar"
        tarGzipName <- encodeFS "repository.tar.gz"
        tgzName <- encodeFS "repository.tgz"
        manifestName <- encodeFS "dojang.toml"
        nestedName <- encodeFS "nested"
        let archiveBytes = zipBytes "nested/dojang.toml" "zip"
            tarArchiveBytes = tarBytes "nested/dojang.toml" "tar"
            cases =
              [ (archiveName, archiveBytes, "zip")
              , (tarName, tarArchiveBytes, "tar")
              ,
                ( tarGzipName
                , GZip.compress tarArchiveBytes
                , "tar"
                )
              , (tgzName, GZip.compress tarArchiveBytes, "tar")
              ]
        mapM_
          ( \(name, bytes, expected) -> do
              stagingName <- encodeFS $ ".staging-" <> show name
              let source = tmpDir </> name
                  staging = tmpDir </> stagingName
              writeFile source $ LazyByteString.toStrict bytes
              Right builtin <- detectBuiltinSource source
              Right () <- stageBuiltinSource builtin staging
              readFile (staging </> nestedName </> manifestName)
                `shouldReturn` expected
          )
          cases

    archiveModeSpecs

    it "rejects traversal before writing any archive entry" $
      withTempDir $ \tmpDir _ -> do
        archiveName <- encodeFS "unsafe.zip"
        stagingName <- encodeFS "staging"
        escapedName <- encodeFS "escaped"
        let archivePath = tmpDir </> archiveName
            staging = tmpDir </> stagingName
        writeFile archivePath $
          LazyByteString.toStrict $
            zipBytes "../escaped" "unsafe"
        result <- stageBuiltinSource (ArchiveSource ZipArchive archivePath) staging
        result `shouldBe` Left (UnsafeArchiveEntry "../escaped")
        isDirectory staging `shouldReturn` False
        isDirectory (tmpDir </> escapedName) `shouldReturn` False

    it "rejects corrupt archives without leaving staging" $
      withTempDir $ \tmpDir _ -> do
        zipName <- encodeFS "corrupt.zip"
        gzipName <- encodeFS "corrupt.tar.gz"
        stagingName <- encodeFS "staging"
        let zipPath = tmpDir </> zipName
            gzipPath = tmpDir </> gzipName
            staging = tmpDir </> stagingName
        writeFile zipPath "not a zip"
        writeFile gzipPath "not gzip"
        zipResult <-
          stageBuiltinSource (ArchiveSource ZipArchive zipPath) staging
        zipResult `shouldSatisfy` isInvalidArchive
        gzipResult <-
          stageBuiltinSource (ArchiveSource TarGzipArchive gzipPath) staging
        gzipResult `shouldSatisfy` isInvalidArchive
        isDirectory staging `shouldReturn` False

    it "rejects arbitrary case-insensitive path collisions before extraction" $
      hedgehog $ do
        suffix <-
          forAll $
            Gen.string
              (Range.linear 0 90)
              (Gen.element $ ['a' .. 'z'] <> ['0' .. '9'])
        let firstPath = "nested/a" <> suffix
            secondPath = "nested/A" <> suffix
        conflicting <-
          evalIO $
            withTempDir $ \tmpDir _ -> do
              archiveName <- encodeFS "collision.tar"
              stagingName <- encodeFS "staging"
              let archivePath = tmpDir </> archiveName
                  staging = tmpDir </> stagingName
              writeFile archivePath $
                LazyByteString.toStrict $
                  Tar.write
                    [ tarFileEntry firstPath "first"
                    , tarFileEntry secondPath "second"
                    ]
              result <-
                stageBuiltinSource
                  (ArchiveSource TarArchive archivePath)
                  staging
              stagingExists <- isDirectory staging
              return (isConflictingArchive result, stagingExists)
        conflicting === (True, False)

    it "rejects Unicode-normalization path collisions before extraction" $
      withTempDir $ \tmpDir _ -> do
        archiveName <- encodeFS "normalization-collision.zip"
        stagingName <- encodeFS "staging"
        let archivePath = tmpDir </> archiveName
            staging = tmpDir </> stagingName
            archive =
              Zip.addEntryToArchive
                (Zip.toEntry "caf\233" 0 "composed")
                $ Zip.addEntryToArchive
                  (Zip.toEntry "cafe\769" 0 "decomposed")
                  Zip.emptyArchive
        writeFile archivePath $
          LazyByteString.toStrict $
            Zip.fromArchive archive
        result <-
          stageBuiltinSource
            (ArchiveSource ZipArchive archivePath)
            staging
        result `shouldSatisfy` isConflictingArchive
        isDirectory staging `shouldReturn` False

    it "rejects links and conflicting archive entries before extraction" $
      withTempDir $ \tmpDir _ -> do
        linkArchiveName <- encodeFS "link.tar"
        conflictArchiveName <- encodeFS "conflict.tar"
        stagingName <- encodeFS "staging"
        let linkArchive = tmpDir </> linkArchiveName
            conflictArchive = tmpDir </> conflictArchiveName
            staging = tmpDir </> stagingName
        writeFile linkArchive $
          LazyByteString.toStrict tarSymlinkBytes
        linkResult <-
          stageBuiltinSource (ArchiveSource TarArchive linkArchive) staging
        linkResult
          `shouldBe` Left (UnsupportedArchiveEntry "link")
        writeFile conflictArchive $
          LazyByteString.toStrict $
            Tar.write
              [ tarFileEntry "duplicate" "first"
              , tarFileEntry "duplicate" "second"
              ]
        conflictResult <-
          stageBuiltinSource
            (ArchiveSource TarArchive conflictArchive)
            staging
        conflictResult
          `shouldBe` Left (ConflictingArchiveEntry "duplicate")
        writeFile conflictArchive $
          LazyByteString.toStrict $
            Tar.write
              [ tarFileEntry "config" "file"
              , tarFileEntry "config.bak" "backup"
              , tarFileEntry "config/nvim/init.vim" "nested"
              ]
        ancestorResult <-
          stageBuiltinSource
            (ArchiveSource TarArchive conflictArchive)
            staging
        ancestorResult
          `shouldBe` Left (ConflictingArchiveEntry "config")
        isDirectory staging `shouldReturn` False

    it "accepts GNU long names and pax global metadata" $
      withTempDir $ \tmpDir _ -> do
        archiveName <- encodeFS "metadata.tar"
        stagingName <- encodeFS "staging"
        longName <- encodeFS $ replicate 110 'a'
        paxDirectoryName <- encodeFS "pax"
        paxFileName <- encodeFS "dojang.toml"
        let archivePath = tmpDir </> archiveName
            staging = tmpDir </> stagingName
            longPath = replicate 110 'a'
            paxEntry =
              let contents = paxRecord "comment" "test"
              in Tar.simpleEntry
                   (tarPath "pax_global_header")
                   ( Tar.OtherEntryType
                       'g'
                       contents
                       (LazyByteString.length contents)
                   )
            longEntry =
              Tar.simpleEntry
                longPath
                (Tar.NormalFile "long" 4)
            paxContents = paxRecord "path" "pax/dojang.toml"
            paxPathEntry =
              Tar.simpleEntry
                (tarPath "pax_extended_header")
                ( Tar.OtherEntryType
                    'x'
                    paxContents
                    (LazyByteString.length paxContents)
                )
        writeFile archivePath $
          LazyByteString.toStrict $
            Tar.write $
              paxEntry
                : Tar.encodeLongNames longEntry
                  <> [paxPathEntry, tarFileEntry "placeholder" "pax"]
        Right () <-
          stageBuiltinSource
            (ArchiveSource TarArchive archivePath)
            staging
        readFile (staging </> longName) `shouldReturn` "long"
        readFile (staging </> paxDirectoryName </> paxFileName)
          `shouldReturn` "pax"


zipBytes :: FilePath -> LazyByteString.ByteString -> LazyByteString.ByteString
zipBytes path contents =
  Zip.fromArchive $
    Zip.addEntryToArchive
      (Zip.toEntry path 0 contents)
      Zip.emptyArchive


tarBytes :: FilePath -> LazyByteString.ByteString -> LazyByteString.ByteString
tarBytes path contents =
  Tar.write [tarFileEntry path contents]


tarFileEntry :: FilePath -> LazyByteString.ByteString -> Tar.Entry
tarFileEntry path contents = Tar.fileEntry (tarPath path) contents

#ifndef mingw32_HOST_OS
archiveBytesWithMode
  :: ArchiveFormat
  -> Word
  -> LazyByteString.ByteString
archiveBytesWithMode format mode = case format of
  ZipArchive ->
    let unixFileMode = (0o100000 :: Word32) .|. fromIntegral mode
        entry =
          (Zip.toEntry "script" 0 "#!/bin/sh\n")
            { Zip.eVersionMadeBy = (3 `shiftL` 8) .|. 20
            , Zip.eExternalFileAttributes = unixFileMode `shiftL` 16
            }
    in Zip.fromArchive $ Zip.addEntryToArchive entry Zip.emptyArchive
  TarArchive ->
    Tar.write
      [ (tarFileEntry "script" "#!/bin/sh\n")
          { Tar.entryPermissions = fromIntegral mode
          }
      ]
  TarGzipArchive ->
    GZip.compress $ archiveBytesWithMode TarArchive mode
#endif


tarPath :: FilePath -> Tar.TarPath
tarPath path = case Tar.toTarPath False path of
  Left err -> error err
  Right value -> value


tarSymlinkBytes :: LazyByteString.ByteString
tarSymlinkBytes =
  case Tar.toLinkTarget "target" of
    Nothing -> error "Invalid test link target."
    Just target ->
      Tar.write
        [ Tar.simpleEntry
            (tarPath "link")
            (Tar.SymbolicLink target)
        ]


paxRecord :: String -> String -> LazyByteString.ByteString
paxRecord key value =
  LazyByteString.fromStrict $ ByteString.pack $ makeRecord 1
 where
  body = key <> "=" <> value <> "\n"
  makeRecord estimate =
    let record = show estimate <> " " <> body
        actual = length record
    in if actual == estimate then record else makeRecord actual


isInvalidArchive :: Either AcquisitionError () -> Bool
isInvalidArchive (Left (InvalidArchive _)) = True
isInvalidArchive _ = False


isConflictingArchive :: Either AcquisitionError () -> Bool
isConflictingArchive (Left (ConflictingArchiveEntry _)) = True
isConflictingArchive _ = False

#ifdef mingw32_HOST_OS
archiveModeSpecs :: Spec
archiveModeSpecs = return ()
#else
archiveModeSpecs :: Spec
archiveModeSpecs = do
  it "preserves arbitrary POSIX file modes from zip and tar archives" $
    hedgehog $ do
      format <- forAll $ Gen.element [ZipArchive, TarArchive, TarGzipArchive]
      mode <- forAll $ Gen.word $ Range.linear 1 0o777
      existingDestination <- forAll Gen.bool
      observed <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            archiveName <- encodeFS "repository.archive"
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            scriptName <- encodeFS "script"
            let archivePath = tmpDir </> archiveName
                staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
            writeFile archivePath $
              LazyByteString.toStrict $
                archiveBytesWithMode format mode
            Right metadata <-
              stageBuiltinSourceWithMetadata
                (ArchiveSource format archivePath)
                staging
            when existingDestination $ createDirectory destination
            _ <-
              publishStagedDirectoryWithMetadata
                metadata
                staging
                destination
            getPortableMode $ destination </> scriptName
      observed === portableModeFromBits mode

  it "uses default permissions when a Unix zip omits external attributes" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "repository.zip"
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "destination"
      scriptName <- encodeFS "script"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
          entry =
            (Zip.toEntry "script" 0 "#!/bin/sh\n")
              { Zip.eVersionMadeBy = (3 `shiftL` 8) .|. 20
              , Zip.eExternalFileAttributes = 0
              }
      writeFile archivePath $
        LazyByteString.toStrict $
          Zip.fromArchive $
            Zip.addEntryToArchive entry Zip.emptyArchive
      Right metadata <-
        stageBuiltinSourceWithMetadata
          (ArchiveSource ZipArchive archivePath)
          staging
      _ <-
        publishStagedDirectoryWithMetadata metadata staging destination
      observed <- getPortableMode $ destination </> scriptName
      observed.posixBits `shouldSatisfy` (/= Just 0)

  it "preserves restrictive directory modes in an existing empty destination" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "repository.tar"
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "destination"
      privateName <- encodeFS "private"
      fileName <- encodeFS "config"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
          directoryEntry =
            (Tar.directoryEntry $ tarPath "private")
              { Tar.entryPermissions = 0o500
              }
          fileEntry =
            (tarFileEntry "private/config" "contents")
              { Tar.entryPermissions = 0o400
              }
      writeFile archivePath $
        LazyByteString.toStrict $
          Tar.write [directoryEntry, fileEntry]
      Right metadata <-
        stageBuiltinSourceWithMetadata
          (ArchiveSource TarArchive archivePath)
          staging
      createDirectory destination
      _ <-
        publishStagedDirectoryWithMetadata metadata staging destination
      getPortableMode (destination </> privateName)
        `shouldReturn` portableModeFromBits 0o500
      getPortableMode (destination </> privateName </> fileName)
        `shouldReturn` portableModeFromBits 0o400
      setPortableMode (destination </> privateName) 0o700

  it "publishes contents when the filesystem cannot restore modes" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "repository.tar"
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "destination"
      scriptName <- encodeFS "script"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
      writeFile archivePath $
        LazyByteString.toStrict $
          archiveBytesWithMode TarArchive 0o700
      Right (Right metadata) <-
        runFailingModeIO $
          stageBuiltinSourceWithMetadata
            (ArchiveSource TarArchive archivePath)
            staging
      createDirectory destination
      published <-
        runFailingModeIO $
          publishStagedDirectoryWithMetadata
            metadata
            staging
            destination
      published `shouldBe` Right ["script"]
      readFile (destination </> scriptName)
        `shouldReturn` "#!/bin/sh\n"
      isDirectory staging `shouldReturn` False


newtype FailingModeIO a
  = FailingModeIO (ExceptT IOError IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadError IOError
    )


runFailingModeIO :: FailingModeIO a -> IO (Either IOError a)
runFailingModeIO (FailingModeIO action) = runExceptT action


instance MonadFileSystem FailingModeIO where
  encodePath value = liftIO (encodePath value :: IO OsPath)
  decodePath value = liftIO (decodePath value :: IO FilePath)
  getCurrentDirectory = liftIO (getCurrentDirectory :: IO OsPath)
  getHomeDirectory = liftIO (getHomeDirectory :: IO OsPath)
  exists value = liftIO (exists value :: IO Bool)
  isFile value = liftIO (isFile value :: IO Bool)
  isRegularFile value = liftIO (isRegularFile value :: IO Bool)
  isDirectory value = liftIO (isDirectory value :: IO Bool)
  isSymlink value = liftIO (isSymlink value :: IO Bool)
  readFile value = liftIO (readFile value :: IO ByteString.ByteString)
  writeFile path contents = liftIO (writeFile path contents :: IO ())
  replaceFile source destination =
    liftIO (replaceFile source destination :: IO ())
  renameDirectory source destination =
    liftIO (renameDirectory source destination :: IO ())
  writeTemporaryFile directory template contents =
    liftIO (writeTemporaryFile directory template contents :: IO OsPath)
  withFileLock _ action = action
  canonicalizePath value = liftIO (canonicalizePath value :: IO OsPath)
  readSymlinkTarget value = liftIO (readSymlinkTarget value :: IO OsPath)
  copyFile source destination =
    liftIO (copyFile source destination :: IO ())
  copyFileWithMetadata source destination =
    liftIO (copyFileWithMetadata source destination :: IO ())
  copyFilePermissions source destination =
    liftIO (copyFilePermissions source destination :: IO ())
  createDirectory value = liftIO (createDirectory value :: IO ())
  removeFile value = liftIO (removeFile value :: IO ())
  removeDirectory value = liftIO (removeDirectory value :: IO ())
  listDirectory value = liftIO (listDirectory value :: IO [OsPath])
  getFileSize value = liftIO (getFileSize value :: IO Integer)
  getPortableMode value = liftIO (getPortableMode value :: IO PortableMode)
  setPortableMode _ _ =
    throwError $ userError "injected mode-restoration failure"
  setPortableWritable path writable =
    liftIO (setPortableWritable path writable :: IO ())
  createSymbolicLink target link fileType =
    liftIO (createSymbolicLink target link fileType :: IO ())
#endif

#ifdef mingw32_HOST_OS
symlinkSpecs :: Spec
symlinkSpecs = return ()
#else
symlinkSpecs :: Spec
symlinkSpecs = do
  it "recreates symbolic links instead of following them" $
    withTempDir $ \tmpDir _ -> do
      sourceName <- encodeFS "source"
      stagingName <- encodeFS "staging"
      targetName <- encodeFS "target"
      linkName <- encodeFS "link"
      let source = tmpDir </> sourceName
          staging = tmpDir </> stagingName
      createDirectory source
      writeFile (source </> targetName) "contents"
      System.Directory.OsPath.createFileLink
        targetName
        (source </> linkName)
      Right () <- stageBuiltinSource (DirectorySource source) staging
      isSymlink (staging </> linkName) `shouldReturn` True
      readSymlinkTarget (staging </> linkName) `shouldReturn` targetName

  it "detects an archive through a symbolic link" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "repository.zip"
      linkName <- encodeFS "current.zip"
      let archivePath = tmpDir </> archiveName
          linkPath = tmpDir </> linkName
      writeFile archivePath $
        LazyByteString.toStrict $
          zipBytes "dojang.toml" "contents"
      System.Directory.OsPath.createFileLink archiveName linkPath
      detectBuiltinSource linkPath
        `shouldReturn` Right (ArchiveSource ZipArchive linkPath)
#endif
