{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.BootstrapSpec (spec) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Archive.Zip qualified as Zip
import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Hedgehog (forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range


#ifndef mingw32_HOST_OS
import System.Directory.OsPath qualified
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
import Dojang.MonadFileSystem
  ( MonadFileSystem (..)
  )
import Dojang.TestUtils (withTempDir)


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
