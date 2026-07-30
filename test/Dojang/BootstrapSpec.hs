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
import Data.Bits (shiftL, (.|.))
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (toLower, toUpper)
import Data.Word (Word16, Word32)
import Hedgehog (assert, evalIO, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.FilePath.Posix qualified as Posix


#ifndef mingw32_HOST_OS
import Control.Exception
  ( AsyncException (UserInterrupt)
  , throw
  )
import Control.Exception qualified as Exception
import Control.Monad (forM_)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
  ( ExceptT (..)
  , MonadError (throwError)
  , runExceptT
  , tryError
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Bits ((.&.))
import Data.Either (isLeft)
import Data.List (isInfixOf)
import System.Directory.OsPath qualified
import System.FilePath qualified as FilePath
import System.IO.Error
  ( doesNotExistErrorType
  , isPermissionError
  , mkIOError
  )
import System.OsPath (OsPath)
import System.Posix.Files qualified as Posix
import System.Timeout (timeout)
import Test.Hspec (expectationFailure)
#endif
import System.OsPath (encodeFS, (</>))
import Test.Hspec
  ( Spec
  , anyIOException
  , describe
  , it
  , shouldBe
  , shouldReturn
  , shouldSatisfy
  , shouldThrow
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
  ( emptyStagedMetadata
  , publishStagedDirectoryWithMetadata
  , stageBuiltinSourceWithMetadata
  )
import Dojang.MonadFileSystem
  ( BoundedFileRead (..)
  , FileIdentity
  , FileModeSnapshot (..)
  , FileSnapshot
  )
#endif
import Dojang.MonadFileSystem
  ( FileType (..)
  , MonadFileSystem (..)
  , dryRunIO
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

    it "rejects arbitrary Windows-forbidden characters" $
      hedgehog $ do
        prefix <-
          forAll $ Gen.string (Range.linear 0 20) Gen.alphaNum
        suffix <-
          forAll $ Gen.string (Range.linear 0 20) Gen.alphaNum
        forbidden <-
          forAll $
            Gen.choice
              [ Gen.element ("<>:\"\\|?*" :: String)
              , toEnum <$> Gen.int (Range.linear 0 31)
              ]
        let path = "nested/" <> prefix <> [forbidden] <> suffix
        normalizeArchiveEntryPath path
          === Left (UnsafeArchiveEntry path)

    it "rejects arbitrary archive paths deeper than the safety limit" $
      hedgehog $ do
        depth <- forAll $ Gen.int $ Range.linear 257 2048
        let path = Posix.joinPath $ replicate depth "a"
        normalizeArchiveEntryPath path
          === Left ArchiveResourceLimitExceeded

    it "rejects arbitrary archive paths longer than the safety limit" $
      hedgehog $ do
        pathLength <- forAll $ Gen.int $ Range.linear 4097 8192
        let path = replicate pathLength 'a'
        normalizeArchiveEntryPath path
          === Left ArchiveResourceLimitExceeded

    it "rejects arbitrary casing of every Windows device name" $
      hedgehog $ do
        device <-
          forAll $
            Gen.element
              [ "con"
              , "prn"
              , "aux"
              , "nul"
              , "clock$"
              , "conin$"
              , "conout$"
              , "com1"
              , "com9"
              , "com\185"
              , "com\178"
              , "com\179"
              , "lpt1"
              , "lpt9"
              , "lpt\185"
              , "lpt\178"
              , "lpt\179"
              ]
        cased <-
          forAll $
            traverse
              (\character -> Gen.element [toLower character, toUpper character])
              device
        extension <-
          forAll $
            Gen.maybe $
              Gen.string (Range.linear 1 20) Gen.alphaNum
        let path = cased <> maybe "" ('.' :) extension
        normalizeArchiveEntryPath path
          === Left (UnsafeArchiveEntry path)

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

    it "rejects an existing empty destination" $
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
          `shouldThrow` anyIOException
        listDirectory destination `shouldReturn` []
        readFile (staging </> manifestName) `shouldReturn` "manifest"

    it "rejects arbitrary portable directory-source path collisions" $
      hedgehog $ do
        suffix <-
          forAll $
            Gen.string
              (Range.linear 0 90)
              (Gen.element $ ['a' .. 'z'] <> ['0' .. '9'])
        (firstPrefix, secondPrefix) <-
          forAll $
            Gen.element
              [ ("entry-a", "entry-A")
              , ("caf\233-", "cafe\769-")
              ]
        let firstName = firstPrefix <> suffix
            secondName = secondPrefix <> suffix
        (result, stagingExists) <-
          evalIO $
            withTempDir $ \tmpDir _ -> do
              sourceName <- encodeFS "source"
              stagingName <- encodeFS "staging"
              first <- encodeFS firstName
              second <- encodeFS secondName
              let source = tmpDir </> sourceName
                  staging = tmpDir </> stagingName
              dryRunIO $ do
                createDirectory source
                writeFile (source </> first) "first"
                writeFile (source </> second) "second"
                staged <-
                  stageBuiltinSource
                    (DirectorySource source)
                    staging
                present <- isDirectory staging
                return (staged, present)
        assert $ isConflictingSource result
        stagingExists === False

    symlinkSpecs

    it "preserves the intrinsic type of a broken directory link" $
      withTempDir $ \tmpDir _ -> do
        sourceName <- encodeFS "source"
        stagingName <- encodeFS "staging"
        linkName <- encodeFS "future-directory"
        missingName <- encodeFS "missing-directory"
        let source = tmpDir </> sourceName
            staging = tmpDir </> stagingName
        result <-
          dryRunIO $ do
            createDirectory source
            createSymbolicLink missingName (source </> linkName) Directory
            staged <- stageBuiltinSource (DirectorySource source) staging
            linkType <- getSymbolicLinkType $ staging </> linkName
            return (staged, linkType)
        result `shouldBe` (Right (), Directory)

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
              [ ("repository.zip", archiveName, archiveBytes, "zip")
              , ("repository.tar", tarName, tarArchiveBytes, "tar")
              ,
                ( "repository.tar.gz"
                , tarGzipName
                , GZip.compress tarArchiveBytes
                , "tar"
                )
              , ("repository.tgz", tgzName, GZip.compress tarArchiveBytes, "tar")
              ]
        mapM_
          ( \(label, name, bytes, expected) -> do
              stagingName <- encodeFS $ ".staging-" <> label
              let source = tmpDir </> name
                  staging = tmpDir </> stagingName
              writeFile source $ LazyByteString.toStrict bytes
              Right builtin <- detectBuiltinSource source
              Right () <- stageBuiltinSource builtin staging
              readFile (staging </> nestedName </> manifestName)
                `shouldReturn` expected
          )
          cases

    it "ignores the conventional tar root directory entry" $
      withTempDir $ \tmpDir _ -> do
        archiveName <- encodeFS "repository.tar"
        stagingName <- encodeFS "staging"
        manifestName <- encodeFS "dojang.toml"
        let archivePath = tmpDir </> archiveName
            staging = tmpDir </> stagingName
        writeFile archivePath $
          LazyByteString.toStrict $
            Tar.write
              [ Tar.directoryEntry $ tarPath "./"
              , tarFileEntry "./dojang.toml" "manifest"
              ]
        stageBuiltinSource
          (ArchiveSource TarArchive archivePath)
          staging
          `shouldReturn` Right ()
        readFile (staging </> manifestName) `shouldReturn` "manifest"

    archiveModeSpecs
    archiveSpecialFileSpecs

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

    it "rejects archives whose declared expansion exceeds the safety limit" $
      withTempDir $ \tmpDir _ -> do
        archiveName <- encodeFS "oversized.zip"
        stagingName <- encodeFS "staging"
        let archivePath = tmpDir </> archiveName
            staging = tmpDir </> stagingName
            oversizedEntry =
              (Zip.toEntry "large" 0 "x")
                { Zip.eUncompressedSize = maxBound
                }
        writeFile archivePath $
          LazyByteString.toStrict $
            Zip.fromArchive $
              Zip.addEntryToArchive oversizedEntry Zip.emptyArchive
        result <-
          stageBuiltinSource
            (ArchiveSource ZipArchive archivePath)
            staging
        result `shouldBe` Left ArchiveResourceLimitExceeded
        isDirectory staging `shouldReturn` False

    it "rejects arbitrary case-insensitive path collisions before extraction" $
      hedgehog $ do
        suffix <-
          forAll $
            Gen.string
              (Range.linear 0 90)
              (Gen.element $ ['a' .. 'z'] <> ['0' .. '9'])
        let firstPath = "nested/file-a" <> suffix
            secondPath = "nested/FILE-A" <> suffix
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

    it "rejects arbitrary collisions among implicit archive directories" $
      hedgehog $ do
        directory <-
          forAll $
            Gen.string
              (Range.linear 1 40)
              (Gen.element ['a' .. 'z'])
        cased <-
          forAll $
            traverse
              (\character -> Gen.element [toLower character, toUpper character])
              directory
        let firstDirectory = "directory-" <> fmap toLower cased
            secondDirectory = "directory-" <> fmap toUpper cased
            firstPath = firstDirectory <> "/first"
            secondPath = secondDirectory <> "/second"
        conflicting <-
          evalIO $
            withTempDir $ \tmpDir _ -> do
              archiveName <- encodeFS "implicit-collision.tar"
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

    it "rejects arbitrary unsupported Unix ZIP entry types" $
      hedgehog $ do
        entryType <-
          forAll $
            Gen.element
              [ 0o010000
              , 0o020000
              , 0o060000
              , 0o120000
              , 0o140000
              ]
        permissions <- forAll $ Gen.word32 $ Range.linear 0 0o777
        suffix <-
          forAll $
            Gen.string
              (Range.linear 1 40)
              (Gen.element $ ['a' .. 'z'] <> ['0' .. '9'])
        let path = "special-" <> suffix
            attributes = (entryType .|. permissions) `shiftL` 16
            entry =
              (Zip.toEntry path 0 "contents")
                { Zip.eVersionMadeBy = (3 `shiftL` 8) .|. 20
                , Zip.eExternalFileAttributes = attributes
                }
        rejected <-
          evalIO $
            withTempDir $ \tmpDir _ -> do
              archiveName <- encodeFS "special.zip"
              stagingName <- encodeFS "staging"
              let archivePath = tmpDir </> archiveName
                  staging = tmpDir </> stagingName
              writeFile archivePath $
                LazyByteString.toStrict $
                  Zip.fromArchive $
                    Zip.addEntryToArchive entry Zip.emptyArchive
              result <-
                stageBuiltinSource
                  (ArchiveSource ZipArchive archivePath)
                  staging
              stagingExists <- isDirectory staging
              return (result, stagingExists)
        rejected
          === (Left (UnsupportedArchiveEntry path), False)

    it "rejects arbitrary non-Unix ZIP symbolic-link metadata" $
      hedgehog $ do
        creatorSystem <-
          forAll $
            Gen.filter (`notElem` [3, 19]) $
              Gen.word16 $
                Range.linear 0 255
        permissions <- forAll $ Gen.word32 $ Range.linear 0 0o777
        suffix <-
          forAll $
            Gen.string
              (Range.linear 1 40)
              (Gen.element $ ['a' .. 'z'] <> ['0' .. '9'])
        let path = "link-" <> suffix
            attributes =
              ((0o120000 :: Word32) .|. permissions) `shiftL` 16
            madeBy =
              (creatorSystem `shiftL` 8) .|. (20 :: Word16)
            entry =
              (Zip.toEntry path 0 "../outside")
                { Zip.eVersionMadeBy = madeBy
                , Zip.eExternalFileAttributes = attributes
                }
        rejected <-
          evalIO $
            withTempDir $ \tmpDir _ -> do
              archiveName <- encodeFS "link.zip"
              stagingName <- encodeFS "staging"
              let archivePath = tmpDir </> archiveName
                  staging = tmpDir </> stagingName
              writeFile archivePath $
                LazyByteString.toStrict $
                  Zip.fromArchive $
                    Zip.addEntryToArchive entry Zip.emptyArchive
              result <-
                stageBuiltinSource
                  (ArchiveSource ZipArchive archivePath)
                  staging
              stagingExists <- isDirectory staging
              return (result, stagingExists)
        rejected
          === (Left (UnsupportedArchiveEntry path), False)

    it "accepts a Unix ZIP directory declared without a trailing slash" $
      withTempDir $ \tmpDir _ -> do
        archiveName <- encodeFS "directory.zip"
        stagingName <- encodeFS "staging"
        nestedName <- encodeFS "nested"
        manifestName <- encodeFS "dojang.toml"
        let archivePath = tmpDir </> archiveName
            staging = tmpDir </> stagingName
            directoryEntry =
              (Zip.toEntry "nested" 0 "")
                { Zip.eVersionMadeBy = (3 `shiftL` 8) .|. 20
                , Zip.eExternalFileAttributes =
                    (0o040755 :: Word32) `shiftL` 16
                }
            fileEntry =
              (Zip.toEntry "nested/dojang.toml" 0 "manifest")
                { Zip.eVersionMadeBy = (3 `shiftL` 8) .|. 20
                , Zip.eExternalFileAttributes =
                    (0o100644 :: Word32) `shiftL` 16
                }
            archive =
              Zip.addEntryToArchive fileEntry $
                Zip.addEntryToArchive directoryEntry Zip.emptyArchive
        writeFile archivePath $
          LazyByteString.toStrict $
            Zip.fromArchive archive
        stageBuiltinSource
          (ArchiveSource ZipArchive archivePath)
          staging
          `shouldReturn` Right ()
        readFile (staging </> nestedName </> manifestName)
          `shouldReturn` "manifest"

    it "accepts arbitrary DOS ZIP directories without trailing slashes" $
      hedgehog $ do
        suffix <-
          forAll $
            Gen.string
              (Range.linear 1 40)
              (Gen.element $ ['a' .. 'z'] <> ['0' .. '9'])
        extraAttributes <-
          forAll $ Gen.word32 $ Range.linear 0 0xff
        let directoryPath = "directory-" <> suffix
            filePath = directoryPath <> "/dojang.toml"
            directoryEntry =
              (Zip.toEntry directoryPath 0 "")
                { Zip.eVersionMadeBy = 20
                , Zip.eExternalFileAttributes =
                    extraAttributes .|. 0x10
                }
            fileEntry =
              (Zip.toEntry filePath 0 "manifest")
                { Zip.eVersionMadeBy = 20
                }
            archive =
              Zip.addEntryToArchive fileEntry $
                Zip.addEntryToArchive directoryEntry Zip.emptyArchive
        observed <-
          evalIO $
            withTempDir $ \tmpDir _ -> do
              archiveName <- encodeFS "directory.zip"
              stagingName <- encodeFS "staging"
              nestedName <- encodeFS directoryPath
              manifestName <- encodeFS "dojang.toml"
              let archivePath = tmpDir </> archiveName
                  staging = tmpDir </> stagingName
              writeFile archivePath $
                LazyByteString.toStrict $
                  Zip.fromArchive archive
              result <-
                stageBuiltinSource
                  (ArchiveSource ZipArchive archivePath)
                  staging
              contents <- case result of
                Right () ->
                  Just <$> readFile (staging </> nestedName </> manifestName)
                Left _ -> return Nothing
              return (result, contents)
        observed === (Right (), Just "manifest")

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

    it "rejects pax record lengths outside a machine Int" $
      withTempDir $ \tmpDir _ -> do
        let cases =
              [ ("excess-digits", "18446744073709551646 a=values\n")
              , ("out-of-range", "9999999999999999999 a=v\n")
              ]
        mapM_
          ( \(label, paxContents) -> do
              archiveName <- encodeFS $ "overflow-pax-" <> label <> ".tar"
              stagingName <- encodeFS $ "staging-" <> label
              let archivePath = tmpDir </> archiveName
                  staging = tmpDir </> stagingName
                  paxEntry =
                    Tar.simpleEntry
                      (tarPath "pax_extended_header")
                      ( Tar.OtherEntryType
                          'x'
                          paxContents
                          (LazyByteString.length paxContents)
                      )
              writeFile archivePath $
                LazyByteString.toStrict $
                  Tar.write [paxEntry, tarFileEntry "dojang.toml" "manifest"]
              result <-
                stageBuiltinSource
                  (ArchiveSource TarArchive archivePath)
                  staging
              result `shouldSatisfy` isInvalidArchive
              isDirectory staging `shouldReturn` False
          )
          cases


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


isConflictingSource :: Either AcquisitionError () -> Bool
isConflictingSource (Left (ConflictingSourceEntry _)) = True
isConflictingSource _ = False

#ifdef mingw32_HOST_OS
archiveModeSpecs :: Spec
archiveModeSpecs = return ()


archiveSpecialFileSpecs :: Spec
archiveSpecialFileSpecs = return ()
#else
archiveModeSpecs :: Spec
archiveModeSpecs = do
  it "preserves arbitrary POSIX file modes from zip and tar archives" $
    hedgehog $ do
      format <- forAll $ Gen.element [ZipArchive, TarArchive, TarGzipArchive]
      mode <- forAll $ Gen.word $ Range.linear 1 0o777
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
            _ <-
              publishStagedDirectoryWithMetadata
                metadata
                staging
                destination
            getPortableMode $ destination </> scriptName
      observed === portableModeFromBits mode

  it "preserves arbitrary source root modes" $
    hedgehog $ do
      sourceKind <- forAll Gen.bool
      sourceMode <- forAll $ (0o700 .|.) <$> Gen.word (Range.linear 0 0o77)
      observed <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            sourceName <- encodeFS "source"
            archiveName <- encodeFS "repository.tar"
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            manifestName <- encodeFS "dojang.toml"
            let source = tmpDir </> sourceName
                archivePath = tmpDir </> archiveName
                staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
            builtin <-
              if sourceKind
                then do
                  createDirectory source
                  writeFile (source </> manifestName) "manifest"
                  setPortableMode source sourceMode
                  return $ DirectorySource source
                else do
                  let rootEntry =
                        (Tar.directoryEntry $ tarPath "./")
                          { Tar.entryPermissions = fromIntegral sourceMode
                          }
                  writeFile archivePath $
                    LazyByteString.toStrict $
                      Tar.write
                        [ rootEntry
                        , tarFileEntry "./dojang.toml" "manifest"
                        ]
                  return $ ArchiveSource TarArchive archivePath
            Right metadata <-
              stageBuiltinSourceWithMetadata builtin staging
            _ <-
              publishStagedDirectoryWithMetadata
                metadata
                staging
                destination
            getPortableMode destination
      observed === portableModeFromBits sourceMode

  it "keeps arbitrary staged contents owner-only before publication" $
    hedgehog $ do
      sourceKind <- forAll Gen.bool
      contents <- forAll $ Gen.bytes $ Range.linear 0 4096
      observed <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            sourceName <- encodeFS "source"
            archiveName <- encodeFS "repository.tar"
            stagingName <- encodeFS "staging"
            manifestName <- encodeFS "dojang.toml"
            let source = tmpDir </> sourceName
                archivePath = tmpDir </> archiveName
                staging = tmpDir </> stagingName
            builtin <-
              if sourceKind
                then do
                  createDirectory source
                  writeFile (source </> manifestName) contents
                  return $ DirectorySource source
                else do
                  writeFile archivePath $
                    LazyByteString.toStrict $
                      Tar.write
                        [ tarFileEntry
                            "dojang.toml"
                            (LazyByteString.fromStrict contents)
                        ]
                  return $ ArchiveSource TarArchive archivePath
            Right _ <- stageBuiltinSourceWithMetadata builtin staging
            getPortableMode staging
      observed === portableModeFromBits 0o700

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

  it "preserves explicit zero modes from arbitrary Unix ZIP entry types" $
    hedgehog $ do
      directory <- forAll Gen.bool
      observed <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            archiveName <- encodeFS "repository.zip"
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            entryName <- encodeFS "entry"
            let archivePath = tmpDir </> archiveName
                staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
                path = if directory then "entry/" else "entry"
                fileType =
                  if directory
                    then 0o040000
                    else 0o100000
                entry =
                  (Zip.toEntry path 0 "")
                    { Zip.eVersionMadeBy = (3 `shiftL` 8) .|. 20
                    , Zip.eExternalFileAttributes =
                        (fileType :: Word32) `shiftL` 16
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
              publishStagedDirectoryWithMetadata
                metadata
                staging
                destination
            let publishedEntry = destination </> entryName
            observedMode <- getPortableMode publishedEntry
            setPortableMode publishedEntry 0o700
            return observedMode
      observed.posixBits === Just 0

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

  it "reports modes that the filesystem only partially restores" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "repository.tar"
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "destination"
      partialName <- encodeFS "partially-restored"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
          entry =
            (tarFileEntry "partially-restored" "contents")
              { Tar.entryPermissions = 0o700
              }
      writeFile archivePath $
        LazyByteString.toStrict $
          Tar.write [entry]
      Right (Right metadata) <-
        runFailingModeIO $
          stageBuiltinSourceWithMetadata
            (ArchiveSource TarArchive archivePath)
            staging
      published <-
        runFailingModeIO $
          publishStagedDirectoryWithMetadata
            metadata
            staging
            destination
      published `shouldBe` Right ["partially-restored"]
      readFile (destination </> partialName) `shouldReturn` "contents"

  it "reports archive modes unavailable on writability-only filesystems" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "repository.tar"
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "destination"
      limitedName <- encodeFS "writability-only"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
          entry =
            (tarFileEntry "writability-only" "contents")
              { Tar.entryPermissions = 0o700
              }
      writeFile archivePath $
        LazyByteString.toStrict $
          Tar.write [entry]
      Right (Right metadata) <-
        runFailingModeIO $
          stageBuiltinSourceWithMetadata
            (ArchiveSource TarArchive archivePath)
            staging
      published <-
        runFailingModeIO $
          publishStagedDirectoryWithMetadata
            metadata
            staging
            destination
      published `shouldBe` Right ["writability-only"]
      readFile (destination </> limitedName) `shouldReturn` "contents"

  it "rejects an existing destination without modifying it" $
    withTempDir $ \tmpDir _ -> do
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "failing-rename"
      manifestName <- encodeFS "dojang.toml"
      let staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
      createDirectory staging
      createDirectory destination
      setPortableMode destination 0o600
      originalIdentity <- getFileIdentity destination
      writeFile (staging </> manifestName) "manifest"
      result <-
        runFailingModeIO $
          publishStagedDirectoryWithMetadata
            emptyStagedMetadata
            staging
            destination
      result `shouldSatisfy` isLeft
      isDirectory destination `shouldReturn` True
      getFileIdentity destination `shouldReturn` originalIdentity
      getPortableMode destination
        `shouldReturn` portableModeFromBits 0o600
      setPortableMode destination 0o700
      listDirectory destination `shouldReturn` []
      readFile (staging </> manifestName) `shouldReturn` "manifest"


archiveSpecialFileSpecs :: Spec
archiveSpecialFileSpecs = do
  it "rejects an archive changed while its contents are read" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "changing-archive.tar"
      stagingName <- encodeFS "staging"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
      writeFile archivePath "archive"
      result <-
        runFailingModeIO $
          stageBuiltinSource
            (ArchiveSource TarArchive archivePath)
            staging
      result `shouldBe` Right (Left ArchiveChangedDuringAcquisition)
      exists staging `shouldReturn` False

  it "reports a staging cleanup failure with the acquisition failure" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "cleanup-failure.tar"
      stagingName <- encodeFS "cleanup-failure-staging"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
      writeFile archivePath $
        LazyByteString.toStrict $
          Tar.write [tarFileEntry "cleanup-trigger" "contents"]
      result <-
        runFailingModeIO $
          stageBuiltinSource
            (ArchiveSource TarArchive archivePath)
            staging
      case result of
        Left err -> do
          Exception.displayException err
            `shouldSatisfy` isInfixOf "injected extraction failure"
          Exception.displayException err
            `shouldSatisfy` isInfixOf "additionally, staging cleanup failed"
          Exception.displayException err
            `shouldSatisfy` isInfixOf "injected cleanup failure"
        Right _ -> expectationFailure "staging unexpectedly succeeded"

  it "lets asynchronous archive-decoding exceptions escape" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "interrupted.zip"
      stagingName <- encodeFS "staging"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
      runFailingModeIO
        ( stageBuiltinSource
            (ArchiveSource ZipArchive archivePath)
            staging
        )
        `shouldThrow` (== UserInterrupt)

  it "copies directory files without buffering them in the acquisition layer" $
    withTempDir $ \tmpDir _ -> do
      sourceName <- encodeFS "source"
      stagingName <- encodeFS "staging"
      streamedName <- encodeFS "streamed"
      let source = tmpDir </> sourceName
          staging = tmpDir </> stagingName
      createDirectory source
      writeFile (source </> streamedName) "contents"
      result <-
        runFailingModeIO $
          stageBuiltinSource
            (DirectorySource source)
            staging
      result `shouldBe` Right (Right ())
      readFile (staging </> streamedName) `shouldReturn` "contents"

  it "does not block on a FIFO inside a directory source" $
    withTempDir $ \tmpDir _ -> do
      sourceName <- encodeFS "source"
      stagingName <- encodeFS "staging"
      pipeName <- encodeFS "pipe"
      let source = tmpDir </> sourceName
          staging = tmpDir </> stagingName
          pipe = source </> pipeName
      createDirectory source
      pipePath <- decodePath pipe
      Posix.createNamedPipe pipePath 0o600
      result <-
        timeout 1000000 $
          stageBuiltinSource
            (DirectorySource source)
            staging
      result
        `shouldBe` Just (Left (UnsupportedSourceEntry "pipe"))
      isDirectory staging `shouldReturn` False

  it "rejects arbitrary FIFO paths with supported archive extensions" $
    hedgehog $ do
      prefix <-
        forAll $
          Gen.string
            (Range.linear 1 40)
            (Gen.element $ ['a' .. 'z'] <> ['0' .. '9'])
      extension <-
        forAll $ Gen.element [".zip", ".tar", ".tar.gz", ".tgz"]
      (detected, archiveFilePath) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            archiveName <- encodeFS $ prefix <> extension
            let archivePath = tmpDir </> archiveName
            path <- decodePath archivePath
            Posix.createNamedPipe path 0o600
            result <- detectBuiltinSource archivePath
            return (result, path)
      detected === Left (SourceDoesNotExist archiveFilePath)

  it "does not block when given a FIFO archive source directly" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "repository.tar"
      stagingName <- encodeFS "staging"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
      archiveFilePath <- decodePath archivePath
      Posix.createNamedPipe archiveFilePath 0o600
      result <-
        timeout 1000000 $
          stageBuiltinSource
            (ArchiveSource TarArchive archivePath)
            staging
      result
        `shouldBe` Just (Left (SourceDoesNotExist archiveFilePath))

  it "widens restrictive staging after interrupted publication" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "repository.tar"
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "interrupted-publish"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
          rootEntry =
            (Tar.directoryEntry $ tarPath "./")
              { Tar.entryPermissions = 0o000
              }
      writeFile archivePath $
        LazyByteString.toStrict $
          Tar.write [rootEntry, tarFileEntry "dojang.toml" "manifest"]
      Right (Right metadata) <-
        runFailingModeIO $
          stageBuiltinSourceWithMetadata
            (ArchiveSource TarArchive archivePath)
            staging
      runFailingModeIO
        ( publishStagedDirectoryWithMetadata
            metadata
            staging
            destination
        )
        `shouldThrow` (== UserInterrupt)
      mode <- getPortableMode staging
      fmap (.&. 0o700) mode.posixBits `shouldBe` Just 0o700
      removeDirectoryRecursively staging
      isDirectory staging `shouldReturn` False

  it "widens restrictive staging after a publication error" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "repository.tar"
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "failing-rename"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
          rootEntry =
            (Tar.directoryEntry $ tarPath "./")
              { Tar.entryPermissions = 0o000
              }
      writeFile archivePath $
        LazyByteString.toStrict $
          Tar.write [rootEntry, tarFileEntry "dojang.toml" "manifest"]
      Right (Right metadata) <-
        runFailingModeIO $
          stageBuiltinSourceWithMetadata
            (ArchiveSource TarArchive archivePath)
            staging
      result <-
        runFailingModeIO $
          publishStagedDirectoryWithMetadata
            metadata
            staging
            destination
      case result of
        Left err ->
          Exception.displayException err
            `shouldSatisfy` isInfixOf "injected rename failure"
        Right _ -> expectationFailure "Expected the injected rename failure."
      mode <- getPortableMode staging
      fmap (.&. 0o700) mode.posixBits `shouldBe` Just 0o700
      exists destination `shouldReturn` False
      removeDirectoryRecursively staging


newtype FailingModeIO a
  = FailingModeIO (ExceptT IOError IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadError IOError
    )


runFailingModeIO :: FailingModeIO a -> IO (Either IOError a)
runFailingModeIO (FailingModeIO action) = runExceptT action


instance MonadFileSystem FailingModeIO where
  createPrivateDirectoryDurably path =
    liftIO (createPrivateDirectoryDurably path :: IO ())
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
  readRegularFile value = do
    path <- liftIO (decodePath value :: IO FilePath)
    case FilePath.takeFileName path of
      "interrupted.zip" -> return $ Just $ throw UserInterrupt
      "streamed" -> throwError $ userError "buffered read forbidden"
      _ -> liftIO (readRegularFile value :: IO (Maybe ByteString.ByteString))
  readRegularFileBounded limit value = do
    path <- liftIO (decodePath value :: IO FilePath)
    case FilePath.takeFileName path of
      "changing-archive.tar" -> return FileChangedDuringRead
      "interrupted.zip" -> return $ BoundedFileContents $ throw UserInterrupt
      _ -> liftIO (readRegularFileBounded limit value :: IO BoundedFileRead)
  copyRegularFile source destination =
    liftIO (copyRegularFile source destination :: IO Bool)
  copyRegularFileWithSnapshot snapshot source destination =
    liftIO $
      copyRegularFileWithSnapshot
        snapshot
        source
        destination
  writeFile path contents = do
    path' <- liftIO (decodePath path :: IO FilePath)
    if FilePath.takeFileName path' == "cleanup-trigger"
      then throwError $ userError "injected extraction failure"
      else liftIO (writeFile path contents :: IO ())
  replaceFile source destination =
    liftIO (replaceFile source destination :: IO ())
  renameDirectory source destination = do
    destinationPath <- liftIO (decodePath destination :: IO FilePath)
    case FilePath.takeFileName destinationPath of
      "failing-rename" ->
        throwError $ userError "injected rename failure"
      "interrupted-publish" -> liftIO $ Exception.throwIO UserInterrupt
      _ -> liftIO (renameDirectory source destination :: IO ())
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
  createPrivateDirectory value =
    liftIO (createPrivateDirectory value :: IO ())
  removeFile value = liftIO (removeFile value :: IO ())
  removeDirectory value = liftIO (removeDirectory value :: IO ())
  removeDirectoryRecursivelyIfIdentity value identity = do
    path <- liftIO (decodePath value :: IO FilePath)
    if FilePath.takeFileName path == "cleanup-failure-staging"
      then throwError $ userError "injected cleanup failure"
      else
        liftIO $
          removeDirectoryRecursivelyIfIdentity value identity
  listDirectory value = liftIO (listDirectory value :: IO [OsPath])
  getFileSize value = liftIO (getFileSize value :: IO Integer)
  getFileIdentity value =
    liftIO (getFileIdentity value :: IO (Maybe FileIdentity))
  getFileSnapshot value =
    liftIO (getFileSnapshot value :: IO (Maybe FileSnapshot))
  getFileModeSnapshot value = do
    identity <- getFileIdentity value
    case identity of
      Nothing -> return Nothing
      Just entryIdentity ->
        Just . FileModeSnapshot entryIdentity <$> getPortableMode value
  getPortableMode value = do
    path <- liftIO (decodePath value :: IO FilePath)
    case FilePath.takeFileName path of
      "partially-restored" -> return $ portableModeFromBits 0o600
      "partially-restored-root" -> return $ portableModeFromBits 0o600
      "writability-only" ->
        return PortableMode{posixBits = Nothing, writable = True}
      _ -> liftIO (getPortableMode value :: IO PortableMode)
  setPortableMode path mode = do
    path' <- liftIO (decodePath path :: IO FilePath)
    if FilePath.takeFileName path' == "script"
      then throwError $ userError "injected mode-restoration failure"
      else liftIO (setPortableMode path mode :: IO ())
  setPortableWritable path writable =
    liftIO (setPortableWritable path writable :: IO ())
  createSymbolicLink target link fileType =
    liftIO (createSymbolicLink target link fileType :: IO ())


data CurrentDirectoryRace
  = ModifySourceBeforeCopy OsPath ByteString.ByteString
  | AddSourceEntryBeforeCopy OsPath ByteString.ByteString
  | ReplaceSourceBeforeCopy FileType OsPath OsPath
  | SwapDirectoryDuringModeRead OsPath OsPath OsPath
  | ChangeSourceModeBeforeCopy OsPath Word
  | ChangeSourceModeDuringValidation OsPath Word
  | VanishSourceEntryDuringEnumeration OsPath OsPath
  | VanishSourceEntryDuringVerification OsPath OsPath OsPath

newtype CurrentDirectoryIO a
  = CurrentDirectoryIO
      (ReaderT (OsPath, Maybe CurrentDirectoryRace) (ExceptT IOError IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadError IOError
    )


runCurrentDirectoryIOWithRace
  :: OsPath
  -> CurrentDirectoryRace
  -> CurrentDirectoryIO a
  -> IO (Either IOError a)
runCurrentDirectoryIOWithRace
  currentDirectory
  race
  (CurrentDirectoryIO action) =
    runExceptT $
      runReaderT
        action
        (currentDirectory, Just race)


instance MonadFileSystem CurrentDirectoryIO where
  createPrivateDirectoryDurably path =
    liftIO (createPrivateDirectoryDurably path :: IO ())
  encodePath "." = CurrentDirectoryIO $ fst <$> ask
  encodePath value = liftIO (encodePath value :: IO OsPath)
  decodePath value = liftIO (decodePath value :: IO FilePath)
  getCurrentDirectory = CurrentDirectoryIO $ fst <$> ask
  getHomeDirectory = liftIO (getHomeDirectory :: IO OsPath)
  exists value = liftIO (exists value :: IO Bool)
  isFile value = liftIO (isFile value :: IO Bool)
  isRegularFile value = liftIO (isRegularFile value :: IO Bool)
  isDirectory value = liftIO (isDirectory value :: IO Bool)
  isSymlink value = liftIO (isSymlink value :: IO Bool)
  readFile value = liftIO (readFile value :: IO ByteString.ByteString)
  readRegularFile value =
    liftIO (readRegularFile value :: IO (Maybe ByteString.ByteString))
  readRegularFileBounded limit value =
    liftIO (readRegularFileBounded limit value :: IO BoundedFileRead)
  copyRegularFile source destination =
    liftIO (copyRegularFile source destination :: IO Bool)
  copyRegularFileWithSnapshot snapshot source destination =
    liftIO $
      copyRegularFileWithSnapshot snapshot source destination
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
  getSymbolicLinkType value =
    liftIO (getSymbolicLinkType value :: IO FileType)
  copyFile source destination =
    liftIO (copyFile source destination :: IO ())
  copyFileWithMetadata source destination =
    liftIO (copyFileWithMetadata source destination :: IO ())
  copyFilePermissions source destination =
    liftIO (copyFilePermissions source destination :: IO ())
  createDirectory value = liftIO (createDirectory value :: IO ())
  createPrivateDirectory value = do
    liftIO (createPrivateDirectory value :: IO ())
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just (ModifySourceBeforeCopy sourceEntry replacement) ->
        liftIO $ writeFile sourceEntry replacement
      Just (AddSourceEntryBeforeCopy sourceEntry contents) ->
        liftIO $ writeFile sourceEntry contents
      Just (ReplaceSourceBeforeCopy sourceType sourceEntry replacementTarget) ->
        liftIO $ do
          case sourceType of
            Directory -> removeDirectoryRecursively sourceEntry
            File -> removeFile sourceEntry
            Symlink -> removeFile sourceEntry
          createSymbolicLink
            replacementTarget
            sourceEntry
            sourceType
      Just (SwapDirectoryDuringModeRead _ _ _) -> return ()
      Just (ChangeSourceModeBeforeCopy sourceEntry mode) ->
        liftIO $ setPortableMode sourceEntry mode
      Just (ChangeSourceModeDuringValidation _ _) -> return ()
      Just (VanishSourceEntryDuringEnumeration _ _) -> return ()
      Just (VanishSourceEntryDuringVerification _ _ _) -> return ()
      Nothing -> return ()
  removeFile value = liftIO (removeFile value :: IO ())
  removeDirectory value = liftIO (removeDirectory value :: IO ())
  listDirectory value = liftIO (listDirectory value :: IO [OsPath])
  listDirectoryRecursivelyStrict value ignorePatterns = do
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just (VanishSourceEntryDuringEnumeration source vanished)
        | value == source -> do
            path <- decodePath vanished
            throwError $
              mkIOError
                doesNotExistErrorType
                "fstatat"
                Nothing
                (Just path)
      Just (VanishSourceEntryDuringVerification source vanished staging)
        | value == source -> do
            stagingExists <- exists staging
            if stagingExists
              then do
                path <- decodePath vanished
                throwError $
                  mkIOError
                    doesNotExistErrorType
                    "fstatat"
                    Nothing
                    (Just path)
              else delegate
      _ ->
        delegate
   where
    delegate =
      liftIO
        ( listDirectoryRecursivelyStrict value ignorePatterns
            :: IO [(FileType, OsPath)]
        )
  getFileSize value = liftIO (getFileSize value :: IO Integer)
  getFileIdentity value =
    liftIO (getFileIdentity value :: IO (Maybe FileIdentity))
  getFileSnapshot value =
    liftIO (getFileSnapshot value :: IO (Maybe FileSnapshot))
  getFileModeSnapshot value = do
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just (ChangeSourceModeDuringValidation sourceEntry changedMode)
        | value == sourceEntry ->
            liftIO $ do
              snapshot <- getFileModeSnapshot value
              setPortableMode value changedMode
              return snapshot
      _ ->
        liftIO (getFileModeSnapshot value :: IO (Maybe FileModeSnapshot))
  getPortableMode value = do
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just (SwapDirectoryDuringModeRead sourceEntry replacement backup)
        | value == sourceEntry ->
            liftIO $ do
              renameDirectory sourceEntry backup
              renameDirectory replacement sourceEntry
              mode <- getPortableMode sourceEntry
              renameDirectory sourceEntry replacement
              renameDirectory backup sourceEntry
              return mode
      _ -> liftIO (getPortableMode value :: IO PortableMode)
  setPortableMode path mode =
    liftIO (setPortableMode path mode :: IO ())
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
  it "preserves dry-run permission errors during strict traversal" $
    withTempDir $ \tmpDir _ -> do
      sourceName <- encodeFS "source"
      directoryName <- encodeFS "restricted"
      entryName <- encodeFS "entry"
      stagingName <- encodeFS "staging"
      let source = tmpDir </> sourceName
          restricted = source </> directoryName
          staging = tmpDir </> stagingName
      createDirectory source
      createDirectory restricted
      writeFile (restricted </> entryName) "contents"
      setPortableMode restricted 0o400
      result <-
        Exception.finally
          ( tryError $
              dryRunIO $
                stageBuiltinSource (DirectorySource source) staging
          )
          (setPortableMode restricted 0o700)
      case result of
        Left err -> err `shouldSatisfy` isPermissionError
        Right value ->
          expectationFailure $
            "Expected a permission error, got: " <> show value

  it "classifies arbitrary vanished enumerated entries as source changes" $
    hedgehog $ do
      suffix <-
        forAll $
          Gen.string (Range.linear 1 32) Gen.alphaNum
      (stagedResult, stagingExists, vanishedPath) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            sourceName <- encodeFS "source"
            entryName <- encodeFS $ "entry-" <> suffix
            stagingName <- encodeFS "staging"
            let source = tmpDir </> sourceName
                vanished = source </> entryName
                staging = tmpDir </> stagingName
            createDirectory source
            writeFile vanished "contents"
            result <-
              runCurrentDirectoryIOWithRace
                tmpDir
                (VanishSourceEntryDuringEnumeration source vanished)
                (stageBuiltinSource (DirectorySource source) staging)
            present <- exists staging
            decodedVanished <- decodePath vanished
            return (result, present, decodedVanished)
      stagedResult
        === Right (Left $ SourceChangedDuringAcquisition vanishedPath)
      stagingExists === False

  it "reports arbitrary entries vanished during final verification" $
    hedgehog $ do
      suffix <-
        forAll $
          Gen.string (Range.linear 1 32) Gen.alphaNum
      (stagedResult, stagingExists, vanishedPath) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            sourceName <- encodeFS "source"
            entryName <- encodeFS $ "entry-" <> suffix
            stagingName <- encodeFS "staging"
            let source = tmpDir </> sourceName
                vanished = source </> entryName
                staging = tmpDir </> stagingName
            createDirectory source
            writeFile vanished "contents"
            result <-
              runCurrentDirectoryIOWithRace
                tmpDir
                (VanishSourceEntryDuringVerification source vanished staging)
                (stageBuiltinSource (DirectorySource source) staging)
            present <- exists staging
            decodedVanished <- decodePath vanished
            return (result, present, decodedVanished)
      assert $ case stagedResult of
        Left err ->
          ( "bootstrap source entry changed during acquisition: "
              <> vanishedPath
          )
            `isInfixOf` Exception.displayException err
        Right _ -> False
      stagingExists === False

  it "binds arbitrary retained directory modes to source identities" $
    hedgehog $ do
      originalMode <-
        forAll $
          Gen.word $ Range.linear 0o700 0o707
      replacementMode <-
        forAll $
          Gen.word $ Range.linear 0o750 0o757
      retainedMode <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            sourceName <- encodeFS "source"
            directoryName <- encodeFS "directory"
            replacementName <- encodeFS "replacement"
            backupName <- encodeFS "backup"
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            let source = tmpDir </> sourceName
                sourceDirectory = source </> directoryName
                replacement = tmpDir </> replacementName
                backup = tmpDir </> backupName
                staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
            createDirectory source
            createDirectory sourceDirectory
            createDirectory replacement
            setPortableMode sourceDirectory originalMode
            setPortableMode replacement replacementMode
            Right (Right metadata) <-
              runCurrentDirectoryIOWithRace
                tmpDir
                ( SwapDirectoryDuringModeRead
                    sourceDirectory
                    replacement
                    backup
                )
                ( stageBuiltinSourceWithMetadata
                    (DirectorySource source)
                    staging
                )
            _ <-
              publishStagedDirectoryWithMetadata
                metadata
                staging
                destination
            mode <- getPortableMode $ destination </> directoryName
            return mode.posixBits
      retainedMode === Just originalMode

  it "classifies arbitrary source-root changes during validation" $
    hedgehog $ do
      originalMode <-
        forAll $
          Gen.word $ Range.linear 0o700 0o707
      changedMode <-
        forAll $
          Gen.word $ Range.linear 0o750 0o757
      (stagedResult, stagingExists, sourcePath) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            sourceName <- encodeFS "source"
            stagingName <- encodeFS "staging"
            let source = tmpDir </> sourceName
                staging = tmpDir </> stagingName
            createDirectory source
            setPortableMode source originalMode
            result <-
              runCurrentDirectoryIOWithRace
                tmpDir
                (ChangeSourceModeDuringValidation source changedMode)
                (stageBuiltinSource (DirectorySource source) staging)
            present <- exists staging
            decodedSource <- decodePath source
            return (result, present, decodedSource)
      stagedResult
        === Right (Left $ SourceChangedDuringAcquisition sourcePath)
      stagingExists === False

  forM_
    [ ("source root", False)
    , ("nested directory", True)
    ]
    $ \(label, nested) ->
      it ("rejects arbitrary mode changes to the " <> label) $
        hedgehog $ do
          originalMode <-
            forAll $
              Gen.word $ Range.linear 0o700 0o707
          changedMode <-
            forAll $
              Gen.word $ Range.linear 0o750 0o757
          (stagedResult, stagingExists) <-
            evalIO $
              withTempDir $ \tmpDir _ -> do
                sourceName <- encodeFS "source"
                directoryName <- encodeFS "directory"
                stagingName <- encodeFS "staging"
                let source = tmpDir </> sourceName
                    sourceDirectory = source </> directoryName
                    changedPath =
                      if nested then sourceDirectory else source
                    staging = tmpDir </> stagingName
                createDirectory source
                createDirectory sourceDirectory
                setPortableMode changedPath originalMode
                result <-
                  runCurrentDirectoryIOWithRace
                    tmpDir
                    (ChangeSourceModeBeforeCopy changedPath changedMode)
                    ( stageBuiltinSource
                        (DirectorySource source)
                        staging
                    )
                present <- exists staging
                return (result, present)
          case stagedResult of
            Left err ->
              assert $
                "bootstrap source entry changed during acquisition"
                  `isInfixOf` Exception.displayException err
            Right _ -> assert False
          stagingExists === False

  it "rejects arbitrary entries added after directory enumeration" $
    hedgehog $ do
      addedContents <- forAll $ Gen.bytes $ Range.linear 0 4096
      (stagedResult, stagingExists) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            sourceName <- encodeFS "source"
            stagingName <- encodeFS "staging"
            originalName <- encodeFS "original"
            addedName <- encodeFS "added"
            let source = tmpDir </> sourceName
                staging = tmpDir </> stagingName
                added = source </> addedName
            createDirectory source
            writeFile (source </> originalName) "original"
            result <-
              runCurrentDirectoryIOWithRace
                tmpDir
                (AddSourceEntryBeforeCopy added addedContents)
                (stageBuiltinSource (DirectorySource source) staging)
            present <- exists staging
            return (result, present)
      case stagedResult of
        Left err ->
          assert $
            "bootstrap source entry changed during acquisition"
              `isInfixOf` Exception.displayException err
        Right _ -> assert False
      stagingExists === False

  it "rejects arbitrary in-place changes after source validation" $
    hedgehog $ do
      original <- forAll $ Gen.bytes $ Range.linear 0 4096
      suffix <- forAll $ Gen.bytes $ Range.linear 1 64
      let replacement = original <> suffix
      (stagedResult, stagingExists, stagedContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            sourceName <- encodeFS "source"
            stagingName <- encodeFS "staging"
            victimName <- encodeFS "victim"
            let source = tmpDir </> sourceName
                staging = tmpDir </> stagingName
                victim = source </> victimName
                stagedVictim = staging </> victimName
            createDirectory source
            writeFile victim original
            result <-
              runCurrentDirectoryIOWithRace
                tmpDir
                (ModifySourceBeforeCopy victim replacement)
                (stageBuiltinSource (DirectorySource source) staging)
            present <- exists staging
            stagedVictimPresent <- exists stagedVictim
            contents <-
              if stagedVictimPresent
                then readFile stagedVictim
                else return ""
            return (result, present, contents)
      case stagedResult of
        Left err ->
          assert $
            "bootstrap source entry changed during acquisition"
              `isInfixOf` Exception.displayException err
        Right _ -> assert False
      stagingExists === False
      stagedContents === ""

  it "rejects a regular file replaced by a link after validation" $
    hedgehog $ do
      outsideContents <- forAll $ Gen.bytes $ Range.linear 0 4096
      (stagedResult, stagingExists, stagedContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            sourceName <- encodeFS "source"
            stagingName <- encodeFS "staging"
            victimName <- encodeFS "victim"
            outsideName <- encodeFS "outside"
            let source = tmpDir </> sourceName
                staging = tmpDir </> stagingName
                victim = source </> victimName
                outside = tmpDir </> outsideName
                stagedVictim = staging </> victimName
            createDirectory source
            writeFile victim "original"
            writeFile outside outsideContents
            result <-
              runCurrentDirectoryIOWithRace
                tmpDir
                (ReplaceSourceBeforeCopy File victim outside)
                (stageBuiltinSource (DirectorySource source) staging)
            present <- exists staging
            stagedVictimPresent <- exists stagedVictim
            contents <-
              if stagedVictimPresent
                then readFile stagedVictim
                else return ""
            return (result, present, contents)
      case stagedResult of
        Left err ->
          assert $
            "bootstrap source entry changed during acquisition"
              `isInfixOf` Exception.displayException err
        Right _ -> assert False
      stagingExists === False
      stagedContents === ""

  it "rejects a directory ancestor replaced by a link after validation" $
    hedgehog $ do
      outsideContents <- forAll $ Gen.bytes $ Range.linear 0 4096
      (stagedResult, stagingExists, stagedContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            sourceName <- encodeFS "source"
            stagingName <- encodeFS "staging"
            nestedName <- encodeFS "nested"
            victimName <- encodeFS "victim"
            outsideName <- encodeFS "outside"
            let source = tmpDir </> sourceName
                staging = tmpDir </> stagingName
                nested = source </> nestedName
                victim = nested </> victimName
                outside = tmpDir </> outsideName
                stagedVictim = staging </> nestedName </> victimName
            createDirectory source
            createDirectory nested
            createDirectory outside
            writeFile victim "original"
            writeFile (outside </> victimName) outsideContents
            result <-
              runCurrentDirectoryIOWithRace
                tmpDir
                (ReplaceSourceBeforeCopy Directory nested outside)
                (stageBuiltinSource (DirectorySource source) staging)
            present <- exists staging
            stagedVictimPresent <- exists stagedVictim
            contents <-
              if stagedVictimPresent
                then readFile stagedVictim
                else return ""
            return (result, present, contents)
      isLeft stagedResult === True
      stagingExists === False
      stagedContents === ""

  it "preserves arbitrary target modes for symlinked directory sources" $
    hedgehog $ do
      sourceMode <- forAll $ (0o700 .|.) <$> Gen.word (Range.linear 0 0o77)
      observed <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            sourceName <- encodeFS "source"
            sourceLinkName <- encodeFS "source-link"
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            manifestName <- encodeFS "dojang.toml"
            let source = tmpDir </> sourceName
                sourceLink = tmpDir </> sourceLinkName
                staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
            createDirectory source
            writeFile (source </> manifestName) "manifest"
            setPortableMode source sourceMode
            System.Directory.OsPath.createDirectoryLink
              sourceName
              sourceLink
            Right metadata <-
              stageBuiltinSourceWithMetadata
                (DirectorySource sourceLink)
                staging
            _ <-
              publishStagedDirectoryWithMetadata
                metadata
                staging
                destination
            getPortableMode destination
      observed === portableModeFromBits sourceMode

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
      stagingName <- encodeFS "staging"
      manifestName <- encodeFS "dojang.toml"
      let archivePath = tmpDir </> archiveName
          linkPath = tmpDir </> linkName
          staging = tmpDir </> stagingName
      writeFile archivePath $
        LazyByteString.toStrict $
          zipBytes "dojang.toml" "contents"
      System.Directory.OsPath.createFileLink archiveName linkPath
      detectBuiltinSource linkPath
        `shouldReturn` Right (ArchiveSource ZipArchive linkPath)
      stageBuiltinSource (ArchiveSource ZipArchive linkPath) staging
        `shouldReturn` Right ()
      readFile (staging </> manifestName) `shouldReturn` "contents"

  it "does not publish through a replaced destination symbolic link" $
    withTempDir $ \tmpDir _ -> do
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "destination"
      redirectedName <- encodeFS "redirected"
      manifestName <- encodeFS "dojang.toml"
      let staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
          redirected = tmpDir </> redirectedName
      createDirectory staging
      createDirectory redirected
      writeFile (staging </> manifestName) "manifest"
      System.Directory.OsPath.createDirectoryLink
        redirectedName
        destination
      publishStagedDirectory staging destination
        `shouldThrow` anyIOException
      exists (redirected </> manifestName) `shouldReturn` False
      isDirectory staging `shouldReturn` True

#endif
