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
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (toLower, toUpper)
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Word (Word32)
import Hedgehog (assert, evalIO, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.FilePath.Posix qualified as Posix
import System.Info (os)


#ifndef mingw32_HOST_OS
import Control.Concurrent
  ( MVar
  , forkFinally
  , forkIO
  , newEmptyMVar
  , putMVar
  , takeMVar
  , threadDelay
  , tryPutMVar
  , tryTakeMVar
  )
import Control.Exception
  ( AsyncException (ThreadKilled, UserInterrupt)
  , throw
  )
import Control.Exception qualified as Exception
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
  ( ExceptT (..)
  , MonadError (throwError)
  , runExceptT
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import System.Directory.OsPath qualified
import System.FilePath qualified as FilePath
import System.OsPath (OsPath, takeDirectory)
import System.Posix.Files qualified as Posix
import System.Timeout (timeout)
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
  , emptyStagedMetadata
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
  ( FileIdentity
  , FileSnapshot
  , FileType (..)
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

    it "honors atomic exchange support for an existing empty destination" $
      withTempDir $ \tmpDir _ -> do
        stagingName <- encodeFS "staging"
        destinationName <- encodeFS "destination"
        manifestName <- encodeFS "dojang.toml"
        let staging = tmpDir </> stagingName
            destination = tmpDir </> destinationName
        createDirectory staging
        createDirectory destination
        writeFile (staging </> manifestName) "manifest"
        if os == "mingw32"
          then do
            publishStagedDirectory staging destination
              `shouldThrow` anyIOException
            listDirectory destination `shouldReturn` []
            readFile (staging </> manifestName) `shouldReturn` "manifest"
          else do
            publishStagedDirectory staging destination
            isDirectory staging `shouldReturn` False
            readFile (destination </> manifestName) `shouldReturn` "manifest"

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


archiveSpecialFileSpecs :: Spec
archiveSpecialFileSpecs = return ()
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

  it "restores modes before exposing current-directory entries" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "repository.tar"
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "destination"
      scriptName <- encodeFS "script"
      victimName <- encodeFS "victim"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
          script = destination </> scriptName
          victim = tmpDir </> victimName
          entry =
            (tarFileEntry "script" "#!/bin/sh\n")
              { Tar.entryPermissions = 0o700
              }
      writeFile archivePath $
        LazyByteString.toStrict $
          Tar.write [entry]
      Right metadata <-
        stageBuiltinSourceWithMetadata
          (ArchiveSource TarArchive archivePath)
          staging
      createDirectory destination
      writeFile victim "unrelated"
      setPortableMode victim 0o600
      published <-
        runCurrentDirectoryIOWithRace
          destination
          (ReplaceBeforeMode script victim)
          (publishStagedDirectoryWithMetadata metadata staging destination)
      published `shouldBe` Right []
      getPortableMode victim `shouldReturn` portableModeFromBits 0o600
      getPortableMode script `shouldReturn` portableModeFromBits 0o700

  it "rolls back restrictive current-directory entries from quarantine" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "repository.tar"
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "destination"
      privateName <- encodeFS "private"
      let archivePath = tmpDir </> archiveName
          staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
          directoryEntry =
            (Tar.directoryEntry $ tarPath "private")
              { Tar.entryPermissions = 0o000
              }
          fileEntry =
            (tarFileEntry "private/secret" "secret")
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
      published <-
        runCurrentDirectoryIOWithRace
          destination
          (FailStagingRemoval staging)
          (publishStagedDirectoryWithMetadata metadata staging destination)
      published `shouldSatisfy` isLeft
      exists (destination </> privateName) `shouldReturn` False

  it "preserves arbitrary source and destination root modes" $
    hedgehog $ do
      sourceKind <- forAll Gen.bool
      existingDestination <- forAll Gen.bool
      sourceMode <- forAll $ (0o700 .|.) <$> Gen.word (Range.linear 0 0o77)
      destinationMode <-
        forAll $ (0o700 .|.) <$> Gen.word (Range.linear 0 0o77)
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
            when existingDestination $ do
              createDirectory destination
              setPortableMode destination destinationMode
            _ <-
              publishStagedDirectoryWithMetadata
                metadata
                staging
                destination
            getPortableMode destination
      observed
        === portableModeFromBits
          (if existingDestination then destinationMode else sourceMode)

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

  it "rolls back before restrictive modes when staging removal fails" $
    withTempDir $ \tmpDir _ -> do
      archiveName <- encodeFS "repository.tar"
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "destination"
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
      published <-
        runCurrentDirectoryIOWithRace
          destination
          (FailStagingRemoval staging)
          ( publishStagedDirectoryWithMetadata
              metadata
              staging
              destination
          )
      published `shouldSatisfy` isLeft
      listDirectory destination `shouldReturn` []

  it "restores arbitrary destination modes when staging removal fails" $
    hedgehog $ do
      destinationMode <-
        forAll $ (0o500 .|.) <$> Gen.word (Range.linear 0 0o77)
      (published, observedMode) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            manifestName <- encodeFS "dojang.toml"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
            createDirectory staging
            createDirectory destination
            writeFile (staging </> manifestName) "manifest"
            setPortableMode destination destinationMode
            result <-
              runCurrentDirectoryIOWithRace
                tmpDir
                (FailStagingRemoval staging)
                (publishStagedDirectory staging destination)
            mode <- getPortableMode destination
            setPortableMode destination 0o700
            return (result, mode)
      isLeft published === True
      observedMode === portableModeFromBits destinationMode

  it "does not report a discarded source root mode" $
    withTempDir $ \tmpDir _ -> do
      sourceName <- encodeFS "source"
      stagingName <- encodeFS "partially-restored-root"
      destinationName <- encodeFS "destination"
      manifestName <- encodeFS "dojang.toml"
      let source = tmpDir </> sourceName
          staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
      createDirectory source
      writeFile (source </> manifestName) "manifest"
      setPortableMode source 0o700
      Right (Right metadata) <-
        runFailingModeIO $
          stageBuiltinSourceWithMetadata
            (DirectorySource source)
            staging
      createDirectory destination
      setPortableMode destination 0o600
      published <-
        runFailingModeIO $
          publishStagedDirectoryWithMetadata
            metadata
            staging
            destination
      published `shouldBe` Right []

  it "preserves arbitrary restrictive destination root permissions" $
    hedgehog $ do
      destinationMode <-
        forAll $ (0o700 .|.) <$> Gen.word (Range.linear 0 0o77)
      observed <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            manifestName <- encodeFS "dojang.toml"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
            createDirectory staging
            createDirectory destination
            writeFile (staging </> manifestName) "manifest"
            setPortableMode destination destinationMode
            publishStagedDirectory staging destination
            getPortableMode destination
      observed === portableModeFromBits destinationMode

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

  it "rejects an existing destination when atomic exchange is unavailable" $
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
  copyRegularFile source destination =
    liftIO (copyRegularFile source destination :: IO Bool)
  copyRegularFileWithSnapshot snapshot source destination =
    liftIO $
      copyRegularFileWithSnapshot
        snapshot
        source
        destination
  writeFile path contents = liftIO (writeFile path contents :: IO ())
  replaceFile source destination =
    liftIO (replaceFile source destination :: IO ())
  renameDirectory source destination = do
    destinationPath <- liftIO (decodePath destination :: IO FilePath)
    case FilePath.takeFileName destinationPath of
      "failing-rename" ->
        throwError $ userError "injected rename failure"
      "interrupted-publish" -> liftIO $ Exception.throwIO UserInterrupt
      _ -> liftIO (renameDirectory source destination :: IO ())
  exchangeDirectories source destination = do
    destinationPath <- liftIO (decodePath destination :: IO FilePath)
    if FilePath.takeFileName destinationPath == "failing-rename"
      then return False
      else liftIO (exchangeDirectories source destination :: IO Bool)
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
  listDirectory value = liftIO (listDirectory value :: IO [OsPath])
  getFileSize value = liftIO (getFileSize value :: IO Integer)
  getFileIdentity value =
    liftIO (getFileIdentity value :: IO (Maybe FileIdentity))
  getFileSnapshot value =
    liftIO (getFileSnapshot value :: IO (Maybe FileSnapshot))
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
  = CreateBeforeCopy OsPath ByteString.ByteString
  | CreateAfterExchange OsPath OsPath ByteString.ByteString
  | ReplaceDestinationWithCurrentDirectory OsPath OsPath
  | ReplaceDestinationBetweenModeAndIdentity OsPath Word (MVar ())
  | ReplaceDestinationBeforeExchange
      OsPath
      OsPath
      ByteString.ByteString
      Word
  | ReplaceDestinationWithSymlinkBeforeExchange OsPath OsPath OsPath
  | InterruptAfterExchange
      OsPath
      OsPath
      ByteString.ByteString
  | InterruptDuringExchangeRollback
      OsPath
      OsPath
      ByteString.ByteString
      (MVar ())
      (MVar ())
      (MVar ())
  | ModifySourceBeforeCopy OsPath ByteString.ByteString
  | ReplaceSourceBeforeCopy FileType OsPath OsPath
  | FailQuarantine OsPath OsPath
  | ReplaceThenFail OsPath OsPath ByteString.ByteString
  | ReplaceBeforeMode OsPath OsPath
  | ReplaceAfterCopyThenFail
      OsPath
      OsPath
      ByteString.ByteString
      OsPath
  | ReplaceDuringRollback
      OsPath
      OsPath
      ByteString.ByteString
      OsPath
  | InterruptBeforeCopy OsPath
  | FailStagingRemoval OsPath
  | ReplaceBeforeRollbackMode OsPath OsPath OsPath


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


runCurrentDirectoryIO
  :: OsPath -> CurrentDirectoryIO a -> IO (Either IOError a)
runCurrentDirectoryIO currentDirectory (CurrentDirectoryIO action) =
  runExceptT $ runReaderT action (currentDirectory, Nothing)


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


liftCurrentDirectoryIO :: IO a -> CurrentDirectoryIO a
liftCurrentDirectoryIO action =
  CurrentDirectoryIO $ lift $ ExceptT $ Exception.try action


instance MonadFileSystem CurrentDirectoryIO where
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
  copyRegularFileWithSnapshot snapshot source destination =
    liftCurrentDirectoryIO $
      copyRegularFileWithSnapshot
        snapshot
        source
        destination
  copyRegularFileNoReplace source destination = do
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just (CreateBeforeCopy racedPath racedContents)
        | destination == racedPath ->
            liftIO $ do
              present <- exists destination
              unless present $
                writeFile destination racedContents
      Just (ReplaceThenFail trigger replacedPath replacement)
        | destination == trigger -> do
            liftIO $ do
              removeFile replacedPath
              writeFile replacedPath replacement
            throwError $ userError "injected publication failure"
      Just (InterruptBeforeCopy trigger)
        | destination == trigger ->
            liftIO $ Exception.throwIO UserInterrupt
      _ -> return ()
    copied <-
      liftCurrentDirectoryIO $
      copyRegularFileNoReplace source destination
    case racedEntry of
      Just (ReplaceAfterCopyThenFail racedPath replacedPath replacement _)
        | destination == racedPath ->
            liftIO $ do
              removeFile replacedPath
              writeFile replacedPath replacement
      _ -> return ()
    return copied
  writeFile path contents = do
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just (CreateBeforeCopy racedPath racedContents)
        | path == racedPath ->
            liftIO $ do
              present <- exists path
              unless present $
                writeFile path racedContents
      _ -> return ()
    liftIO (writeFile path contents :: IO ())
  replaceFile source destination =
    liftIO (replaceFile source destination :: IO ())
  renameDirectory source destination =
    liftIO (renameDirectory source destination :: IO ())
  renameEntry fileType source destination = do
    (_, racedEntry) <- CurrentDirectoryIO ask
    let move =
          liftCurrentDirectoryIO $
            renameEntry fileType source destination
    case racedEntry of
      Just (CreateBeforeCopy racedPath racedContents)
        | destination == takeDirectory racedPath -> do
            liftIO $ do
              createDirectory destination
              writeFile racedPath racedContents
            move
      Just (ReplaceThenFail trigger replacedPath replacement)
        | destination == takeDirectory trigger -> do
            liftIO $ do
              removeFile replacedPath
              writeFile replacedPath replacement
            throwError $ userError "injected publication failure"
      Just (ReplaceAfterCopyThenFail racedPath replacedPath replacement _)
        | destination == racedPath -> do
            move
            liftIO $ do
              renameEntry fileType replacedPath source
              writeFile replacedPath replacement
      Just (ReplaceDuringRollback racedPath displacedPath replacement _)
        | source == racedPath -> do
            liftIO $ do
              renameEntry fileType racedPath displacedPath
              writeFile racedPath replacement
            move
      Just (InterruptBeforeCopy trigger)
        | destination == takeDirectory trigger ->
            liftIO $ Exception.throwIO UserInterrupt
      Just (FailQuarantine published _)
        | source == published ->
            throwError $ userError "injected quarantine failure"
      _ -> move
  exchangeDirectories source destination = do
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just
        ( ReplaceDestinationBeforeExchange
            racedStaging
            racedDestination
            contents
            replacementMode
          )
          | source == racedStaging && destination == racedDestination ->
              liftIO $ do
                destinationEntries <-
                  System.Directory.OsPath.listDirectory destination
                when (null destinationEntries) $ do
                  removeDirectory destination
                  writeFile destination contents
                  setPortableMode destination replacementMode
      Just
        ( ReplaceDestinationWithSymlinkBeforeExchange
            racedStaging
            racedDestination
            target
          )
          | source == racedStaging && destination == racedDestination ->
              liftIO $ do
                destinationEntries <-
                  System.Directory.OsPath.listDirectory destination
                when (null destinationEntries) $ do
                  removeDirectory destination
                  createSymbolicLink target destination File
      _ -> return ()
    concurrentPresent <- case racedEntry of
      Just (CreateAfterExchange racedDestination concurrentPath _)
        | destination == racedDestination ->
            liftIO $ exists concurrentPath
      _ -> return False
    exchanged <-
      liftIO (exchangeDirectories source destination :: IO Bool)
    case racedEntry of
      Just (CreateAfterExchange racedDestination concurrentPath contents)
        | destination == racedDestination
            && exchanged
            && not concurrentPresent ->
            liftIO $ writeFile concurrentPath contents
      Just (InterruptAfterExchange racedDestination concurrentPath contents)
        | destination == racedDestination && exchanged ->
            liftIO $ writeFile concurrentPath contents
      Just
        ( InterruptDuringExchangeRollback
            racedDestination
            concurrentPath
            contents
            exchangeDone
            _
            _
          )
          | destination == racedDestination && exchanged -> liftIO $ do
              firstExchange <- tryPutMVar exchangeDone ()
              when firstExchange $
                writeFile concurrentPath contents
      _ -> return ()
    return exchanged
  writeTemporaryFile directory template contents =
    liftIO (writeTemporaryFile directory template contents :: IO OsPath)
  withFileLock _ action = action
  canonicalizePath value = do
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just
        ( ReplaceDestinationWithCurrentDirectory
            racedDestination
            currentDirectory
          )
          | value == racedDestination -> liftIO $ do
              removeDirectory racedDestination
              createSymbolicLink
                currentDirectory
                racedDestination
                Directory
      _ -> return ()
    liftIO (canonicalizePath value :: IO OsPath)
  readSymlinkTarget value = liftIO (readSymlinkTarget value :: IO OsPath)
  copyFile source destination =
    liftIO (copyFile source destination :: IO ())
  copyFileWithMetadata source destination =
    liftIO (copyFileWithMetadata source destination :: IO ())
  copyFilePermissions source destination =
    liftIO (copyFilePermissions source destination :: IO ())
  createDirectory value = liftIO (createDirectory value :: IO ())
  createPrivateDirectory value =
    do
      liftIO (createPrivateDirectory value :: IO ())
      (_, racedEntry) <- CurrentDirectoryIO ask
      case racedEntry of
        Just (ModifySourceBeforeCopy sourceEntry replacement) ->
          liftIO $ writeFile sourceEntry replacement
        Just
          ( ReplaceSourceBeforeCopy
              sourceType
              sourceEntry
              replacementTarget
            ) ->
          liftIO $ do
            case sourceType of
              Directory -> removeDirectoryRecursively sourceEntry
              File -> removeFile sourceEntry
              Symlink -> removeFile sourceEntry
            createSymbolicLink
              replacementTarget
              sourceEntry
              sourceType
        _ -> return ()
  removeFile value =
    liftCurrentDirectoryIO (removeFile value :: IO ())
  removeDirectory value =
    do
      (_, racedEntry) <- CurrentDirectoryIO ask
      case racedEntry of
        Just (FailStagingRemoval staging)
          | value == staging ->
              throwError $ userError "injected staging removal failure"
        Just (ReplaceAfterCopyThenFail _ _ _ staging)
          | value == staging ->
              throwError $ userError "injected staging removal failure"
        Just (ReplaceDuringRollback _ _ _ staging)
          | value == staging ->
              throwError $ userError "injected staging removal failure"
        Just (FailQuarantine _ staging)
          | value == staging ->
              throwError $ userError "injected staging removal failure"
        Just (ReplaceBeforeRollbackMode staging _ _)
          | value == staging ->
              throwError $ userError "injected staging removal failure"
        _ -> liftCurrentDirectoryIO (removeDirectory value :: IO ())
  listDirectory value = liftIO (listDirectory value :: IO [OsPath])
  getFileSize value = liftIO (getFileSize value :: IO Integer)
  getFileIdentity value = do
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just (InterruptAfterExchange _ concurrentPath _)
        | value == takeDirectory concurrentPath ->
            liftIO $ Exception.throwIO UserInterrupt
      _ -> return ()
    identity <- liftIO (getFileIdentity value :: IO (Maybe FileIdentity))
    case racedEntry of
      Just (ReplaceDuringRollback racedPath displacedPath replacement _)
        | value == racedPath -> liftIO $ do
            renameEntry File racedPath displacedPath
            writeFile racedPath replacement
      _ -> return ()
    return identity
  captureDirectorySnapshot value = do
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just
        ( ReplaceDestinationWithCurrentDirectory
            racedDestination
            currentDirectory
          )
          | value == racedDestination -> liftIO $ do
              removeDirectory racedDestination
              createSymbolicLink
                currentDirectory
                racedDestination
                Directory
      Just
        ( ReplaceDestinationBetweenModeAndIdentity
            racedDestination
            replacementMode
            replaced
          )
          | value == racedDestination -> liftIO $ do
              firstReplacement <- tryPutMVar replaced ()
              when firstReplacement $ do
                removeDirectory racedDestination
                createDirectory racedDestination
                setPortableMode racedDestination replacementMode
      _ -> return ()
    liftCurrentDirectoryIO $ captureDirectorySnapshot value
  getFileSnapshot value =
    liftIO (getFileSnapshot value :: IO (Maybe FileSnapshot))
  getPortableMode value = do
    mode <- liftIO (getPortableMode value :: IO PortableMode)
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just
        ( ReplaceDestinationBetweenModeAndIdentity
            racedDestination
            replacementMode
            replaced
          )
          | value == racedDestination -> liftIO $ do
              firstReplacement <- tryPutMVar replaced ()
              when firstReplacement $ do
                removeDirectory racedDestination
                createDirectory racedDestination
                setPortableMode racedDestination replacementMode
      _ -> return ()
    return mode
  setPortableMode path mode = do
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just (ReplaceBeforeMode racedPath victimPath)
        | path == racedPath -> liftIO $ do
            removeFile racedPath
            createSymbolicLink victimPath racedPath File
      Just
        ( InterruptDuringExchangeRollback
            racedDestination
            _
            _
            exchangeDone
            rollbackStarted
            releaseRollback
          )
          | path == racedDestination -> liftIO $ do
              exchanged <- tryTakeMVar exchangeDone
              case exchanged of
                Just () -> do
                  putMVar rollbackStarted ()
                  takeMVar releaseRollback
                Nothing -> return ()
      Just
        ( ReplaceBeforeRollbackMode
            _
            racedDestination
            victimPath
          )
          | path == racedDestination -> liftIO $ do
              removeDirectory racedDestination
              createSymbolicLink victimPath racedDestination Directory
      _ -> return ()
    liftIO (setPortableMode path mode :: IO ())
  setPortableWritable path writable =
    liftIO (setPortableWritable path writable :: IO ())
  restoreDirectoryModeFromSnapshot path snapshot = do
    (_, racedEntry) <- CurrentDirectoryIO ask
    case racedEntry of
      Just
        ( ReplaceBeforeRollbackMode
            _
            racedDestination
            victimPath
          )
          | path == racedDestination -> liftIO $ do
              removeDirectory racedDestination
              createSymbolicLink victimPath racedDestination Directory
      Just
        ( InterruptDuringExchangeRollback
            racedDestination
            _
            _
            exchangeDone
            rollbackStarted
            releaseRollback
          )
          | path == racedDestination -> liftIO $ do
              exchanged <- tryTakeMVar exchangeDone
              case exchanged of
                Just () -> do
                  putMVar rollbackStarted ()
                  takeMVar releaseRollback
                Nothing -> return ()
      _ -> return ()
    liftCurrentDirectoryIO $
      restoreDirectoryModeFromSnapshot path snapshot
  createSymbolicLink target link fileType =
    liftIO (createSymbolicLink target link fileType :: IO ())
#endif

#ifdef mingw32_HOST_OS
symlinkSpecs :: Spec
symlinkSpecs = return ()
#else
symlinkSpecs :: Spec
symlinkSpecs = do
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

  it "preserves CWD identity through a symlinked parent path" $
    withTempDir $ \tmpDir _ -> do
      realParentName <- encodeFS "real-parent"
      aliasParentName <- encodeFS "alias-parent"
      destinationName <- encodeFS "destination"
      stagingName <- encodeFS "staging"
      manifestName <- encodeFS "dojang.toml"
      let realParent = tmpDir </> realParentName
          aliasParent = tmpDir </> aliasParentName
          realDestination = realParent </> destinationName
          aliasDestination = aliasParent </> destinationName
          staging = tmpDir </> stagingName
      createDirectory realParent
      createDirectory realDestination
      createDirectory staging
      writeFile (staging </> manifestName) "manifest"
      System.Directory.OsPath.createDirectoryLink
        realParentName
        aliasParent
      realDestinationPath <- decodePath realDestination
      originalId <-
        Posix.fileID <$> Posix.getFileStatus realDestinationPath
      published <-
        runCurrentDirectoryIO realDestination $
          publishStagedDirectory staging aliasDestination
      published `shouldBe` Right ()
      readFile (realDestination </> manifestName)
        `shouldReturn` "manifest"
      publishedId <-
        Posix.fileID <$> Posix.getFileStatus realDestinationPath
      publishedId `shouldBe` originalId

  it "preserves arbitrary files raced into the current directory" $
    hedgehog $ do
      concurrentContents <-
        forAll $ Gen.bytes $ Range.linear 0 4096
      (published, observedContents, ownedFileExists) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            ownedName <- encodeFS "owned"
            nestedName <- encodeFS "nested"
            victimName <- encodeFS "victim"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
                owned = destination </> ownedName
                stagedNested = staging </> nestedName
                victim = destination </> nestedName </> victimName
            createDirectory staging
            createDirectory stagedNested
            createDirectory destination
            writeFile (staging </> ownedName) "owned"
            writeFile (stagedNested </> victimName) "staged"
            result <-
              runCurrentDirectoryIOWithRace
                destination
                (CreateBeforeCopy victim concurrentContents)
                (publishStagedDirectory staging destination)
            contents <- readFile victim
            ownedExists <- exists owned
            return (result, contents, ownedExists)
      isLeft published === True
      observedContents === concurrentContents
      ownedFileExists === False

  it "preserves arbitrary entries replaced before CWD rollback" $
    hedgehog $ do
      replacementContents <-
        forAll $ Gen.bytes $ Range.linear 0 4096
      (published, observedContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            ownedName <- encodeFS "owned"
            nestedName <- encodeFS "nested"
            triggerName <- encodeFS "trigger"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
                owned = destination </> ownedName
                stagedNested = staging </> nestedName
                trigger = destination </> nestedName </> triggerName
            createDirectory staging
            createDirectory stagedNested
            createDirectory destination
            writeFile (staging </> ownedName) "bootstrap"
            writeFile (stagedNested </> triggerName) "trigger"
            result <-
              runCurrentDirectoryIOWithRace
                destination
                (ReplaceThenFail trigger owned replacementContents)
                (publishStagedDirectory staging destination)
            contents <- readFile owned
            return (result, contents)
      isLeft published === True
      observedContents === replacementContents

  it "does not claim a replacement raced in after entry creation" $
    hedgehog $ do
      replacementContents <-
        forAll $ Gen.bytes $ Range.linear 0 4096
      (published, replacementExists, observedContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            ownedName <- encodeFS "owned"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
                owned = destination </> ownedName
            createDirectory staging
            createDirectory destination
            writeFile (staging </> ownedName) "bootstrap"
            result <-
              runCurrentDirectoryIOWithRace
                destination
                ( ReplaceAfterCopyThenFail
                    owned
                    owned
                    replacementContents
                    staging
                )
                (publishStagedDirectory staging destination)
            present <- exists owned
            contents <- if present then readFile owned else return ""
            return (result, present, contents)
      isLeft published === True
      replacementExists === True
      observedContents === replacementContents

  it "atomically preserves arbitrary replacements raced into CWD rollback" $
    hedgehog $ do
      replacementContents <-
        forAll $ Gen.bytes $ Range.linear 0 4096
      (published, replacementExists, observedContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            ownedName <- encodeFS "owned"
            displacedName <- encodeFS "displaced"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
                owned = destination </> ownedName
                displaced = tmpDir </> displacedName
            createDirectory staging
            createDirectory destination
            writeFile (staging </> ownedName) "bootstrap"
            result <-
              runCurrentDirectoryIOWithRace
                destination
                ( ReplaceDuringRollback
                    owned
                    displaced
                    replacementContents
                    staging
                )
                (publishStagedDirectory staging destination)
            present <- exists owned
            contents <- if present then readFile owned else return ""
            return (result, present, contents)
      isLeft published === True
      replacementExists === True
      observedContents === replacementContents

  it "reports a quarantine failure that leaves a published entry" $
    hedgehog $ do
      contents <- forAll $ Gen.bytes $ Range.linear 0 4096
      (publishedResult, publishedExists, publishedContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            ownedName <- encodeFS "owned"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
                owned = destination </> ownedName
            createDirectory staging
            createDirectory destination
            writeFile (staging </> ownedName) contents
            result <-
              runCurrentDirectoryIOWithRace
                destination
                (FailQuarantine owned staging)
                (publishStagedDirectory staging destination)
            present <- exists owned
            observed <- if present then readFile owned else return ""
            return (result, present, observed)
      case publishedResult of
        Left err ->
          assert $
            "injected quarantine failure"
              `isInfixOf` Exception.displayException err
        Right _ -> assert False
      publishedExists === True
      publishedContents === contents

  it "rejects a destination redirected to the current directory" $
    hedgehog $ do
      manifestContents <-
        forAll $ Gen.bytes $ Range.linear 0 4096
      (published, currentEntries, destinationIsSymlink, stagedContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            currentName <- encodeFS "current"
            manifestName <- encodeFS "dojang.toml"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
                current = tmpDir </> currentName
                manifest = staging </> manifestName
            createDirectory staging
            createDirectory destination
            createDirectory current
            writeFile manifest manifestContents
            result <-
              runCurrentDirectoryIOWithRace
                current
                ( ReplaceDestinationWithCurrentDirectory
                    destination
                    current
                )
                (publishStagedDirectory staging destination)
            entries <- listDirectory current
            symbolicLink <- isSymlink destination
            stagingPresent <- isDirectory staging
            contents <-
              if stagingPresent then readFile manifest else return ""
            return (result, entries, symbolicLink, contents)
      isLeft published === True
      currentEntries === []
      destinationIsSymlink === True
      stagedContents === manifestContents

  it "binds an arbitrary destination mode to its directory identity" $
    hedgehog $ do
      replacementMode <-
        forAll $ (0o700 .|.) <$> Gen.word (Range.linear 0 0o7)
      observedMode <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            manifestName <- encodeFS "dojang.toml"
            replaced <- newEmptyMVar
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
            createDirectory staging
            createDirectory destination
            writeFile (staging </> manifestName) "manifest"
            setPortableMode destination 0o755
            published <-
              runCurrentDirectoryIOWithRace
                tmpDir
                ( ReplaceDestinationBetweenModeAndIdentity
                    destination
                    replacementMode
                    replaced
                )
                (publishStagedDirectory staging destination)
            published `shouldBe` Right ()
            getPortableMode destination
      observedMode === portableModeFromBits replacementMode

  it "preserves arbitrary entries raced into a directory exchange" $
    hedgehog $ do
      concurrentContents <-
        forAll $ Gen.bytes $ Range.linear 0 4096
      (published, destinationContents, stagedContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            manifestName <- encodeFS "dojang.toml"
            concurrentName <- encodeFS "concurrent"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
                concurrent = staging </> concurrentName
            createDirectory staging
            createDirectory destination
            writeFile (staging </> manifestName) "manifest"
            result <-
              runCurrentDirectoryIOWithRace
                tmpDir
                ( CreateAfterExchange
                    destination
                    concurrent
                    concurrentContents
                )
                (publishStagedDirectory staging destination)
            destinationValue <- readFile $ destination </> concurrentName
            stagingValue <- readFile $ staging </> manifestName
            return (result, destinationValue, stagingValue)
      isLeft published === True
      destinationContents === concurrentContents
      stagedContents === "manifest"

  it "restores an arbitrary file raced into a directory exchange" $
    hedgehog $ do
      concurrentContents <-
        forAll $ Gen.bytes $ Range.linear 0 4096
      replacementMode <-
        forAll $ (0o600 .|.) <$> Gen.word (Range.linear 0 0o77)
      ( published
        , destinationIsFile
        , destinationContents
        , destinationMode
        , stagingIsDirectory
        , stagingContents
        ) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            manifestName <- encodeFS "dojang.toml"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
            createDirectory staging
            createDirectory destination
            writeFile (staging </> manifestName) "manifest"
            setPortableMode destination 0o500
            result <-
              runCurrentDirectoryIOWithRace
                tmpDir
                ( ReplaceDestinationBeforeExchange
                    staging
                    destination
                    concurrentContents
                    replacementMode
                )
                (publishStagedDirectory staging destination)
            destinationRegular <- isRegularFile destination
            destinationValue <-
              if destinationRegular then readFile destination else return ""
            destinationMode' <- getPortableMode destination
            stagingDirectory <- isDirectory staging
            stagingValue <-
              if stagingDirectory
                then readFile $ staging </> manifestName
                else return ""
            return
              ( result
              , destinationRegular
              , destinationValue
              , destinationMode'
              , stagingDirectory
              , stagingValue
              )
      isLeft published === True
      destinationIsFile === True
      destinationContents === concurrentContents
      destinationMode === portableModeFromBits replacementMode
      stagingIsDirectory === True
      stagingContents === "manifest"

  it "preserves a symlink target raced into a directory exchange" $
    hedgehog $ do
      targetContents <- forAll $ Gen.bytes $ Range.linear 0 4096
      targetMode <-
        forAll $ (0o600 .|.) <$> Gen.word (Range.linear 0 0o77)
      (published, destinationIsSymlink, observedMode, stagingContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            targetName <- encodeFS "target"
            manifestName <- encodeFS "dojang.toml"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
                target = tmpDir </> targetName
            createDirectory staging
            createDirectory destination
            writeFile (staging </> manifestName) "manifest"
            writeFile target targetContents
            setPortableMode destination 0o500
            setPortableMode target targetMode
            result <-
              runCurrentDirectoryIOWithRace
                tmpDir
                ( ReplaceDestinationWithSymlinkBeforeExchange
                    staging
                    destination
                    target
                )
                (publishStagedDirectory staging destination)
            symbolicLink <- isSymlink destination
            mode <- getPortableMode target
            staged <- readFile $ staging </> manifestName
            return (result, symbolicLink, mode, staged)
      isLeft published === True
      destinationIsSymlink === True
      observedMode === portableModeFromBits targetMode
      stagingContents === "manifest"

  it "does not chmod a symlink target raced into exchange rollback" $
    hedgehog $ do
      targetMode <-
        forAll $ (0o600 .|.) <$> Gen.word (Range.linear 0 0o77)
      (published, destinationIsSymlink, observedMode, stagingContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            targetName <- encodeFS "target"
            manifestName <- encodeFS "dojang.toml"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
                target = tmpDir </> targetName
            createDirectory staging
            createDirectory destination
            writeFile (staging </> manifestName) "manifest"
            writeFile target "unrelated"
            setPortableMode destination 0o500
            setPortableMode target targetMode
            result <-
              runCurrentDirectoryIOWithRace
                tmpDir
                ( ReplaceBeforeRollbackMode
                    staging
                    destination
                    target
                )
                (publishStagedDirectory staging destination)
            symbolicLink <- isSymlink destination
            mode <- getPortableMode target
            staged <- readFile $ staging </> manifestName
            return (result, symbolicLink, mode, staged)
      case published of
        Left err ->
          assert $
            "injected staging removal failure"
              `isInfixOf` Exception.displayException err
        Right _ -> assert False
      destinationIsSymlink === True
      observedMode === portableModeFromBits targetMode
      stagingContents === "manifest"

  it "rolls back an interrupted directory exchange" $
    hedgehog $ do
      concurrentContents <-
        forAll $ Gen.bytes $ Range.linear 0 4096
      (interrupted, destinationContents, stagingContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            manifestName <- encodeFS "dojang.toml"
            concurrentName <- encodeFS "concurrent"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
                concurrent = staging </> concurrentName
            createDirectory staging
            createDirectory destination
            writeFile (staging </> manifestName) "manifest"
            result <-
              Exception.try $
                runCurrentDirectoryIOWithRace
                  tmpDir
                  ( InterruptAfterExchange
                      destination
                      concurrent
                      concurrentContents
                  )
                  (publishStagedDirectory staging destination)
            destinationValue <- readFile $ destination </> concurrentName
            stagingValue <- readFile $ staging </> manifestName
            return
              ( result :: Either AsyncException (Either IOError ())
              , destinationValue
              , stagingValue
              )
      assert $ isLeft interrupted
      destinationContents === concurrentContents
      stagingContents === "manifest"

  it "finishes exchange rollback through a second interruption" $
    hedgehog $ do
      concurrentContents <-
        forAll $ Gen.bytes $ Range.linear 0 4096
      (finished, destinationContents, stagingContents) <-
        evalIO $
          withTempDir $ \tmpDir _ -> do
            stagingName <- encodeFS "staging"
            destinationName <- encodeFS "destination"
            manifestName <- encodeFS "dojang.toml"
            concurrentName <- encodeFS "concurrent"
            let staging = tmpDir </> stagingName
                destination = tmpDir </> destinationName
                concurrent = staging </> concurrentName
            createDirectory staging
            createDirectory destination
            writeFile (staging </> manifestName) "manifest"
            exchangeDone <- newEmptyMVar
            rollbackStarted <- newEmptyMVar
            releaseRollback <- newEmptyMVar
            result <- newEmptyMVar
            worker <-
              forkFinally
                ( runCurrentDirectoryIOWithRace
                    tmpDir
                    ( InterruptDuringExchangeRollback
                        destination
                        concurrent
                        concurrentContents
                        exchangeDone
                        rollbackStarted
                        releaseRollback
                    )
                    (publishStagedDirectory staging destination)
                )
                (putMVar result)
            takeMVar rollbackStarted
            _ <- forkIO $ Exception.throwTo worker ThreadKilled
            threadDelay 10000
            putMVar releaseRollback ()
            outcome <- timeout 5000000 $ takeMVar result
            destinationValue <- readFile $ destination </> concurrentName
            stagingValue <- readFile $ staging </> manifestName
            return (outcome, destinationValue, stagingValue)
      assert $ maybe False isLeft finished
      destinationContents === concurrentContents
      stagingContents === "manifest"

  it "rolls back CWD publication interrupted between entries" $
    withTempDir $ \tmpDir _ -> do
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "destination"
      ownedName <- encodeFS "owned"
      nestedName <- encodeFS "nested"
      triggerName <- encodeFS "trigger"
      let staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
          owned = destination </> ownedName
          stagedNested = staging </> nestedName
          trigger = destination </> nestedName </> triggerName
      createDirectory staging
      createDirectory stagedNested
      createDirectory destination
      writeFile (staging </> ownedName) "bootstrap"
      writeFile (stagedNested </> triggerName) "trigger"
      interrupted <-
        Exception.try $
          runCurrentDirectoryIOWithRace
            destination
            (InterruptBeforeCopy trigger)
            (publishStagedDirectory staging destination)
      interrupted `shouldBe` Left UserInterrupt
      exists owned `shouldReturn` False
      exists (destination </> nestedName) `shouldReturn` False
#endif
