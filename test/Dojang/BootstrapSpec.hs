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
import Data.Word (Word32)
import Hedgehog (evalIO, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range


#ifndef mingw32_HOST_OS
import Control.Exception (AsyncException (UserInterrupt), throw)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except
  ( ExceptT
  , MonadError (throwError)
  , runExceptT
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import System.Directory.OsPath qualified
import System.FilePath qualified as FilePath
import System.OsPath (OsPath)
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
        let firstDirectory = fmap toLower cased
            secondDirectory = fmap toUpper cased
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

  it "restores an existing empty destination when its rename fails" $
    withTempDir $ \tmpDir _ -> do
      stagingName <- encodeFS "staging"
      destinationName <- encodeFS "failing-rename"
      manifestName <- encodeFS "dojang.toml"
      let staging = tmpDir </> stagingName
          destination = tmpDir </> destinationName
      createDirectory staging
      createDirectory destination
      setPortableMode destination 0o600
      writeFile (staging </> manifestName) "manifest"
      result <-
        runFailingModeIO $
          publishStagedDirectoryWithMetadata
            emptyStagedMetadata
            staging
            destination
      result `shouldSatisfy` either (const True) (const False)
      isDirectory destination `shouldReturn` True
      listDirectory destination `shouldReturn` []
      getPortableMode destination
        `shouldReturn` portableModeFromBits 0o600
      isDirectory staging `shouldReturn` True
      stagingMode <- getPortableMode staging
      fmap (.&. 0o700) stagingMode.posixBits
        `shouldBe` Just 0o700


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
  readRegularFile value = do
    path <- liftIO (decodePath value :: IO FilePath)
    case FilePath.takeFileName path of
      "interrupted.zip" -> return $ Just $ throw UserInterrupt
      "streamed" -> throwError $ userError "buffered read forbidden"
      _ -> liftIO (readRegularFile value :: IO (Maybe ByteString.ByteString))
  copyRegularFile source destination =
    liftIO (copyRegularFile source destination :: IO Bool)
  writeFile path contents = liftIO (writeFile path contents :: IO ())
  replaceFile source destination =
    liftIO (replaceFile source destination :: IO ())
  renameDirectory source destination = do
    destinationPath <- liftIO (decodePath destination :: IO FilePath)
    if FilePath.takeFileName destinationPath == "failing-rename"
      then throwError $ userError "injected rename failure"
      else liftIO (renameDirectory source destination :: IO ())
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


newtype CurrentDirectoryIO a
  = CurrentDirectoryIO (ReaderT OsPath (ExceptT IOError IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadError IOError
    )


runCurrentDirectoryIO
  :: OsPath -> CurrentDirectoryIO a -> IO (Either IOError a)
runCurrentDirectoryIO currentDirectory (CurrentDirectoryIO action) =
  runExceptT $ runReaderT action currentDirectory


instance MonadFileSystem CurrentDirectoryIO where
  encodePath "." = CurrentDirectoryIO ask
  encodePath value = liftIO (encodePath value :: IO OsPath)
  decodePath value = liftIO (decodePath value :: IO FilePath)
  getCurrentDirectory = CurrentDirectoryIO ask
  getHomeDirectory = liftIO (getHomeDirectory :: IO OsPath)
  exists value = liftIO (exists value :: IO Bool)
  isFile value = liftIO (isFile value :: IO Bool)
  isRegularFile value = liftIO (isRegularFile value :: IO Bool)
  isDirectory value = liftIO (isDirectory value :: IO Bool)
  isSymlink value = liftIO (isSymlink value :: IO Bool)
  readFile value = liftIO (readFile value :: IO ByteString.ByteString)
  readRegularFile value =
    liftIO (readRegularFile value :: IO (Maybe ByteString.ByteString))
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
#endif
