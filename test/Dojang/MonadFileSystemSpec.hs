{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.MonadFileSystemSpec (spec) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (sort, sortOn)
import GHC.IO.Exception (IOErrorType (InappropriateType))
import System.IO.Error
  ( alreadyExistsErrorType
  , doesNotExistErrorType
  , ioeGetErrorString
  , ioeGetErrorType
  , ioeGetFileName
  , ioeGetLocation
  , isDoesNotExistError
  )
import Prelude hiding (readFile, writeFile)
import Prelude qualified (readFile, writeFile)

import Control.Monad.Except (MonadError (catchError), tryError)
import Data.ByteString qualified (readFile, writeFile)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (constantFrom)
import System.Directory.OsPath
  ( createDirectoryLink
  , createFileLink
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  )
import System.Directory.OsPath qualified (createDirectory)
import System.FilePath (combine)
import System.OsPath (dropFileName, encodeFS, (</>))
import Test.Hspec (Spec, describe, it, runIO, specify, xit, xspecify)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldContain
  , shouldReturn
  , shouldSatisfy
  , shouldThrow
  )
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.MonadFileSystem
  ( FileType (..)
  , MonadFileSystem (..)
  , dryRunIO
  , tryDryRunIO
  )
import Dojang.TestUtils (withTempDir)


packageYamlFP :: FilePath
packageYamlFP = "package.yaml"


testFP :: FilePath
testFP = "test"


nonExistentFP :: FilePath
nonExistentFP = "---non-existent---"


nonExistentFP' :: FilePath
nonExistentFP' = "---non-existent-2---"


spec :: Spec
spec = do
  packageYamlP <- runIO $ encodeFS packageYamlFP
  testP <- runIO $ encodeFS testFP
  nonExistentP <- runIO $ encodeFS nonExistentFP
  nonExistentP' <- runIO $ encodeFS nonExistentFP'

  -- cSpell:ignore quux corge
  foo <- runIO $ encodeFS "foo"
  bar <- runIO $ encodeFS "bar"
  baz <- runIO $ encodeFS "baz"
  qux <- runIO $ encodeFS "qux"
  quux <- runIO $ encodeFS "quux"
  corge <- runIO $ encodeFS "corge"

  symlinkAvailable <- runIO $ withTempDir $ \tmpDir tmpDir' ->
    ( do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        createFileLink foo (tmpDir </> bar)
        return True
    )
      `catchError` const (return False)
  let symIt = if symlinkAvailable then it else xit
  let symSpecify = if symlinkAvailable then specify else xspecify

  let withFixture action = withTempDir $ \tmpDir tmpDir' -> do
        () <- Prelude.writeFile (tmpDir' `combine` "foo") ""
        () <- System.Directory.OsPath.createDirectory $ tmpDir </> bar
        () <- System.Directory.OsPath.createDirectory $ tmpDir </> baz
        () <-
          System.Directory.OsPath.createDirectory
            (tmpDir </> baz </> qux)
        action tmpDir tmpDir'

  describe "MonadFileSystem IO" $ do
    specify "encodePath" $ hedgehog $ do
      filePath <- forAll $ Gen.string (constantFrom 0 0 256) Gen.unicode
      filePath' <- liftIO $ encodePath filePath >>= decodePath
      filePath' === filePath

    specify "exists" $ do
      exists packageYamlP `shouldReturn` True
      exists testP `shouldReturn` True
      exists nonExistentP `shouldReturn` False

    specify "isFile" $ do
      isFile packageYamlP `shouldReturn` True
      isFile testP `shouldReturn` False
      isFile nonExistentP `shouldReturn` False

    specify "isDirectory" $ do
      isDirectory packageYamlP `shouldReturn` False
      isDirectory testP `shouldReturn` True
      isDirectory nonExistentP `shouldReturn` False

    symSpecify "isSymlink" $ do
      isSymlink packageYamlP `shouldReturn` False
      isSymlink testP `shouldReturn` False
      isSymlink nonExistentP `shouldReturn` False
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        createFileLink foo (tmpDir </> bar)
        isSymlink (tmpDir </> bar) `shouldReturn` True

    specify "readFile" $ withTempDir $ \tmpDir tmpDir' -> do
      () <- Prelude.writeFile (tmpDir' `combine` "foo") "Foo contents"
      contents <- readFile $ tmpDir </> foo
      contents `shouldBe` "Foo contents"
      Left error' <- tryError $ readFile nonExistentP
      ioeGetErrorType error' `shouldBe` doesNotExistErrorType
      ioeGetFileName error' `shouldBe` Just nonExistentFP

    specify "writeFile" $ withTempDir $ \tmpDirP tmpDirFP -> do
      () <- writeFile (tmpDirP </> nonExistentP) "foo"
      Data.ByteString.readFile (tmpDirFP `combine` nonExistentFP)
        `shouldReturn` "foo"

    describe "readSymlinkTarget" $ do
      symIt "tells the target path of a symbolic link" $ withTempDir $ \tmpDir tmpDir' -> do
        () <- Prelude.writeFile (tmpDir' `combine` "foo") ""
        createFileLink foo (tmpDir </> bar)
        readSymlinkTarget (tmpDir </> bar) `shouldReturn` foo

      it "fails with IOError if the path does not exist" $ do
        readSymlinkTarget nonExistentP `shouldThrow` \e -> do
          isDoesNotExistError e && ioeGetFileName e == Just nonExistentFP

    specify "copyFile" $ withTempDir $ \tmpDirP tmpDirFP -> do
      () <- copyFile packageYamlP (tmpDirP </> nonExistentP)
      contents <- Data.ByteString.readFile (tmpDirFP `combine` nonExistentFP)
      original <- Data.ByteString.readFile packageYamlFP
      contents `shouldBe` original

    specify "createDirectory" $ withTempDir $ \tmpDirP _ -> do
      () <- createDirectory (tmpDirP </> nonExistentP)
      doesDirectoryExist (tmpDirP </> nonExistentP)
        `shouldReturn` True

    specify "removeFile" $ withTempDir $ \tmpDirP tmpDirFP -> do
      Data.ByteString.writeFile (tmpDirFP `combine` nonExistentFP) ""
      doesFileExist (tmpDirP </> nonExistentP) `shouldReturn` True
      () <- removeFile (tmpDirP </> nonExistentP)
      doesFileExist (tmpDirP </> nonExistentP) `shouldReturn` False

    specify "removeDirectory" $ withTempDir $ \tmpDirP tmpDirFP -> do
      () <- createDirectory (tmpDirP </> nonExistentP)
      () <- removeDirectory (tmpDirP </> nonExistentP)
      doesDirectoryExist (tmpDirP </> nonExistentP)
        `shouldReturn` False
      removeDirectory (tmpDirP </> nonExistentP) `shouldThrow` \e ->
        isDoesNotExistError e
          && (ioeGetFileName e == Just (tmpDirFP `combine` nonExistentFP))

    specify "removeDirectoryRecursively" $ withFixture $ \tmpDirP tmpDirFP -> do
      removeDirectoryRecursively tmpDirP
      doesPathExist tmpDirP `shouldReturn` False
      removeDirectoryRecursively tmpDirP `shouldThrow` \e ->
        isDoesNotExistError e && ioeGetFileName e == Just tmpDirFP

    specify "listDirectory" $ withFixture $ \tmpDir _ -> do
      result <- listDirectory tmpDir
      sort result `shouldBe` [bar, baz, foo]

    describe "listDirectoryRecursively" $ do
      specify "basic behavior" $ withFixture $ \tmpDir tmpDir' -> do
        () <- Prelude.writeFile (tmpDir' `combine` "bar" `combine` "quux") ""
        () <- Prelude.writeFile (tmpDir' `combine` "baz" `combine` "corge") ""
        result <- listDirectoryRecursively tmpDir []
        sortOn snd result
          `shouldBe` [ (Directory, bar)
                     , (File, bar </> quux)
                     , (Directory, baz)
                     , (File, baz </> corge)
                     , (Directory, baz </> qux)
                     , (File, foo)
                     ]

      it "filters out files by ignorePatterns" $ withFixture $ \tmpDir tmpDir' -> do
        () <- Prelude.writeFile (tmpDir' `combine` "bar" `combine` "quux") ""
        () <- Prelude.writeFile (tmpDir' `combine` "baz" `combine` "corge") ""
        result <- listDirectoryRecursively tmpDir ["bar/q*", "baz"]
        sortOn snd result `shouldBe` [(Directory, bar), (File, foo)]

      symIt "distinguishes symlinks from regular files and directories"
        $ withFixture
        $ \tmpDir tmpDir' -> do
          () <- Prelude.writeFile (tmpDir' `combine` "bar" `combine` "quux") ""
          () <- createFileLink quux (tmpDir </> baz </> corge)
          () <- createDirectoryLink baz (tmpDir </> corge)
          result <- listDirectoryRecursively tmpDir []
          sortOn snd result
            `shouldBe` [ (Directory, bar)
                       , (File, bar </> quux)
                       , (Directory, baz)
                       , (Symlink, baz </> corge)
                       , (Directory, baz </> qux)
                       , (Symlink, corge)
                       , (File, foo)
                       ]

    specify "getFileSize" $ withFixture $ \tmpDir tmpDirFP -> do
      Data.ByteString.writeFile (tmpDirFP `combine` "foo") "asdf"
      getFileSize (tmpDir </> foo) `shouldReturn` 4
      Left e <- tryError $ getFileSize (tmpDir </> nonExistentP)
      ioeGetErrorType e `shouldBe` doesNotExistErrorType
      ioeGetFileName e `shouldBe` Just (tmpDirFP `combine` nonExistentFP)
      Left e' <- tryError $ getFileSize tmpDir
      ioeGetErrorType e' `shouldBe` InappropriateType
      ioeGetLocation e' `shouldBe` "getFileSize"
      ioeGetFileName e' `shouldBe` Just tmpDirFP
      show e' `shouldContain` "it is a directory"

  describe "DryRunIO" $ do
    specify "encodePath" $ hedgehog $ do
      filePath <- forAll $ Gen.string (constantFrom 0 0 256) Gen.unicode
      filePath' <- liftIO $ dryRunIO (encodePath filePath >>= decodePath)
      filePath' === filePath

    describe "isFile" $ do
      it "checks an actual file that exists on the real file system" $ do
        packageYamlIsFile <- dryRunIO $ isFile packageYamlP
        packageYamlIsFile `shouldBe` True

      it "checks an actual directory that exists on the real file system" $ do
        testIsFile <- dryRunIO $ isFile testP
        testIsFile `shouldBe` False

      it "checks a non-existent file" $ do
        nonExistentIsFile <- dryRunIO $ isFile nonExistentP
        nonExistentIsFile `shouldBe` False

      it "checks a virtual file (Contents) that exists in memory" $ do
        nonExistentIsFile <- dryRunIO $ do
          () <- writeFile nonExistentP ""
          isFile nonExistentP
        nonExistentIsFile `shouldBe` True

      it "checks a virtual file (Copied) that exists in memory" $ do
        nonExistentIsFile <- dryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          isFile nonExistentP
        nonExistentIsFile `shouldBe` True

      it "checks a virtual directory that exists in memory" $ do
        nonExistentIsFile <- dryRunIO $ do
          () <- createDirectory nonExistentP
          isFile nonExistentP
        nonExistentIsFile `shouldBe` False

      it "checks a virtual non-existent file" $ do
        packageYamlIsFile <- dryRunIO $ do
          () <- removeFile packageYamlP
          isFile packageYamlP
        packageYamlIsFile `shouldBe` False

    describe "isDirectory" $ do
      it "check an actual file that exists on the real file system" $ do
        packageYamlIsDir <- dryRunIO $ isDirectory packageYamlP
        packageYamlIsDir `shouldBe` False

      it "checks an actual directory that exists on the real file system" $ do
        testIsDir <- dryRunIO $ isDirectory testP
        testIsDir `shouldBe` True

      it "checks a non-existent directory" $ do
        nonExistentIsDir <- dryRunIO $ isDirectory nonExistentP
        nonExistentIsDir `shouldBe` False

      it "checks a virtual directory that exists in memory" $ do
        nonExistentIsDir' <- dryRunIO $ do
          () <- createDirectory nonExistentP
          isDirectory nonExistentP
        nonExistentIsDir' `shouldBe` True

      it "checks a virtual file (Copied) that exists in memory" $ do
        nonExistentIsDir'' <- dryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          isDirectory nonExistentP
        nonExistentIsDir'' `shouldBe` False

      it "checks a virtual file (Contents) that exists in memory" $ do
        nonExistentIsDir''' <- dryRunIO $ do
          () <- writeFile nonExistentP ""
          isDirectory nonExistentP
        nonExistentIsDir''' `shouldBe` False

      it "checks a virtual non-existent directory" $ do
        packageYamlIsFile <- dryRunIO $ do
          () <- removeFile packageYamlP
          isDirectory packageYamlP
        packageYamlIsFile `shouldBe` False

    describe "isSymlink" $ do
      symIt "checks an actual symlink that exists on the real file system"
        $ withTempDir
        $ \tmpDir tmpDir' -> do
          () <- Prelude.writeFile (tmpDir' `combine` "foo") ""
          createFileLink foo (tmpDir </> bar)
          dryRunIO (isSymlink $ tmpDir </> bar) `shouldReturn` True

      it "return False if a path does not exist" $ do
        dryRunIO (isSymlink nonExistentP) `shouldReturn` False

      it "checks a virtual file that exists in memory" $ do
        nonExistentExists' <- dryRunIO $ do
          () <- writeFile nonExistentP ""
          isSymlink nonExistentP
        nonExistentExists' `shouldBe` False

    describe "exists" $ do
      it "checks an actual file that exists on the real file system" $ do
        packageYamlExists <- dryRunIO $ exists packageYamlP
        packageYamlExists `shouldBe` True

      it "checks an actual directory that exists on the real file system" $ do
        testExists <- dryRunIO $ exists testP
        testExists `shouldBe` True

      it "checks a non-existent file" $ do
        nonExistentExists <- dryRunIO $ exists nonExistentP
        nonExistentExists `shouldBe` False

      it "checks a virtual file (Contents) that exists in memory" $ do
        nonExistentExists' <- dryRunIO $ do
          () <- writeFile nonExistentP ""
          exists nonExistentP
        nonExistentExists' `shouldBe` True

      it "checks a virtual file (Copied) that exists in memory" $ do
        nonExistentExists'' <- dryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          exists nonExistentP
        nonExistentExists'' `shouldBe` True

      it "checks a virtual directory that exists in memory" $ do
        nonExistentExists''' <- dryRunIO $ do
          () <- createDirectory nonExistentP
          exists nonExistentP
        nonExistentExists''' `shouldBe` True

      it "checks a virtual non-existent file" $ do
        packageYamlIsFile <- dryRunIO $ do
          () <- removeFile packageYamlP
          exists packageYamlP
        packageYamlIsFile `shouldBe` False

    describe "readFile" $ do
      it "can read an actual file that exists on the real file system"
        $ withTempDir
        $ \tmpDir tmpDir' -> do
          () <- Prelude.writeFile (tmpDir' `combine` "foo") "Foo contents"
          packageYamlData <- dryRunIO $ readFile $ tmpDir </> foo
          packageYamlData `shouldBe` "Foo contents"

      it "can't read a directory that exists on the real file system" $ do
        Left failToReadTest <- tryDryRunIO $ readFile testP
        ioeGetErrorType failToReadTest `shouldBe` InappropriateType
        ioeGetFileName failToReadTest `shouldBe` Just testFP

      it "can't read a non-existent file" $ do
        Left failToReadNonExistent <- tryDryRunIO $ readFile nonExistentP
        ioeGetErrorType failToReadNonExistent `shouldBe` doesNotExistErrorType
        ioeGetFileName failToReadNonExistent
          `shouldBe` Just nonExistentFP

      it "can read a virtual file (Contents) that exists in memory" $ do
        nonExistentData <- dryRunIO $ do
          () <- writeFile nonExistentP "foo"
          readFile nonExistentP
        nonExistentData `shouldBe` "foo"

      it "can read a virtual file (Copied) that exists in memory"
        $ withTempDir
        $ \tmpDir tmpDir' -> do
          Prelude.writeFile (tmpDir' `combine` "foo") "Foo contents"
          nonExistentData <- dryRunIO $ do
            () <- copyFile (tmpDir </> foo) nonExistentP
            () <- writeFile packageYamlP "foo" -- must not affect the result
            () <- copyFile nonExistentP nonExistentP'
            () <- writeFile nonExistentP "bar" -- must not affect the result
            readFile nonExistentP'
          nonExistentData `shouldBe` "Foo contents"

      it "can't read a virtual directory that exists in memory" $ do
        Left failToReadNonExistent <- tryDryRunIO $ do
          () <- createDirectory nonExistentP
          readFile nonExistentP
        ioeGetErrorType failToReadNonExistent `shouldBe` InappropriateType
        ioeGetFileName failToReadNonExistent
          `shouldBe` Just nonExistentFP
        ioeGetLocation failToReadNonExistent `shouldBe` "readFile"
        show failToReadNonExistent `shouldContain` "is a directory"

      it "can't read a virtual non-existent file" $ do
        Left failToReadNonExistent <- tryDryRunIO $ do
          () <- removeFile packageYamlP
          readFile packageYamlP
        ioeGetErrorType failToReadNonExistent `shouldBe` doesNotExistErrorType
        ioeGetFileName failToReadNonExistent `shouldBe` Just packageYamlFP
        ioeGetLocation failToReadNonExistent `shouldBe` "readFile"
        show failToReadNonExistent `shouldContain` "no such file"

    describe "writeFile" $ do
      it "can write data to an non-existent file" $ do
        data' <- dryRunIO $ do
          () <- writeFile nonExistentP "foo"
          readFile nonExistentP
        data' `shouldBe` "foo"
        withTempDir $ \tmpDir tmpDir' -> do
          () <- Prelude.writeFile (tmpDir' `combine` "foo") "Foo contents"
          data'' <- dryRunIO $ do
            () <- removeFile $ tmpDir </> foo
            () <- writeFile (tmpDir </> foo) "bar"
            readFile $ tmpDir </> foo
          data'' `shouldBe` "bar"
          Prelude.readFile (tmpDir' `combine` "foo")
            `shouldReturn` "Foo contents"

      it "can write data to an existing file"
        $ withTempDir
        $ \tmpDir tmpDir' -> do
          () <- Prelude.writeFile (tmpDir' `combine` "foo") "Foo contents"
          data' <- dryRunIO $ do
            () <- writeFile (tmpDir </> foo) "modified"
            readFile (tmpDir </> foo)
          data' `shouldBe` "modified"
          Prelude.readFile (tmpDir' `combine` "foo")
            `shouldReturn` "Foo contents"
          data'' <- dryRunIO $ do
            () <- writeFile nonExistentP "bar"
            () <- writeFile nonExistentP "baz"
            readFile nonExistentP
          data'' `shouldBe` "baz"
          doesFileExist nonExistentP `shouldReturn` False

      it "can't write data to a directory" $ do
        Left failToWriteToTest <- tryDryRunIO $ writeFile testP "foo"
        ioeGetErrorType failToWriteToTest `shouldBe` InappropriateType
        ioeGetFileName failToWriteToTest `shouldBe` Just testFP
        ioeGetLocation failToWriteToTest `shouldBe` "writeFile"
        show failToWriteToTest `shouldContain` "is a directory"
        Left failToWriteToTest' <- tryDryRunIO $ do
          () <- createDirectory nonExistentP
          writeFile nonExistentP "foo"
        ioeGetErrorType failToWriteToTest' `shouldBe` InappropriateType
        ioeGetFileName failToWriteToTest' `shouldBe` Just nonExistentFP
        ioeGetLocation failToWriteToTest' `shouldBe` "writeFile"
        show failToWriteToTest' `shouldContain` "is a directory"

      it "can't write data to a file path in a non-existent directory" $ do
        Left failToWrite <- tryDryRunIO $ do
          writeFile (nonExistentP </> nonExistentP) "foo"
        ioeGetErrorType failToWrite `shouldBe` doesNotExistErrorType
        ioeGetFileName failToWrite
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToWrite `shouldBe` "writeFile"
        show failToWrite `shouldContain` "no parent directory"
        Left failToWrite' <- tryDryRunIO $ do
          () <- removeFile packageYamlP
          writeFile (packageYamlP </> nonExistentP) "foo"
        ioeGetErrorType failToWrite' `shouldBe` doesNotExistErrorType
        ioeGetFileName failToWrite'
          `shouldBe` Just (packageYamlFP `combine` nonExistentFP)
        ioeGetLocation failToWrite' `shouldBe` "writeFile"
        show failToWrite' `shouldContain` "no parent directory"

      it "can't write data to a file path inside a non-directory file" $ do
        Left failToWrite <- tryDryRunIO $ do
          writeFile (packageYamlP </> nonExistentP) "bar"
        ioeGetErrorType failToWrite `shouldBe` InappropriateType
        ioeGetFileName failToWrite
          `shouldBe` Just (packageYamlFP `combine` nonExistentFP)
        ioeGetLocation failToWrite `shouldBe` "writeFile"
        show failToWrite `shouldContain` "not inside a directory"
        Left failToWrite' <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          writeFile (nonExistentP </> nonExistentP) "bar"
        ioeGetErrorType failToWrite' `shouldBe` InappropriateType
        ioeGetFileName failToWrite'
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToWrite' `shouldBe` "writeFile"
        show failToWrite' `shouldContain` "not inside a directory"
        Left failToWrite'' <- tryDryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          writeFile (nonExistentP </> nonExistentP) "bar"
        ioeGetErrorType failToWrite'' `shouldBe` InappropriateType
        ioeGetFileName failToWrite''
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToWrite'' `shouldBe` "writeFile"
        show failToWrite'' `shouldContain` "not inside a directory"

    describe "readSymlinkTarget" $ do
      symIt "returns the target path of a symbolic link"
        $ withTempDir
        $ \tmpDir tmpDir' -> do
          () <- Prelude.writeFile (tmpDir' `combine` "foo") ""
          createFileLink foo (tmpDir </> bar)
          dryRunIO (readSymlinkTarget $ tmpDir </> bar) `shouldReturn` foo

      it "fails with non-existent file" $ do
        dryRunIO (readSymlinkTarget nonExistentP) `shouldThrow` \e ->
          isDoesNotExistError e && ioeGetFileName e == Just nonExistentFP

      it "just fails with any virtual files as they can't be a symlink" $ do
        Left failToReadSymlink <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          readSymlinkTarget nonExistentP
        ioeGetErrorType failToReadSymlink `shouldBe` InappropriateType
        ioeGetFileName failToReadSymlink `shouldBe` Just nonExistentFP
        ioeGetLocation failToReadSymlink `shouldBe` "readSymlinkTarget"
        show failToReadSymlink `shouldContain` "not a symbolic link"

      it "fails with an overlaid removal tag" $ do
        Left failToReadSymlink <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          () <- removeFile nonExistentP
          readSymlinkTarget nonExistentP
        ioeGetErrorType failToReadSymlink `shouldBe` doesNotExistErrorType
        ioeGetFileName failToReadSymlink `shouldBe` Just nonExistentFP
        ioeGetLocation failToReadSymlink `shouldBe` "readSymlinkTarget"
        show failToReadSymlink `shouldContain` "no such file"

    describe "copyFile" $ do
      it "copies a source file to a destination path" $ do
        data' <- dryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          readFile nonExistentP
        packageYamlData <- Data.ByteString.readFile packageYamlFP
        data' `shouldBe` packageYamlData
        doesFileExist nonExistentP `shouldReturn` False
        data'' <- dryRunIO $ do
          () <- writeFile nonExistentP "foo"
          () <- copyFile nonExistentP nonExistentP'
          readFile nonExistentP'
        data'' `shouldBe` "foo"
        doesFileExist nonExistentP' `shouldReturn` False
        data''' <- dryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          () <- copyFile nonExistentP nonExistentP'
          readFile nonExistentP'
        data''' `shouldBe` packageYamlData
        doesFileExist nonExistentP' `shouldReturn` False

      it "can copy a source file to an existing file by overwriting it" $ do
        data' <- dryRunIO $ do
          () <- writeFile nonExistentP "foo"
          () <- copyFile packageYamlP nonExistentP
          readFile nonExistentP
        packageYamlData <- Data.ByteString.readFile packageYamlFP
        data' `shouldBe` packageYamlData
        doesPathExist nonExistentP `shouldReturn` False

      it "can't copy a non-existent file" $ do
        Left failToCopy <- tryDryRunIO $ copyFile nonExistentP nonExistentP'
        ioeGetErrorType failToCopy `shouldBe` doesNotExistErrorType
        ioeGetFileName failToCopy `shouldBe` Just nonExistentFP
        ioeGetLocation failToCopy `shouldBe` "copyFile"
        show failToCopy `shouldContain` "source does not exist"
        Left failToCopy' <- tryDryRunIO $ do
          () <- removeFile packageYamlP
          copyFile packageYamlP nonExistentP
        ioeGetErrorType failToCopy' `shouldBe` doesNotExistErrorType
        ioeGetFileName failToCopy' `shouldBe` Just packageYamlFP
        ioeGetLocation failToCopy' `shouldBe` "copyFile"
        show failToCopy' `shouldContain` "source does not exist"

      it "can't copy a directory" $ do
        Left failToCopy <- tryDryRunIO $ copyFile testP nonExistentP
        ioeGetErrorType failToCopy `shouldBe` InappropriateType
        ioeGetFileName failToCopy `shouldBe` Just testFP
        ioeGetLocation failToCopy `shouldBe` "copyFile"
        show failToCopy `shouldContain` "source is a directory"
        doesPathExist nonExistentP `shouldReturn` False
        Left failToCopy' <- tryDryRunIO $ do
          () <- createDirectory nonExistentP
          copyFile nonExistentP nonExistentP'
        ioeGetErrorType failToCopy' `shouldBe` InappropriateType
        ioeGetFileName failToCopy' `shouldBe` Just nonExistentFP
        ioeGetLocation failToCopy' `shouldBe` "copyFile"
        show failToCopy' `shouldContain` "source is a directory"
        doesPathExist nonExistentP' `shouldReturn` False

      it "can't copy a file to a file path that has no parent directory" $ do
        Left failToCopy <- tryDryRunIO $ do
          copyFile packageYamlP (nonExistentP </> nonExistentP)
        failToCopy `shouldSatisfy` isDoesNotExistError
        ioeGetFileName failToCopy
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToCopy `shouldBe` "copyFile"
        show failToCopy `shouldContain` "no parent directory"
        Left failToCopy' <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          () <- removeFile nonExistentP
          copyFile packageYamlP (nonExistentP </> nonExistentP)
        failToCopy' `shouldSatisfy` isDoesNotExistError
        ioeGetFileName failToCopy'
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToCopy' `shouldBe` "copyFile"
        show failToCopy' `shouldContain` "no parent directory"

      it "can't copy a file to a file path inside a non-directory path" $ do
        Left failToCopy <- tryDryRunIO $ do
          copyFile packageYamlP (packageYamlP </> nonExistentP)
        ioeGetErrorType failToCopy `shouldBe` InappropriateType
        ioeGetFileName failToCopy
          `shouldBe` Just (packageYamlFP `combine` nonExistentFP)
        ioeGetLocation failToCopy `shouldBe` "copyFile"
        show failToCopy `shouldContain` "not inside a directory"
        Left failToCopy' <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          copyFile packageYamlP (nonExistentP </> nonExistentP)
        ioeGetErrorType failToCopy `shouldBe` InappropriateType
        ioeGetFileName failToCopy'
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToCopy' `shouldBe` "copyFile"
        show failToCopy' `shouldContain` "not inside a directory"
        Left failToCopy'' <- tryDryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          copyFile nonExistentP (nonExistentP </> nonExistentP)
        ioeGetErrorType failToCopy `shouldBe` InappropriateType
        ioeGetFileName failToCopy''
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToCopy'' `shouldBe` "copyFile"
        show failToCopy'' `shouldContain` "not inside a directory"

      it "can't copy a file to a directory path" $ do
        Left failToCopy <- tryDryRunIO $ copyFile packageYamlP testP
        ioeGetErrorType failToCopy `shouldBe` InappropriateType
        ioeGetFileName failToCopy `shouldBe` Just testFP
        ioeGetLocation failToCopy `shouldBe` "copyFile"
        show failToCopy `shouldContain` "destination is a directory"
        doesDirectoryExist testP `shouldReturn` True
        Left failToCopy' <- tryDryRunIO $ do
          () <- createDirectory nonExistentP
          copyFile packageYamlP nonExistentP
        ioeGetErrorType failToCopy' `shouldBe` InappropriateType
        ioeGetFileName failToCopy' `shouldBe` Just nonExistentFP
        ioeGetLocation failToCopy' `shouldBe` "copyFile"
        show failToCopy' `shouldContain` "destination is a directory"

    describe "createDirectory" $ do
      it "creates an empty directory" $ do
        dryRunIO (createDirectory nonExistentP >> isDirectory nonExistentP)
          `shouldReturn` True

      it "can't create a directory inside a non-existent path" $ do
        Left failToCreate <- tryDryRunIO $ do
          createDirectory (nonExistentP </> nonExistentP)
        ioeGetErrorType failToCreate `shouldBe` doesNotExistErrorType
        ioeGetFileName failToCreate
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToCreate `shouldBe` "createDirectory"
        show failToCreate `shouldContain` "no parent directory"
        Left failToCreate' <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          () <- removeFile nonExistentP
          createDirectory (nonExistentP </> nonExistentP)
        ioeGetErrorType failToCreate' `shouldBe` doesNotExistErrorType
        ioeGetFileName failToCreate'
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToCreate' `shouldBe` "createDirectory"
        show failToCreate' `shouldContain` "no parent directory"

      it "can't create a directory inside a non-directory path" $ do
        Left failToCreate <- tryDryRunIO $ do
          createDirectory (packageYamlP </> nonExistentP)
        ioeGetErrorType failToCreate `shouldBe` InappropriateType
        ioeGetFileName failToCreate
          `shouldBe` Just (packageYamlFP `combine` nonExistentFP)
        ioeGetLocation failToCreate `shouldBe` "createDirectory"
        doesFileExist packageYamlP `shouldReturn` True
        Left failToCreate' <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          createDirectory (nonExistentP </> nonExistentP)
        ioeGetErrorType failToCreate' `shouldBe` InappropriateType
        ioeGetFileName failToCreate'
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToCreate' `shouldBe` "createDirectory"
        show failToCreate' `shouldContain` "not inside a directory"
        Left failToCreate'' <- tryDryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          createDirectory (nonExistentP </> nonExistentP)
        ioeGetErrorType failToCreate'' `shouldBe` InappropriateType
        ioeGetFileName failToCreate''
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToCreate'' `shouldBe` "createDirectory"
        show failToCreate'' `shouldContain` "not inside a directory"

      it "can't create a directory by overwriting an existing file" $ do
        Left failToCreate <- tryDryRunIO $ do
          createDirectory packageYamlP
        ioeGetErrorType failToCreate `shouldBe` alreadyExistsErrorType
        ioeGetFileName failToCreate `shouldBe` Just packageYamlFP
        ioeGetLocation failToCreate `shouldBe` "createDirectory"
        show failToCreate `shouldContain` "destination is already a file"
        doesFileExist packageYamlP `shouldReturn` True
        Left failToCreate' <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          createDirectory nonExistentP
        ioeGetErrorType failToCreate' `shouldBe` alreadyExistsErrorType
        ioeGetFileName failToCreate' `shouldBe` Just nonExistentFP
        ioeGetLocation failToCreate' `shouldBe` "createDirectory"
        show failToCreate' `shouldContain` "destination is already a file"
        Left failToCreate'' <- tryDryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          createDirectory nonExistentP
        ioeGetErrorType failToCreate'' `shouldBe` alreadyExistsErrorType
        ioeGetFileName failToCreate'' `shouldBe` Just nonExistentFP
        ioeGetLocation failToCreate'' `shouldBe` "createDirectory"
        show failToCreate'' `shouldContain` "destination is already a file"

      it "can't create a directory if it's already a directory" $ do
        Left failToCreate <- tryDryRunIO $ do
          createDirectory testP
        ioeGetErrorType failToCreate `shouldBe` alreadyExistsErrorType
        ioeGetFileName failToCreate `shouldBe` Just testFP
        ioeGetLocation failToCreate `shouldBe` "createDirectory"
        show failToCreate `shouldContain` "destination is already a directory"
        doesDirectoryExist testP `shouldReturn` True
        Left failToCreate' <- tryDryRunIO $ do
          () <- createDirectory nonExistentP
          createDirectory nonExistentP
        ioeGetErrorType failToCreate' `shouldBe` alreadyExistsErrorType
        ioeGetFileName failToCreate' `shouldBe` Just nonExistentFP
        ioeGetLocation failToCreate' `shouldBe` "createDirectory"
        show failToCreate' `shouldContain` "destination is already a directory"

    describe "removeFile" $ do
      it "removes an existing file" $ do
        dryRunIO (removeFile packageYamlP >> exists packageYamlP)
          `shouldReturn` False
        doesFileExist packageYamlP `shouldReturn` True
        fileExists <- dryRunIO $ do
          () <- writeFile nonExistentP ""
          () <- removeFile nonExistentP
          exists nonExistentP
        fileExists `shouldBe` False
        fileExists' <- dryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          () <- removeFile nonExistentP
          exists nonExistentP
        fileExists' `shouldBe` False

      it "can't remove a non-existent file" $ do
        Left failToRemove <- tryDryRunIO $ removeFile nonExistentP
        ioeGetErrorType failToRemove `shouldBe` doesNotExistErrorType
        ioeGetFileName failToRemove `shouldBe` Just nonExistentFP
        ioeGetLocation failToRemove `shouldBe` "removeFile"
        show failToRemove `shouldContain` "no such file"
        Left failToRemove' <- tryDryRunIO $ do
          () <- removeFile packageYamlP
          removeFile packageYamlP
        ioeGetErrorType failToRemove' `shouldBe` doesNotExistErrorType
        ioeGetFileName failToRemove' `shouldBe` Just packageYamlFP
        ioeGetLocation failToRemove' `shouldBe` "removeFile"
        show failToRemove' `shouldContain` "no such file"

      it "can't remove a directory" $ do
        Left failToRemove <- tryDryRunIO $ removeFile testP
        ioeGetErrorType failToRemove `shouldBe` InappropriateType
        ioeGetFileName failToRemove `shouldBe` Just testFP
        ioeGetLocation failToRemove `shouldBe` "removeFile"
        show failToRemove `shouldContain` "is a directory"
        doesDirectoryExist testP `shouldReturn` True
        Left failToRemove' <- tryDryRunIO $ do
          () <- createDirectory nonExistentP
          removeFile nonExistentP
        ioeGetErrorType failToRemove' `shouldBe` InappropriateType
        ioeGetFileName failToRemove' `shouldBe` Just nonExistentFP
        ioeGetLocation failToRemove' `shouldBe` "removeFile"
        show failToRemove' `shouldContain` "is a directory"

    describe "removeDirectory" $ do
      it "removes an existing directory" $ do
        result <- dryRunIO $ do
          createDirectory nonExistentP
          removeDirectory nonExistentP
          exists nonExistentP
        result `shouldBe` False

      it "can't remove a non-existent directory" $ do
        Left failToRemove <- tryDryRunIO $ removeDirectory nonExistentP
        failToRemove `shouldSatisfy` isDoesNotExistError
        ioeGetFileName failToRemove `shouldBe` Just nonExistentFP
        Left failToRemove' <- tryDryRunIO $ do
          () <- removeFile packageYamlP
          removeDirectory packageYamlP
        failToRemove' `shouldSatisfy` isDoesNotExistError
        ioeGetFileName failToRemove' `shouldBe` Just packageYamlFP

      it "can't remove a non-directory" $ do
        Left failToRemove <- tryDryRunIO $ removeDirectory packageYamlP
        ioeGetErrorType failToRemove `shouldBe` InappropriateType
        ioeGetFileName failToRemove `shouldBe` Just packageYamlFP
        doesFileExist packageYamlP `shouldReturn` True
        Left failToRemove' <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          removeDirectory nonExistentP
        ioeGetErrorType failToRemove' `shouldBe` InappropriateType
        ioeGetFileName failToRemove' `shouldBe` Just nonExistentFP
        Left failToRemove'' <- tryDryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          removeDirectory nonExistentP
        ioeGetErrorType failToRemove'' `shouldBe` InappropriateType
        ioeGetFileName failToRemove'' `shouldBe` Just nonExistentFP

    describe "removeDirectoryRecursively" $ do
      it "removes an existing directory recursively" $ do
        result <- dryRunIO $ do
          createDirectory nonExistentP
          writeFile (nonExistentP </> foo) ""
          createDirectory (nonExistentP </> bar)
          writeFile (nonExistentP </> bar </> baz) ""
          removeDirectoryRecursively nonExistentP
          exists nonExistentP
        result `shouldBe` False

      it "can't remove a non-existent directory" $ do
        Left failToRemove <- tryDryRunIO $ do
          removeDirectoryRecursively nonExistentP
        failToRemove `shouldSatisfy` isDoesNotExistError
        ioeGetFileName failToRemove `shouldBe` Just nonExistentFP
        Left failToRemove' <- tryDryRunIO $ do
          () <- removeFile packageYamlP
          removeDirectoryRecursively packageYamlP
        failToRemove' `shouldSatisfy` isDoesNotExistError
        ioeGetFileName failToRemove' `shouldBe` Just packageYamlFP

      it "can't remove a non-directory" $ do
        Left failToRemove <- tryDryRunIO $ removeDirectoryRecursively packageYamlP
        ioeGetErrorType failToRemove `shouldBe` InappropriateType
        ioeGetFileName failToRemove `shouldBe` Just packageYamlFP
        doesFileExist packageYamlP `shouldReturn` True
        Left failToRemove' <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          removeDirectoryRecursively nonExistentP
        ioeGetErrorType failToRemove' `shouldBe` InappropriateType
        ioeGetFileName failToRemove' `shouldBe` Just nonExistentFP
        Left failToRemove'' <- tryDryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          removeDirectoryRecursively nonExistentP
        ioeGetErrorType failToRemove'' `shouldBe` InappropriateType
        ioeGetFileName failToRemove'' `shouldBe` Just nonExistentFP

    describe "createDirectories" $ do
      it "creates a directory and its ancestors if needed" $ do
        result <- dryRunIO $ do
          () <- createDirectories (nonExistentP </> nonExistentP)
          isDirectory (nonExistentP </> nonExistentP)
        result `shouldBe` True
        doesDirectoryExist nonExistentP `shouldReturn` False

      it "can't create a directory if any of its ancestors is a file" $ do
        Left failToCreate <- tryDryRunIO $ do
          createDirectories (packageYamlP </> nonExistentP </> nonExistentP)
        ioeGetErrorType failToCreate `shouldBe` InappropriateType
        ioeGetFileName failToCreate `shouldBe` Just packageYamlFP
        ioeGetLocation failToCreate `shouldBe` "createDirectories"
        show failToCreate
          `shouldContain` "one of its ancestors is a non-directory file"

    describe "listDirectory" $ do
      it "lists direct children in a directory" $ do
        withFixture $ \tmpDir tmpDir' -> do
          result <- dryRunIO $ listDirectory tmpDir
          result `shouldBe` [bar, baz, foo]
          () <- Prelude.writeFile (tmpDir' `combine` "corge") ""
          result' <- dryRunIO $ do
            () <- writeFile (tmpDir </> qux) ""
            () <- createDirectory (tmpDir </> quux)
            () <- writeFile (tmpDir </> corge) ""
            () <- writeFile (dropFileName tmpDir </> foo) ""
            listDirectory tmpDir
          result' `shouldBe` [bar, baz, corge, foo, quux, qux]

      it "lists direct children in a virtual directory" $ do
        result <- dryRunIO $ do
          () <- createDirectory nonExistentP
          () <- writeFile (nonExistentP </> foo) ""
          () <- createDirectory (nonExistentP </> bar)
          () <- createDirectory (nonExistentP </> baz)
          () <- createDirectory (nonExistentP </> baz </> qux)
          listDirectory nonExistentP
        result `shouldBe` [bar, baz, foo]

      it "hides removed files in the sandbox" $ withFixture $ \tmpDir _ -> do
        result <- dryRunIO $ do
          () <- removeFile (tmpDir </> foo)
          listDirectory tmpDir
        result `shouldBe` [bar, baz]

      it "fails if a specified path does not exist" $ do
        Left e <- tryDryRunIO $ listDirectory nonExistentP
        e `shouldSatisfy` isDoesNotExistError
        ioeGetFileName e `shouldBe` Just nonExistentFP
        ioeGetErrorType e `shouldBe` doesNotExistErrorType
        ioeGetLocation e `shouldBe` "listDirectory"
        Left e' <- tryDryRunIO $ do
          () <- removeFile packageYamlP
          listDirectory packageYamlP
        e' `shouldSatisfy` isDoesNotExistError
        ioeGetFileName e' `shouldBe` Just packageYamlFP
        ioeGetLocation e' `shouldBe` "listDirectory"
        show e' `shouldContain` "no such directory"

      it "fails if a specified path is a regular file" $ do
        Left e <- tryDryRunIO $ listDirectory packageYamlP
        ioeGetErrorType e `shouldBe` InappropriateType
        ioeGetFileName e `shouldBe` Just packageYamlFP
        ioeGetLocation e `shouldBe` "listDirectory"
        Left e' <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          listDirectory nonExistentP
        ioeGetErrorType e' `shouldBe` InappropriateType
        ioeGetFileName e' `shouldBe` Just nonExistentFP
        ioeGetLocation e' `shouldBe` "listDirectory"
        show e' `shouldContain` "not a directory"
        Left e'' <- tryDryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          listDirectory nonExistentP
        ioeGetErrorType e'' `shouldBe` InappropriateType
        ioeGetFileName e'' `shouldBe` Just nonExistentFP
        ioeGetLocation e'' `shouldBe` "listDirectory"
        show e'' `shouldContain` "not a directory"

    describe "getFileSize" $ do
      it "returns the size of a file" $ withTempDir $ \tmpDir tmpDirFP -> do
        Data.ByteString.writeFile (tmpDirFP `combine` "foo") "asdf"
        dryRunIO (getFileSize $ tmpDir </> foo) `shouldReturn` 4
        size <- dryRunIO $ do
          () <- writeFile (tmpDir </> bar) "asdf asdf"
          getFileSize $ tmpDir </> bar
        size `shouldBe` 9
        size' <- dryRunIO $ do
          () <- writeFile (tmpDir </> bar) "asdf"
          () <- copyFile (tmpDir </> bar) (tmpDir </> baz)
          getFileSize $ tmpDir </> baz
        size' `shouldBe` 4

      it "fails if path doesn't exist" $ withTempDir $ \tmpDir tmpDirFP -> do
        Left e <- tryDryRunIO $ getFileSize $ tmpDir </> nonExistentP
        ioeGetFileName e `shouldBe` Just (tmpDirFP `combine` nonExistentFP)
        ioeGetErrorType e `shouldBe` doesNotExistErrorType
        Left e' <- tryDryRunIO $ do
          () <- removeFile packageYamlP
          getFileSize packageYamlP
        ioeGetFileName e' `shouldBe` Just packageYamlFP
        ioeGetErrorString e' `shouldBe` "getFileSize: no such file"

      it "fails if path is a directory" $ withTempDir $ \tmpDir tmpDirFP -> do
        Left e <- tryDryRunIO $ getFileSize tmpDir
        ioeGetFileName e `shouldBe` Just tmpDirFP
        ioeGetErrorString e `shouldBe` "getFileSize: it is a directory"
        Left e' <- tryDryRunIO $ do
          () <- createDirectory nonExistentP
          getFileSize nonExistentP
        ioeGetFileName e' `shouldBe` Just nonExistentFP
        ioeGetErrorString e' `shouldBe` "getFileSize: it is a directory"
