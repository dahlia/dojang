{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.MonadFileSystemSpec (spec) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (sort, sortOn)
import System.IO.Error
  ( doesNotExistErrorType
  , ioeGetErrorString
  , ioeGetErrorType
  , ioeGetFileName
  , permissionErrorType
  )
import System.Info (os)
import Prelude hiding (readFile, writeFile)
import Prelude qualified (writeFile)

import Data.ByteString qualified (readFile, writeFile)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (constantFrom)
import System.Directory.OsPath
  ( doesDirectoryExist
  , doesFileExist
  , doesPathExist
  )
import System.Directory.OsPath qualified (createDirectory)
import System.FilePath (combine)
import System.OsPath (dropFileName, encodeFS, (</>))
import Test.Hspec (Spec, describe, it, runIO, specify)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldNotReturn, shouldReturn)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.MonadFileSystem (FileType (..), MonadFileSystem (..), dryRunIO)
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

    specify "readFile" $ do
      Right contents <- readFile packageYamlP
      contents' <- Data.ByteString.readFile packageYamlFP
      contents `shouldBe` contents'
      Left error' <- readFile nonExistentP
      ioeGetErrorType error' `shouldBe` doesNotExistErrorType
      ioeGetFileName error' `shouldBe` Just nonExistentFP

    specify "writeFile" $ do
      withTempDir $ \tmpDirP tmpDirFP -> do
        Right () <- writeFile (tmpDirP </> nonExistentP) "foo"
        Data.ByteString.readFile (tmpDirFP `combine` nonExistentFP)
          `shouldReturn` "foo"

    specify "copyFile" $ do
      withTempDir $ \tmpDirP tmpDirFP -> do
        Right () <- copyFile packageYamlP (tmpDirP </> nonExistentP)
        contents <- Data.ByteString.readFile (tmpDirFP `combine` nonExistentFP)
        original <- Data.ByteString.readFile packageYamlFP
        contents `shouldBe` original

    specify "createDirectory" $ do
      withTempDir $ \tmpDirP _ -> do
        Right () <- createDirectory (tmpDirP </> nonExistentP)
        doesDirectoryExist (tmpDirP </> nonExistentP)
          `shouldReturn` True

    specify "removeFile" $ do
      withTempDir $ \tmpDirP tmpDirFP -> do
        Data.ByteString.writeFile (tmpDirFP `combine` nonExistentFP) ""
        doesFileExist (tmpDirP </> nonExistentP) `shouldReturn` True
        Right () <- removeFile (tmpDirP </> nonExistentP)
        doesFileExist (tmpDirP </> nonExistentP) `shouldReturn` False

    specify "listDirectory" $ withFixture $ \tmpDir _ -> do
      Right result <- listDirectory tmpDir
      sort result `shouldBe` [bar, baz, foo]

    specify "listDirectoryRecursively" $ withFixture $ \tmpDir tmpDir' -> do
      () <- Prelude.writeFile (tmpDir' `combine` "bar" `combine` "quux") ""
      () <- Prelude.writeFile (tmpDir' `combine` "baz" `combine` "corge") ""
      Right result <- listDirectoryRecursively tmpDir
      sortOn snd result
        `shouldBe` [ (Directory, bar)
                   , (File, bar </> quux)
                   , (Directory, baz)
                   , (File, baz </> corge)
                   , (Directory, baz </> qux)
                   , (File, foo)
                   ]

    specify "getFileSize" $ withFixture $ \tmpDir tmpDirFP -> do
      Data.ByteString.writeFile (tmpDirFP `combine` "foo") "asdf"
      getFileSize (tmpDir </> foo) `shouldReturn` Right 4
      Left e <- getFileSize (tmpDir </> nonExistentP)
      ioeGetErrorType e `shouldBe` doesNotExistErrorType
      ioeGetFileName e `shouldBe` Just (tmpDirFP `combine` nonExistentFP)
      Left e' <- getFileSize tmpDir
      ioeGetErrorString e' `shouldBe` "getFileSize: it is a directory"
      ioeGetFileName e' `shouldBe` Just tmpDirFP

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
          Right () <- writeFile nonExistentP ""
          isFile nonExistentP
        nonExistentIsFile `shouldBe` True

      it "checks a virtual file (Copied) that exists in memory" $ do
        nonExistentIsFile <- dryRunIO $ do
          Right () <- copyFile packageYamlP nonExistentP
          isFile nonExistentP
        nonExistentIsFile `shouldBe` True

      it "checks a virtual directory that exists in memory" $ do
        nonExistentIsFile <- dryRunIO $ do
          Right () <- createDirectory nonExistentP
          isFile nonExistentP
        nonExistentIsFile `shouldBe` False

      it "checks a virtual non-existent file" $ do
        packageYamlIsFile <- dryRunIO $ do
          Right () <- removeFile packageYamlP
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
          Right () <- createDirectory nonExistentP
          isDirectory nonExistentP
        nonExistentIsDir' `shouldBe` True

      it "checks a virtual file (Copied) that exists in memory" $ do
        nonExistentIsDir'' <- dryRunIO $ do
          Right () <- copyFile packageYamlP nonExistentP
          isDirectory nonExistentP
        nonExistentIsDir'' `shouldBe` False

      it "checks a virtual file (Contents) that exists in memory" $ do
        nonExistentIsDir''' <- dryRunIO $ do
          Right () <- writeFile nonExistentP ""
          isDirectory nonExistentP
        nonExistentIsDir''' `shouldBe` False

      it "checks a virtual non-existent directory" $ do
        packageYamlIsFile <- dryRunIO $ do
          Right () <- removeFile packageYamlP
          isDirectory packageYamlP
        packageYamlIsFile `shouldBe` False

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
          Right () <- writeFile nonExistentP ""
          exists nonExistentP
        nonExistentExists' `shouldBe` True

      it "checks a virtual file (Copied) that exists in memory" $ do
        nonExistentExists'' <- dryRunIO $ do
          Right () <- copyFile packageYamlP nonExistentP
          exists nonExistentP
        nonExistentExists'' `shouldBe` True

      it "checks a virtual directory that exists in memory" $ do
        nonExistentExists''' <- dryRunIO $ do
          Right () <- createDirectory nonExistentP
          exists nonExistentP
        nonExistentExists''' `shouldBe` True

      it "checks a virtual non-existent file" $ do
        packageYamlIsFile <- dryRunIO $ do
          Right () <- removeFile packageYamlP
          exists packageYamlP
        packageYamlIsFile `shouldBe` False

    describe "readFile" $ do
      it "can read an actual file that exists on the real file system" $ do
        Right packageYamlData <- dryRunIO $ readFile packageYamlP
        packageYamlData' <- Data.ByteString.readFile packageYamlFP
        packageYamlData `shouldBe` packageYamlData'

      it "can't read a directory that exists on the real file system" $ do
        Left failToReadTest <- dryRunIO $ readFile testP
        when (os == "mingw32") $ do
          ioeGetErrorType failToReadTest `shouldBe` permissionErrorType
        ioeGetFileName failToReadTest `shouldBe` Just testFP

      it "can't read a non-existent file" $ do
        Left failToReadNonExistent <- dryRunIO $ readFile nonExistentP
        ioeGetErrorType failToReadNonExistent `shouldBe` doesNotExistErrorType
        ioeGetFileName failToReadNonExistent
          `shouldBe` Just nonExistentFP

      it "can read a virtual file (Contents) that exists in memory" $ do
        Right nonExistentData <- dryRunIO $ do
          Right () <- writeFile nonExistentP "foo"
          readFile nonExistentP
        nonExistentData `shouldBe` "foo"

      it "can read a virtual file (Copied) that exists in memory" $ do
        Right nonExistentData <- dryRunIO $ do
          Right () <- copyFile packageYamlP nonExistentP
          Right () <- writeFile packageYamlP "foo" -- must not affect the result
          Right () <- copyFile nonExistentP nonExistentP'
          Right () <- writeFile nonExistentP "bar" -- must not affect the result
          readFile nonExistentP'
        packageYamlData <- Data.ByteString.readFile packageYamlFP
        nonExistentData `shouldBe` packageYamlData

      it "can't read a virtual directory that exists in memory" $ do
        Left failToReadNonExistent' <- dryRunIO $ do
          Right () <- createDirectory nonExistentP
          readFile nonExistentP
        ioeGetFileName failToReadNonExistent'
          `shouldBe` Just nonExistentFP
        ioeGetErrorString failToReadNonExistent'
          `shouldBe` "readFile: it is a directory"

      it "can't read a virtual non-existent file" $ do
        Left failToReadNonExistent' <- dryRunIO $ do
          Right () <- removeFile packageYamlP
          readFile packageYamlP
        ioeGetFileName failToReadNonExistent' `shouldBe` Just packageYamlFP
        ioeGetErrorString failToReadNonExistent'
          `shouldBe` "readFile: no such file"

    describe "writeFile" $ do
      it "can write data to an non-existent file" $ do
        Right data' <- dryRunIO $ do
          Right () <- writeFile nonExistentP "foo"
          readFile nonExistentP
        data' `shouldBe` "foo"
        Right data'' <- dryRunIO $ do
          Right () <- removeFile packageYamlP
          Right () <- writeFile packageYamlP "bar"
          readFile packageYamlP
        data'' `shouldBe` "bar"
        Data.ByteString.readFile packageYamlFP `shouldNotReturn` "bar"

      it "can write data to an existing file" $ do
        Right data' <- dryRunIO $ do
          Right () <- writeFile packageYamlP "foo"
          readFile packageYamlP
        data' `shouldBe` "foo"
        Data.ByteString.readFile packageYamlFP `shouldNotReturn` "foo"
        Right data'' <- dryRunIO $ do
          Right () <- writeFile nonExistentP "bar"
          Right () <- writeFile nonExistentP "baz"
          readFile nonExistentP
        data'' `shouldBe` "baz"
        doesFileExist nonExistentP `shouldReturn` False

      it "can't write data to a directory" $ do
        Left failToWriteToTest <- dryRunIO $ writeFile testP "foo"
        ioeGetFileName failToWriteToTest `shouldBe` Just testFP
        ioeGetErrorString failToWriteToTest
          `shouldBe` "writeFile: destination is a directory"
        Left failToWriteToTest' <- dryRunIO $ do
          Right () <- createDirectory nonExistentP
          writeFile nonExistentP "foo"
        ioeGetFileName failToWriteToTest' `shouldBe` Just nonExistentFP
        ioeGetErrorString failToWriteToTest'
          `shouldBe` "writeFile: destination is a directory"

      it "can't write data to a file path in a non-existent directory" $ do
        Left failToWrite <- dryRunIO $ do
          writeFile (nonExistentP </> nonExistentP) "foo"
        ioeGetFileName failToWrite
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetErrorString failToWrite
          `shouldBe` "writeFile: no parent directory"
        Left failToWrite' <- dryRunIO $ do
          Right () <- removeFile packageYamlP
          writeFile (packageYamlP </> nonExistentP) "foo"
        ioeGetFileName failToWrite'
          `shouldBe` Just (packageYamlFP `combine` nonExistentFP)
        ioeGetErrorString failToWrite'
          `shouldBe` "writeFile: no parent directory"

      it "can't write data to a file path inside a non-directory file" $ do
        Left failToWrite <- dryRunIO $ do
          writeFile (packageYamlP </> nonExistentP) "bar"
        ioeGetFileName failToWrite
          `shouldBe` Just (packageYamlFP `combine` nonExistentFP)
        ioeGetErrorString failToWrite
          `shouldBe` "writeFile: destination must be inside a directory"
        Left failToWrite' <- dryRunIO $ do
          Right () <- writeFile nonExistentP ""
          writeFile (nonExistentP </> nonExistentP) "bar"
        ioeGetFileName failToWrite'
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetErrorString failToWrite'
          `shouldBe` "writeFile: destination must be inside a directory"
        Left failToWrite'' <- dryRunIO $ do
          Right () <- copyFile packageYamlP nonExistentP
          writeFile (nonExistentP </> nonExistentP) "bar"
        ioeGetFileName failToWrite''
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetErrorString failToWrite''
          `shouldBe` "writeFile: destination must be inside a directory"

    describe "copyFile" $ do
      it "copies a source file to a destination path" $ do
        Right data' <- dryRunIO $ do
          Right () <- copyFile packageYamlP nonExistentP
          readFile nonExistentP
        packageYamlData <- Data.ByteString.readFile packageYamlFP
        data' `shouldBe` packageYamlData
        doesFileExist nonExistentP `shouldReturn` False
        Right data'' <- dryRunIO $ do
          Right () <- writeFile nonExistentP "foo"
          Right () <- copyFile nonExistentP nonExistentP'
          readFile nonExistentP'
        data'' `shouldBe` "foo"
        doesFileExist nonExistentP' `shouldReturn` False
        Right data''' <- dryRunIO $ do
          Right () <- copyFile packageYamlP nonExistentP
          Right () <- copyFile nonExistentP nonExistentP'
          readFile nonExistentP'
        data''' `shouldBe` packageYamlData
        doesFileExist nonExistentP' `shouldReturn` False

      it "can copy a source file to an existing file by overwriting it" $ do
        Right data' <- dryRunIO $ do
          Right () <- writeFile nonExistentP "foo"
          Right () <- copyFile packageYamlP nonExistentP
          readFile nonExistentP
        packageYamlData <- Data.ByteString.readFile packageYamlFP
        data' `shouldBe` packageYamlData
        doesPathExist nonExistentP `shouldReturn` False

      it "can't copy a non-existent file" $ do
        Left failToCopy <- dryRunIO $ copyFile nonExistentP nonExistentP'
        ioeGetFileName failToCopy `shouldBe` Just nonExistentFP
        ioeGetErrorString failToCopy
          `shouldBe` "copyFile: source does not exist"
        Left failToCopy' <- dryRunIO $ do
          Right () <- removeFile packageYamlP
          copyFile packageYamlP nonExistentP
        ioeGetFileName failToCopy' `shouldBe` Just packageYamlFP
        ioeGetErrorString failToCopy'
          `shouldBe` "copyFile: source does not exist"

      it "can't copy a directory" $ do
        Left failToCopy <- dryRunIO $ copyFile testP nonExistentP
        ioeGetFileName failToCopy `shouldBe` Just testFP
        ioeGetErrorString failToCopy
          `shouldBe` "copyFile: source is a directory"
        doesPathExist nonExistentP `shouldReturn` False
        Left failToCopy' <- dryRunIO $ do
          Right () <- createDirectory nonExistentP
          copyFile nonExistentP nonExistentP'
        ioeGetFileName failToCopy' `shouldBe` Just nonExistentFP
        ioeGetErrorString failToCopy'
          `shouldBe` "copyFile: source is a directory"
        doesPathExist nonExistentP' `shouldReturn` False

      it "can't copy a file to a file path that has no parent directory" $ do
        Left failToCopy <- dryRunIO $ do
          copyFile packageYamlP (nonExistentP </> nonExistentP)
        ioeGetFileName failToCopy
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetErrorString failToCopy
          `shouldBe` "copyFile: destination has no parent directory"
        Left failToCopy' <- dryRunIO $ do
          Right () <- writeFile nonExistentP ""
          Right () <- removeFile nonExistentP
          copyFile packageYamlP (nonExistentP </> nonExistentP)
        ioeGetFileName failToCopy'
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetErrorString failToCopy'
          `shouldBe` "copyFile: destination has no parent directory"

      it "can't copy a file to a file path inside a non-directory path" $ do
        Left failToCopy <- dryRunIO $ do
          copyFile packageYamlP (packageYamlP </> nonExistentP)
        ioeGetFileName failToCopy
          `shouldBe` Just (packageYamlFP `combine` nonExistentFP)
        ioeGetErrorString failToCopy
          `shouldBe` "copyFile: destination must be inside a directory"
        Left failToCopy' <- dryRunIO $ do
          Right () <- writeFile nonExistentP ""
          copyFile packageYamlP (nonExistentP </> nonExistentP)
        ioeGetFileName failToCopy'
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetErrorString failToCopy'
          `shouldBe` "copyFile: destination must be inside a directory"
        Left failToCopy'' <- dryRunIO $ do
          Right () <- copyFile packageYamlP nonExistentP
          copyFile nonExistentP (nonExistentP </> nonExistentP)
        ioeGetFileName failToCopy''
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetErrorString failToCopy''
          `shouldBe` "copyFile: destination must be inside a directory"

      it "can't copy a file to a directory path" $ do
        Left failToCopy <- dryRunIO $ copyFile packageYamlP testP
        ioeGetFileName failToCopy `shouldBe` Just testFP
        ioeGetErrorString failToCopy
          `shouldBe` "copyFile: destination is a directory"
        doesDirectoryExist testP `shouldReturn` True
        Left failToCopy' <- dryRunIO $ do
          Right () <- createDirectory nonExistentP
          copyFile packageYamlP nonExistentP
        ioeGetFileName failToCopy' `shouldBe` Just nonExistentFP
        ioeGetErrorString failToCopy'
          `shouldBe` "copyFile: destination is a directory"

    describe "createDirectory" $ do
      it "creates an empty directory" $ do
        dryRunIO (createDirectory nonExistentP >> isDirectory nonExistentP)
          `shouldReturn` True

      it "can't create a directory inside a non-existent path" $ do
        Left failToCreate <- dryRunIO $ do
          createDirectory (nonExistentP </> nonExistentP)
        ioeGetFileName failToCreate
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetErrorString failToCreate
          `shouldBe` "createDirectory: no parent directory"
        Left failToCreate' <- dryRunIO $ do
          Right () <- writeFile nonExistentP ""
          Right () <- removeFile nonExistentP
          createDirectory (nonExistentP </> nonExistentP)
        ioeGetFileName failToCreate'
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetErrorString failToCreate'
          `shouldBe` "createDirectory: no parent directory"

      it "can't create a directory inside a non-directory path" $ do
        Left failToCreate <- dryRunIO $ do
          createDirectory (packageYamlP </> nonExistentP)
        ioeGetFileName failToCreate
          `shouldBe` Just (packageYamlFP `combine` nonExistentFP)
        ioeGetErrorString failToCreate
          `shouldBe` "createDirectory: destination must be inside a directory"
        doesFileExist packageYamlP `shouldReturn` True
        Left failToCreate' <- dryRunIO $ do
          Right () <- writeFile nonExistentP ""
          createDirectory (nonExistentP </> nonExistentP)
        ioeGetFileName failToCreate'
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetErrorString failToCreate'
          `shouldBe` "createDirectory: destination must be inside a directory"
        Left failToCreate'' <- dryRunIO $ do
          Right () <- copyFile packageYamlP nonExistentP
          createDirectory (nonExistentP </> nonExistentP)
        ioeGetFileName failToCreate''
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetErrorString failToCreate''
          `shouldBe` "createDirectory: destination must be inside a directory"

      it "can't create a directory by overwriting an existing file" $ do
        Left failToCreate <- dryRunIO $ do
          createDirectory packageYamlP
        ioeGetFileName failToCreate `shouldBe` Just packageYamlFP
        ioeGetErrorString failToCreate
          `shouldBe` "createDirectory: destination is already a file"
        doesFileExist packageYamlP `shouldReturn` True
        Left failToCreate' <- dryRunIO $ do
          Right () <- writeFile nonExistentP ""
          createDirectory nonExistentP
        ioeGetFileName failToCreate' `shouldBe` Just nonExistentFP
        ioeGetErrorString failToCreate'
          `shouldBe` "createDirectory: destination is already a file"
        Left failToCreate'' <- dryRunIO $ do
          Right () <- copyFile packageYamlP nonExistentP
          createDirectory nonExistentP
        ioeGetFileName failToCreate'' `shouldBe` Just nonExistentFP
        ioeGetErrorString failToCreate''
          `shouldBe` "createDirectory: destination is already a file"

      it "can't create a directory if it's already a directory" $ do
        Left failToCreate <- dryRunIO $ do
          createDirectory testP
        ioeGetFileName failToCreate `shouldBe` Just testFP
        ioeGetErrorString failToCreate
          `shouldBe` "createDirectory: destination is already a directory"
        doesDirectoryExist testP `shouldReturn` True
        Left failToCreate' <- dryRunIO $ do
          Right () <- createDirectory nonExistentP
          createDirectory nonExistentP
        ioeGetFileName failToCreate' `shouldBe` Just nonExistentFP
        ioeGetErrorString failToCreate'
          `shouldBe` "createDirectory: destination is already a directory"

    describe "removeFile" $ do
      it "removes an existing file" $ do
        dryRunIO (removeFile packageYamlP >> exists packageYamlP)
          `shouldReturn` False
        doesFileExist packageYamlP `shouldReturn` True
        fileExists <- dryRunIO $ do
          Right () <- writeFile nonExistentP ""
          Right () <- removeFile nonExistentP
          exists nonExistentP
        fileExists `shouldBe` False
        fileExists' <- dryRunIO $ do
          Right () <- copyFile packageYamlP nonExistentP
          Right () <- removeFile nonExistentP
          exists nonExistentP
        fileExists' `shouldBe` False

      it "can't remove a non-existent file" $ do
        Left failToRemove <- dryRunIO $ removeFile nonExistentP
        ioeGetFileName failToRemove `shouldBe` Just nonExistentFP
        ioeGetErrorString failToRemove
          `shouldBe` "removeFile: no such file"
        Left failToRemove' <- dryRunIO $ do
          Right () <- removeFile packageYamlP
          removeFile packageYamlP
        ioeGetFileName failToRemove' `shouldBe` Just packageYamlFP
        ioeGetErrorString failToRemove'
          `shouldBe` "removeFile: no such file"

      it "can't remove a directory" $ do
        Left failToRemove <- dryRunIO $ removeFile testP
        ioeGetFileName failToRemove `shouldBe` Just testFP
        ioeGetErrorString failToRemove
          `shouldBe` "removeFile: it is a directory"
        doesDirectoryExist testP `shouldReturn` True
        Left failToRemove' <- dryRunIO $ do
          Right () <- createDirectory nonExistentP
          removeFile nonExistentP
        ioeGetFileName failToRemove' `shouldBe` Just nonExistentFP
        ioeGetErrorString failToRemove'
          `shouldBe` "removeFile: it is a directory"

    describe "createDirectories" $ do
      it "creates a directory and its ancestors if needed" $ do
        result <- dryRunIO $ do
          Right () <- createDirectories (nonExistentP </> nonExistentP)
          isDirectory (nonExistentP </> nonExistentP)
        result `shouldBe` True
        doesDirectoryExist nonExistentP `shouldReturn` False

      it "can't create a directory if any of its ancestors is a file" $ do
        Left failToCreate <- dryRunIO $ do
          createDirectories (packageYamlP </> nonExistentP </> nonExistentP)
        ioeGetFileName failToCreate `shouldBe` Just packageYamlFP
        ioeGetErrorString failToCreate
          `shouldBe` "createDirectories: one of its ancestors is a file"

    describe "listDirectory" $ do
      it "lists direct children in a directory" $ do
        withFixture $ \tmpDir tmpDir' -> do
          result <- dryRunIO $ listDirectory tmpDir
          result `shouldBe` Right [bar, baz, foo]
          () <- Prelude.writeFile (tmpDir' `combine` "corge") ""
          result' <- dryRunIO $ do
            Right () <- writeFile (tmpDir </> qux) ""
            Right () <- createDirectory (tmpDir </> quux)
            Right () <- writeFile (tmpDir </> corge) ""
            Right () <- writeFile (dropFileName tmpDir </> foo) ""
            listDirectory tmpDir
          result' `shouldBe` Right [bar, baz, corge, foo, quux, qux]

      it "lists direct children in a virtual directory" $ do
        result <- dryRunIO $ do
          Right () <- createDirectory nonExistentP
          Right () <- writeFile (nonExistentP </> foo) ""
          Right () <- createDirectory (nonExistentP </> bar)
          Right () <- createDirectory (nonExistentP </> baz)
          Right () <- createDirectory (nonExistentP </> baz </> qux)
          listDirectory nonExistentP
        result `shouldBe` Right [bar, baz, foo]

      it "hides removed files in the sandbox" $ withFixture $ \tmpDir _ -> do
        result <- dryRunIO $ do
          Right () <- removeFile (tmpDir </> foo)
          listDirectory tmpDir
        result `shouldBe` Right [bar, baz]

      it "fails if a specified path does not exist" $ do
        Left e <- dryRunIO $ listDirectory nonExistentP
        ioeGetFileName e `shouldBe` Just nonExistentFP
        ioeGetErrorType e `shouldBe` doesNotExistErrorType
        Left e' <- dryRunIO $ do
          Right () <- removeFile packageYamlP
          listDirectory packageYamlP
        ioeGetFileName e' `shouldBe` Just packageYamlFP
        ioeGetErrorString e' `shouldBe` "listDirectory: no such directory"

      it "fails if a specified path is a regular file" $ do
        Left e <- dryRunIO $ listDirectory packageYamlP
        ioeGetFileName e `shouldBe` Just packageYamlFP
        Left e' <- dryRunIO $ do
          Right () <- writeFile nonExistentP ""
          listDirectory nonExistentP
        ioeGetFileName e' `shouldBe` Just nonExistentFP
        ioeGetErrorString e' `shouldBe` "listDirectory: not a directory"
        Left e'' <- dryRunIO $ do
          Right () <- copyFile packageYamlP nonExistentP
          listDirectory nonExistentP
        ioeGetFileName e'' `shouldBe` Just nonExistentFP
        ioeGetErrorString e'' `shouldBe` "listDirectory: not a directory"

    describe "getFileSize" $ do
      it "returns the size of a file" $ withTempDir $ \tmpDir tmpDirFP -> do
        Data.ByteString.writeFile (tmpDirFP `combine` "foo") "asdf"
        dryRunIO (getFileSize $ tmpDir </> foo) `shouldReturn` Right 4
        Right size <- dryRunIO $ do
          Right () <- writeFile (tmpDir </> bar) "asdf asdf"
          getFileSize $ tmpDir </> bar
        size `shouldBe` 9
        Right size' <- dryRunIO $ do
          Right () <- writeFile (tmpDir </> bar) "asdf"
          Right () <- copyFile (tmpDir </> bar) (tmpDir </> baz)
          getFileSize $ tmpDir </> baz
        size' `shouldBe` 4

      it "fails if path doesn't exist" $ withTempDir $ \tmpDir tmpDirFP -> do
        Left e <- dryRunIO $ getFileSize $ tmpDir </> nonExistentP
        ioeGetFileName e `shouldBe` Just (tmpDirFP `combine` nonExistentFP)
        ioeGetErrorType e `shouldBe` doesNotExistErrorType
        Left e' <- dryRunIO $ do
          Right () <- removeFile packageYamlP
          getFileSize packageYamlP
        ioeGetFileName e' `shouldBe` Just packageYamlFP
        ioeGetErrorString e' `shouldBe` "getFileSize: no such file"

      it "fails if path is a directory" $ withTempDir $ \tmpDir tmpDirFP -> do
        Left e <- dryRunIO $ getFileSize tmpDir
        ioeGetFileName e `shouldBe` Just tmpDirFP
        ioeGetErrorString e `shouldBe` "getFileSize: it is a directory"
        Left e' <- dryRunIO $ do
          Right () <- createDirectory nonExistentP
          getFileSize nonExistentP
        ioeGetFileName e' `shouldBe` Just nonExistentFP
        ioeGetErrorString e' `shouldBe` "getFileSize: it is a directory"
