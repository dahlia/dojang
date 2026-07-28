{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.MonadFileSystemSpec (spec) where

import Control.Concurrent
  ( forkFinally
  , forkIO
  , newEmptyMVar
  , putMVar
  , readMVar
  , takeMVar
  , threadDelay
  , tryPutMVar
  , tryReadMVar
  )
import Control.Exception qualified as Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bits (xor)
import Data.Foldable (traverse_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, sort, sortOn)
import Data.Time.Clock (addUTCTime)
import GHC.IO.Exception
  ( IOErrorType (InappropriateType, InvalidArgument)
  )
import System.IO.Error
  ( alreadyExistsErrorType
  , doesNotExistErrorType
  , ioeGetErrorType
  , ioeGetFileName
  , ioeGetLocation
  , isAlreadyExistsError
  , isDoesNotExistError
  , isPermissionError
  , mkIOError
  )
import Prelude hiding (readFile, writeFile)
import Prelude qualified (readFile, writeFile)

import Control.Monad (replicateM)
import Control.Monad.Except (MonadError (catchError), tryError)
import Data.ByteString qualified (length, map, readFile, writeFile)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (constantFrom)
import System.Directory.OsPath
  ( createDirectoryLink
  , createFileLink
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  )
import System.Directory.OsPath qualified as OsDirectory
import System.FilePath (combine)


#ifndef mingw32_HOST_OS
import Data.Bits ((.&.))
import Data.Char (chr)
import Data.Either (isRight)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..))
import System.Posix.Files qualified as Posix
import System.IO (IOMode (WriteMode), hSetFileSize, withBinaryFile)
import System.Process
  ( CreateProcess (env)
  , proc
  , readCreateProcessWithExitCode
  )
#endif

import System.Info (os)
import System.OsPath
  ( OsPath
  , decodeFS
  , dropFileName
  , encodeFS
  , normalise
  , (</>)
  )
import System.Timeout (timeout)
import Test.Hspec
  ( Spec
  , describe
  , expectationFailure
  , it
  , runIO
  , specify
  , xit
  , xspecify
  )
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldContain
  , shouldReturn
  , shouldSatisfy
  , shouldStartWith
  , shouldThrow
  )
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.MonadFileSystem
  ( BoundedFileRead (..)
  , FileType (..)
  , MonadFileSystem (..)
  , captureDirectoryPathIdentity
  , dryRunIO
  , isNoReplaceUnsupportedError
  , matchesDirectoryPathIdentity
  , noReplaceUnsupportedError
  , tryDryRunIO
  , writeFileAtomicallyIfSnapshot
  )
import Dojang.TestUtils
  ( withTempDir
  )


#ifndef mingw32_HOST_OS
import Dojang.TestUtils (supportsNonUtf8FileNames)
import Test.Hspec (pendingWith)
#endif
import Dojang.Types.RouteMetadata
  ( PortableMode (..)
  , portableModeFromBits
  )


packageYamlFP :: FilePath
packageYamlFP = "package.yaml"


testFP :: FilePath
testFP = "test"


nonExistentFP :: FilePath
nonExistentFP = "---non-existent---"


nonExistentFP' :: FilePath
nonExistentFP' = "---non-existent-2---"

#ifdef mingw32_HOST_OS
posixRegularFileSpec :: Spec
posixRegularFileSpec = pure ()


posixLockFileSpec :: Spec
posixLockFileSpec = pure ()


posixPortableModeSpec :: Spec
posixPortableModeSpec = pure ()


posixPrivateDirectorySpec :: Spec
posixPrivateDirectorySpec = pure ()


posixDryRunPortableModeSpec :: Spec
posixDryRunPortableModeSpec = pure ()


posixCopyInterruptionSpec :: Spec
posixCopyInterruptionSpec = pure ()


posixTraversalRaceSpec :: Spec
posixTraversalRaceSpec = pure ()


posixNativeTraversalSpec :: Spec
posixNativeTraversalSpec = pure ()
#else
posixRegularFileSpec :: Spec
posixRegularFileSpec =
  specify "isRegularFile rejects special files" $
    withTempDir $ \tmpDir _ -> do
      pipeName <- encodeFS "named-pipe"
      let pipe = tmpDir </> pipeName
      pipe' <- decodeFS pipe
      Posix.createNamedPipe pipe' 0o600
      isFile pipe `shouldReturn` True
      isRegularFile pipe `shouldReturn` False


posixLockFileSpec :: Spec
posixLockFileSpec =
  specify "withFileLock rejects special files" $
    withTempDir $ \tmpDir _ -> do
      pipeName <- encodeFS "named-pipe.lock"
      let pipe = tmpDir </> pipeName
      pipe' <- decodeFS pipe
      Posix.createNamedPipe pipe' 0o600
      completed <- timeout 1000000 $ tryError $ withFileLock pipe $ return ()
      case completed of
        Just (Left err) -> ioeGetErrorType err `shouldBe` InappropriateType
        Just (Right ()) -> fail "withFileLock accepted a named pipe."
        Nothing -> fail "withFileLock blocked while opening a named pipe."


posixPortableModeSpec :: Spec
posixPortableModeSpec = do
  specify "getPortableMode observes exact POSIX bits" $
    withTempDir $ \tmpDir tmpDir' -> do
      fooName <- encodeFS "posix-mode-probe"
      let fooFP = tmpDir' `combine` "posix-mode-probe"
      Prelude.writeFile fooFP ""
      Posix.setFileMode fooFP 0o600
      getPortableMode (tmpDir </> fooName)
        `shouldReturn` portableModeFromBits 0o600
      Posix.setFileMode fooFP 0o755
      getPortableMode (tmpDir </> fooName)
        `shouldReturn` portableModeFromBits 0o755
      Posix.setFileMode fooFP 0o444
      getPortableMode (tmpDir </> fooName)
        `shouldReturn` portableModeFromBits 0o444

  specify "setPortableMode applies exact POSIX bits" $
    withTempDir $ \tmpDir tmpDir' -> do
      fooName <- encodeFS "posix-mode-probe"
      let fooFP = tmpDir' `combine` "posix-mode-probe"
      Prelude.writeFile fooFP ""
      setPortableMode (tmpDir </> fooName) 0o600 :: IO ()
      mode <- Posix.fileMode <$> Posix.getFileStatus fooFP
      (mode .&. 0o777) `shouldBe` 0o600

  specify "setPortableWritable touches only the owner-write bit" $
    withTempDir $ \tmpDir tmpDir' -> do
      fooName <- encodeFS "posix-mode-probe"
      let fooFP = tmpDir' `combine` "posix-mode-probe"
      Prelude.writeFile fooFP ""
      Posix.setFileMode fooFP 0o444
      setPortableWritable (tmpDir </> fooName) True :: IO ()
      mode <- Posix.fileMode <$> Posix.getFileStatus fooFP
      (mode .&. 0o777) `shouldBe` 0o644
      setPortableWritable (tmpDir </> fooName) False
      mode' <- Posix.fileMode <$> Posix.getFileStatus fooFP
      (mode' .&. 0o777) `shouldBe` 0o444


posixPrivateDirectorySpec :: Spec
posixPrivateDirectorySpec =
  specify privateDirectoryTestName $ do
    probe <- lookupEnv privateDirectoryProbeVariable
    case probe of
      Just privatePath -> do
        private <- encodeFS privatePath
        Exception.bracket
          (Posix.setFileCreationMask 0o777)
          Posix.setFileCreationMask
          $ \_ -> do
            createPrivateDirectory private :: IO ()
            getPortableMode private
              `shouldReturn` portableModeFromBits 0o700
      Nothing ->
        withTempDir $ \tmpDir _ -> do
          privateName <- encodeFS "private"
          let private = tmpDir </> privateName
          privatePath <- decodeFS private
          executable <- getExecutablePath
          environment <- getEnvironment
          let childEnvironment =
                (privateDirectoryProbeVariable, privatePath)
                  : filter
                    ((/= privateDirectoryProbeVariable) . fst)
                    environment
              child =
                (proc executable ["--match", privateDirectoryTestName])
                  { env = Just childEnvironment
                  }
          (exitCode, standardOutput, standardError) <-
            readCreateProcessWithExitCode child ""
          case exitCode of
            ExitSuccess -> return ()
            ExitFailure _ ->
              expectationFailure $ standardOutput <> standardError


privateDirectoryTestName :: String
privateDirectoryTestName =
  "createPrivateDirectory overrides a restrictive umask"


privateDirectoryProbeVariable :: String
privateDirectoryProbeVariable =
  "DOJANG_TEST_PRIVATE_DIRECTORY_UMASK_PROBE"


posixDryRunPortableModeSpec :: Spec
posixDryRunPortableModeSpec = do
  specify "getPortableMode reads exact bits through the overlay" $
    withTempDir $ \tmpDir tmpDir' -> do
      fooName <- encodeFS "posix-mode-probe"
      let fooFP = tmpDir' `combine` "posix-mode-probe"
      Prelude.writeFile fooFP ""
      Posix.setFileMode fooFP 0o600
      dryRunIO (getPortableMode $ tmpDir </> fooName)
        `shouldReturn` portableModeFromBits 0o600

  specify "setPortableMode does not touch the disk" $
    withTempDir $ \tmpDir tmpDir' -> do
      fooName <- encodeFS "posix-mode-probe"
      let fooFP = tmpDir' `combine` "posix-mode-probe"
      Prelude.writeFile fooFP ""
      Posix.setFileMode fooFP 0o644
      observed <- dryRunIO $ do
        () <- setPortableMode (tmpDir </> fooName) 0o600
        getPortableMode $ tmpDir </> fooName
      observed `shouldBe` portableModeFromBits 0o600
      mode <- Posix.fileMode <$> Posix.getFileStatus fooFP
      (mode .&. 0o777) `shouldBe` 0o644


posixCopyInterruptionSpec :: Spec
posixCopyInterruptionSpec = do
  specify
    "copyRegularFileWithSnapshot rejects an in-place source mutation"
    $ withTempDir
    $ \tmpDir tmpDir' -> do
      sourceName <- encodeFS "source"
      destinationName <- encodeFS "destination"
      let source = tmpDir </> sourceName
          destination = tmpDir </> destinationName
          sourcePath = tmpDir' `combine` "source"
          sourceSize = 128 * 1024 * 1024
      withBinaryFile sourcePath WriteMode $ \handle ->
        hSetFileSize handle sourceSize
      Just snapshot <- getFileSnapshot source
      stopMutating <- newEmptyMVar
      mutationFinished <- newEmptyMVar
      Posix.setFileSize sourcePath $ fromIntegral $ sourceSize `div` 2
      _ <-
        forkFinally
          ( let mutate size = do
                  stopped <- tryReadMVar stopMutating
                  case stopped of
                    Just () -> return ()
                    Nothing -> do
                      Posix.setFileSize sourcePath $ fromIntegral size
                      threadDelay 100
                      mutate $
                        if size == sourceSize
                          then sourceSize `div` 2
                          else sourceSize
            in mutate sourceSize
          )
          (const $ putMVar mutationFinished ())
      threadDelay 1000
      result <- newEmptyMVar
      _ <-
        forkIO $ do
          copied <-
            copyRegularFileWithSnapshot
              snapshot
              source
              destination
          putMVar result copied
      copied <- timeout 5000000 $ takeMVar result
      putMVar stopMutating ()
      timeout 5000000 (takeMVar mutationFinished)
        `shouldReturn` Just ()
      copied `shouldBe` Just False
      exists destination `shouldReturn` False


  specify "readRegularFileBounded rejects an in-place source mutation" $
    withTempDir $ \tmpDir tmpDir' -> do
      sourceName <- encodeFS "source"
      let source = tmpDir </> sourceName
          sourcePath = tmpDir' `combine` "source"
          sourceSize :: Int
          sourceSize = 128 * 1024 * 1024
      withBinaryFile sourcePath WriteMode $ \handle ->
        hSetFileSize handle $ fromIntegral sourceSize
      stopMutating <- newEmptyMVar
      mutationFinished <- newEmptyMVar
      _ <-
        forkFinally
          ( let mutate size = do
                  stopped <- tryReadMVar stopMutating
                  case stopped of
                    Just () -> return ()
                    Nothing -> do
                      Posix.setFileSize sourcePath $ fromIntegral size
                      threadDelay 100
                      mutate $
                        if size == sourceSize
                          then sourceSize `div` 2
                          else sourceSize
            in mutate $ sourceSize `div` 2
          )
          (const $ putMVar mutationFinished ())
      threadDelay 1000
      result <- newEmptyMVar
      _ <-
        forkIO $ do
          observed <- readRegularFileBounded (sourceSize + 1) source
          putMVar result observed
      observed <- timeout 5000000 $ takeMVar result
      putMVar stopMutating ()
      timeout 5000000 (takeMVar mutationFinished)
        `shouldReturn` Just ()
      observed `shouldBe` Just FileChangedDuringRead


posixTraversalRaceSpec :: Spec
posixTraversalRaceSpec = do
  specify "listDirectoryPinned never follows a raced directory link" $
    withTempDir $ \tmpDir _ -> do
      nestedName <- encodeFS "nested-pinned"
      parkedName <- encodeFS "parked-pinned"
      outsideName <- encodeFS "outside-pinned"
      sentinelName <- encodeFS "outside-sentinel"
      let nested = tmpDir </> nestedName
          parked = tmpDir </> parkedName
          outside = tmpDir </> outsideName
      createDirectory nested
      createDirectory outside
      writeFile (outside </> sentinelName) "outside"
      stopMutating <- newEmptyMVar
      mutationFinished <- newEmptyMVar
      firstMutation <- newEmptyMVar
      completedMutations <- newIORef (0 :: Int)
      _ <-
        forkFinally
          ( let mutate = do
                  stopped <- tryReadMVar stopMutating
                  case stopped of
                    Just () -> return ()
                    Nothing -> do
                      OsDirectory.renameDirectory nested parked
                      createDirectoryLink outside nested
                      removeFile nested
                      OsDirectory.renameDirectory parked nested
                      atomicModifyIORef' completedMutations $ \count ->
                        (count + 1, ())
                      _ <- tryPutMVar firstMutation ()
                      mutate
            in mutate
          )
          (putMVar mutationFinished)
      takeMVar firstMutation
      outcomes <-
        replicateM 100 (tryError $ listDirectoryPinned nested)
          `Exception.finally` putMVar stopMutating ()
      mutationOutcome <- timeout 5000000 $ takeMVar mutationFinished
      case mutationOutcome of
        Nothing -> expectationFailure "the mutation thread did not stop"
        Just (Left exception) ->
          expectationFailure $
            "the mutation thread failed: " <> show exception
        Just (Right ()) -> return ()
      readIORef completedMutations >>= (`shouldSatisfy` (> 0))
      outcomes
        `shouldSatisfy` all
          ( \case
              Left _ -> True
              Right entries -> sentinelName `notElem` entries
          )

  specify "listDirectoryRecursively never follows a raced directory link" $
    withTempDir $ \tmpDir _ -> do
      nestedName <- encodeFS "nested"
      parkedName <- encodeFS "parked"
      outsideName <- encodeFS "outside"
      sentinelName <- encodeFS "outside-sentinel"
      fillerNames <-
        traverse (encodeFS . ("filler-" <>) . show) [1 .. 512 :: Int]
      let nested = tmpDir </> nestedName
          parked = tmpDir </> parkedName
          outside = tmpDir </> outsideName
          escaped = nestedName </> sentinelName
      createDirectory nested
      createDirectory outside
      writeFile (outside </> sentinelName) "outside"
      mapM_ (\name -> writeFile (tmpDir </> name) "") fillerNames
      stopMutating <- newEmptyMVar
      mutationFinished <- newEmptyMVar
      completedMutations <- newIORef (0 :: Int)
      _ <-
        forkFinally
          ( let mutate = do
                  stopped <- tryReadMVar stopMutating
                  case stopped of
                    Just () -> return ()
                    Nothing -> do
                      OsDirectory.renameDirectory nested parked
                      createDirectoryLink outside nested
                      removeFile nested
                      OsDirectory.renameDirectory parked nested
                      atomicModifyIORef' completedMutations $ \count ->
                        (count + 1, ())
                      mutate
            in mutate
          )
          (putMVar mutationFinished)
      outcomes <-
        replicateM 100 (tryError $ listDirectoryRecursively tmpDir [])
          `Exception.finally` putMVar stopMutating ()
      mutationOutcome <- timeout 5000000 $ takeMVar mutationFinished
      case mutationOutcome of
        Nothing -> expectationFailure "the mutation thread did not stop"
        Just (Left exception) ->
          expectationFailure $
            "the mutation thread failed: " <> show exception
        Just (Right ()) -> return ()
      readIORef completedMutations >>= (`shouldSatisfy` (> 0))
      outcomes `shouldSatisfy` any isRight
      outcomes
        `shouldSatisfy` all
          ( \case
              Left _ -> True
              Right entries -> (File, escaped) `notElem` entries
          )


posixNativeTraversalSpec :: Spec
posixNativeTraversalSpec = do
  supported <-
    runIO $
      withTempDir $ \tmpDir _ ->
        supportsNonUtf8FileNames tmpDir
  let description =
        "listDirectoryRecursively preserves arbitrary native name bytes"
  if supported
    then
      specify description $
        hedgehog $ do
          byte <- forAll $ Gen.word8 $ constantFrom 0x80 0x80 0xff
          observed <-
            liftIO $
              withTempDir $ \tmpDir _ -> do
                name <- encodeFS [chr $ 0xdc00 + fromIntegral byte]
                writeFile (tmpDir </> name) ""
                entries <- listDirectoryRecursively tmpDir []
                return (name, entries)
          case observed of
            (name, entries) -> entries === [(File, name)]
    else
      specify description $
        pendingWith "The filesystem rejects filenames that are not valid UTF-8."
#endif


vanishedEntryTraversalSpecs :: Spec
vanishedEntryTraversalSpecs = do
  specify "tolerant traversal omits entries vanished after enumeration" $ do
    (outcomes, stableName, _) <-
      runVanishedEntryTraversalRace $ \path ->
        listDirectoryRecursively path []
    outcomes `shouldSatisfy` all isToleratedTraversalOutcome
    outcomes `shouldSatisfy` all (containsStableEntry stableName)

  specify "strict traversal rejects entries vanished after enumeration" $ do
    (outcomes, stableName, rootPath) <-
      runVanishedEntryTraversalRace $ \path ->
        listDirectoryRecursivelyStrict path []
    outcomes
      `shouldSatisfy` any
        ( \case
            Left err ->
              isDoesNotExistError err || isWindowsDeletePendingError err
            Right _ -> False
        )
    outcomes `shouldSatisfy` all (containsStableEntry stableName)
    [err | Left err <- outcomes, isDoesNotExistError err]
      `shouldSatisfy` all
        ( maybe False (rootPath `isPrefixOf`)
            . ioeGetFileName
        )


runVanishedEntryTraversalRace
  :: (OsPath -> IO [(FileType, OsPath)])
  -> IO ([Either IOError [(FileType, OsPath)]], OsPath, FilePath)
runVanishedEntryTraversalRace listTree =
  withTempDir $ \tmpDir _ -> do
    stableName <- encodeFS "stable"
    racedNames <-
      mapM (encodeFS . ("raced-" <>) . show) [1 .. 64 :: Int]
    fillerNames <-
      mapM (encodeFS . ("filler-" <>) . show) [1 .. 2048 :: Int]
    writeFile (tmpDir </> stableName) ""
    mapM_ (\name -> writeFile (tmpDir </> name) "") racedNames
    mapM_ (\name -> writeFile (tmpDir </> name) "") fillerNames
    stopMutating <- newEmptyMVar
    mutationFinished <- newEmptyMVar
    completedMutations <- newIORef (0 :: Int)
    _ <-
      forkFinally
        ( let mutate (name : remaining) = do
                stopped <- tryReadMVar stopMutating
                case stopped of
                  Just () -> return ()
                  Nothing -> do
                    removeFile $ tmpDir </> name
                    threadDelay 50
                    recreateRacedFile $ tmpDir </> name
                    atomicModifyIORef' completedMutations $ \count ->
                      (count + 1, ())
                    mutate remaining
              mutate [] = mutate racedNames
          in mutate racedNames
        )
        (putMVar mutationFinished)
    outcomes <-
      replicateM 100 (tryError $ listTree tmpDir)
        `Exception.finally` putMVar stopMutating ()
    mutationOutcome <- timeout 5000000 $ takeMVar mutationFinished
    case mutationOutcome of
      Nothing -> expectationFailure "the mutation thread did not stop"
      Just (Left exception) ->
        expectationFailure $
          "the mutation thread failed: " <> show exception
      Just (Right ()) -> return ()
    readIORef completedMutations >>= (`shouldSatisfy` (> 0))
    rootPath <- decodeFS tmpDir
    return (outcomes, stableName, rootPath)


containsStableEntry
  :: OsPath -> Either IOError [(FileType, OsPath)] -> Bool
containsStableEntry _ (Left _) = True
containsStableEntry stableName (Right entries) =
  (File, stableName) `elem` entries


isToleratedTraversalOutcome
  :: Either IOError [(FileType, OsPath)] -> Bool
isToleratedTraversalOutcome (Left err) =
  isWindowsDeletePendingError err
isToleratedTraversalOutcome (Right _) = True


recreateRacedFile :: OsPath -> IO ()
recreateRacedFile path =
  writeFile path "" `catchError` \err ->
    if isWindowsDeletePendingError err
      then threadDelay 50 >> recreateRacedFile path
      else ioError err


isWindowsDeletePendingError :: IOError -> Bool
isWindowsDeletePendingError err =
  os == "mingw32" && isPermissionError err


isInappropriateTypeError :: Either IOError a -> Bool
isInappropriateTypeError (Left err) = ioeGetErrorType err == InappropriateType
isInappropriateTypeError (Right _) = False


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
        () <- OsDirectory.createDirectory $ tmpDir </> bar
        () <- OsDirectory.createDirectory $ tmpDir </> baz
        () <-
          OsDirectory.createDirectory
            (tmpDir </> baz </> qux)
        action tmpDir tmpDir'

  describe "MonadFileSystem IO" $ do
    specify "encodePath" $ hedgehog $ do
      filePath <- forAll $ Gen.string (constantFrom 0 0 256) Gen.unicode
      filePath' <- liftIO $ encodePath filePath >>= decodePath
      filePath' === filePath

    specify "makeAbsolute" $ do
      currentDirectory <- OsDirectory.getCurrentDirectory
      makeAbsolute foo
        `shouldReturn` normalise (currentDirectory </> foo)

    specify "getHomeDirectory" $ do
      homeDirectory <- OsDirectory.getHomeDirectory
      (getHomeDirectory :: IO OsPath) `shouldReturn` homeDirectory

    specify "exists" $ do
      exists packageYamlP `shouldReturn` True
      exists testP `shouldReturn` True
      exists nonExistentP `shouldReturn` False

    specify "isFile" $ do
      isFile packageYamlP `shouldReturn` True
      isFile testP `shouldReturn` False
      isFile nonExistentP `shouldReturn` False

    specify "isRegularFile" $ do
      isRegularFile packageYamlP `shouldReturn` True
      isRegularFile testP `shouldReturn` False
      isRegularFile nonExistentP `shouldReturn` False

    posixRegularFileSpec
    posixPrivateDirectorySpec

    specify "isDirectory" $ do
      isDirectory packageYamlP `shouldReturn` False
      isDirectory testP `shouldReturn` True
      isDirectory nonExistentP `shouldReturn` False

    specify
      "directory path identities reject arbitrary replaced ancestors"
      $ hedgehog
      $ do
        depth <- forAll $ Gen.int $ constantFrom 1 1 8
        suffix <-
          forAll $
            Gen.string
              (constantFrom 1 1 24)
              (Gen.element $ ['a' .. 'z'] <> ['0' .. '9'])
        unchanged <-
          liftIO $
            withTempDir $ \tmpDir _ -> do
              rootName <- encodeFS $ "root-" <> suffix
              movedName <- encodeFS $ "moved-" <> suffix
              childNames <-
                traverse
                  (encodeFS . ("child-" <>) . show)
                  [1 .. depth]
              let root = tmpDir </> rootName
                  moved = tmpDir </> movedName
                  selected = foldl (</>) root childNames
              createDirectories selected
              Just identity <- captureDirectoryPathIdentity selected
              renameDirectory root moved
              createDirectory root
              case childNames of
                [] -> fail "A generated directory path had no child."
                first : _ ->
                  renameDirectory
                    (moved </> first)
                    (root </> first)
              matchesDirectoryPathIdentity identity
        unchanged === False

    symSpecify "isSymlink" $ do
      isSymlink packageYamlP `shouldReturn` False
      isSymlink testP `shouldReturn` False
      isSymlink nonExistentP `shouldReturn` False
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        createFileLink foo (tmpDir </> bar)
        isSymlink (tmpDir </> bar) `shouldReturn` True

    symSpecify "createDirectories rejects a symbolic-link ancestor" $
      withTempDir $ \tmpDir _ -> do
        createDirectory $ tmpDir </> foo
        createDirectoryLink foo $ tmpDir </> bar
        barPath <- decodePath $ tmpDir </> bar
        Left failure <-
          tryError $ createDirectories $ tmpDir </> bar </> baz
        ioeGetErrorType failure `shouldBe` InappropriateType
        ioeGetFileName failure `shouldBe` Just barPath
        ioeGetLocation failure `shouldStartWith` "createDirectories"
        show failure
          `shouldContain` "one of its ancestors is a symbolic link"

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

    specify "replaceFile" $ withTempDir $ \tmpDirP _ -> do
      writeFile (tmpDirP </> foo) "new"
      writeFile (tmpDirP </> bar) "old"
      replaceFile (tmpDirP </> foo) (tmpDirP </> bar)
      readFile (tmpDirP </> bar) `shouldReturn` "new"
      exists (tmpDirP </> foo) `shouldReturn` False

    it "conditionally replaces arbitrary matching snapshots" $ hedgehog $ do
      original <- forAll $ Gen.bytes $ constantFrom 0 0 4096
      replacement <- forAll $ Gen.bytes $ constantFrom 0 0 4096
      observed <- liftIO $ withTempDir $ \tmpDir _ -> do
        let source = tmpDir </> foo
            destination = tmpDir </> bar
        writeFile destination original
        Just snapshot <- getFileSnapshot destination
        Just modeSnapshot <- getFileModeSnapshot destination
        writeFile source replacement
        replaced <-
          replaceFileIfSnapshot
            snapshot
            modeSnapshot
            original
            source
            destination
        contents <- readFile destination
        sourceExists <- exists source
        return (replaced, contents, sourceExists)
      observed === (True, replacement, False)

    it "rejects arbitrary changes before conditional replacement" $ hedgehog $ do
      original <- forAll $ Gen.bytes $ constantFrom 0 0 4096
      concurrent <-
        forAll $
          Gen.filter (/= original) $
            Gen.bytes $
              constantFrom 0 0 4096
      replacement <- forAll $ Gen.bytes $ constantFrom 0 0 4096
      observed <- liftIO $ withTempDir $ \tmpDir _ -> do
        let source = tmpDir </> foo
            destination = tmpDir </> bar
        writeFile destination original
        Just snapshot <- getFileSnapshot destination
        Just modeSnapshot <- getFileModeSnapshot destination
        writeFile source replacement
        writeFile destination concurrent
        replaced <-
          replaceFileIfSnapshot
            snapshot
            modeSnapshot
            original
            source
            destination
        contents <- readFile destination
        sourceExists <- exists source
        return (replaced, contents, sourceExists)
      observed === (False, concurrent, True)

    it "rejects mode changes before conditional replacement" $
      withTempDir $ \tmpDir _ -> do
        let source = tmpDir </> foo
            destination = tmpDir </> bar
        writeFile destination "original"
        Just snapshot <- getFileSnapshot destination
        Just modeSnapshot <- getFileModeSnapshot destination
        originalMode <- getPortableMode destination
        writeFile source "replacement"
        setPortableWritable destination $ not originalMode.writable
        replaceFileIfSnapshot
          snapshot
          modeSnapshot
          "original"
          source
          destination
          `shouldReturn` False
        readFile destination `shouldReturn` "original"
        exists source `shouldReturn` True

    it "conditionally replaces a matching read-only snapshot" $
      withTempDir $ \tmpDir _ -> do
        let source = tmpDir </> foo
            destination = tmpDir </> bar
        writeFile destination "original"
        setPortableWritable destination False
        Just snapshot <- getFileSnapshot destination
        Just modeSnapshot <- getFileModeSnapshot destination
        writeFile source "replacement"
        setPortableWritable source False
        replaceFileIfSnapshot
          snapshot
          modeSnapshot
          "original"
          source
          destination
          `shouldReturn` True
        readFile destination `shouldReturn` "replacement"
        mode <- getPortableMode destination
        mode.writable `shouldBe` False
        names <- mapM decodeFS =<< listDirectory tmpDir
        names
          `shouldSatisfy` all (not . isPrefixOf ".dojang-replaced-")

    it "removes a read-only staged file after conditional rejection" $
      withTempDir $ \tmpDir _ -> do
        let destination = tmpDir </> bar
        writeFile destination "original"
        Just snapshot <- getFileSnapshot destination
        Just modeSnapshot <- getFileModeSnapshot destination
        writeFile destination "concurrent-change"
        writeFileAtomicallyIfSnapshot
          snapshot
          modeSnapshot
          "original"
          destination
          "dojang-merge.tmp"
          "replacement"
          (portableModeFromBits 0o444)
          `shouldReturn` False
        readFile destination `shouldReturn` "concurrent-change"
        names <- mapM decodeFS =<< listDirectory tmpDir
        names `shouldSatisfy` all (not . isPrefixOf "dojang-merge.tmp")

    describe "withFileLock" $ do
      it "creates a missing regular lock file" $
        withTempDir $ \tmpDir _ -> do
          let lock = tmpDir </> nonExistentP
          withFileLock lock (return True) `shouldReturn` True
          isRegularFile lock `shouldReturn` True

      it "accepts an existing regular lock file" $
        withTempDir $ \tmpDir _ -> do
          let lock = tmpDir </> nonExistentP
          writeFile lock ""
          withFileLock lock (return True) `shouldReturn` True

      symIt "rejects a symlinked lock file" $
        withTempDir $ \tmpDir _ -> do
          let target = tmpDir </> nonExistentP
          let lock = tmpDir </> nonExistentP'
          writeFile target ""
          createFileLink nonExistentP lock
          result <- tryError $ withFileLock lock $ return ()
          result `shouldSatisfy` isInappropriateTypeError

      symIt "rejects a dangling lock symlink without creating its target" $
        withTempDir $ \tmpDir _ -> do
          let target = tmpDir </> nonExistentP
          let lock = tmpDir </> nonExistentP'
          createFileLink nonExistentP lock
          result <- tryError $ withFileLock lock $ return ()
          result `shouldSatisfy` isInappropriateTypeError
          exists target `shouldReturn` False
          isSymlink lock `shouldReturn` True

      it "rejects a directory as a lock file" $
        withTempDir $ \tmpDir _ -> do
          let lock = tmpDir </> nonExistentP
          createDirectory lock
          result <- tryError $ withFileLock lock $ return ()
          result `shouldSatisfy` isInappropriateTypeError

      posixLockFileSpec

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

    specify
      "copyRegularFileWithSnapshot rejects arbitrary replaced sources"
      $ hedgehog
      $ do
        original <- forAll $ Gen.bytes $ constantFrom 0 0 4096
        replacement <- forAll $ Gen.bytes $ constantFrom 0 0 4096
        (copied, destinationExists) <-
          liftIO $
            withTempDir $ \tmpDir _ -> do
              let source = tmpDir </> foo
                  outside = tmpDir </> bar
                  destination = tmpDir </> baz
              writeFile source original
              writeFile outside replacement
              Just snapshot <- getFileSnapshot source
              removeFile source
              createSymbolicLink outside source File
              result <-
                copyRegularFileWithSnapshot
                  snapshot
                  source
                  destination
              present <- exists destination
              return (result, present)
        copied === False
        destinationExists === False

    specify
      "copyRegularFileWithSnapshot rejects changes before opening the source"
      $ hedgehog
      $ do
        original <- forAll $ Gen.bytes $ constantFrom 1 1 4096
        let replacement = Data.ByteString.map (`xor` 0xff) original
        (copied, destinationExists) <-
          liftIO $
            withTempDir $ \tmpDir _ -> do
              let source = tmpDir </> foo
                  destination = tmpDir </> baz
              writeFile source original
              originalTime <- OsDirectory.getModificationTime source
              Just snapshot <- getFileSnapshot source
              writeFile source replacement
              OsDirectory.setModificationTime
                source
                (addUTCTime 2 originalTime)
              result <-
                copyRegularFileWithSnapshot
                  snapshot
                  source
                  destination
              present <- exists destination
              return (result, present)
        copied === False
        destinationExists === False

    specify "readRegularFileBounded enforces arbitrary byte limits" $
      hedgehog $ do
        contents <- forAll $ Gen.bytes $ constantFrom 0 0 4096
        limit <- forAll $ Gen.int $ constantFrom 0 0 4096
        observed <-
          liftIO $
            withTempDir $ \tmpDir _ -> do
              writeFile (tmpDir </> foo) contents
              readRegularFileBounded limit $ tmpDir </> foo
        if Data.ByteString.length contents > limit
          then observed === FileSizeLimitExceeded
          else observed === BoundedFileContents contents

    posixCopyInterruptionSpec

    specify "renameDirectory" $ withTempDir $ \tmpDir _ -> do
      createDirectory $ tmpDir </> foo
      writeFile (tmpDir </> foo </> bar) "contents"
      renameDirectory (tmpDir </> foo) (tmpDir </> baz)
      isDirectory (tmpDir </> foo) `shouldReturn` False
      readFile (tmpDir </> baz </> bar) `shouldReturn` "contents"

    specify "renameDirectory refuses an existing destination" $
      withTempDir $ \tmpDir _ -> do
        createDirectory $ tmpDir </> foo
        createDirectory $ tmpDir </> baz
        renameDirectory (tmpDir </> foo) (tmpDir </> baz)
          `shouldThrow` isAlreadyExistsError
        isDirectory (tmpDir </> foo) `shouldReturn` True
        isDirectory (tmpDir </> baz) `shouldReturn` True

    specify "classifies only unsupported no-replace errors" $ do
      let invalid =
            mkIOError
              InvalidArgument
              "renameDirectory"
              Nothing
              Nothing
      isNoReplaceUnsupportedError
        (noReplaceUnsupportedError "destination")
        `shouldBe` True
      isNoReplaceUnsupportedError invalid `shouldBe` False

    specify "renameDirectory allows only one concurrent publisher" $
      withTempDir $ \tmpDir _ -> do
        start <- newEmptyMVar
        sourceNames <-
          mapM (encodeFS . ("source-" <>) . show) [1 .. 128 :: Int]
        mapM_ (createDirectory . (tmpDir </>)) sourceNames
        results <-
          mapM
            ( \sourceName -> do
                result <- newEmptyMVar
                _ <-
                  forkIO $ do
                    readMVar start
                    outcome <-
                      tryError $
                        renameDirectory
                          (tmpDir </> sourceName)
                          (tmpDir </> baz)
                    putMVar result outcome
                return result
            )
            sourceNames
        putMVar start ()
        outcomes <- mapM takeMVar results
        length [() | Right () <- outcomes] `shouldBe` 1

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

    specify "removeDirectoryRecursivelyIfIdentity removes its directory" $
      withTempDir $ \tmpDir _ -> do
        ownedName <- encodeFS "owned"
        childName <- encodeFS "child"
        let owned = tmpDir </> ownedName
        createDirectory owned
        writeFile (owned </> childName) "owned"
        Just identity <- getFileIdentity owned
        removeDirectoryRecursivelyIfIdentity owned identity
          `shouldReturn` True
        exists owned `shouldReturn` False

    specify
      "removeDirectoryRecursivelyIfIdentity preserves arbitrary replacements"
      $ hedgehog
      $ do
        contents <- forAll $ Gen.bytes $ constantFrom 0 0 4096
        liftIO $
          withTempDir $ \tmpDir _ -> do
            ownedName <- encodeFS "owned"
            movedName <- encodeFS "moved"
            sentinelName <- encodeFS "sentinel"
            let owned = tmpDir </> ownedName
                moved = tmpDir </> movedName
            createDirectory owned
            Just identity <- getFileIdentity owned
            renameDirectory owned moved
            createDirectory owned
            writeFile (owned </> sentinelName) contents
            removeDirectoryRecursivelyIfIdentity owned identity
              `shouldReturn` False
            readFile (owned </> sentinelName) `shouldReturn` contents
            sort <$> listDirectory tmpDir
              `shouldReturn` sort [ownedName, movedName]

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

      symIt "distinguishes symlinks from regular files and directories" $
        withFixture $
          \tmpDir tmpDir' -> do
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

      posixTraversalRaceSpec
      posixNativeTraversalSpec
      vanishedEntryTraversalSpecs

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

    specify "readRegularFileBounded enforces arbitrary copied-file limits" $
      hedgehog $ do
        contents <- forAll $ Gen.bytes $ constantFrom 0 0 4096
        limit <- forAll $ Gen.int $ constantFrom 0 0 4096
        observed <-
          liftIO $
            withTempDir $ \tmpDir _ -> do
              let source = tmpDir </> foo
                  copied = tmpDir </> bar
              writeFile source contents
              dryRunIO $ do
                copyFile source copied
                readRegularFileBounded limit copied
        if Data.ByteString.length contents > limit
          then observed === FileSizeLimitExceeded
          else observed === BoundedFileContents contents

    specify "makeAbsolute" $ do
      currentDirectory <- OsDirectory.getCurrentDirectory
      dryRunIO (makeAbsolute foo)
        `shouldReturn` normalise (currentDirectory </> foo)

    specify "getHomeDirectory" $ do
      homeDirectory <- OsDirectory.getHomeDirectory
      dryRunIO getHomeDirectory `shouldReturn` homeDirectory

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
      symIt "checks an actual symlink that exists on the real file system" $
        withTempDir $
          \tmpDir tmpDir' -> do
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
      it "can read an actual file that exists on the real file system" $
        withTempDir $
          \tmpDir tmpDir' -> do
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

      it "can read a virtual file (Copied) that exists in memory" $
        withTempDir $
          \tmpDir tmpDir' -> do
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

      it "can write data to an existing file" $
        withTempDir $
          \tmpDir tmpDir' -> do
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

    describe "replaceFile" $ do
      it "replaces the destination only in the dry-run overlay" $
        withTempDir $ \tmpDir tmpDir' -> do
          Prelude.writeFile (tmpDir' `combine` "foo") "source"
          Prelude.writeFile (tmpDir' `combine` "bar") "destination"
          (sourceExists, destinationContents) <- dryRunIO $ do
            replaceFile (tmpDir </> foo) (tmpDir </> bar)
            (,) <$> exists (tmpDir </> foo) <*> readFile (tmpDir </> bar)
          sourceExists `shouldBe` False
          destinationContents `shouldBe` "source"
          Prelude.readFile (tmpDir' `combine` "foo") `shouldReturn` "source"
          Prelude.readFile (tmpDir' `combine` "bar")
            `shouldReturn` "destination"

    describe "readSymlinkTarget" $ do
      symIt "returns the target path of a symbolic link" $
        withTempDir $
          \tmpDir tmpDir' -> do
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

    describe "copyRegularFileWithSnapshot" $
      it "retains arbitrary dry-run copies by reference" $
        hedgehog $ do
          sourceContents <-
            forAll $ Gen.bytes $ constantFrom 0 0 4096
          let changedContents = sourceContents <> "changed"
          (copied, observed) <-
            liftIO $
              withTempDir $ \tmpDir tmpDir' -> do
                let source = tmpDir </> foo
                    destination = tmpDir </> bar
                    sourcePath = tmpDir' `combine` "foo"
                Data.ByteString.writeFile sourcePath sourceContents
                dryRunIO $ do
                  Just snapshot <- getFileSnapshot source
                  result <-
                    copyRegularFileWithSnapshot
                      snapshot
                      source
                      destination
                  liftIO $
                    Data.ByteString.writeFile
                      sourcePath
                      changedContents
                  contents <- readFile destination
                  return (result, contents)
          copied === True
          observed === changedContents

    describe "createDirectory" $ do
      it "creates an empty directory" $ do
        dryRunIO (createDirectory nonExistentP >> isDirectory nonExistentP)
          `shouldReturn` True

      it "creates a directory after removing a backing file" $ do
        ( dryRunIO $ do
            removeFile packageYamlP
            createDirectory packageYamlP
            isDirectory packageYamlP
          )
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
        ioeGetLocation failToCreate `shouldStartWith` "createDirectory"
        doesFileExist packageYamlP `shouldReturn` True
        Left failToCreate' <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          createDirectory (nonExistentP </> nonExistentP)
        ioeGetErrorType failToCreate' `shouldBe` InappropriateType
        ioeGetFileName failToCreate'
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToCreate' `shouldStartWith` "createDirectory"
        show failToCreate' `shouldContain` "not inside a directory"
        Left failToCreate'' <- tryDryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          createDirectory (nonExistentP </> nonExistentP)
        ioeGetErrorType failToCreate'' `shouldBe` InappropriateType
        ioeGetFileName failToCreate''
          `shouldBe` Just (nonExistentFP `combine` nonExistentFP)
        ioeGetLocation failToCreate'' `shouldStartWith` "createDirectory"
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
        ioeGetLocation failToRemove
          `shouldStartWith` "removeDirectoryRecursively"
        ioeGetFileName failToRemove `shouldBe` Just packageYamlFP
        doesFileExist packageYamlP `shouldReturn` True
        Left failToRemove' <- tryDryRunIO $ do
          () <- writeFile nonExistentP ""
          removeDirectoryRecursively nonExistentP
        ioeGetErrorType failToRemove' `shouldBe` InappropriateType
        ioeGetLocation failToRemove'
          `shouldStartWith` "removeDirectoryRecursively"
        ioeGetFileName failToRemove' `shouldBe` Just nonExistentFP
        Left failToRemove'' <- tryDryRunIO $ do
          () <- copyFile packageYamlP nonExistentP
          removeDirectoryRecursively nonExistentP
        ioeGetErrorType failToRemove'' `shouldBe` InappropriateType
        ioeGetLocation failToRemove''
          `shouldStartWith` "removeDirectoryRecursively"
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
        ioeGetLocation failToCreate `shouldStartWith` "createDirectories"
        show failToCreate
          `shouldContain` "one of its ancestors is a non-directory file"

    describe "renameDirectory" $ do
      it "moves a virtual tree without touching the real filesystem" $
        withTempDir $ \tmpDir _ -> do
          result <- dryRunIO $ do
            createDirectory $ tmpDir </> foo
            writeFile (tmpDir </> foo </> bar) "contents"
            renameDirectory (tmpDir </> foo) (tmpDir </> baz)
            (,,)
              <$> isDirectory (tmpDir </> foo)
              <*> isDirectory (tmpDir </> baz)
              <*> readFile (tmpDir </> baz </> bar)
          result `shouldBe` (False, True, "contents")
          OsDirectory.doesDirectoryExist (tmpDir </> baz)
            `shouldReturn` False

      it "preserves arbitrary virtual directory modes" $
        hedgehog $ do
          modes <-
            forAll $
              Gen.list
                (constantFrom 1 1 6)
                (Gen.word $ constantFrom 0o000 0o000 0o777)
          observed <-
            liftIO $
              withTempDir $ \tmpDir _ -> do
                components <-
                  traverse
                    (encodeFS . ("nested-" <>) . show)
                    [1 .. length modes - 1]
                let source = tmpDir </> foo
                    destination = tmpDir </> baz
                    sourceDirectories = scanl (</>) source components
                    destinationDirectories =
                      scanl (</>) destination components
                dryRunIO $ do
                  traverse_ createDirectory sourceDirectories
                  sequence_ $
                    zipWith
                      setPortableMode
                      sourceDirectories
                      modes
                  renameDirectory source destination
                  traverse getPortableMode destinationDirectories
          observed === fmap portableModeFromBits modes

      it "refuses an existing virtual destination" $ do
        Left err <- tryDryRunIO $ do
          createDirectory nonExistentP
          createDirectory nonExistentP'
          renameDirectory nonExistentP nonExistentP'
        err `shouldSatisfy` isAlreadyExistsError

      it "does not create a destination for a missing source" $ do
        result <- dryRunIO $ do
          _ <- tryError $ renameDirectory nonExistentP nonExistentP'
          isDirectory nonExistentP'
        result `shouldBe` False

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
        ioeGetLocation e `shouldStartWith` "listDirectory"
        Left e' <- tryDryRunIO $ do
          () <- removeFile packageYamlP
          listDirectory packageYamlP
        e' `shouldSatisfy` isDoesNotExistError
        ioeGetFileName e' `shouldBe` Just packageYamlFP
        ioeGetLocation e' `shouldStartWith` "listDirectory"
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
        ioeGetErrorType e `shouldBe` doesNotExistErrorType
        ioeGetFileName e `shouldBe` Just (tmpDirFP `combine` nonExistentFP)
        ioeGetLocation e `shouldBe` "getFileSize"
        Left e' <- tryDryRunIO $ do
          () <- removeFile packageYamlP
          getFileSize packageYamlP
        ioeGetErrorType e' `shouldBe` doesNotExistErrorType
        ioeGetFileName e' `shouldBe` Just packageYamlFP
        ioeGetLocation e' `shouldBe` "getFileSize"
        show e' `shouldContain` "does not exist"

      it "fails if path is a directory" $ withTempDir $ \tmpDir tmpDirFP -> do
        Left e <- tryDryRunIO $ getFileSize tmpDir
        ioeGetErrorType e `shouldBe` InappropriateType
        ioeGetFileName e `shouldBe` Just tmpDirFP
        ioeGetLocation e `shouldBe` "getFileSize"
        show e `shouldContain` "not a regular file, but a directory"
        Left e' <- tryDryRunIO $ do
          () <- createDirectory nonExistentP
          getFileSize nonExistentP
        ioeGetErrorType e' `shouldBe` InappropriateType
        ioeGetFileName e' `shouldBe` Just nonExistentFP
        ioeGetLocation e' `shouldBe` "getFileSize"
        show e' `shouldContain` "not a regular file, but a directory"

  describe "createSymbolicLink (IO)" $ do
    symSpecify "creates a file link" $ withTempDir $ \tmpDir tmpDir' -> do
      Prelude.writeFile (tmpDir' `combine` "foo") "linked contents"
      createSymbolicLink foo (tmpDir </> bar) File :: IO ()
      isSymlink (tmpDir </> bar) `shouldReturn` True
      readSymlinkTarget (tmpDir </> bar) `shouldReturn` foo
      readFile (tmpDir </> bar) `shouldReturn` "linked contents"

    symSpecify "creates a directory link" $ withTempDir $ \tmpDir _ -> do
      OsDirectory.createDirectory $ tmpDir </> baz
      createSymbolicLink (tmpDir </> baz) (tmpDir </> bar) Directory :: IO ()
      isSymlink (tmpDir </> bar) `shouldReturn` True
      isDirectory (tmpDir </> bar) `shouldReturn` True

    symSpecify "leaves a broken link that is still a symlink" $
      withTempDir $ \tmpDir _ -> do
        createSymbolicLink nonExistentP (tmpDir </> bar) File :: IO ()
        isSymlink (tmpDir </> bar) `shouldReturn` True
        exists (tmpDir </> bar) `shouldReturn` False
        isRegularFile (tmpDir </> bar) `shouldReturn` False
        readSymlinkTarget (tmpDir </> bar) `shouldReturn` nonExistentP

    symSpecify "fails when the link path already exists" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        Left e <-
          tryError (createSymbolicLink bar (tmpDir </> foo) File :: IO ())
        e `shouldSatisfy` isAlreadyExistsError

  describe "createSymbolicLink (DryRunIO)" $ do
    symSpecify "overlays a symlink without touching the disk" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") "real contents"
        observed <- dryRunIO $ do
          () <- createSymbolicLink foo (tmpDir </> bar) File
          (,,,)
            <$> isSymlink (tmpDir </> bar)
            <*> readSymlinkTarget (tmpDir </> bar)
            <*> readFile (tmpDir </> bar)
            <*> isRegularFile (tmpDir </> bar)
        observed `shouldBe` (True, foo, "real contents", False)
        doesPathExist (tmpDir </> bar) `shouldReturn` False

    symSpecify "keeps a broken overlaid link observable as a symlink" $
      withTempDir $ \tmpDir _ -> do
        observed <- dryRunIO $ do
          () <- createSymbolicLink nonExistentP (tmpDir </> bar) File
          (,,)
            <$> isSymlink (tmpDir </> bar)
            <*> exists (tmpDir </> bar)
            <*> isFile (tmpDir </> bar)
        observed `shouldBe` (True, False, False)

    symSpecify "follows overlaid links to overlaid targets" $
      withTempDir $ \tmpDir _ -> do
        observed <- dryRunIO $ do
          () <- writeFile (tmpDir </> foo) "overlaid contents"
          () <- createSymbolicLink foo (tmpDir </> bar) File
          (,,)
            <$> exists (tmpDir </> bar)
            <*> isFile (tmpDir </> bar)
            <*> readFile (tmpDir </> bar)
        observed `shouldBe` (True, True, "overlaid contents")

    symSpecify "follows overlaid directory links" $
      withTempDir $ \tmpDir _ -> do
        observed <- dryRunIO $ do
          () <- createDirectory (tmpDir </> baz)
          () <- writeFile (tmpDir </> baz </> foo) "inside"
          () <- createSymbolicLink (tmpDir </> baz) (tmpDir </> bar) Directory
          (,)
            <$> isDirectory (tmpDir </> bar)
            <*> listDirectory (tmpDir </> bar)
        observed `shouldBe` (True, [foo])

    symSpecify "removes an overlaid symlink without following it" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") "kept"
        observed <- dryRunIO $ do
          () <- createSymbolicLink foo (tmpDir </> bar) File
          () <- removeFile (tmpDir </> bar)
          (,) <$> isSymlink (tmpDir </> bar) <*> exists (tmpDir </> foo)
        observed `shouldBe` (False, True)
        Prelude.readFile (tmpDir' `combine` "foo") `shouldReturn` "kept"

    symSpecify "fails when the link path already exists" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        Left e <-
          tryDryRunIO $ createSymbolicLink bar (tmpDir </> foo) File
        e `shouldSatisfy` isAlreadyExistsError
        Left e' <- tryDryRunIO $ do
          () <- writeFile (tmpDir </> baz) "occupied"
          createSymbolicLink bar (tmpDir </> baz) File
        e' `shouldSatisfy` isAlreadyExistsError

    symSpecify "sees target writes made after link creation" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") "old"
        observed <- dryRunIO $ do
          () <- createSymbolicLink foo (tmpDir </> bar) File
          () <- writeFile (tmpDir </> foo) "new"
          readFile (tmpDir </> bar)
        observed `shouldBe` "new"

    symSpecify "fails on overlaid link cycles instead of looping" $
      withTempDir $ \tmpDir _ -> do
        Left e <- tryDryRunIO $ do
          () <- createSymbolicLink bar (tmpDir </> foo) File
          () <- createSymbolicLink foo (tmpDir </> bar) File
          readFile (tmpDir </> foo)
        ioeGetErrorType e `shouldBe` InvalidArgument
        Left e' <- tryDryRunIO $ do
          () <- createSymbolicLink bar (tmpDir </> foo) File
          () <- createSymbolicLink foo (tmpDir </> bar) File
          exists (tmpDir </> foo)
        ioeGetErrorType e' `shouldBe` InvalidArgument

    symSpecify "copies through an overlaid source link" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") "linked"
        observed <- dryRunIO $ do
          () <- createSymbolicLink foo (tmpDir </> bar) File
          () <- copyFile (tmpDir </> bar) (tmpDir </> baz)
          readFile (tmpDir </> baz)
        observed `shouldBe` "linked"

    symSpecify "rejects copying a broken overlaid source link" $
      withTempDir $ \tmpDir _ -> do
        Left e <- tryDryRunIO $ do
          () <- createSymbolicLink nonExistentP (tmpDir </> bar) File
          copyFile (tmpDir </> bar) (tmpDir </> baz)
        e `shouldSatisfy` isDoesNotExistError

    symSpecify "keeps historical copies of retargeted links" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") "original"
        Prelude.writeFile (tmpDir' `combine` "qux") "retargeted"
        observed <- dryRunIO $ do
          () <- createSymbolicLink foo (tmpDir </> bar) File
          () <- copyFile (tmpDir </> bar) (tmpDir </> baz)
          () <- removeFile (tmpDir </> bar)
          () <- createSymbolicLink qux (tmpDir </> bar) File
          (,) <$> readFile (tmpDir </> baz) <*> readFile (tmpDir </> bar)
        observed `shouldBe` ("original", "retargeted")

    symSpecify "allows re-creating a link after removal" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") "real"
        observed <- dryRunIO $ do
          () <- writeFile (tmpDir </> baz) "occupied"
          () <- removeFile (tmpDir </> baz)
          () <- createSymbolicLink foo (tmpDir </> baz) File
          (,) <$> isSymlink (tmpDir </> baz) <*> readFile (tmpDir </> baz)
        observed `shouldBe` (True, "real")
  describe "portable modes (IO)" $ do
    specify "getPortableMode observes writability" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        observed <- getPortableMode (tmpDir </> foo) :: IO PortableMode
        observed.writable `shouldBe` True

    specify "getPortableMode fails on a missing path" $
      withTempDir $ \tmpDir _ ->
        tryError (getPortableMode (tmpDir </> nonExistentP) :: IO PortableMode)
          >>= \case
            Left e -> e `shouldSatisfy` isDoesNotExistError
            Right mode -> expectationFailure $ "Unexpected mode: " <> show mode

    specify "setPortableWritable widens and narrows writability" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        setPortableWritable (tmpDir </> foo) False :: IO ()
        narrowed <- getPortableMode (tmpDir </> foo) :: IO PortableMode
        narrowed.writable `shouldBe` False
        setPortableWritable (tmpDir </> foo) True
        widened <- getPortableMode (tmpDir </> foo) :: IO PortableMode
        widened.writable `shouldBe` True

    posixPortableModeSpec

  describe "portable modes (DryRunIO)" $ do
    specify "getPortableMode reads through to the real filesystem" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        observed <- dryRunIO $ getPortableMode $ tmpDir </> foo
        observed.writable `shouldBe` True

    specify "setPortableMode overlays a mode" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        observed <- dryRunIO $ do
          () <- setPortableMode (tmpDir </> foo) 0o600
          getPortableMode $ tmpDir </> foo
        observed `shouldBe` portableModeFromBits 0o600

    specify "setPortableMode fails on a missing path" $
      withTempDir $ \tmpDir _ -> do
        Left e <- tryDryRunIO $ setPortableMode (tmpDir </> nonExistentP) 0o600
        e `shouldSatisfy` isDoesNotExistError

    specify "setPortableWritable overlays only writability" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        observed <- dryRunIO $ do
          () <- setPortableMode (tmpDir </> foo) 0o600
          () <- setPortableWritable (tmpDir </> foo) False
          getPortableMode $ tmpDir </> foo
        observed `shouldBe` portableModeFromBits 0o400

    specify "getPortableMode keeps a written file's prior mode" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        observed <- dryRunIO $ do
          () <- setPortableMode (tmpDir </> foo) 0o600
          () <- writeFile (tmpDir </> foo) "rewritten"
          getPortableMode $ tmpDir </> foo
        observed `shouldBe` portableModeFromBits 0o600

    specify "getPortableMode resets overlaid modes after removal" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        observed <- dryRunIO $ do
          () <- setPortableMode (tmpDir </> foo) 0o600
          () <- removeFile (tmpDir </> foo)
          () <- writeFile (tmpDir </> foo) "recreated"
          getPortableMode $ tmpDir </> foo
        observed `shouldBe` portableModeFromBits 0o644

    specify "getPortableMode follows Copied overlays" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        observed <- dryRunIO $ do
          () <- setPortableMode (tmpDir </> foo) 0o600
          () <- copyFile (tmpDir </> foo) (tmpDir </> bar)
          getPortableMode $ tmpDir </> bar
        observed `shouldBe` portableModeFromBits 0o600

    specify "getPortableMode keeps historical modes of copies" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        observed <- dryRunIO $ do
          () <- setPortableMode (tmpDir </> foo) 0o600
          () <- copyFile (tmpDir </> foo) (tmpDir </> bar)
          -- Later source changes must not affect the copy:
          () <- setPortableMode (tmpDir </> foo) 0o444
          getPortableMode $ tmpDir </> bar
        observed `shouldBe` portableModeFromBits 0o600

    specify "getPortableMode survives removal of a copy's source" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        observed <- dryRunIO $ do
          () <- setPortableMode (tmpDir </> foo) 0o600
          () <- copyFile (tmpDir </> foo) (tmpDir </> bar)
          () <- removeFile (tmpDir </> foo)
          getPortableMode $ tmpDir </> bar
        observed `shouldBe` portableModeFromBits 0o600

    specify "getPortableMode fails on removed or missing paths" $
      withTempDir $ \tmpDir tmpDir' -> do
        Prelude.writeFile (tmpDir' `combine` "foo") ""
        Left e <- tryDryRunIO $ do
          () <- removeFile (tmpDir </> foo)
          getPortableMode $ tmpDir </> foo
        e `shouldSatisfy` isDoesNotExistError
        Left e' <- tryDryRunIO $ getPortableMode $ tmpDir </> nonExistentP
        e' `shouldSatisfy` isDoesNotExistError

    posixDryRunPortableModeSpec
