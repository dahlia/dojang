{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.MergeSpec (spec) where

import Control.Exception (bracket_)
import Control.Monad.Except
  ( ExceptT
  , MonadError
  , catchError
  , runExceptT
  , throwError
  , tryError
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.Encoding qualified as Text
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Info (os)
import System.OsPath (OsPath, encodeFS, takeDirectory, (</>))
import Test.Hspec (Spec, describe, it, runIO, xit)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldReturn
  , shouldSatisfy
  )
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.MonadFileSystem qualified as FileSystem
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Merge
  ( MergeCommitError (..)
  , MergeCommitReplica (..)
  , MergeContentsState (..)
  , MergeInputError (..)
  , MergeInputRole (..)
  , MergeResultError (..)
  , MergeTextInput (..)
  , MergeWorkspace (..)
  , classifyMergeContents
  , commitMergeRecoveryGuarded
  , commitMergeResultGuarded
  , mergeCommitOrder
  , observeMergeTextInput
  , prepareMergeWorkspace
  , readMergeResult
  , revalidateMergeTextInput
  )
import Dojang.Types.RouteMetadata
  ( PortableMode (..)
  , RouteMode (..)
  , portableModeFromBits
  , posixFileModeBits
  , satisfiesPortableMode
  )


spec :: Spec
spec = do
  symlinkAvailable <- runIO $ withTempDir $ \root _ -> do
    targetName <- encodeFS "missing-target"
    linkName <- encodeFS "link"
    ( FileSystem.createSymbolicLink
        (root </> targetName)
        (root </> linkName)
        FileSystem.File
        >> return True
      )
      `catchError` const (return False)
  let symlinkIt = if symlinkAvailable then it else xit

  describe "classifyMergeContents" $
    it "classifies arbitrary snapshots from their exact bytes" $
      hedgehog $ do
        expected <- forAll Gen.enumBounded
        base <- forAll utf8Text
        firstChange <- forAll $ Gen.filter (/= base) utf8Text
        secondChange <-
          forAll $
            Gen.filter
              (\value -> value /= base && value /= firstChange)
              utf8Text
        sourceChanged <- forAll Gen.bool
        let (source, destination) = case expected of
              ConflictingMergeContents ->
                (firstChange, secondChange)
              RecoverableMergeContents ->
                (firstChange, firstChange)
              ConvergedMergeContents ->
                (base, base)
              OneSidedMergeContents ->
                if sourceChanged
                  then (firstChange, base)
                  else (base, firstChange)
        classifyMergeContents source base destination === expected

  describe "observeMergeTextInput" $ do
    it "captures arbitrary UTF-8 text without changing its bytes" $ hedgehog $ do
      contents <- forAll utf8Text
      observed <- liftIO $ withMergeFile contents $ \path ->
        observeMergeTextInput SourceInput path
      fmap (.contents) observed === Right contents

    it "rejects missing, non-regular, NUL-containing, and invalid UTF-8 inputs" $
      withTempDir $ \root _ -> do
        missingName <- encodeFS "missing"
        directoryName <- encodeFS "directory"
        nulName <- encodeFS "nul-containing"
        binaryName <- encodeFS "binary"
        let missing = root </> missingName
            directory = root </> directoryName
            nul = root </> nulName
            binary = root </> binaryName
        FileSystem.createDirectory directory
        FileSystem.writeFile nul "before\NULafter"
        FileSystem.writeFile binary $ ByteString.pack [0xff, 0xfe]
        observeMergeTextInput BaseInput missing
          `shouldReturn` Left (MissingMergeInput BaseInput missing)
        observeMergeTextInput BaseInput directory
          `shouldReturn` Left (UnsupportedMergeInput BaseInput directory)
        observeMergeTextInput BaseInput nul
          `shouldReturn` Left (NulMergeInput BaseInput nul)
        observeMergeTextInput BaseInput binary
          `shouldReturn` Left (InvalidUtf8MergeInput BaseInput binary)

    it "accepts an input that remains unchanged after observation" $
      withMergeFile "contents" $ \path -> do
        Right input <- observeMergeTextInput SourceInput path
        revalidateMergeTextInput input `shouldReturn` True

    it "detects arbitrary content changes after observation" $ hedgehog $ do
      original <- forAll utf8Text
      replacement <- forAll $ Gen.filter (/= original) utf8Text
      valid <- liftIO $ withMergeFile original $ \path -> do
        Right input <- observeMergeTextInput DestinationInput path
        FileSystem.writeFile path replacement
        revalidateMergeTextInput input
      valid === False

    it "detects mode-only changes after observation" $
      withMergeFile "contents" $ \path -> do
        Right input <- observeMergeTextInput DestinationInput path
        initial <- FileSystem.getPortableMode path
        FileSystem.setPortableWritable path $ not initial.writable
        revalidateMergeTextInput input `shouldReturn` False
        FileSystem.setPortableWritable path initial.writable

    symlinkIt "rejects a dangling symbolic link as unsupported" $
      withTempDir $ \root _ -> do
        targetName <- encodeFS "missing-target"
        linkName <- encodeFS "link"
        let target = root </> targetName
            link = root </> linkName
        FileSystem.createSymbolicLink target link FileSystem.File
        observeMergeTextInput BaseInput link
          `shouldReturn` Left (UnsupportedMergeInput BaseInput link)

    it "classifies an unreadable regular input" $
      if os == "mingw32"
        then return ()
        else withMergeFile "contents" $ \path ->
          bracket_
            (FileSystem.setPortableMode path 0o000)
            (FileSystem.setPortableMode path 0o600)
            ( observeMergeTextInput SourceInput path
                `shouldReturn` Left (UnreadableMergeInput SourceInput path)
            )

    it "classifies an input beneath an inaccessible ancestor" $
      if os == "mingw32"
        then return ()
        else withMergeFile "contents" $ \path ->
          bracket_
            (FileSystem.setPortableMode (takeDirectory path) 0o000)
            (FileSystem.setPortableMode (takeDirectory path) 0o700)
            ( observeMergeTextInput SourceInput path
                `shouldReturn` Left (UnreadableMergeInput SourceInput path)
            )

    it "invalidates an observed input beneath an inaccessible ancestor" $
      if os == "mingw32"
        then return ()
        else withMergeFile "contents" $ \path -> do
          Right input <- observeMergeTextInput SourceInput path
          bracket_
            (FileSystem.setPortableMode (takeDirectory path) 0o000)
            (FileSystem.setPortableMode (takeDirectory path) 0o700)
            (revalidateMergeTextInput input `shouldReturn` False)

  describe "prepareMergeWorkspace" $ do
    it "copies every input and initializes the result from the destination" $
      withTempDir $ \root _ -> do
        sourceName <- encodeFS "source-input"
        baseName <- encodeFS "base-input"
        destinationName <- encodeFS "destination-input"
        workspaceName <- encodeFS "workspace"
        let sourcePath = root </> sourceName
            basePath = root </> baseName
            destinationPath = root </> destinationName
            workspacePath = root </> workspaceName
        FileSystem.writeFile sourcePath "source"
        FileSystem.writeFile basePath "base"
        FileSystem.writeFile destinationPath "destination"
        Right source <- observeMergeTextInput SourceInput sourcePath
        Right base <- observeMergeTextInput BaseInput basePath
        Right destination <-
          observeMergeTextInput DestinationInput destinationPath
        workspace <-
          prepareMergeWorkspace workspacePath source base destination
        FileSystem.readFile workspace.source
          `shouldReturn` "source"
        FileSystem.readFile workspace.base
          `shouldReturn` "base"
        FileSystem.readFile workspace.destination
          `shouldReturn` "destination"
        FileSystem.readFile workspace.result
          `shouldReturn` "destination"
        workspaceMode <- FileSystem.getPortableMode workspace.root
        workspaceMode.writable `shouldBe` True
        workspaceMode.posixBits
          `shouldSatisfy` maybe True (== 0o700)
        mapM_
          assertPrivateFile
          [ workspace.source
          , workspace.base
          , workspace.destination
          , workspace.result
          ]

    it "removes partial contents when private-file setup fails" $
      withThreeInputs $ \source base destination -> do
        workspaceName <- encodeFS "workspace"
        let workspacePath = takeDirectory source.path </> workspaceName
        result <-
          runFailingWorkspaceIO $
            prepareMergeWorkspace workspacePath source base destination
        result `shouldSatisfy` either (const True) (const False)
        FileSystem.exists workspacePath `shouldReturn` False

    it "does not write inputs into a replaced workspace directory" $
      withThreeInputs $ \source base destination -> do
        workspaceName <- encodeFS "workspace"
        parkedName <- encodeFS "parked-workspace"
        sentinelName <- encodeFS "replacement"
        let parent = takeDirectory source.path
            workspacePath = parent </> workspaceName
            parkedPath = parent </> parkedName
            sentinelPath = workspacePath </> sentinelName
        replaced <- newIORef False
        result <-
          runReplacingWorkspaceIO
            workspacePath
            parkedPath
            sentinelPath
            replaced
            $ prepareMergeWorkspace
              workspacePath
              source
              base
              destination
        result `shouldSatisfy` either (const True) (const False)
        FileSystem.readFile sentinelPath `shouldReturn` "replacement"
        FileSystem.listDirectory workspacePath
          `shouldReturn` [sentinelName]

  describe "readMergeResult" $ do
    it "accepts arbitrary UTF-8 text from a regular result file" $ hedgehog $ do
      contents <- forAll utf8Text
      observed <- liftIO $ withMergeFile contents readMergeResult
      observed === Right contents

    it "rejects missing, non-regular, NUL-containing, and invalid UTF-8 results" $
      withTempDir $ \root _ -> do
        missingName <- encodeFS "missing"
        directoryName <- encodeFS "directory"
        nulName <- encodeFS "nul-containing"
        binaryName <- encodeFS "binary"
        let missing = root </> missingName
            directory = root </> directoryName
            nul = root </> nulName
            binary = root </> binaryName
        FileSystem.createDirectory directory
        FileSystem.writeFile nul "before\NULafter"
        FileSystem.writeFile binary $ ByteString.pack [0xff, 0xfe]
        readMergeResult missing
          `shouldReturn` Left (MissingMergeResult missing)
        readMergeResult directory
          `shouldReturn` Left (UnsupportedMergeResult directory)
        readMergeResult nul
          `shouldReturn` Left (NulMergeResult nul)
        readMergeResult binary
          `shouldReturn` Left (InvalidUtf8MergeResult binary)

    symlinkIt "rejects a dangling symbolic link as an unsupported result" $
      withTempDir $ \root _ -> do
        targetName <- encodeFS "missing-target"
        linkName <- encodeFS "link"
        let target = root </> targetName
            link = root </> linkName
        FileSystem.createSymbolicLink target link FileSystem.File
        readMergeResult link
          `shouldReturn` Left (UnsupportedMergeResult link)

    it "classifies an unreadable regular result" $
      if os == "mingw32"
        then return ()
        else withMergeFile "merged" $ \path ->
          bracket_
            (FileSystem.setPortableMode path 0o000)
            (FileSystem.setPortableMode path 0o600)
            ( readMergeResult path
                `shouldReturn` Left (UnreadableMergeResult path)
            )

    it "classifies a result beneath an inaccessible ancestor" $
      if os == "mingw32"
        then return ()
        else withMergeFile "merged" $ \path ->
          bracket_
            (FileSystem.setPortableMode (takeDirectory path) 0o000)
            (FileSystem.setPortableMode (takeDirectory path) 0o700)
            ( readMergeResult path
                `shouldReturn` Left (UnreadableMergeResult path)
            )

  describe "mergeCommitOrder" $ do
    it "commits source, destination, and intermediate exactly once in order" $
      hedgehog $ do
        prefixLength <- forAll $ Gen.int $ Range.linear 0 3
        let order = mergeCommitOrder
        order
          === [ SourceCommitReplica
              , DestinationCommitReplica
              , IntermediateCommitReplica
              ]
        length order === 3
        take prefixLength order
          === take
            prefixLength
            [ SourceCommitReplica
            , DestinationCommitReplica
            , IntermediateCommitReplica
            ]

  describe "commitMergeResultGuarded" $ do
    it "writes arbitrary UTF-8 results in the documented order" $ hedgehog $ do
      result <- forAll utf8Text
      observed <- liftIO $ withThreeInputs $ \source base destination -> do
        orderRef <- newIORef []
        committed <-
          commitMergeResultGuarded
            (\replica -> modifyIORef' orderRef (<> [replica]))
            DefaultMode
            source
            base
            destination
            result
        order <- readIORef orderRef
        sourceAfter <- FileSystem.readFile source.path
        destinationAfter <- FileSystem.readFile destination.path
        baseAfter <- FileSystem.readFile base.path
        return
          ( committed
          , order
          , [sourceAfter, destinationAfter, baseAfter]
          )
      observed
        === ( Right ()
            , mergeCommitOrder
            , replicate 3 result
            )

    it "stops after source when the destination changes before its step" $
      withThreeInputs $ \source base destination -> do
        let result = "merged"
            concurrent = "concurrent"
        committed <-
          commitMergeResultGuarded
            ( \replica ->
                if replica == DestinationCommitReplica
                  then FileSystem.writeFile destination.path concurrent
                  else return ()
            )
            DefaultMode
            source
            base
            destination
            result
        committed
          `shouldBe` Left
            (MergeInputsChanged $ NonEmpty.singleton DestinationInput)
        FileSystem.readFile source.path `shouldReturn` result
        FileSystem.readFile destination.path `shouldReturn` concurrent
        FileSystem.readFile base.path `shouldReturn` base.contents

    it "preserves a source edit made while its result is staged" $
      withThreeInputs $ \source base destination -> do
        let result = "merged"
            concurrent = "concurrent"
        committed <-
          runRacingCommitIO
            (ChangeWhileStaging source.path concurrent)
            ( commitMergeResultGuarded
                (const $ return ())
                DefaultMode
                source
                base
                destination
                result
            )
        committed
          `shouldBe` Right
            (Left $ MergeInputsChanged $ NonEmpty.singleton SourceInput)
        FileSystem.readFile source.path `shouldReturn` concurrent
        FileSystem.readFile destination.path
          `shouldReturn` destination.contents
        FileSystem.readFile base.path `shouldReturn` base.contents

    it "rejects arbitrary staged replacements before publication" $
      hedgehog $ do
        result <- forAll utf8Text
        replacement <-
          forAll $
            Gen.filter (/= result) $
              Gen.bytes $
                Range.linear 0 512
        observed <- liftIO $ withThreeInputs $ \source base destination -> do
          committed <-
            runRacingCommitIO
              (ReplaceStagedBeforePublication replacement)
              ( commitMergeResultGuarded
                  (const $ return ())
                  DefaultMode
                  source
                  base
                  destination
                  result
              )
          sourceAfter <- FileSystem.readFile source.path
          destinationAfter <- FileSystem.readFile destination.path
          baseAfter <- FileSystem.readFile base.path
          return
            ( committed
            , sourceAfter
            , destinationAfter
            , baseAfter
            , source.contents
            , destination.contents
            , base.contents
            )
        let
          ( committed
            , sourceAfter
            , destinationAfter
            , baseAfter
            , sourceBefore
            , destinationBefore
            , baseBefore
            ) = observed
        committed
          === Right
            ( Left $
                MergeInputsChanged $
                  NonEmpty.singleton SourceInput
            )
        sourceAfter === sourceBefore
        destinationAfter === destinationBefore
        baseAfter === baseBefore

    symlinkIt "does not apply a declared mode through a replacement link" $
      withThreeInputs $ \source base destination -> do
        externalName <- encodeFS "external"
        let external = takeDirectory source.path </> externalName
        FileSystem.writeFile external "external"
        FileSystem.setPortableMode external 0o600
        committed <-
          runRacingCommitIO
            (ReplaceBeforeMode destination.path external)
            ( commitMergeResultGuarded
                (const $ return ())
                ReadOnly
                source
                base
                destination
                "merged"
            )
        committed `shouldBe` Right (Right ())
        FileSystem.isSymlink destination.path `shouldReturn` False
        FileSystem.readFile external `shouldReturn` "external"
        externalMode <- FileSystem.getPortableMode external
        externalMode.posixBits `shouldSatisfy` maybe True (== 0o600)

    it "leaves the baseline old when its write step fails" $
      withThreeInputs $ \source base destination -> do
        let result = "merged"
        committed <-
          tryError $
            commitMergeResultGuarded
              ( \replica ->
                  if replica == IntermediateCommitReplica
                    then throwError $ userError "injected baseline failure"
                    else return ()
              )
              DefaultMode
              source
              base
              destination
              result
        committed `shouldSatisfy` either (const True) (const False)
        FileSystem.readFile source.path `shouldReturn` result
        FileSystem.readFile destination.path `shouldReturn` result
        FileSystem.readFile base.path `shouldReturn` base.contents

    it "applies every declared portable mode to destination and baseline" $
      hedgehog $ do
        declaredMode <-
          forAll $
            Gen.element
              [Private, Executable, PrivateExecutable, ReadOnly]
        observed <- liftIO $ withThreeInputs $ \source base destination -> do
          committed <-
            commitMergeResultGuarded
              (const $ return ())
              declaredMode
              source
              base
              destination
              "merged"
          destinationMode <- FileSystem.getPortableMode destination.path
          baseMode <- FileSystem.getPortableMode base.path
          return (committed, destinationMode, baseMode)
        let Just expectedBits = posixFileModeBits declaredMode
            expectedMode = portableModeFromBits expectedBits
            (committed, destinationMode, baseMode) = observed
        committed === Right ()
        satisfiesPortableMode destinationMode expectedMode === True
        satisfiesPortableMode baseMode expectedMode === True

  describe "commitMergeRecoveryGuarded" $ do
    it "repairs only the baseline for arbitrary accepted results" $ hedgehog $ do
      result <- forAll utf8Text
      observed <- liftIO $ withThreeInputs $ \source base destination -> do
        FileSystem.writeFile source.path result
        FileSystem.writeFile destination.path result
        Right refreshedSource <-
          observeMergeTextInput SourceInput source.path
        Right refreshedDestination <-
          observeMergeTextInput DestinationInput destination.path
        orderRef <- newIORef []
        committed <-
          commitMergeRecoveryGuarded
            (\replica -> modifyIORef' orderRef (<> [replica]))
            DefaultMode
            refreshedSource
            base
            refreshedDestination
        order <- readIORef orderRef
        replicas <-
          mapM
            FileSystem.readFile
            [source.path, destination.path, base.path]
        return (committed, order, replicas)
      observed
        === ( Right ()
            , [IntermediateCommitReplica]
            , replicate 3 result
            )

    it "rejects divergent authoritative inputs without mutation" $
      withThreeInputs $ \source base destination -> do
        committed <-
          commitMergeRecoveryGuarded
            (const $ return ())
            DefaultMode
            source
            base
            destination
        committed `shouldBe` Left MergeRecoveryInputsDiffer
        FileSystem.readFile source.path `shouldReturn` source.contents
        FileSystem.readFile destination.path
          `shouldReturn` destination.contents
        FileSystem.readFile base.path `shouldReturn` base.contents

    it "restores every declared mode on destination and baseline" $ hedgehog $ do
      declaredMode <-
        forAll $
          Gen.element
            [Private, Executable, PrivateExecutable, ReadOnly]
      observed <- liftIO $ withThreeInputs $ \source base destination -> do
        let result = "merged"
        FileSystem.writeFile source.path result
        FileSystem.writeFile destination.path result
        Right refreshedSource <-
          observeMergeTextInput SourceInput source.path
        Right refreshedDestination <-
          observeMergeTextInput DestinationInput destination.path
        committed <-
          commitMergeRecoveryGuarded
            (const $ return ())
            declaredMode
            refreshedSource
            base
            refreshedDestination
        destinationMode <- FileSystem.getPortableMode destination.path
        baseMode <- FileSystem.getPortableMode base.path
        return (committed, destinationMode, baseMode)
      let Just expectedBits = posixFileModeBits declaredMode
          expectedMode = portableModeFromBits expectedBits
          (committed, destinationMode, baseMode) = observed
      committed === Right ()
      satisfiesPortableMode destinationMode expectedMode === True
      satisfiesPortableMode baseMode expectedMode === True

    symlinkIt "does not restore a mode through a replacement link" $
      withThreeInputs $ \source base destination -> do
        let result = "merged"
        FileSystem.writeFile source.path result
        FileSystem.writeFile destination.path result
        Right refreshedSource <-
          observeMergeTextInput SourceInput source.path
        Right refreshedDestination <-
          observeMergeTextInput DestinationInput destination.path
        externalName <- encodeFS "recovery-external"
        let external = takeDirectory source.path </> externalName
        FileSystem.writeFile external "external"
        FileSystem.setPortableMode external 0o600
        committed <-
          runRacingCommitIO
            (ReplaceBeforeMode destination.path external)
            ( commitMergeRecoveryGuarded
                (const $ return ())
                ReadOnly
                refreshedSource
                base
                refreshedDestination
            )
        committed `shouldBe` Right (Right ())
        FileSystem.isSymlink destination.path `shouldReturn` False
        FileSystem.readFile external `shouldReturn` "external"
        externalMode <- FileSystem.getPortableMode external
        externalMode.posixBits `shouldSatisfy` maybe True (== 0o600)


utf8Text :: Gen ByteString.ByteString
utf8Text =
  Text.encodeUtf8
    <$> Gen.text
      (Range.linear 0 512)
      (Gen.filter (/= '\NUL') Gen.unicode)


withMergeFile :: ByteString.ByteString -> (OsPath -> IO a) -> IO a
withMergeFile contents action =
  withTempDir $ \root _ -> do
    name <- encodeFS "input"
    let path = root </> name
    FileSystem.writeFile path contents
    action path


withThreeInputs
  :: (MergeTextInput -> MergeTextInput -> MergeTextInput -> IO a)
  -> IO a
withThreeInputs action =
  withTempDir $ \root _ -> do
    sourceName <- encodeFS "source"
    baseName <- encodeFS "base"
    destinationName <- encodeFS "destination"
    let sourcePath = root </> sourceName
        basePath = root </> baseName
        destinationPath = root </> destinationName
    FileSystem.writeFile sourcePath "source"
    FileSystem.writeFile basePath "base"
    FileSystem.writeFile destinationPath "destination"
    Right source <- observeMergeTextInput SourceInput sourcePath
    Right base <- observeMergeTextInput BaseInput basePath
    Right destination <-
      observeMergeTextInput DestinationInput destinationPath
    action source base destination


assertPrivateFile :: OsPath -> IO ()
assertPrivateFile path = do
  mode <- FileSystem.getPortableMode path
  mode.writable `shouldBe` True
  mode.posixBits `shouldSatisfy` maybe True (== 0o600)


newtype FailingWorkspaceIO a
  = FailingWorkspaceIO
      (ReaderT WorkspaceSetupFailure (ExceptT IOError IO) a)
  deriving (Functor, Applicative, Monad, MonadError IOError)


data WorkspaceSetupFailure
  = FailPrivateFileMode
  | ReplaceWorkspaceBeforeWrite
      OsPath
      OsPath
      OsPath
      (IORef Bool)


runFailingWorkspaceIO
  :: FailingWorkspaceIO a
  -> IO (Either IOError a)
runFailingWorkspaceIO (FailingWorkspaceIO action) =
  runExceptT $ runReaderT action FailPrivateFileMode


runReplacingWorkspaceIO
  :: OsPath
  -> OsPath
  -> OsPath
  -> IORef Bool
  -> FailingWorkspaceIO a
  -> IO (Either IOError a)
runReplacingWorkspaceIO
  workspace
  parked
  sentinel
  replaced
  (FailingWorkspaceIO action) =
    runExceptT $
      runReaderT
        action
        (ReplaceWorkspaceBeforeWrite workspace parked sentinel replaced)


data CommitRace
  = ChangeWhileStaging OsPath ByteString.ByteString
  | ReplaceStagedBeforePublication ByteString.ByteString
  | ReplaceBeforeMode OsPath OsPath


newtype RacingCommitIO a
  = RacingCommitIO (ReaderT CommitRace (ExceptT IOError IO) a)
  deriving (Functor, Applicative, Monad, MonadError IOError)


runRacingCommitIO
  :: CommitRace
  -> RacingCommitIO a
  -> IO (Either IOError a)
runRacingCommitIO race (RacingCommitIO action) =
  runExceptT $ runReaderT action race


instance FileSystem.MonadFileSystem RacingCommitIO where
  encodePath value =
    RacingCommitIO $ liftIO (FileSystem.encodePath value :: IO OsPath)
  decodePath value =
    RacingCommitIO $ liftIO (FileSystem.decodePath value :: IO FilePath)
  getCurrentDirectory =
    RacingCommitIO $
      liftIO (FileSystem.getCurrentDirectory :: IO OsPath)
  getHomeDirectory =
    RacingCommitIO $ liftIO (FileSystem.getHomeDirectory :: IO OsPath)
  exists value =
    RacingCommitIO $ liftIO (FileSystem.exists value :: IO Bool)
  isFile value =
    RacingCommitIO $ liftIO (FileSystem.isFile value :: IO Bool)
  isRegularFile value =
    RacingCommitIO $ liftIO (FileSystem.isRegularFile value :: IO Bool)
  isDirectory value =
    RacingCommitIO $ liftIO (FileSystem.isDirectory value :: IO Bool)
  isSymlink value =
    RacingCommitIO $ liftIO (FileSystem.isSymlink value :: IO Bool)
  readFile value =
    RacingCommitIO $ liftIO (FileSystem.readFile value)
  readRegularFile value =
    RacingCommitIO $ liftIO (FileSystem.readRegularFile value)
  writeFile path contents =
    RacingCommitIO $ liftIO (FileSystem.writeFile path contents :: IO ())
  replaceFile source destination =
    RacingCommitIO $
      liftIO (FileSystem.replaceFile source destination :: IO ())
  writeTemporaryFile directory template contents = do
    temporary <-
      RacingCommitIO $
        liftIO $
          ( FileSystem.writeTemporaryFile directory template contents
              :: IO OsPath
          )
    race <- RacingCommitIO ask
    case race of
      ChangeWhileStaging path concurrent ->
        RacingCommitIO $
          liftIO (FileSystem.writeFile path concurrent :: IO ())
      ReplaceStagedBeforePublication _ -> return ()
      ReplaceBeforeMode _ _ -> return ()
    return temporary
  withFileLock _ action = action
  canonicalizePath value =
    RacingCommitIO $
      liftIO (FileSystem.canonicalizePath value :: IO OsPath)
  readSymlinkTarget value =
    RacingCommitIO $
      liftIO (FileSystem.readSymlinkTarget value :: IO OsPath)
  copyFile source destination =
    RacingCommitIO $
      liftIO (FileSystem.copyFile source destination :: IO ())
  copyFileWithMetadata source destination =
    RacingCommitIO $
      liftIO (FileSystem.copyFileWithMetadata source destination :: IO ())
  copyFilePermissions source destination =
    RacingCommitIO $
      liftIO (FileSystem.copyFilePermissions source destination :: IO ())
  createDirectory value =
    RacingCommitIO $
      liftIO (FileSystem.createDirectory value :: IO ())
  removeFile value =
    RacingCommitIO $ liftIO (FileSystem.removeFile value :: IO ())
  removeDirectory value =
    RacingCommitIO $
      liftIO (FileSystem.removeDirectory value :: IO ())
  removeDirectoryRecursivelyIfIdentity value identity =
    RacingCommitIO $
      liftIO $
        ( FileSystem.removeDirectoryRecursivelyIfIdentity value identity
            :: IO Bool
        )
  listDirectory value =
    RacingCommitIO $
      liftIO (FileSystem.listDirectory value :: IO [OsPath])
  getFileSize value =
    RacingCommitIO $ liftIO (FileSystem.getFileSize value :: IO Integer)
  getFileIdentity value =
    RacingCommitIO $
      liftIO (FileSystem.getFileIdentity value :: IO (Maybe FileSystem.FileIdentity))
  getFileSnapshot value =
    RacingCommitIO $
      liftIO (FileSystem.getFileSnapshot value :: IO (Maybe FileSystem.FileSnapshot))
  getFileModeSnapshot value =
    RacingCommitIO $
      liftIO $
        ( FileSystem.getFileModeSnapshot value
            :: IO (Maybe FileSystem.FileModeSnapshot)
        )
  getPortableMode value =
    RacingCommitIO $ liftIO (FileSystem.getPortableMode value)
  setPortableMode path bits = do
    race <- RacingCommitIO ask
    case race of
      ReplaceStagedBeforePublication replacement ->
        RacingCommitIO $ liftIO $ do
          FileSystem.setPortableMode path bits
          FileSystem.removeFile path
          FileSystem.writeFile path replacement
          FileSystem.setPortableMode path bits
      ReplaceBeforeMode watched external
        | path == watched ->
            RacingCommitIO $ liftIO $ do
              FileSystem.removeFile watched
              FileSystem.createSymbolicLink
                external
                watched
                FileSystem.File
              FileSystem.setPortableMode watched bits
      _ ->
        RacingCommitIO $
          liftIO (FileSystem.setPortableMode path bits :: IO ())
  setPortableWritable path writable =
    do
      race <- RacingCommitIO ask
      case race of
        ReplaceStagedBeforePublication replacement ->
          RacingCommitIO $ liftIO $ do
            FileSystem.setPortableWritable path writable
            FileSystem.removeFile path
            FileSystem.writeFile path replacement
            FileSystem.setPortableWritable path writable
        _ ->
          RacingCommitIO $
            liftIO (FileSystem.setPortableWritable path writable :: IO ())
  createSymbolicLink target link fileType =
    RacingCommitIO $
      liftIO $
        (FileSystem.createSymbolicLink target link fileType :: IO ())


instance FileSystem.MonadFileSystem FailingWorkspaceIO where
  encodePath value =
    FailingWorkspaceIO $ liftIO (FileSystem.encodePath value :: IO OsPath)
  decodePath value =
    FailingWorkspaceIO $ liftIO (FileSystem.decodePath value :: IO FilePath)
  getCurrentDirectory =
    FailingWorkspaceIO $
      liftIO (FileSystem.getCurrentDirectory :: IO OsPath)
  getHomeDirectory =
    FailingWorkspaceIO $ liftIO (FileSystem.getHomeDirectory :: IO OsPath)
  exists value =
    FailingWorkspaceIO $ liftIO (FileSystem.exists value :: IO Bool)
  isFile value =
    FailingWorkspaceIO $ liftIO (FileSystem.isFile value :: IO Bool)
  isRegularFile value =
    FailingWorkspaceIO $ liftIO (FileSystem.isRegularFile value :: IO Bool)
  isDirectory value =
    FailingWorkspaceIO $ liftIO (FileSystem.isDirectory value :: IO Bool)
  isSymlink value =
    FailingWorkspaceIO $ liftIO (FileSystem.isSymlink value :: IO Bool)
  readFile value =
    FailingWorkspaceIO $ liftIO (FileSystem.readFile value)
  writeFile path contents =
    FailingWorkspaceIO $ liftIO (FileSystem.writeFile path contents :: IO ())
  replaceFile source destination =
    FailingWorkspaceIO $
      liftIO (FileSystem.replaceFile source destination :: IO ())
  writeTemporaryFile directory template contents =
    FailingWorkspaceIO $
      liftIO $
        ( FileSystem.writeTemporaryFile directory template contents
            :: IO OsPath
        )
  withFileLock _ action = action
  canonicalizePath value =
    FailingWorkspaceIO $
      liftIO (FileSystem.canonicalizePath value :: IO OsPath)
  readSymlinkTarget value =
    FailingWorkspaceIO $
      liftIO (FileSystem.readSymlinkTarget value :: IO OsPath)
  copyFile source destination =
    FailingWorkspaceIO $
      liftIO (FileSystem.copyFile source destination :: IO ())
  copyFileWithMetadata source destination =
    FailingWorkspaceIO $
      liftIO (FileSystem.copyFileWithMetadata source destination :: IO ())
  copyFilePermissions source destination =
    FailingWorkspaceIO $
      liftIO (FileSystem.copyFilePermissions source destination :: IO ())
  createDirectory value =
    FailingWorkspaceIO $
      liftIO (FileSystem.createDirectory value :: IO ())
  createPrivateFileInDirectoryIfIdentity
    pathIdentity
    identity
    entryName
    contents = do
      failure <- FailingWorkspaceIO ask
      case failure of
        FailPrivateFileMode ->
          throwError $ userError "injected private-file mode failure"
        ReplaceWorkspaceBeforeWrite workspace parked sentinel replaced -> do
          alreadyReplaced <-
            FailingWorkspaceIO $ liftIO $ readIORef replaced
          if alreadyReplaced
            then return ()
            else FailingWorkspaceIO $ liftIO $ do
              writeIORef replaced True
              FileSystem.renameDirectory workspace parked
              FileSystem.createPrivateDirectory workspace
              FileSystem.writeFile sentinel "replacement"
          FailingWorkspaceIO $
            liftIO $
              ( FileSystem.createPrivateFileInDirectoryIfIdentity
                  pathIdentity
                  identity
                  entryName
                  contents
                  :: IO Bool
              )
  removeFile value =
    FailingWorkspaceIO $ liftIO (FileSystem.removeFile value :: IO ())
  removeDirectory value =
    FailingWorkspaceIO $
      liftIO (FileSystem.removeDirectory value :: IO ())
  removeDirectoryRecursivelyIfIdentity value identity =
    FailingWorkspaceIO $
      liftIO $
        ( FileSystem.removeDirectoryRecursivelyIfIdentity value identity
            :: IO Bool
        )
  listDirectory value =
    FailingWorkspaceIO $
      liftIO (FileSystem.listDirectory value :: IO [OsPath])
  getFileSize value =
    FailingWorkspaceIO $ liftIO (FileSystem.getFileSize value :: IO Integer)
  getFileIdentity value =
    FailingWorkspaceIO $
      liftIO (FileSystem.getFileIdentity value :: IO (Maybe FileSystem.FileIdentity))
  getPortableMode value =
    FailingWorkspaceIO $ liftIO (FileSystem.getPortableMode value)
  setPortableMode path bits =
    FailingWorkspaceIO $
      liftIO (FileSystem.setPortableMode path bits :: IO ())
  setPortableWritable path writable =
    FailingWorkspaceIO $
      liftIO (FileSystem.setPortableWritable path writable :: IO ())
  createSymbolicLink target link fileType =
    FailingWorkspaceIO $
      liftIO $
        (FileSystem.createSymbolicLink target link fileType :: IO ())
