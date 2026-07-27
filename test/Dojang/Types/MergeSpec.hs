{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.MergeSpec (spec) where

import Control.Monad.Except (catchError, throwError, tryError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.Encoding qualified as Text
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsPath (OsPath, encodeFS, (</>))
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
        nulName <- encodeFS "nul"
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

  describe "readMergeResult" $ do
    it "accepts arbitrary UTF-8 text from a regular result file" $ hedgehog $ do
      contents <- forAll utf8Text
      observed <- liftIO $ withMergeFile contents readMergeResult
      observed === Right contents

    it "rejects missing, non-regular, NUL-containing, and invalid UTF-8 results" $
      withTempDir $ \root _ -> do
        missingName <- encodeFS "missing"
        directoryName <- encodeFS "directory"
        nulName <- encodeFS "nul"
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
