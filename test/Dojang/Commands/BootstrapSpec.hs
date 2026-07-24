{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.BootstrapSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (isInfixOf, isPrefixOf)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Hedgehog (forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (addTrailingPathSeparator)
import System.IO
  ( SeekMode (AbsoluteSeek)
  , hClose
  , hFlush
  , hSeek
  , stderr
  )
import System.IO.Temp (withSystemTempFile)
import System.Info (os)
import System.OsPath (OsPath, decodeFS, encodeFS, takeDirectory, (</>))
import Test.Hspec
  ( Spec
  , describe
  , it
  , pendingWith
  , sequential
  , shouldBe
  , shouldReturn
  , shouldSatisfy
  , shouldThrow
  )
import Test.Hspec.Hedgehog (hedgehog, (===))
import Prelude hiding (readFile, writeFile)

import Dojang.App
  ( AppEnv (..)
  , runAppWithoutLogging
  )
import Dojang.CommandEffect (ProcessRequest (..))
import Dojang.Commands.Bootstrap
  ( bootstrap
  , initialize
  , makeTransportProcessRequest
  , normalizeBootstrapDestination
  , redactTransportSource
  )
import Dojang.Commands.Init (InitPreset (Amd64Linux))
import Dojang.ExitCodes
  ( cliError
  , externalProgramNonZeroExit
  , machineStateError
  , manifestReadError
  )
import Dojang.MonadFileSystem
  ( MonadFileSystem (..)
  , dryRunIO
  )
import Dojang.Syntax.Transport (readTransportConfig)
import Dojang.TestUtils (withHome, withTempDir)
import Dojang.Types.MachineState
  ( MachineState (firstApplied)
  , listRepositoryStates
  , readMachineId
  )
import Dojang.Types.Transport (lookupTransport)


spec :: Spec
spec = sequential $ do
  describe "makeTransportProcessRequest" $ do
    it "preserves arbitrary source and destination arguments without a shell" $
      hedgehog $ do
        source <- forAll $ Gen.text (Range.linear 0 200) Gen.unicodeAll
        destination <- forAll $ Gen.string (Range.linear 0 200) Gen.unicode
        let Right config =
              readTransportConfig
                ( "[transports.copy]\n"
                    <> "command = [\"copy\", \"--\", \"{source}\", "
                    <> "\"{destination}\"]\n"
                    <> "inherit-environment = [\"PATH\"]\n"
                    <> "[transports.copy.environment]\n"
                    <> "TOKEN = \"fixed\"\n"
                )
            Right transport = lookupTransport "copy" config
            request =
              makeTransportProcessRequest
                "linux"
                [("PATH", "host"), ("SECRET", "excluded")]
                transport
                source
                destination
        request.executable === "copy"
        request.arguments === ["--", Text.unpack source, destination]
        request.environment
          === Just [("PATH", "host"), ("TOKEN", "fixed")]

    it "redacts arbitrary credential-bearing sources from diagnostics" $
      hedgehog $ do
        credential <- forAll $ Gen.string (Range.linear 1 100) Gen.alphaNum
        let sourceText =
              Text.pack $
                "https://token:" <> credential <> "@example.com/repository.git"
            destination = "/staging/repository"
            Right config =
              readTransportConfig
                ( "[transports.copy]\n"
                    <> "command = [\"copy\", \"--\", \"{source}\", "
                    <> "\"{destination}\"]\n"
                )
            Right transport = lookupTransport "copy" config
            request =
              makeTransportProcessRequest
                "linux"
                []
                transport
                sourceText
                destination
            rendered = show $ redactTransportSource sourceText request
        (Text.unpack sourceText `isInfixOf` rendered) === False
        ("<redacted>" `isInfixOf` rendered) === True

    it "normalizes a current-directory path beside its parent" $
      withTempDir $ \tmp _ -> do
        destinationName <- encodeFS "destination"
        period <- encodeFS "."
        let destination = tmp </> destinationName
        normalizeBootstrapDestination (destination </> period)
          `shouldBe` destination

  describe "initialize" $ do
    it "rejects bootstrap-only options without --from" $
      withBootstrapFixture $ \_ _ destination _ home appEnv -> do
        let cases =
              [ initialize Nothing (Just "copy") Nothing [] True False Nothing []
              , initialize Nothing Nothing (Just destination) [] True False Nothing []
              , initialize Nothing Nothing Nothing [] True True Nothing []
              ]
        forM_ cases $ \command ->
          withHome home (runAppWithoutLogging appEnv command)
            `shouldThrow` (== cliError)
        exists destination `shouldReturn` False

    it "rejects incompatible options with --from" $
      withBootstrapFixture $ \_ sourceText destination _ home appEnv -> do
        let cases =
              [ initialize
                  (Just sourceText)
                  Nothing
                  Nothing
                  [Amd64Linux]
                  True
                  True
                  Nothing
                  []
              , initialize
                  (Just sourceText)
                  Nothing
                  (Just destination)
                  []
                  True
                  True
                  Nothing
                  []
              ]
        forM_ cases $ \command ->
          withHome home (runAppWithoutLogging appEnv command)
            `shouldThrow` (== cliError)
        exists destination `shouldReturn` False

  describe "bootstrap" $ do
    it "acquires, enrolls, and applies a local repository" $
      withBootstrapFixture $ \source sourceText destination stateRoot home appEnv -> do
        result <-
          withHome home $
            runAppWithoutLogging appEnv $
              bootstrap
                sourceText
                Nothing
                Nothing
                []
                True
                True
                Nothing
                []
        result `shouldBe` ExitSuccess
        manifestName <- encodeFS "dojang.toml"
        readFile (destination </> manifestName)
          `shouldReturn` validManifest
        readFile (source </> manifestName)
          `shouldReturn` validManifest
        machineResult <- readMachineId stateRoot
        machineId <- case machineResult of
          Right (Just value) -> return value
          unexpected -> fail $ "Unexpected machine id: " <> show unexpected
        statesResult <- listRepositoryStates stateRoot machineId
        states <- case statesResult of
          Right value -> return value
          Left err -> fail $ "Unexpected state error: " <> show err
        fmap (.firstApplied) states `shouldBe` [True]

    nativeSourcePathSpec

    it "publishes into an existing empty destination" $
      withBootstrapFixture $ \_ sourceText destination _ home appEnv -> do
        manifestName <- encodeFS "dojang.toml"
        createDirectory destination
        result <-
          withHome home $
            runAppWithoutLogging appEnv $
              bootstrap
                sourceText
                Nothing
                Nothing
                []
                True
                True
                Nothing
                []
        result `shouldBe` ExitSuccess
        readFile (destination </> manifestName)
          `shouldReturn` validManifest

    it "normalizes a destination with a trailing separator" $
      withBootstrapFixture $ \_ sourceText destination _ home appEnv -> do
        destinationPath <- decodeFS destination
        configuredDestination <-
          encodeFS $ addTrailingPathSeparator destinationPath
        manifestName <- encodeFS "dojang.toml"
        result <-
          withHome home
            $ runAppWithoutLogging
              appEnv{sourceDirectory = configuredDestination}
            $ bootstrap
              sourceText
              Nothing
              Nothing
              []
              True
              True
              Nothing
              []
        result `shouldBe` ExitSuccess
        readFile (destination </> manifestName)
          `shouldReturn` validManifest

    it "simulates local acquisition without touching destination or state" $
      withBootstrapFixture $ \_ sourceText destination stateRoot home appEnv -> do
        result <-
          withHome home $
            dryRunIO $
              runAppWithoutLogging appEnv{dryRun = True} $
                bootstrap
                  sourceText
                  Nothing
                  Nothing
                  []
                  True
                  True
                  Nothing
                  []
        result `shouldBe` ExitSuccess
        exists destination `shouldReturn` False
        exists stateRoot `shouldReturn` False

    it "requires --yes before a non-interactive bootstrap mutates anything" $
      withBootstrapFixture $ \_ sourceText destination _ home appEnv -> do
        withHome
          home
          ( runAppWithoutLogging appEnv $
              bootstrap
                sourceText
                Nothing
                Nothing
                []
                True
                False
                Nothing
                []
          )
          `shouldThrow` (== cliError)
        exists destination `shouldReturn` False

    it "rejects a nonempty destination before creating staging" $
      withBootstrapFixture $ \_ sourceText destination _ home appEnv -> do
        childName <- encodeFS "keep"
        createDirectory destination
        writeFile (destination </> childName) "untouched"
        withHome
          home
          ( runAppWithoutLogging appEnv $
              bootstrap
                sourceText
                Nothing
                Nothing
                []
                True
                True
                Nothing
                []
          )
          `shouldThrow` (== cliError)
        readFile (destination </> childName) `shouldReturn` "untouched"

    it "cleans staging when repository validation fails" $
      withBootstrapFixture $ \source sourceText destination _ home appEnv -> do
        manifestName <- encodeFS "dojang.toml"
        writeFile (source </> manifestName) "invalid = ["
        withHome
          home
          ( runAppWithoutLogging appEnv $
              bootstrap
                sourceText
                Nothing
                Nothing
                []
                True
                True
                Nothing
                []
          )
          `shouldThrow` (== manifestReadError)
        exists destination `shouldReturn` False
        parentEntries <- traverse decodeFS =<< listDirectory (takeDirectory destination)
        parentEntries
          `shouldSatisfy` all (not . isPrefixOf ".dojang-bootstrap-")

    it "reports a missing destination parent as a command-line error" $
      withBootstrapFixture $ \_ sourceText destination _ home appEnv -> do
        missingName <- encodeFS "missing"
        nestedName <- encodeFS "repository"
        let nestedDestination =
              takeDirectory destination </> missingName </> nestedName
        withHome
          home
          ( runAppWithoutLogging
              appEnv{sourceDirectory = nestedDestination}
              $ bootstrap
                sourceText
                Nothing
                Nothing
                []
                True
                True
                Nothing
                []
          )
          `shouldThrow` (== cliError)
        exists nestedDestination `shouldReturn` False

    it "keeps an acquired legacy repository when enrollment fails" $
      withBootstrapFixture $ \source sourceText destination _ home appEnv -> do
        manifestName <- encodeFS "dojang.toml"
        writeFile (source </> manifestName) legacyManifest
        withHome
          home
          ( runAppWithoutLogging appEnv $
              bootstrap
                sourceText
                Nothing
                Nothing
                []
                True
                True
                Nothing
                []
          )
          `shouldThrow` (== machineStateError)
        readFile (destination </> manifestName)
          `shouldReturn` legacyManifest

    externalTransportSpec


withBootstrapFixture
  :: (OsPath -> FilePath -> OsPath -> OsPath -> OsPath -> AppEnv -> IO a)
  -> IO a
withBootstrapFixture action =
  withTempDir $ \tmp _ -> do
    sourceName <- encodeFS "source repository"
    destinationName <- encodeFS "destination repository"
    stateName <- encodeFS "state"
    homeName <- encodeFS "home"
    manifestName <- encodeFS "dojang.toml"
    envName <- encodeFS "dojang-env.toml"
    let source = tmp </> sourceName
        destination = tmp </> destinationName
        stateRoot = tmp </> stateName
        home = tmp </> homeName
        appEnv =
          AppEnv
            destination
            True
            Nothing
            stateRoot
            manifestName
            envName
            False
            False
    createDirectory source
    createDirectory home
    writeFile (source </> manifestName) validManifest
    sourceText <- decodeFS source
    action source sourceText destination stateRoot home appEnv


validManifest :: ByteString
validManifest =
  "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
    <> "[dirs]\n"
    <> "[files]\n"
    <> "[ignores]\n"
    <> "[monikers]\n"


legacyManifest :: ByteString
legacyManifest =
  "[dirs]\n"
    <> "[files]\n"
    <> "[ignores]\n"
    <> "[monikers]\n"


externalTransportSpec :: Spec
externalTransportSpec =
  it "runs a shell-free configured external transport" $
    if os == "mingw32"
      then pendingWith "The fixture uses a POSIX shell script."
      else withTempDir $ \tmp _ -> do
        sourceName <- encodeFS "source-token-secret;touch-pwned"
        destinationName <- encodeFS "destination"
        stateName <- encodeFS "state"
        homeName <- encodeFS "home"
        manifestName <- encodeFS "dojang.toml"
        envName <- encodeFS "dojang-env.toml"
        scriptName <- encodeFS "copy transport.sh"
        failingScriptName <- encodeFS "failing transport.sh"
        configName <- encodeFS "transports.toml"
        failingConfigName <- encodeFS "failing-transports.toml"
        pwnedName <- encodeFS "pwned"
        markerName <- encodeFS "transport-ran"
        let source = tmp </> sourceName
            destination = tmp </> destinationName
            stateRoot = tmp </> stateName
            home = tmp </> homeName
            script = tmp </> scriptName
            failingScript = tmp </> failingScriptName
            config = tmp </> configName
            failingConfig = tmp </> failingConfigName
            marker = tmp </> markerName
            appEnv =
              AppEnv
                destination
                True
                Nothing
                stateRoot
                manifestName
                envName
                False
                False
        createDirectory source
        createDirectory home
        writeFile (source </> manifestName) validManifest
        writeFile script $
          "#!/bin/sh\n/bin/touch -- \"$3\"\nexec /bin/cp -R -- \"$1\" \"$2\"\n"
        setPortableMode script 0o755
        scriptPath <- decodeFS script
        markerPath <- decodeFS marker
        writeFile config $
          encodeUtf8 $
            Text.pack $
              "[transports.copy]\ncommand = ["
                <> show scriptPath
                <> ", \"{source}\", \"{destination}\", "
                <> show markerPath
                <> "]\n"
        sourceText <- decodeFS source
        (dryRunOutput, dryRunResult) <-
          captureStderr $
            withHome home $
              dryRunIO $
                runAppWithoutLogging appEnv{dryRun = True} $
                  bootstrap
                    sourceText
                    (Just "copy")
                    (Just config)
                    []
                    True
                    True
                    Nothing
                    []
        dryRunResult `shouldBe` ExitSuccess
        dryRunOutput
          `shouldSatisfy` (not . ByteString.isInfixOf "token-secret")
        dryRunOutput
          `shouldSatisfy` ByteString.isInfixOf "<redacted>"
        exists marker `shouldReturn` False
        exists destination `shouldReturn` False
        exists stateRoot `shouldReturn` False
        result <-
          withHome home $
            runAppWithoutLogging appEnv $
              bootstrap
                sourceText
                (Just "copy")
                (Just config)
                []
                True
                True
                Nothing
                []
        result `shouldBe` ExitSuccess
        readFile (destination </> manifestName)
          `shouldReturn` validManifest
        exists marker `shouldReturn` True

        exists (tmp </> pwnedName) `shouldReturn` False

        failureDestinationName <- encodeFS "failure destination"
        failureStateName <- encodeFS "failure state"
        let failureDestination = tmp </> failureDestinationName
            failureStateRoot = tmp </> failureStateName
            failureEnv =
              appEnv
                { sourceDirectory = failureDestination
                , stateDirectory = failureStateRoot
                }
        writeFile failingScript $
          "#!/bin/sh\n/bin/touch -- \"$2\"\nexit 23\n"
        setPortableMode failingScript 0o755
        failingScriptPath <- decodeFS failingScript
        writeFile failingConfig $
          encodeUtf8 $
            Text.pack $
              "[transports.copy]\ncommand = ["
                <> show failingScriptPath
                <> ", \"{source}\", \"{destination}\"]\n"
        withHome
          home
          ( runAppWithoutLogging failureEnv $
              bootstrap
                sourceText
                (Just "copy")
                (Just failingConfig)
                []
                True
                True
                Nothing
                []
          )
          `shouldThrow` (== externalProgramNonZeroExit)
        exists failureDestination `shouldReturn` False
        exists failureStateRoot `shouldReturn` False
        entries <- traverse decodeFS =<< listDirectory tmp
        entries `shouldSatisfy` all (not . isPrefixOf ".dojang-bootstrap-")

#ifdef mingw32_HOST_OS
nativeSourcePathSpec :: Spec
nativeSourcePathSpec = return ()
#else
nativeSourcePathSpec :: Spec
nativeSourcePathSpec =
  it "preserves non-UTF-8 bytes in a local source path" $
    withBootstrapFixture $ \source _ destination _ home appEnv -> do
      sourceName <- encodeFS $ "source-" <> [toEnum 0xdc80]
      let nativeSource = takeDirectory source </> sourceName
      renameDirectory source nativeSource
      sourceArgument <- decodeFS nativeSource
      result <-
        withHome home $
          runAppWithoutLogging appEnv $
            bootstrap
              sourceArgument
              Nothing
              Nothing
              []
              True
              True
              Nothing
              []
      result `shouldBe` ExitSuccess
      manifestName <- encodeFS "dojang.toml"
      readFile (destination </> manifestName)
        `shouldReturn` validManifest
#endif


captureStderr :: IO a -> IO (ByteString, a)
captureStderr action =
  withSystemTempFile "dojang-bootstrap-spec-stderr" $ \_ captureHandle ->
    bracket (hDuplicate stderr) restore $ \_ -> do
      hDuplicateTo captureHandle stderr
      result <- action
      hFlush stderr
      hSeek captureHandle AbsoluteSeek 0
      captured <- ByteString.hGetContents captureHandle
      return (captured, result)
 where
  restore original = do
    hDuplicateTo original stderr
    hClose original
