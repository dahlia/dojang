{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.BootstrapSpec (spec) where

import Control.Exception (bracket)
import Control.Exception qualified as Exception
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (chr)
import Data.List (isInfixOf, isPrefixOf)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Hedgehog (evalIO, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory.OsPath qualified
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
import System.Timeout (timeout)
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
  , manifestUninitialized
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
                (Text.unpack source)
                destination
        request.executable === "copy"
        request.arguments === ["--", Text.unpack source, destination]
        request.environment
          === Just [("PATH", "host"), ("TOKEN", "fixed")]

    it "redacts arbitrary credential-bearing sources from diagnostics" $
      hedgehog $ do
        credential <- forAll $ Gen.string (Range.linear 1 100) Gen.alphaNum
        let source =
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
                source
                destination
            rendered = show $ redactTransportSource source request
        (source `isInfixOf` rendered) === False
        ("<redacted>" `isInfixOf` rendered) === True

    nativeTransportInputSpec

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

    it "rejects a manifest symlink that changes target after publication" $
      if os == "mingw32"
        then pendingWith "Creating symbolic links requires Windows privileges."
        else withTempDir $ \tmp _ -> do
          repositoriesName <- encodeFS "repositories"
          sourceName <- encodeFS "source"
          destinationName <- encodeFS "destination"
          stateName <- encodeFS "state"
          homeName <- encodeFS "home"
          manifestName <- encodeFS "dojang.toml"
          envName <- encodeFS "dojang-env.toml"
          escapedManifestName <- encodeFS "escaped-manifest.toml"
          relativeTarget <- encodeFS "../../escaped-manifest.toml"
          let repositories = tmp </> repositoriesName
              source = repositories </> sourceName
              destination = repositories </> destinationName
              stagedTarget = repositories </> escapedManifestName
              publishedTarget = tmp </> escapedManifestName
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
          createDirectory repositories
          createDirectory source
          createDirectory home
          writeFile stagedTarget validManifest
          System.Directory.OsPath.createFileLink
            relativeTarget
            (source </> manifestName)
          sourceText <- decodeFS source
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
          exists destination `shouldReturn` False
          exists publishedTarget `shouldReturn` False
          readFile stagedTarget `shouldReturn` validManifest

    it "rejects an embedded parent traversal in the manifest path" $
      withTempDir $ \tmp _ -> do
        repositoriesName <- encodeFS "repositories"
        sourceName <- encodeFS "source"
        childName <- encodeFS "sub"
        destinationName <- encodeFS "destination"
        stateName <- encodeFS "state"
        homeName <- encodeFS "home"
        envName <- encodeFS "dojang-env.toml"
        escapedManifestName <- encodeFS "escaped-manifest.toml"
        escapingManifest <- encodeFS "sub/../../../escaped-manifest.toml"
        let repositories = tmp </> repositoriesName
            source = repositories </> sourceName
            destination = repositories </> destinationName
            stagedTarget = repositories </> escapedManifestName
            publishedTarget = tmp </> escapedManifestName
            stateRoot = tmp </> stateName
            home = tmp </> homeName
            appEnv =
              AppEnv
                destination
                True
                Nothing
                stateRoot
                escapingManifest
                envName
                False
                False
        createDirectory repositories
        createDirectory source
        createDirectory (source </> childName)
        createDirectory home
        writeFile stagedTarget validManifest
        sourceText <- decodeFS source
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
        exists destination `shouldReturn` False
        exists publishedTarget `shouldReturn` False
        readFile stagedTarget `shouldReturn` validManifest

    it "rejects an absolute bootstrap manifest path" $
      withBootstrapFixture $ \source sourceText destination _ home appEnv -> do
        manifestName <- encodeFS "dojang.toml"
        withHome
          home
          ( runAppWithoutLogging
              appEnv{manifestFile = source </> manifestName}
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
        exists destination `shouldReturn` False

    it "rejects a drive-relative bootstrap manifest path" $
      if os /= "mingw32"
        then pendingWith "Drive-relative paths exist only on Windows."
        else withBootstrapFixture $ \_ sourceText destination _ home appEnv -> do
          driveRelative <- encodeFS "C:dojang.toml"
          withHome
            home
            ( runAppWithoutLogging
                appEnv{manifestFile = driveRelative}
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
          exists destination `shouldReturn` False

    it "rejects a symbolic-link ancestor in the manifest path" $
      if os == "mingw32"
        then pendingWith "Creating symbolic links requires Windows privileges."
        else withTempDir $ \tmp _ -> do
          repositoriesName <- encodeFS "repositories"
          sourceName <- encodeFS "source"
          destinationName <- encodeFS "destination"
          stateName <- encodeFS "state"
          homeName <- encodeFS "home"
          manifestName <- encodeFS "dojang.toml"
          envName <- encodeFS "dojang-env.toml"
          configName <- encodeFS "config"
          linkedConfigName <- encodeFS "linked-config"
          relativeTarget <- encodeFS "../../linked-config"
          configuredManifest <- encodeFS "config/dojang.toml"
          let repositories = tmp </> repositoriesName
              source = repositories </> sourceName
              destination = repositories </> destinationName
              stagedConfig = repositories </> linkedConfigName
              publishedConfig = tmp </> linkedConfigName
              stateRoot = tmp </> stateName
              home = tmp </> homeName
              appEnv =
                AppEnv
                  destination
                  True
                  Nothing
                  stateRoot
                  configuredManifest
                  envName
                  False
                  False
          createDirectory repositories
          createDirectory source
          createDirectory stagedConfig
          createDirectory home
          writeFile (stagedConfig </> manifestName) validManifest
          System.Directory.OsPath.createDirectoryLink
            relativeTarget
            (source </> configName)
          sourceText <- decodeFS source
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
          exists destination `shouldReturn` False
          exists publishedConfig `shouldReturn` False
          readFile (stagedConfig </> manifestName)
            `shouldReturn` validManifest

    it "rejects arbitrary manifest parent components before publication" $
      hedgehog $ do
        depth <- forAll $ Gen.int $ Range.linear 1 8
        filename <-
          forAll $
            Gen.string
              (Range.linear 1 40)
              (Gen.element $ ['a' .. 'z'] <> ['0' .. '9'])
        outcomes <-
          evalIO $
            withBootstrapFixture $
              \_ sourceText destination _ home appEnv -> do
                manifests <-
                  traverse
                    encodeFS
                    [ concat (replicate depth "../")
                        <> filename
                        <> ".toml"
                    , "sub/"
                        <> concat (replicate depth "../")
                        <> filename
                        <> ".toml"
                    ]
                traverse
                  ( \manifest -> do
                      result <-
                        Exception.try
                          $ withHome home
                          $ runAppWithoutLogging
                            appEnv{manifestFile = manifest}
                          $ bootstrap
                            sourceText
                            Nothing
                            Nothing
                            []
                            True
                            True
                            Nothing
                            []
                      present <- exists destination
                      return (result, present)
                  )
                  manifests
        outcomes === replicate 2 (Left cliError, False)

    nativeSourcePathSpec

    it "rejects an existing empty destination before acquisition" $
      withBootstrapFixture $ \_ sourceText destination stateRoot home appEnv -> do
        createDirectory destination
        (standardError, result) <-
          captureStderr $
            Exception.try $
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
        result `shouldBe` Left cliError
        standardError
          `shouldSatisfy` ByteString.isPrefixOf "Error:"
        standardError
          `shouldSatisfy` ByteString.isInfixOf "--repository-dir"
        listDirectory destination `shouldReturn` []
        exists stateRoot `shouldReturn` False

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

    it "rejects an existing empty destination during dry-run" $
      withBootstrapFixture $ \_ sourceText destination stateRoot home appEnv -> do
        createDirectory destination
        withHome
          home
          ( dryRunIO $
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
          )
          `shouldThrow` (== cliError)
        listDirectory destination `shouldReturn` []
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

    it "rejects arbitrary destinations nested inside a directory source" $
      hedgehog $ do
        suffix <-
          forAll $
            Gen.string
              (Range.linear 1 40)
              (Gen.element $ ['a' .. 'z'] <> ['0' .. '9'])
        (rejected, destinationExists, sourceEntries) <-
          evalIO $
            withBootstrapFixture $
              \source sourceText _ _ home appEnv -> do
                destinationName <- encodeFS $ "clone-" <> suffix
                let destination = source </> destinationName
                result <-
                  Exception.try
                    $ withHome home
                    $ runAppWithoutLogging
                      appEnv{sourceDirectory = destination}
                    $ bootstrap
                      sourceText
                      Nothing
                      Nothing
                      []
                      True
                      True
                      Nothing
                      []
                present <- exists destination
                entries <- traverse decodeFS =<< listDirectory source
                return (result, present, entries)
        rejected === Left cliError
        destinationExists === False
        sourceEntries === ["dojang.toml"]

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

    it "reports a missing manifest as an uninitialized repository" $
      withBootstrapFixture $ \source sourceText destination _ home appEnv -> do
        manifestName <- encodeFS "dojang.toml"
        removeFile $ source </> manifestName
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
          `shouldThrow` (== manifestUninitialized)
        exists destination `shouldReturn` False

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
        (output, ()) <-
          captureStderr $
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
        output
          `shouldSatisfy` (not . ByteString.isInfixOf "may require manual removal")
        readFile (destination </> manifestName)
          `shouldReturn` legacyManifest

    externalTransportSpec
    specialTransportManifestSpec
    destinationParentReplacementSpec
    symbolicLinkParentSpec
    symbolicLinkParentReplacementSpec
    cleanupWarningSpec


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
        setPortableMode tmp 0o755
        writeFile (source </> manifestName) validManifest
        writeFile script $
          "#!/bin/sh\n"
            <> "test -n \"$(find \"$(dirname -- \"$2\")\""
            <> " -prune -perm 0700 -print)\""
            <> " || exit 24\n"
            <> "/bin/touch -- \"$3\"\n"
            <> "exec /bin/cp -R -- \"$1\" \"$2\"\n"
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
specialTransportManifestSpec :: Spec
specialTransportManifestSpec = return ()


destinationParentReplacementSpec :: Spec
destinationParentReplacementSpec = return ()


symbolicLinkParentSpec :: Spec
symbolicLinkParentSpec = return ()


symbolicLinkParentReplacementSpec :: Spec
symbolicLinkParentReplacementSpec = return ()
#else
specialTransportManifestSpec :: Spec
specialTransportManifestSpec =
  it "rejects a special-file manifest created by a transport" $
    withTempDir $ \tmp _ -> do
      sourceName <- encodeFS "source"
      destinationName <- encodeFS "destination"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      scriptName <- encodeFS "fifo-transport.sh"
      configName <- encodeFS "fifo-transport.toml"
      let source = tmp </> sourceName
          destination = tmp </> destinationName
          stateRoot = tmp </> stateName
          home = tmp </> homeName
          script = tmp </> scriptName
          config = tmp </> configName
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
      writeFile script $
        "#!/bin/sh\n"
          <> "mkdir -p -- \"$2\"\n"
          <> "mkfifo -- \"$2/dojang.toml\"\n"
      setPortableMode script 0o755
      scriptPath <- decodeFS script
      writeFile config $
        encodeUtf8 $
          Text.pack $
            "[transports.fifo]\ncommand = ["
              <> show scriptPath
              <> ", \"{source}\", \"{destination}\"]\n"
      sourceText <- decodeFS source
      result <-
        timeout 30000000 $
          Exception.try $
            withHome home $
              runAppWithoutLogging appEnv $
                bootstrap
                  sourceText
                  (Just "fifo")
                  (Just config)
                  []
                  True
                  True
                  Nothing
                  []
      result `shouldBe` Just (Left cliError)
      exists destination `shouldReturn` False


destinationParentReplacementSpec :: Spec
destinationParentReplacementSpec =
  it "rejects a destination parent replaced during transport" $
    withTempDir $ \tmp _ -> do
      repositoriesName <- encodeFS "repositories"
      sourceName <- encodeFS "source"
      destinationName <- encodeFS "destination"
      redirectedName <- encodeFS "redirected"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      scriptName <- encodeFS "replace-parent.sh"
      configName <- encodeFS "replace-parent.toml"
      let repositories = tmp </> repositoriesName
          source = tmp </> sourceName
          destination = repositories </> destinationName
          redirected = tmp </> redirectedName
          redirectedDestination = redirected </> destinationName
          stateRoot = tmp </> stateName
          home = tmp </> homeName
          script = tmp </> scriptName
          config = tmp </> configName
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
      createDirectory repositories
      createDirectory source
      createDirectory home
      writeFile (source </> manifestName) validManifest
      writeFile script $
        "#!/bin/sh\n"
          <> "parent=$(dirname -- \"$(dirname -- \"$2\")\")\n"
          <> "mv -- \"$parent\" \"$parent-moved\"\n"
          <> "mkdir -p -- \"$3\"\n"
          <> "ln -s -- \"$3\" \"$parent\"\n"
          <> "mkdir -p -- \"$2\"\n"
          <> "cp -- \"$1/dojang.toml\" \"$2/dojang.toml\"\n"
      setPortableMode script 0o755
      scriptPath <- decodeFS script
      redirectedPath <- decodeFS redirected
      writeFile config $
        encodeUtf8 $
          Text.pack $
            "[transports.replace]\ncommand = ["
              <> show scriptPath
              <> ", \"{source}\", \"{destination}\", "
              <> show redirectedPath
              <> "]\n"
      sourceText <- decodeFS source
      (output, result) <-
        captureStderr $
          Exception.try $
          withHome home $
            runAppWithoutLogging appEnv $
              bootstrap
                sourceText
                (Just "replace")
                (Just config)
                []
                True
                True
                Nothing
                []
      result `shouldBe` Left cliError
      exists redirectedDestination `shouldReturn` False
      output
        `shouldSatisfy`
          ByteString.isInfixOf
            "The original staging tree may require manual removal"


symbolicLinkParentSpec :: Spec
symbolicLinkParentSpec =
  it "accepts a destination beneath a stable symbolic-link parent" $
    withTempDir $ \tmp _ -> do
      actualName <- encodeFS "actual"
      linkName <- encodeFS "linked"
      sourceName <- encodeFS "source"
      destinationName <- encodeFS "destination"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let actual = tmp </> actualName
          linked = tmp </> linkName
          source = tmp </> sourceName
          destination = linked </> destinationName
          published = actual </> destinationName
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
      createDirectory actual
      createDirectory source
      createDirectory home
      writeFile (source </> manifestName) validManifest
      System.Directory.OsPath.createDirectoryLink actual linked
      sourceText <- decodeFS source
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
      readFile (published </> manifestName)
        `shouldReturn` validManifest


symbolicLinkParentReplacementSpec :: Spec
symbolicLinkParentReplacementSpec =
  it "rejects a destination parent symlink repointed during transport" $
    withTempDir $ \tmp _ -> do
      actualName <- encodeFS "actual"
      evilName <- encodeFS "evil"
      linkName <- encodeFS "linked"
      sourceName <- encodeFS "source"
      destinationName <- encodeFS "destination"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      scriptName <- encodeFS "repoint-parent.sh"
      configName <- encodeFS "repoint-parent.toml"
      let actual = tmp </> actualName
          evil = tmp </> evilName
          linked = tmp </> linkName
          source = tmp </> sourceName
          destination = linked </> destinationName
          redirectedDestination = evil </> destinationName
          stateRoot = tmp </> stateName
          home = tmp </> homeName
          script = tmp </> scriptName
          config = tmp </> configName
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
      createDirectory actual
      createDirectory evil
      createDirectory source
      createDirectory home
      writeFile (source </> manifestName) validManifest
      System.Directory.OsPath.createDirectoryLink actual linked
      writeFile script $
        "#!/bin/sh\n"
          <> "parent=$(dirname -- \"$(dirname -- \"$2\")\")\n"
          <> "ln -sfn -- \"$3\" \"$parent\"\n"
          <> "mkdir -p -- \"$2\"\n"
          <> "cp -- \"$1/dojang.toml\" \"$2/dojang.toml\"\n"
      setPortableMode script 0o755
      scriptPath <- decodeFS script
      evilPath <- decodeFS evil
      writeFile config $
        encodeUtf8 $
          Text.pack $
            "[transports.repoint]\ncommand = ["
              <> show scriptPath
              <> ", \"{source}\", \"{destination}\", "
              <> show evilPath
              <> "]\n"
      sourceText <- decodeFS source
      result <-
        Exception.try $
          withHome home $
            runAppWithoutLogging appEnv $
              bootstrap
                sourceText
                (Just "repoint")
                (Just config)
                []
                True
                True
                Nothing
                []
      result `shouldBe` Left cliError
      exists redirectedDestination `shouldReturn` False
#endif

#ifdef mingw32_HOST_OS
cleanupWarningSpec :: Spec
cleanupWarningSpec = return ()
#else
cleanupWarningSpec :: Spec
cleanupWarningSpec =
  it "warns when a failed transport staging tree cannot be cleaned" $
    withTempDir $ \tmp _ -> do
      sourceName <- encodeFS "source"
      destinationName <- encodeFS "destination"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      scriptName <- encodeFS "cleanup-failure.sh"
      configName <- encodeFS "cleanup-failure.toml"
      let source = tmp </> sourceName
          destination = tmp </> destinationName
          stateRoot = tmp </> stateName
          home = tmp </> homeName
          script = tmp </> scriptName
          config = tmp </> configName
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
      writeFile script $
        "#!/bin/sh\n"
          <> "chmod 0500 \"$(dirname -- \"$(dirname -- \"$2\")\")\"\n"
          <> "exit 23\n"
      setPortableMode script 0o755
      scriptPath <- decodeFS script
      writeFile config $
        encodeUtf8 $
          Text.pack $
            "[transports.copy]\ncommand = ["
              <> show scriptPath
              <> ", \"{source}\", \"{destination}\"]\n"
      sourceText <- decodeFS source
      (output, ()) <-
        captureStderr $
          ( withHome
              home
              ( runAppWithoutLogging appEnv $
                  bootstrap
                    sourceText
                    (Just "copy")
                    (Just config)
                    []
                    True
                    True
                    Nothing
                    []
              )
              `shouldThrow` (== externalProgramNonZeroExit)
          )
            `Exception.finally` setPortableMode tmp 0o700
      output
        `shouldSatisfy`
          ByteString.isInfixOf
            "Could not clean the bootstrap staging directory"
#endif

#ifdef mingw32_HOST_OS
nativeTransportInputSpec :: Spec
nativeTransportInputSpec = return ()


nativeSourcePathSpec :: Spec
nativeSourcePathSpec = return ()
#else
nativeTransportInputSpec :: Spec
nativeTransportInputSpec =
  it "preserves arbitrary native bytes in transport inputs" $
    hedgehog $ do
      byte <- forAll $ Gen.word8 Range.constantBounded
      let escaped = [chr $ 0xdc00 + fromIntegral byte]
          source = "source-" <> escaped
          destination = "destination-" <> escaped
          inherited = "inherited-" <> escaped
          Right config =
            readTransportConfig
              ( "[transports.copy]\n"
                  <> "command = [\"copy\", \"{source}\", "
                  <> "\"{destination}\"]\n"
                  <> "inherit-environment = [\"PATH\"]\n"
              )
          Right transport = lookupTransport "copy" config
          request =
            makeTransportProcessRequest
              "linux"
              [("PATH", inherited)]
              transport
              source
              destination
      request.arguments === [source, destination]
      request.environment === Just [("PATH", inherited)]


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
