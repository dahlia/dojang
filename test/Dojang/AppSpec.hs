{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.AppSpec (spec) where

import Control.Exception qualified
import Control.Monad.Logger (LogLevel (LevelWarn), fromLogStr)
import Data.ByteString.Char8 qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map


#ifndef mingw32_HOST_OS
import Control.Exception (bracket_)
#endif

import Options.Applicative.Path (hyphen)
import System.Directory.OsPath qualified
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.Info (os)
import System.OsPath (encodeFS, (</>))
import Test.Hspec (Spec, anyIOException, it, runIO, sequential, xit)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldReturn, shouldThrow)
import Prelude hiding (readFile, writeFile)

import Dojang.App
  ( App
  , AppEnv (..)
  , applyAutomaticRepositorySelection
  , automaticSelectionUsesCheckoutManifest
  , currentEnvironment'
  , liftApp
  , prepareMachineState
  , runAppResultWithoutLogging
  , runAppWithLogging
  , runAppWithoutLogging
  , startAndAwaitAppProcess
  , validateRepositoryCheckout
  )
import Dojang.CommandEffect
  ( CommandEffect (..)
  , CommandEffectKind (ProcessExecution)
  , CommandEffectResponse (..)
  , CommandEffectTest
  , MonadCommandEffect (abortCommand, runProcess)
  , MonadProcessControl (startProcess)
  , OutputStream (OutputStandard)
  , ProcessRequest (executable)
  , ProcessResult (ProcessUnavailable)
  , emptyProcessRequest
  , lookupEnvironmentVariable
  , runCommandEffectTest
  , startedProcess
  )
import Dojang.Commands.Env qualified as Env
import Dojang.ExitCodes (machineStateError)
import Dojang.MonadFileSystem
  ( MonadFileSystem
      ( createDirectories
      , exists
      , readFile
      , removeDirectory
      , writeFile
      )
  , dryRunIO
  )
import Dojang.Syntax.Env (writeEnvironment)
import Dojang.TestUtils (withHome, withTempDir)
import Dojang.Types.Environment (Kernel (Kernel), emptyEnvironment, lookupFact)
import Dojang.Types.MachineState
  ( MachineState (..)
  , StateError (..)
  , repositoryStatePath
  , updateMachineFacts
  )
import Dojang.Types.Manifest (Manifest (Manifest))
import Dojang.Types.Registry
  ( Registry (Registry)
  , registryFilename
  , writeRegistry
  )
import Dojang.Types.RepositoryId (parseRepositoryId)


spec :: Spec
spec = sequential $ do
  symlinkAvailable <- runIO $ withTempDir $ \tmp _ -> do
    targetName <- encodeFS "target"
    aliasName <- encodeFS "alias"
    let target = tmp </> targetName
    createDirectories target
    ( System.Directory.OsPath.createDirectoryLink target (tmp </> aliasName)
        >> return True
      )
      `catchIO` const (return False)
  let redirectedHomeIt = if os == "mingw32" then xit else it
  let posixIt = if os == "mingw32" then xit else it
  let redirectedHomeSymlinkIt =
        if symlinkAvailable then redirectedHomeIt else xit

  it "returns command exits as values" $ do
    path <- encodeFS "."
    let appEnv = AppEnv path True Nothing path path path False False
    runAppResultWithoutLogging
      appEnv
      (abortCommand machineStateError :: App IO ())
      `shouldReturn` Left machineStateError

  it "refuses process execution through a dry-run App" $ do
    path <- encodeFS "."
    let appEnv = AppEnv path True Nothing path path path True False
        request = emptyProcessRequest{executable = "does-not-exist"}
    dryRunIO (runAppWithoutLogging appEnv $ runProcess request)
      `shouldReturn` ProcessUnavailable ProcessExecution
    started <- dryRunIO $ runAppWithoutLogging appEnv $ startProcess request
    case started of
      Left result -> result `shouldBe` ProcessUnavailable ProcessExecution
      Right _ -> fail "dry-run App unexpectedly started a process"

  it "cancels a process interrupted during its registered handoff" $ do
    path <- encodeFS "."
    startMask <- newIORef Control.Exception.Unmasked
    cancelled <- newIORef False
    let appEnv = AppEnv path True Nothing path path path False False
        start register = do
          liftApp (Control.Exception.getMaskingState >>= writeIORef startMask)
          let process =
                startedProcess
                  (return $ ProcessUnavailable ProcessExecution)
                  (liftApp $ writeIORef cancelled True)
          _ <- register process
          liftApp $ Control.Exception.throwIO $ userError "interrupted"
    runAppWithoutLogging appEnv (startAndAwaitAppProcess start)
      `shouldThrow` anyIOException
    readIORef startMask `shouldReturn` Control.Exception.MaskedInterruptible
    readIORef cancelled `shouldReturn` True

  it "cancels a process when its registered handoff aborts" $ do
    path <- encodeFS "."
    cancelled <- newIORef False
    let appEnv = AppEnv path True Nothing path path path False False
        start register = do
          let process =
                startedProcess
                  (abortCommand machineStateError)
                  (liftApp $ writeIORef cancelled True)
          _ <- register process
          return $ Right process
    runAppResultWithoutLogging appEnv (startAndAwaitAppProcess start)
      `shouldReturn` Left machineStateError
    readIORef cancelled `shouldReturn` True

  it "restores masking while waiting and cancels an interrupted process" $ do
    path <- encodeFS "."
    waitMask <- newIORef Control.Exception.MaskedUninterruptible
    cancelled <- newIORef False
    let appEnv = AppEnv path True Nothing path path path False False
        start register = do
          let process =
                startedProcess
                  ( do
                      liftApp $
                        Control.Exception.getMaskingState >>= writeIORef waitMask
                      liftApp $ Control.Exception.throwIO $ userError "interrupted"
                  )
                  (liftApp $ writeIORef cancelled True)
          _ <- register process
          return $ Right process
    runAppWithoutLogging appEnv (startAndAwaitAppProcess start)
      `shouldThrow` anyIOException
    readIORef waitMask `shouldReturn` Control.Exception.Unmasked
    readIORef cancelled `shouldReturn` True

  it "routes App effects through its scripted interpreter" $ do
    path <- encodeFS "."
    let appEnv = AppEnv path True Nothing path path path False False
        action = lookupEnvironmentVariable "EDITOR" :: App CommandEffectTest (Maybe String)
    result <-
      runCommandEffectTest [EnvironmentValue $ Just "scripted-editor"] $
        runAppResultWithoutLogging appEnv action
    result
      `shouldBe` Right
        ( Right $ Just "scripted-editor"
        , [EnvironmentLookup "EDITOR"]
        )

  it "runs a command entirely through the scripted interpreter" $ do
    path <- encodeFS "."
    let appEnv = AppEnv path True Nothing path path path False False
        environment = emptyEnvironment "linux" "x86_64" $ Kernel "Linux" "6.0"
    result <-
      runCommandEffectTest [HostEnvironmentValue environment] $
        runAppResultWithoutLogging appEnv $
          Env.env True hyphen
    result
      `shouldBe` Right
        ( Right ExitSuccess
        ,
          [ HostEnvironmentRead
          , StreamWrite OutputStandard $ writeEnvironment environment
          ]
        )

  posixIt "reads a complete environment file without detecting the host" $
    withTempDir $ \tmp tmpPath -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      createDirectories checkout
      writeFile
        (checkout </> envName)
        ( "os = \"linux\"\n"
            <> "arch = \"x86_64\"\n"
            <> "[kernel]\n"
            <> "name = \"Linux\"\n"
            <> "release = \"6.0\"\n"
        )
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              (tmp </> stateName)
              manifestName
              envName
              False
              False
      environment <-
        withPath tmpPath $ runAppWithoutLogging appEnv currentEnvironment'
      lookupFact "os" environment `shouldBe` Just "linux"

  it "combines a facts-only environment file with the detected host" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      createDirectories checkout
      writeFile
        (checkout </> envName)
        "[facts]\nclass = \"work\"\n"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              (tmp </> stateName)
              manifestName
              envName
              False
              False
      environment <- runAppWithoutLogging appEnv currentEnvironment'
      lookupFact "class" environment `shouldBe` Just "work"

  it "inspects a repository environment without creating machine state" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      writeFile
        (checkout </> manifestName)
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      writeFile
        (checkout </> envName)
        ( "os = \"linux\"\n"
            <> "arch = \"x86_64\"\n"
            <> "[kernel]\n"
            <> "name = \"Linux\"\n"
            <> "release = \"6.0\"\n"
            <> "[facts]\n"
            <> "class = \"work\"\n"
        )
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      environment <-
        withHome home $ runAppWithoutLogging appEnv currentEnvironment'
      lookupFact "class" environment `shouldBe` Just "work"
      exists stateRoot `shouldReturn` False

  it "inspects a legacy repository environment without machine state" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      writeFile (checkout </> manifestName) ""
      writeFile
        (checkout </> envName)
        ( "os = \"linux\"\n"
            <> "arch = \"x86_64\"\n"
            <> "[kernel]\n"
            <> "name = \"Linux\"\n"
            <> "release = \"6.0\"\n"
        )
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      environment <-
        withHome home $ runAppWithoutLogging appEnv currentEnvironment'
      lookupFact "os" environment `shouldBe` Just "linux"
      exists stateRoot `shouldReturn` False

  it "loads persisted facts without rewriting machine state" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      writeFile
        (checkout </> manifestName)
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      writeFile
        (checkout </> envName)
        ( "os = \"linux\"\n"
            <> "arch = \"x86_64\"\n"
            <> "[kernel]\n"
            <> "name = \"Linux\"\n"
            <> "release = \"6.0\"\n"
        )
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      state <-
        withHome home $ runAppWithoutLogging appEnv $ prepareMachineState manifest
      enrolled <-
        updateMachineFacts
          stateRoot
          state.updatedTime
          state
          Nothing
          (Map.singleton "class" "work")
      case enrolled of
        Left err -> fail $ "Unexpected state error: " <> show err
        Right _ -> return ()
      let statePath = repositoryStatePath stateRoot repositoryId
      before <- readFile statePath
      environment <-
        withHome home $ runAppWithoutLogging appEnv currentEnvironment'
      lookupFact "class" environment `shouldBe` Just "work"
      readFile statePath `shouldReturn` before

  it "lets repository facts override a facts-only default file" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      writeFile
        (checkout </> manifestName)
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      writeFile (checkout </> envName) "[facts]\nclass = \"work\"\n"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      state <-
        withHome home $ runAppWithoutLogging appEnv $ prepareMachineState manifest
      enrolled <-
        updateMachineFacts
          stateRoot
          state.updatedTime
          state
          (Just envName)
          (Map.singleton "class" "personal")
      case enrolled of
        Left err -> fail $ "Unexpected state error: " <> show err
        Right _ -> return ()
      environment <-
        withHome home $ runAppWithoutLogging appEnv currentEnvironment'
      lookupFact "class" environment `shouldBe` Just "personal"

  it "lets an explicit facts-only environment override repository facts" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      simulationName <- encodeFS "simulation.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      writeFile
        (checkout </> manifestName)
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      writeFile (checkout </> simulationName) "[facts]\nclass = \"work\"\n"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              simulationName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      state <-
        withHome home $ runAppWithoutLogging appEnv $ prepareMachineState manifest
      enrolled <-
        updateMachineFacts
          stateRoot
          state.updatedTime
          state
          Nothing
          (Map.singleton "class" "personal")
      case enrolled of
        Left err -> fail $ "Unexpected state error: " <> show err
        Right _ -> return ()
      environment <-
        withHome home $ runAppWithoutLogging appEnv currentEnvironment'
      lookupFact "class" environment `shouldBe` Just "work"

  it "loads associated facts once during detected-host fallback" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      factsName <- encodeFS "facts.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      writeFile
        (checkout </> manifestName)
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      writeFile (checkout </> factsName) "[fact]\nclass = \"work\"\n"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      state <-
        withHome home $ runAppWithoutLogging appEnv $ prepareMachineState manifest
      enrolled <-
        updateMachineFacts
          stateRoot
          state.updatedTime
          state
          (Just factsName)
          Map.empty
      case enrolled of
        Left err -> fail $ "Unexpected state error: " <> show err
        Right _ -> return ()
      warnings <- newIORef []
      _ <-
        withHome home $
          runAppWithLogging appEnv currentEnvironment' $
            \_ _ level message ->
              if level == LevelWarn
                then modifyIORef' warnings (fromLogStr message :)
                else return ()
      messages <- readIORef warnings
      length
        (filter (ByteString.isInfixOf "unexpected key") messages)
        `shouldBe` 1

  it "resolves a relative facts file from a moved checkout" $
    withTempDir $ \tmp _ -> do
      oldCheckoutName <- encodeFS "old-checkout"
      movedCheckoutName <- encodeFS "moved-checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      factsName <- encodeFS "facts.toml"
      let oldCheckout = tmp </> oldCheckoutName
      let movedCheckout = tmp </> movedCheckoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories oldCheckout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let oldAppEnv =
            AppEnv
              oldCheckout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      state <-
        withHome home $
          runAppWithoutLogging oldAppEnv $
            prepareMachineState manifest
      enrolled <-
        updateMachineFacts
          stateRoot
          state.updatedTime
          state
          (Just factsName)
          Map.empty
      case enrolled of
        Left err -> fail $ "Unexpected state error: " <> show err
        Right _ -> return ()
      removeDirectory oldCheckout
      createDirectories movedCheckout
      writeFile
        (movedCheckout </> manifestName)
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      writeFile
        (movedCheckout </> factsName)
        "[facts]\nclass = \"work\"\n"
      let movedAppEnv = oldAppEnv{sourceDirectory = movedCheckout}
      let statePath = repositoryStatePath stateRoot repositoryId
      before <- readFile statePath
      environment <-
        withHome home $ runAppWithoutLogging movedAppEnv currentEnvironment'
      lookupFact "class" environment `shouldBe` Just "work"
      readFile statePath `shouldReturn` before

  redirectedHomeSymlinkIt "preserves first-apply history across checkout aliases" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      aliasName <- encodeFS "checkout-alias"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let alias = tmp </> aliasName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      System.Directory.OsPath.createDirectoryLink checkout alias
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      writeRegistry (home </> registryFilename) $ Registry checkout
      let appEnv =
            AppEnv
              alias
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      state <-
        withHome home $ runAppWithoutLogging appEnv $ prepareMachineState manifest
      (state.firstApplied) `shouldBe` True

  redirectedHomeIt "rejects a malformed legacy registry before creating state" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      writeFile (home </> registryFilename) "invalid toml"
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      withHome home (runAppWithoutLogging appEnv $ prepareMachineState manifest)
        `shouldThrow` (== machineStateError)

  posixUnreadableRegistrySpec

  it "ignores a malformed legacy registry after state exists" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      first <-
        withHome home $ runAppWithoutLogging appEnv $ prepareMachineState manifest
      writeFile (home </> registryFilename) "invalid toml"
      second <-
        withHome home $ runAppWithoutLogging appEnv $ prepareMachineState manifest
      second.repositoryId `shouldBe` first.repositoryId
      second.intermediatePath `shouldBe` first.intermediatePath
      second.firstApplied `shouldBe` first.firstApplied

  it "rejects an auto-selected checkout reused by another repository" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      state <-
        withHome home $ runAppWithoutLogging appEnv $ prepareMachineState manifest
      writeFile
        (checkout </> manifestName)
        "repository-id = \"223e4567-e89b-42d3-a456-426614174000\"\n"
      result <- validateRepositoryCheckout state
      result `shouldBe` Left (StaleRepositoryCheckout repositoryId checkout)

  it "accepts an auto-selected checkout with the recorded identity" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      state <-
        withHome home $ runAppWithoutLogging appEnv $ prepareMachineState manifest
      writeFile
        (checkout </> manifestName)
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      result <- validateRepositoryCheckout state
      result `shouldBe` Right checkout

  it "preserves the recorded manifest during automatic selection" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      defaultManifestName <- encodeFS "dojang.toml"
      customManifestName <- encodeFS "custom.toml"
      explicitManifestName <- encodeFS "explicit.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let storedAppEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              customManifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      state <-
        withHome home $
          runAppWithoutLogging storedAppEnv $
            prepareMachineState manifest
      let parsedAppEnv =
            storedAppEnv
              { sourceDirectory = tmp
              , repositoryExplicit = False
              , manifestFile = defaultManifestName
              }
      let automaticallySelected =
            applyAutomaticRepositorySelection False state parsedAppEnv
      automaticallySelected.sourceDirectory `shouldBe` checkout
      automaticallySelected.manifestFile `shouldBe` state.manifestPath
      let explicitlySelected =
            applyAutomaticRepositorySelection
              True
              state
              parsedAppEnv{manifestFile = explicitManifestName}
      explicitlySelected.manifestFile `shouldBe` explicitManifestName

      automaticSelectionUsesCheckoutManifest state explicitlySelected
        >>= (`shouldBe` True)
      automaticSelectionUsesCheckoutManifest
        state
        explicitlySelected{manifestFile = tmp </> explicitManifestName}
        >>= (`shouldBe` False)

  it "rejects a missing auto-selected checkout" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      state <-
        withHome home $ runAppWithoutLogging appEnv $ prepareMachineState manifest
      System.Directory.OsPath.removeDirectory checkout
      result <- validateRepositoryCheckout state
      result `shouldBe` Left (StaleRepositoryCheckout repositoryId checkout)

  it "does not let an absolute external manifest validate a stale checkout" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      externalManifestName <- encodeFS "external-dojang.toml"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let externalManifest = tmp </> externalManifestName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      state <-
        withHome home $ runAppWithoutLogging appEnv $ prepareMachineState manifest
      writeFile
        externalManifest
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      result <- validateRepositoryCheckout state
      result `shouldBe` Left (StaleRepositoryCheckout repositoryId checkout)

  it "moves state when the old checkout declares another identity" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      movedName <- encodeFS "moved-checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let moved = tmp </> movedName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories moved
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      let appEnv source =
            AppEnv
              source
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      _ <-
        withHome home $
          runAppWithoutLogging (appEnv checkout) $
            prepareMachineState manifest
      writeFile
        (checkout </> manifestName)
        "repository-id = \"223e4567-e89b-42d3-a456-426614174000\"\n"
      state <-
        withHome home $
          runAppWithoutLogging (appEnv moved) $
            prepareMachineState manifest
      state.checkoutPath `shouldBe` moved

  it "rejects a moved checkout while the old path declares the same identity" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      movedName <- encodeFS "moved-checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let moved = tmp </> movedName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories moved
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      let appEnv source =
            AppEnv
              source
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      _ <-
        withHome home $
          runAppWithoutLogging (appEnv checkout) $
            prepareMachineState manifest
      writeFile
        (checkout </> manifestName)
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      withHome
        home
        ( runAppWithoutLogging (appEnv moved) $
            prepareMachineState manifest
        )
        `shouldThrow` (== machineStateError)

  it "uses the recorded custom manifest to detect a duplicate checkout" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      duplicateName <- encodeFS "duplicate-checkout"
      firstManifestName <- encodeFS "first.toml"
      secondManifestName <- encodeFS "second.toml"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let duplicate = tmp </> duplicateName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories duplicate
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      let manifestContents =
            "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      writeFile (checkout </> firstManifestName) manifestContents
      writeFile (duplicate </> secondManifestName) manifestContents
      let appEnv source manifestName =
            AppEnv
              source
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      firstState <-
        withHome home $
          runAppWithoutLogging (appEnv checkout firstManifestName) $
            prepareMachineState manifest
      firstState.manifestPath `shouldBe` checkout </> firstManifestName
      withHome
        home
        ( runAppWithoutLogging
            (appEnv duplicate $ duplicate </> secondManifestName)
            $ prepareMachineState manifest
        )
        `shouldThrow` (== machineStateError)

  it "uses a recorded external manifest to detect a duplicate checkout" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      duplicateName <- encodeFS "duplicate-checkout"
      externalManifestName <- encodeFS "external.toml"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let duplicate = tmp </> duplicateName
      let externalManifest = tmp </> externalManifestName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      createDirectories checkout
      createDirectories duplicate
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      writeFile
        externalManifest
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n"
      let appEnv source =
            AppEnv
              source
              True
              Nothing
              stateRoot
              externalManifest
              envName
              False
              False
      firstState <-
        withHome home $
          runAppWithoutLogging (appEnv checkout) $
            prepareMachineState manifest
      firstState.manifestPath `shouldBe` externalManifest
      withHome
        home
        ( runAppWithoutLogging (appEnv duplicate) $
            prepareMachineState manifest
        )
        `shouldThrow` (== machineStateError)

  it "reports repository state preparation I/O failures as state errors" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      blockerName <- encodeFS "blocker"
      snapshotName <- encodeFS "snapshot"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      let blocker = tmp </> blockerName
      createDirectories checkout
      createDirectories home
      writeFile blocker "not a directory"
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let appEnv =
            AppEnv
              checkout
              True
              (Just $ blocker </> snapshotName)
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      withHome home (runAppWithoutLogging appEnv $ prepareMachineState manifest)
        `shouldThrow` (== machineStateError)

#ifdef mingw32_HOST_OS
posixUnreadableRegistrySpec :: Spec
posixUnreadableRegistrySpec = pure ()
#else
posixUnreadableRegistrySpec :: Spec
posixUnreadableRegistrySpec =
  it "reports legacy registry I/O failures as machine-state errors" $
    withTempDir $ \tmp _ -> do
      checkoutName <- encodeFS "checkout"
      stateName <- encodeFS "state"
      homeName <- encodeFS "home"
      manifestName <- encodeFS "dojang.toml"
      envName <- encodeFS "dojang-env.toml"
      let checkout = tmp </> checkoutName
      let stateRoot = tmp </> stateName
      let home = tmp </> homeName
      let registryPath = home </> registryFilename
      createDirectories checkout
      createDirectories home
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      writeRegistry registryPath $ Registry checkout
      permissions <- System.Directory.OsPath.getPermissions registryPath
      let unreadable =
            System.Directory.OsPath.setOwnerReadable False permissions
      let appEnv =
            AppEnv
              checkout
              True
              Nothing
              stateRoot
              manifestName
              envName
              False
              False
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
      bracket_
        (System.Directory.OsPath.setPermissions registryPath unreadable)
        (System.Directory.OsPath.setPermissions registryPath permissions)
        ( withHome
            home
            (runAppWithoutLogging appEnv $ prepareMachineState manifest)
            `shouldThrow` (== machineStateError)
        )
#endif


catchIO :: IO a -> (IOError -> IO a) -> IO a
catchIO = Control.Exception.catch


withPath :: String -> IO a -> IO a
withPath path action =
  Control.Exception.bracket (lookupEnv "PATH") restore $ \_ -> do
    setEnv "PATH" path
    action
 where
  restore Nothing = unsetEnv "PATH"
  restore (Just previous) = setEnv "PATH" previous
