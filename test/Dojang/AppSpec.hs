{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.AppSpec (spec) where

import Control.Exception qualified


#ifndef mingw32_HOST_OS
import Control.Exception (bracket_)
#endif

import System.Directory.OsPath qualified
import System.Info (os)
import System.OsPath (encodeFS, (</>))
import Test.Hspec (Spec, it, runIO, sequential, xit)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldThrow)
import Prelude hiding (writeFile)

import Dojang.App
  ( AppEnv (..)
  , applyAutomaticRepositorySelection
  , automaticSelectionUsesCheckoutManifest
  , prepareMachineState
  , runAppWithoutLogging
  , validateRepositoryCheckout
  )
import Dojang.ExitCodes (machineStateError)
import Dojang.MonadFileSystem
  ( MonadFileSystem (createDirectories, writeFile)
  )
import Dojang.TestUtils (withHome, withTempDir)
import Dojang.Types.MachineState (MachineState (..), StateError (..))
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
  let redirectedHomeSymlinkIt =
        if symlinkAvailable then redirectedHomeIt else xit

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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
      let manifest = Manifest (Just repositoryId) mempty mempty mempty mempty
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
