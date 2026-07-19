{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.ApplySpec (spec) where

import Control.Exception (bracket_)
import Control.Monad (when)
import Control.Monad.Except (catchError)
import Data.HashMap.Strict (singleton)
import Data.Map.Strict qualified as Map
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath (OsPath, encodeFS, (</>))
import Test.Hspec (Spec, describe, it, sequential)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldReturn, shouldThrow)
import Prelude hiding (readFile, writeFile)

import Dojang.App (AppEnv (..), runAppWithoutLogging)
import Dojang.Commands.Apply (apply)
import Dojang.ExitCodes (conflictError, manifestReadError)
import Dojang.MonadFileSystem
  ( FileType (Directory, File)
  , MonadFileSystem (..)
  , dryRunIO
  )
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression (FilePathExpression (Substitution))
import Dojang.Types.FileRoute
  ( RouteKind (CopyRoute, SymlinkRoute)
  , RouteMode (DefaultMode, Private)
  , RouteTarget (RouteTarget)
  , fileRoutePreservingOrder
  )
import Dojang.Types.MachineState
  ( MachineState (..)
  , readMachineId
  , readRepositoryState
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (..)
  , TargetFingerprint (SymlinkFingerprint)
  )
import Dojang.Types.Manifest (Manifest (..), manifest)
import Dojang.Types.Manifest qualified as Manifest
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.RepositoryId (parseRepositoryId)
import Dojang.Types.RouteMetadata
  ( PortableMode (..)
  , portableModeFromBits
  )


spec :: Spec
spec = sequential $ do
  describe "apply" $ do
    it "does not update destinations outside the selected paths" $
      withTwoManagedFiles $ \appEnv sourceA destinationA destinationB -> do
        (result, destinationAContents, destinationBContents, targets) <- dryRunIO $ do
          result <- runAppWithoutLogging appEnv (apply True [sourceA])
          destinationAContents <- readFile destinationA
          destinationBContents <- readFile destinationB
          Right (Just machineId) <- readMachineId appEnv.stateDirectory
          let Right repositoryId' =
                parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
          Right (Just state) <-
            readRepositoryState appEnv.stateDirectory repositoryId' machineId
          return
            ( result
            , destinationAContents
            , destinationBContents
            , Map.elems state.targetRecords
            )
        result `shouldBe` ExitSuccess
        destinationAContents `shouldBe` "source a"
        destinationBContents `shouldBe` "destination b"
        (.destinationPath) <$> targets `shouldBe` [destinationA]
        exists appEnv.stateDirectory `shouldReturn` False

    it "keeps tracked files managed when they match an ignore pattern" $
      withTrackedIgnoredFile $ \appEnv intermediate destination -> do
        (result, intermediateContents, destinationContents) <- dryRunIO $ do
          result <- runAppWithoutLogging appEnv (apply False [])
          intermediateContents <- readFile intermediate
          destinationContents <- readFile destination
          return (result, intermediateContents, destinationContents)
        result `shouldBe` ExitSuccess
        intermediateContents `shouldBe` "source contents"
        destinationContents `shouldBe` "source contents"

    it "reclaims superseded target-snapshot transaction roots" $
      withTwoManagedFiles $ \appEnv _ _ _ -> do
        runAppWithoutLogging appEnv (apply True []) `shouldReturn` ExitSuccess
        runAppWithoutLogging appEnv (apply True []) `shouldReturn` ExitSuccess
        Right (Just machineId) <- readMachineId appEnv.stateDirectory
        let Right repositoryId' =
              parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        Right (Just state) <-
          readRepositoryState appEnv.stateDirectory repositoryId' machineId
        transactions <- listDirectory state.targetSnapshotRoot
        length transactions `shouldBe` 1

    it "reclaims baselines when a route becomes a link" $
      withKindSwitchRoute $ \appEnv source manifestPath linkManifest -> do
        let Right repositoryId' =
              parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        (result, baselineExisted, result', fingerprints, baselineRemains) <-
          dryRunIO $ do
            result <- runAppWithoutLogging appEnv (apply True [])
            Right (Just machineId) <- readMachineId appEnv.stateDirectory
            Right (Just state) <-
              readRepositoryState appEnv.stateDirectory repositoryId' machineId
            let [copyRecord] = Map.elems state.targetRecords
            baselineExisted <- exists copyRecord.snapshotPath
            -- The same route switches from a copied kind to a deployment
            -- link; its materialized baseline must not leak:
            () <- writeManifestFile linkManifest manifestPath
            result' <- runAppWithoutLogging appEnv (apply True [])
            Right (Just state') <-
              readRepositoryState appEnv.stateDirectory repositoryId' machineId
            baselineRemains <- exists copyRecord.snapshotPath
            return
              ( result
              , baselineExisted
              , result'
              , (.fingerprint) <$> Map.elems state'.targetRecords
              , baselineRemains
              )
        result `shouldBe` ExitSuccess
        baselineExisted `shouldBe` True
        result' `shouldBe` ExitSuccess
        fingerprints `shouldBe` [SymlinkFingerprint source]
        baselineRemains `shouldBe` False

    it "deploys and repairs symbolic-link routes" $
      withSymlinkRoute $ \appEnv source destination -> do
        (result, isLink, target) <- dryRunIO $ do
          result <- runAppWithoutLogging appEnv (apply True [])
          isLink <- isSymlink destination
          target <- readSymlinkTarget destination
          return (result, isLink, target)
        result `shouldBe` ExitSuccess
        isLink `shouldBe` True
        target `shouldBe` source
        -- The deployed link is recorded in machine state with its stored
        -- target as the snapshot:
        records <- dryRunIO $ do
          _ <- runAppWithoutLogging appEnv (apply True [])
          Right (Just machineId) <- readMachineId appEnv.stateDirectory
          let Right repositoryId' =
                parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
          Right (Just state) <-
            readRepositoryState appEnv.stateDirectory repositoryId' machineId
          return $ Map.elems state.targetRecords
        [record.fingerprint | record <- records]
          `shouldBe` [SymlinkFingerprint source]
        -- A link with the wrong target is repaired:
        (result', target') <- dryRunIO $ do
          wrongName <- encodePath "wrong-target"
          () <- writeFile (appEnv.sourceDirectory </> wrongName) "wrong"
          () <-
            createSymbolicLink
              (appEnv.sourceDirectory </> wrongName)
              destination
              File
          result' <- runAppWithoutLogging appEnv (apply True [])
          target' <- readSymlinkTarget destination
          return (result', target')
        result' `shouldBe` ExitSuccess
        target' `shouldBe` source

    it "refuses to replace an entry with a link without force" $
      withSymlinkRoute $ \appEnv _ destination -> do
        dryRunIO
          ( do
              () <- writeFile destination "existing"
              runAppWithoutLogging appEnv (apply False [])
          )
          `shouldThrow` (== conflictError)
        -- With force, the entry is replaced by the link:
        (result', isLink) <- dryRunIO $ do
          () <- writeFile destination "existing"
          result' <- runAppWithoutLogging appEnv (apply True [])
          isLink <- isSymlink destination
          return (result', isLink)
        result' `shouldBe` ExitSuccess
        isLink `shouldBe` True

    it "agrees between dry-run and real mode application" $ do
      let observe appEnv destination = do
            result <- runAppWithoutLogging appEnv (apply True [])
            contents <- readFile destination
            mode <- getPortableMode destination
            return (result, contents, mode.writable)
      dryObserved <-
        withPrivateModeFile $ \appEnv destination ->
          dryRunIO $ observe appEnv destination
      realObserved <-
        withPrivateModeFile $ \appEnv destination ->
          observe appEnv destination
      dryObserved `shouldBe` realObserved
      dryObserved `shouldBe` (ExitSuccess, "private contents", True)

    it "agrees between dry-run and real link deployment" $ do
      symlinkAvailable <- withTempDir $ \tmpDir _ -> do
        probeTarget <- encodeFS "probe-target"
        probeLink <- encodeFS "probe-link"
        writeFile (tmpDir </> probeTarget) ""
        ( do
            createSymbolicLink
              (tmpDir </> probeTarget)
              (tmpDir </> probeLink)
              File
            return True
          )
          `catchError` const (return False)
      when symlinkAvailable $ do
        let observe appEnv source destination = do
              result <- runAppWithoutLogging appEnv (apply True [])
              isLink <- isSymlink destination
              target <- readSymlinkTarget destination
              return (result, isLink, target == source)
        dryObserved <-
          withSymlinkRoute $ \appEnv source destination ->
            dryRunIO $ observe appEnv source destination
        realObserved <-
          withSymlinkRoute $ \appEnv source destination ->
            observe appEnv source destination
        dryObserved `shouldBe` realObserved
        dryObserved `shouldBe` (ExitSuccess, True, True)

    it "applies a declared private mode to recreated destinations" $
      withPrivateModeFile $ \appEnv destination -> do
        (result, contents, observedMode) <- dryRunIO $ do
          result <- runAppWithoutLogging appEnv (apply True [])
          contents <- readFile destination
          observedMode <- getPortableMode destination
          return (result, contents, observedMode)
        result `shouldBe` ExitSuccess
        contents `shouldBe` "private contents"
        observedMode `shouldBe` portableModeFromBits 0o600

    it "applies a declared directory mode to the route root" $
      withPrivateModeDirectory $ \appEnv destinationDir destinationFile -> do
        (result, dirMode, fileMode) <- dryRunIO $ do
          result <- runAppWithoutLogging appEnv (apply True [])
          dirMode <- getPortableMode destinationDir
          fileMode <- getPortableMode destinationFile
          return (result, dirMode, fileMode)
        result `shouldBe` ExitSuccess
        dirMode `shouldBe` portableModeFromBits 0o700
        fileMode `shouldBe` portableModeFromBits 0o600

    it "reconciles metadata-only drift toward the declared mode" $
      withPrivateModeFile $ \appEnv destination -> do
        (result, observedMode) <- dryRunIO $ do
          -- Converge contents first so only the mode drifts:
          () <- writeFile destination "private contents"
          result <- runAppWithoutLogging appEnv (apply True [])
          observedMode <- getPortableMode destination
          return (result, observedMode)
        result `shouldBe` ExitSuccess
        observedMode `shouldBe` reconciledPrivateFileMode

    it "rejects traversing route names before mutating destinations" $
      withTempDir $ \tmpDir _ -> do
        repositoryName <- encodeFS "repository"
        manifestName <- encodeFS "dojang.toml"
        envName <- encodeFS "dojang-env.toml"
        stateName <- encodeFS "state"
        outsideName <- encodeFS "outside"
        destinationName <- encodeFS "destination"
        homeName <- encodeFS "home"
        let repository = tmpDir </> repositoryName
        let manifestPath = repository </> manifestName
        let outside = tmpDir </> outsideName
        let destination = tmpDir </> destinationName
        let home = tmpDir </> homeName
        let stateRoot = tmpDir </> stateName
        let appEnv =
              AppEnv
                repository
                False
                Nothing
                stateRoot
                manifestName
                envName
                False
                False
        createDirectories repository
        createDirectories home
        writeFile outside "outside source"
        writeFile destination "original destination"
        writeFile
          manifestPath
          "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n[dirs]\n[files]\n\"../outside\" = [{ when = \"always\", path = \"$DEST\" }]\n[ignores]\n[monikers]\n"
        withEnvVars
          [ ("DEST", Just destination)
          , ("HOME", Just home)
          , ("USERPROFILE", Just home)
          ]
          $ runAppWithoutLogging appEnv (apply True [])
            `shouldThrow` (== manifestReadError)
        readFile destination `shouldReturn` "original destination"
        exists stateRoot `shouldReturn` False


withTwoManagedFiles
  :: (AppEnv -> OsPath -> OsPath -> OsPath -> IO a)
  -> IO a
withTwoManagedFiles action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeA <- encodeFS "managed-a"
  routeB <- encodeFS "managed-b"
  destinationAName <- encodeFS "destination-a"
  destinationBName <- encodeFS "destination-b"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let intermediateA = repository </> intermediateDir </> routeA
  let intermediateB = repository </> intermediateDir </> routeB
  let sourceA = repository </> routeA
  let sourceB = repository </> routeB
  let destinationA = tmpDir </> destinationAName
  let destinationB = tmpDir </> destinationBName
  let home = tmpDir </> homeName
  let Right always = parseMonikerName "always"
  let Right repositoryId' =
        parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
  let manifest' =
        ( ( manifest
              (singleton always Always)
              ( Map.fromList
                  [ (routeA, [(always, Just $ Substitution "DEST_A")])
                  , (routeB, [(always, Just $ Substitution "DEST_B")])
                  ]
              )
              mempty
              mempty
              mempty
          )
            :: Manifest
        )
          { Manifest.repositoryId = Just repositoryId'
          }
  let appEnv =
        AppEnv
          repository
          False
          (Just intermediateDir)
          (tmpDir </> stateDir)
          manifestFilename
          envFilename
          False
          False

  createDirectories $ repository </> intermediateDir
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile sourceA "source a"
  writeFile intermediateA "base a"
  writeFile destinationA "base a"
  writeFile sourceB "base b"
  writeFile intermediateB "base b"
  writeFile destinationB "destination b"

  withEnvVars
    [ ("DEST_A", Just destinationA)
    , ("DEST_B", Just destinationB)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv sourceA destinationA destinationB


withPrivateModeFile
  :: (AppEnv -> OsPath -> IO a)
  -> IO a
withPrivateModeFile action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "private-file"
  destinationName <- encodeFS "destination"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let source = repository </> routeName
  let intermediate = repository </> intermediateDir </> routeName
  let destination = tmpDir </> destinationName
  let home = tmpDir </> homeName
  let Right repositoryId' =
        parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
  let route =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $
                RouteTarget (Substitution "DEST_PRIVATE") Private CopyRoute
            )
          ]
          File
  let manifest' =
        Manifest
          { Manifest.repositoryId = Just repositoryId'
          , monikers = mempty
          , variables = mempty
          , fileRoutes = Map.fromList [(routeName, route)]
          , ignorePatterns = mempty
          , hooks = mempty
          }
  let appEnv =
        AppEnv
          repository
          False
          (Just intermediateDir)
          (tmpDir </> stateDir)
          manifestFilename
          envFilename
          False
          False

  createDirectories $ repository </> intermediateDir
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile source "private contents"
  writeFile intermediate "base"
  writeFile destination "base"

  withEnvVars
    [ ("DEST_PRIVATE", Just destination)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv destination


withKindSwitchRoute
  :: (AppEnv -> OsPath -> OsPath -> Manifest -> IO a)
  -> IO a
withKindSwitchRoute action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "switched-file"
  destinationName <- encodeFS "destination"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let source = repository </> routeName
  let destination = tmpDir </> destinationName
  let home = tmpDir </> homeName
  let Right repositoryId' =
        parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
  let routeOf kind' =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $ RouteTarget (Substitution "DEST_FILE") DefaultMode kind'
            )
          ]
          File
  let manifestOf kind' =
        Manifest
          { Manifest.repositoryId = Just repositoryId'
          , monikers = mempty
          , variables = mempty
          , fileRoutes = Map.fromList [(routeName, routeOf kind')]
          , ignorePatterns = mempty
          , hooks = mempty
          }
  let appEnv =
        AppEnv
          repository
          False
          (Just intermediateDir)
          (tmpDir </> stateDir)
          manifestFilename
          envFilename
          False
          False

  createDirectories $ repository </> intermediateDir
  createDirectories home
  writeManifestFile (manifestOf CopyRoute) $ repository </> manifestFilename
  writeFile source "switched contents"

  withEnvVars
    [ ("DEST_FILE", Just destination)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action
      appEnv
      source
      (repository </> manifestFilename)
      (manifestOf SymlinkRoute)


withSymlinkRoute
  :: (AppEnv -> OsPath -> OsPath -> IO a)
  -> IO a
withSymlinkRoute action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "linked-file"
  destinationName <- encodeFS "destination"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let source = repository </> routeName
  let destination = tmpDir </> destinationName
  let home = tmpDir </> homeName
  let Right repositoryId' =
        parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
  let route =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $
                RouteTarget
                  (Substitution "DEST_LINK")
                  DefaultMode
                  SymlinkRoute
            )
          ]
          File
  let manifest' =
        Manifest
          { Manifest.repositoryId = Just repositoryId'
          , monikers = mempty
          , variables = mempty
          , fileRoutes = Map.fromList [(routeName, route)]
          , ignorePatterns = mempty
          , hooks = mempty
          }
  let appEnv =
        AppEnv
          repository
          False
          (Just intermediateDir)
          (tmpDir </> stateDir)
          manifestFilename
          envFilename
          False
          False

  createDirectories $ repository </> intermediateDir
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile source "linked contents"

  withEnvVars
    [ ("DEST_LINK", Just destination)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv source destination


withPrivateModeDirectory
  :: (AppEnv -> OsPath -> OsPath -> IO a)
  -> IO a
withPrivateModeDirectory action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "private-dir"
  fileName' <- encodeFS "inner-file"
  destinationName <- encodeFS "destination-dir"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let source = repository </> routeName
  let destination = tmpDir </> destinationName
  let home = tmpDir </> homeName
  let Right repositoryId' =
        parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
  let route =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $
                RouteTarget (Substitution "DEST_DIR") Private CopyRoute
            )
          ]
          Directory
  let manifest' =
        Manifest
          { Manifest.repositoryId = Just repositoryId'
          , monikers = mempty
          , variables = mempty
          , fileRoutes = Map.fromList [(routeName, route)]
          , ignorePatterns = mempty
          , hooks = mempty
          }
  let appEnv =
        AppEnv
          repository
          False
          (Just intermediateDir)
          (tmpDir </> stateDir)
          manifestFilename
          envFilename
          False
          False

  createDirectories $ repository </> intermediateDir
  createDirectories source
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile (source </> fileName') "inner contents"

  withEnvVars
    [ ("DEST_DIR", Just destination)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv destination (destination </> fileName')


withTrackedIgnoredFile
  :: (AppEnv -> OsPath -> OsPath -> IO a)
  -> IO a
withTrackedIgnoredFile action = withTempDir $ \tmpDir _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestFilename <- encodeFS "dojang.toml"
  envFilename <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "managed-directory"
  filename <- encodeFS "tracked-file"
  destinationDir <- encodeFS "destination"
  homeName <- encodeFS "home"
  let repository = tmpDir </> sourceDir
  let sourceRoot = repository </> routeName
  let intermediateRoot = repository </> intermediateDir </> routeName
  let destinationRoot = tmpDir </> destinationDir
  let source = sourceRoot </> filename
  let intermediate = intermediateRoot </> filename
  let destination = destinationRoot </> filename
  let home = tmpDir </> homeName
  let Right always = parseMonikerName "always"
  let Right repositoryId' =
        parseRepositoryId "223e4567-e89b-42d3-a456-426614174000"
  let manifest' =
        ( ( manifest
              (singleton always Always)
              mempty
              ( Map.singleton
                  routeName
                  [(always, Just $ Substitution "DEST_DIR")]
              )
              (Map.singleton routeName ["*"])
              mempty
          )
            :: Manifest
        )
          { Manifest.repositoryId = Just repositoryId'
          }
  let appEnv =
        AppEnv
          repository
          False
          (Just intermediateDir)
          (tmpDir </> stateDir)
          manifestFilename
          envFilename
          False
          False

  createDirectories sourceRoot
  createDirectories intermediateRoot
  createDirectories destinationRoot
  createDirectories home
  writeManifestFile manifest' $ repository </> manifestFilename
  writeFile source "source contents"
  writeFile intermediate "base contents"
  writeFile destination "base contents"

  withEnvVars
    [ ("DEST_DIR", Just destinationRoot)
    , ("HOME", Just home)
    , ("USERPROFILE", Just home)
    ]
    $ action appEnv intermediate destination

-- Windows cannot observe POSIX permission bits, so a pre-existing
-- writable file already satisfies the declared private mode there and
-- no mode reconciliation is planned.
#ifdef mingw32_HOST_OS
reconciledPrivateFileMode :: PortableMode
reconciledPrivateFileMode = PortableMode Nothing True
#else
reconciledPrivateFileMode :: PortableMode
reconciledPrivateFileMode = portableModeFromBits 0o600
#endif


withEnvVars :: [(String, Maybe OsPath)] -> IO a -> IO a
withEnvVars [] action = action
withEnvVars ((name, value) : rest) action = do
  oldValue <- lookupEnv name
  bracket_
    (setOrUnset value)
    (maybe (unsetEnv name) (setEnv name) oldValue)
    (withEnvVars rest action)
 where
  setOrUnset Nothing = unsetEnv name
  setOrUnset (Just value') = decodePath value' >>= setEnv name
