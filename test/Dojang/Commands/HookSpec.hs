{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.HookSpec (spec) where

import Control.Exception (bracket_)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)


#ifndef mingw32_HOST_OS
import Data.Char (chr)
import System.OsString qualified as OsString
#endif

import Data.ByteString qualified as ByteString
import Data.HashMap.Strict (empty)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory.OsPath qualified as OsDirectory
import System.Environment
  ( getArgs
  , getExecutablePath
  , lookupEnv
  , setEnv
  , unsetEnv
  )
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath qualified as FilePath
import System.OsPath (OsPath, encodeFS, normalise, (</>))
import Test.Hspec
  ( Spec
  , describe
  , it
  , runIO
  , sequential
  , shouldBe
  , shouldReturn
  , shouldThrow
  )
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.App (AppEnv (..), runAppWithoutLogging)
import Dojang.Commands.Hook
  ( HookEnv (..)
  , HookScopePath (..)
  , commandHookTypes
  , defaultHookProcessRunner
  , disambiguatedHookScopePaths
  , executeHooks
  , hookDueReason
  , hookIsDue
  , hookRecursionKey
  , hooksSuppressed
  , mergeHookEnvironment
  , normalizeHookScopePath
  , renderHookDryRun
  , shouldRunHook
  , withCommandHooks
  )
import Dojang.ExitCodes (machineStateError)
import Dojang.MonadFileSystem qualified as FileSystem
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Context (Context (..))
import Dojang.Types.Environment (Environment (..), Kernel (..))
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.Hook
  ( Hook (..)
  , HookPolicy (..)
  , HookType (..)
  , parseHookId
  )
import Dojang.Types.MachineState
  ( HookExecution (HookExecution)
  , MachineState (..)
  , encodeMachineState
  , managedTargetSnapshotRoot
  , parseMachineId
  , repositoryStateDirectory
  , repositoryStatePath
  )
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.Repository (Repository (..))
import Dojang.Types.RepositoryId (parseRepositoryId, repositoryIdText)


encodePath :: String -> OsPath
encodePath = unsafePerformIO . encodeFS


spec :: Spec
spec = sequential $ do
  scopeRepositoryName <- runIO $ encodeFS "hook-scope-repository"
  scopeFirstCallerName <- runIO $ encodeFS "hook-scope-caller-a"
  scopeSecondCallerName <- runIO $ encodeFS "hook-scope-caller-b"
  scopeRepository <- runIO $ OsDirectory.makeAbsolute scopeRepositoryName
  scopeFirstCaller <- runIO $ OsDirectory.makeAbsolute scopeFirstCallerName
  scopeSecondCaller <- runIO $ OsDirectory.makeAbsolute scopeSecondCallerName

  describe "mergeHookEnvironment" $ do
    let dojangEnv = [("DOJANG_OS", "linux")]
    let parentEnv = [("Dojang_OS", "stale"), ("PATH", "/bin")]

    it "replaces differently cased variable names on Windows" $ do
      mergeHookEnvironment "mingw32" dojangEnv parentEnv
        `shouldBe` [("DOJANG_OS", "linux"), ("PATH", "/bin")]

    it "preserves differently cased variable names on POSIX" $ do
      mergeHookEnvironment "linux" dojangEnv parentEnv
        `shouldBe` dojangEnv ++ parentEnv

    it "removes stale values for omitted hook context" $ hedgehog $ do
      pathIndex <- forAll $ Gen.int (Range.linear 0 1000)
      let pathVariable = "DOJANG_PATH_" <> show pathIndex
      let inherited =
            [ ("DOJANG_COMMAND_OUTCOME", "success")
            , ("DOJANG_EXIT_CODE", "0")
            , (pathVariable, "stale")
            , ("DOJANG_ALLOW_HOOK_RECURSION", "1")
            , ("DOJANG_TEST_PARENT", "inherited")
            ]
      let merged = mergeHookEnvironment "linux" dojangEnv inherited
      lookup "DOJANG_COMMAND_OUTCOME" merged === Nothing
      lookup "DOJANG_EXIT_CODE" merged === Nothing
      lookup pathVariable merged === Nothing
      lookup "DOJANG_ALLOW_HOOK_RECURSION" merged === Just "1"
      lookup "DOJANG_TEST_PARENT" merged === Just "inherited"

    it "removes omitted hook context case-insensitively on Windows" $ do
      mergeHookEnvironment
        "mingw32"
        dojangEnv
        [ ("Dojang_Command_Outcome", "success")
        , ("dojang_path_7", "stale")
        , ("PATH", "C:\\Windows")
        ]
        `shouldBe` [("DOJANG_OS", "linux"), ("PATH", "C:\\Windows")]

  describe "commandHookTypes" $ do
    it "covers only the supported command lifecycles" $ do
      commandHookTypes "apply" `shouldBe` Just (PreApply, PostApply)
      commandHookTypes "reflect" `shouldBe` Just (PreReflect, PostReflect)
      commandHookTypes "diff" `shouldBe` Just (PreDiff, PostDiff)
      commandHookTypes "status" `shouldBe` Just (PreStatus, PostStatus)
      commandHookTypes "edit" `shouldBe` Just (PreEdit, PostEdit)
      commandHookTypes "unmanage" `shouldBe` Just (PreUnmanage, PostUnmanage)
      map commandHookTypes ["init", "migrate", "env", "forget"]
        `shouldBe` replicate 4 Nothing

  describe "normalizeHookScopePath" $ do
    it "keeps repository selectors independent of the caller directory" $
      hedgehog $ do
        suffix <- forAll $ Gen.text (Range.linear 1 40) Gen.alphaNum
        selector <- liftIO $ encodeFS $ "route-" <> unpack suffix
        firstRepositoryScope <-
          liftIO $
            normalizeHookScopePath
              scopeRepository
              scopeFirstCaller
              (RepositoryRelativePath selector)
        secondRepositoryScope <-
          liftIO $
            normalizeHookScopePath
              scopeRepository
              scopeSecondCaller
              (RepositoryRelativePath selector)
        firstCallerScope <-
          liftIO $
            normalizeHookScopePath
              scopeRepository
              scopeFirstCaller
              (CallerRelativePath selector)
        secondCallerScope <-
          liftIO $
            normalizeHookScopePath
              scopeRepository
              scopeSecondCaller
              (CallerRelativePath selector)
        firstRepositoryScope === selector
        secondRepositoryScope === selector
        firstCallerScope === normalise (scopeFirstCaller </> selector)
        secondCallerScope === normalise (scopeSecondCaller </> selector)

  describe "disambiguatedHookScopePaths" $ do
    it "includes a source selector only when target paths use it" $ hedgehog $ do
      sourceText <- forAll $ Gen.text (Range.linear 1 40) Gen.alphaNum
      pathTexts <-
        forAll $
          Gen.list (Range.linear 0 20) $
            Gen.text (Range.linear 1 40) Gen.alphaNum
      source <- liftIO $ encodeFS $ unpack sourceText
      paths <- liftIO $ mapM (encodeFS . unpack) pathTexts
      let callerPaths = CallerRelativePath <$> paths
      disambiguatedHookScopePaths Nothing paths === callerPaths
      disambiguatedHookScopePaths (Just source) paths
        === if null paths
          then []
          else RepositoryRelativePath source : callerPaths

  describe "hooksSuppressed" $ do
    it "allows only one explicitly enabled nested level" $ do
      hooksSuppressed 0 False `shouldBe` False
      hooksSuppressed 1 False `shouldBe` True
      hooksSuppressed 1 True `shouldBe` False
      hooksSuppressed 2 True `shouldBe` True

  describe "hookRecursionKey" $ do
    it "scopes otherwise identical hooks by repository" $ hedgehog $ do
      event <- forAll $ Gen.text (Range.linear 1 40) Gen.alphaNum
      identifier <- forAll $ Gen.text (Range.linear 1 40) Gen.alphaNum
      let Right firstRepository =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let Right secondRepository =
            parseRepositoryId "223e4567-e89b-42d3-a456-426614174000"
      hookRecursionKey firstRepository event identifier
        === repositoryIdText firstRepository
        <> "/"
        <> event
        <> "/"
        <> identifier
      ( hookRecursionKey firstRepository event identifier
          == hookRecursionKey secondRepository event identifier
        )
        === False

  describe "shouldRunHook" $ do
    let env = Environment "linux" "x86_64" $ Kernel "Linux" "6.0.0"

    it "returns True for Always condition" $ do
      let hook =
            Hook
              { hookId = Nothing
              , policy = HookAlways
              , changeKey = Nothing
              , command = encodePath "/bin/echo"
              , args = ["hello"]
              , condition = Always
              , workingDirectory = Nothing
              , ignoreFailure = False
              }
      shouldRunHook empty env hook `shouldBe` True

    it "returns True when OS matches" $ do
      let hook =
            Hook
              { hookId = Nothing
              , policy = HookAlways
              , changeKey = Nothing
              , command = encodePath "/bin/echo"
              , args = []
              , condition = OperatingSystem "linux"
              , workingDirectory = Nothing
              , ignoreFailure = False
              }
      shouldRunHook empty env hook `shouldBe` True

    it "returns False when OS does not match" $ do
      let hook =
            Hook
              { hookId = Nothing
              , policy = HookAlways
              , changeKey = Nothing
              , command = encodePath "/bin/echo"
              , args = []
              , condition = OperatingSystem "windows"
              , workingDirectory = Nothing
              , ignoreFailure = False
              }
      shouldRunHook empty env hook `shouldBe` False

    it "returns True when moniker matches" $ do
      let Right monikerName = parseMonikerName "linux"
      let monikers = [(monikerName, OperatingSystem "linux")] :: MonikerMap
      let hook =
            Hook
              { hookId = Nothing
              , policy = HookAlways
              , changeKey = Nothing
              , command = encodePath "/bin/echo"
              , args = []
              , condition = Moniker monikerName
              , workingDirectory = Nothing
              , ignoreFailure = False
              }
      shouldRunHook monikers env hook `shouldBe` True

    it "returns False when moniker does not match" $ do
      let Right linuxMoniker = parseMonikerName "linux"
      let Right windowsMoniker = parseMonikerName "windows"
      let monikers =
            [ (linuxMoniker, OperatingSystem "linux")
            , (windowsMoniker, OperatingSystem "windows")
            ]
              :: MonikerMap
      let hook =
            Hook
              { hookId = Nothing
              , policy = HookAlways
              , changeKey = Nothing
              , command = encodePath "/bin/echo"
              , args = []
              , condition = Moniker windowsMoniker
              , workingDirectory = Nothing
              , ignoreFailure = False
              }
      shouldRunHook monikers env hook `shouldBe` False

  describe "hookIsDue" $ do
    let execution fingerprint =
          HookExecution
            "pre-apply"
            "cache"
            "on-change"
            fingerprint
            (read "2026-07-15 00:00:00 UTC")

    it "always runs and once runs only without a successful record" $ do
      hookIsDue HookAlways Nothing (Just $ execution Nothing) `shouldBe` True
      hookIsDue HookOnce Nothing Nothing `shouldBe` True
      hookIsDue HookOnce Nothing (Just $ execution Nothing) `shouldBe` False

    it "runs on-change exactly when the fingerprint differs" $ hedgehog $ do
      old <- forAll $ Gen.text (Range.linear 0 40) Gen.alphaNum
      new <- forAll $ Gen.text (Range.linear 0 40) Gen.alphaNum
      hookIsDue HookOnChange (Just new) (Just $ execution $ Just old)
        === (new /= old)

    it "explains why every due stateful policy matches" $ hedgehog $ do
      old <- forAll $ Gen.text (Range.linear 0 40) Gen.alphaNum
      new <- forAll $ Gen.filter (/= old) $ Gen.text (Range.linear 0 40) Gen.alphaNum
      hookDueReason HookOnce Nothing Nothing
        === Just "once policy has no successful execution"
      hookDueReason HookOnChange (Just new) Nothing
        === Just "on-change policy has no successful execution"
      hookDueReason HookOnChange (Just new) (Just $ execution $ Just old)
        === Just "on-change fingerprint changed"

  describe "renderHookDryRun" $ do
    it "includes the event, effective working directory, and due reason" $ do
      renderHookDryRun
        "pre-status"
        "/dotfiles"
        "once policy has no successful execution"
        "/bin/echo"
        ["hello", "world"]
        `shouldBe` "Would run hook (event: pre-status; working directory: /dotfiles; reason: once policy has no successful execution): /bin/echo hello world"

  describe "withCommandHooks" $ do
    it "reloads the post-hook context after the command action" $
      withTempDir $ \tmpDir _ -> do
        command <- hookCommandFor postHookProbePattern postHookProbeSeed
        manifestFilename <- encodeFS "dojang.toml"
        intermediateDir <- encodeFS ".dojang"
        envFilename <- encodeFS "dojang-env.toml"
        stateDir <- encodeFS ".state"
        markerName <- encodeFS "stale-post-hook-ran"
        let hook =
              Hook
                { hookId = Nothing
                , policy = HookAlways
                , changeKey = Nothing
                , command = fst command
                , args = snd command
                , condition = Always
                , workingDirectory = Nothing
                , ignoreFailure = False
                }
        let Right repositoryId =
              parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        let initialManifest =
              Manifest (Just repositoryId) empty Map.empty Map.empty $
                Map.singleton PostStatus [hook]
        let replacementManifest =
              Manifest (Just repositoryId) empty Map.empty Map.empty Map.empty
        let manifestFilePath = tmpDir </> manifestFilename
        let appEnv =
              AppEnv
                tmpDir
                False
                (Just intermediateDir)
                (tmpDir </> stateDir)
                manifestFilename
                envFilename
                False
                False
        runAppWithoutLogging appEnv $
          writeManifestFile initialManifest manifestFilePath
        _ <- runAppWithoutLogging appEnv $
          withCommandHooks "status" [] $ do
            writeManifestFile replacementManifest manifestFilePath
            pure ExitSuccess
        OsDirectory.doesFileExist (tmpDir </> markerName) `shouldReturn` False

  describe "executeHooks" $ do
    posixNonUtf8HookScopeSpec

    it "inherits the parent environment and overrides Dojang variables" $
      withTempDir $ \tmpDir _ -> do
        command <- hookCommand
        manifestFilename <- encodeFS "dojang.toml"
        intermediateDir <- encodeFS ".dojang"
        envFilename <- encodeFS "dojang-env.toml"
        stateDir <- encodeFS ".state"
        let Right identifier = parseHookId "shared"
        let hook =
              Hook
                { hookId = Just identifier
                , policy = HookAlways
                , changeKey = Nothing
                , command = fst command
                , args = snd command
                , condition = Always
                , workingDirectory = Nothing
                , ignoreFailure = False
                }
        let Right repositoryId =
              parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        let Right machineId =
              parseMachineId "223e4567-e89b-42d3-a456-426614174000"
        let manifest' =
              Manifest (Just repositoryId) empty Map.empty Map.empty $
                Map.singleton PreApply [hook]
        let repository = Repository tmpDir (tmpDir </> intermediateDir) manifest'
        let environment = Environment "linux" "x86_64" $ Kernel "Linux" "6.0.0"
        let context = Context repository environment (const $ pure Nothing)
        let appEnv =
              AppEnv
                tmpDir
                False
                (Just intermediateDir)
                (tmpDir </> stateDir)
                manifestFilename
                envFilename
                False
                False
        let hookEnv =
              HookEnv
                tmpDir
                (tmpDir </> manifestFilename)
                False
                "hook-test-os"
                "x86_64"
                "Linux"
                "6.0.0"
                "apply"
                []
                ( MachineState
                    3
                    repositoryId
                    machineId
                    tmpDir
                    (tmpDir </> manifestFilename)
                    (tmpDir </> intermediateDir)
                    (tmpDir </> stateDir)
                    (read "2026-07-15 00:00:00 UTC")
                    (read "2026-07-15 00:00:00 UTC")
                    False
                    []
                    Map.empty
                    Map.empty
                    Map.empty
                )
                (tmpDir </> stateDir)
                defaultHookProcessRunner
        withEnvVars
          [ ("DOJANG_TEST_PARENT", Just "inherited")
          , ("DOJANG_OS", Just "stale")
          ]
          $ runAppWithoutLogging appEnv
          $ executeHooks hookEnv context PreApply
        let ignoredHook = hook{ignoreFailure = True}
        let ignoredManifest =
              Manifest (Just repositoryId) empty Map.empty Map.empty $
                Map.singleton PreApply [ignoredHook]
        let ignoredRepository =
              Repository tmpDir (tmpDir </> intermediateDir) ignoredManifest
        let ignoredContext =
              Context ignoredRepository environment (const $ pure Nothing)
        let failingHookEnv =
              hookEnv{processRunner = const $ pure $ Left $ userError "not found"}
        runAppWithoutLogging appEnv $
          executeHooks failingHookEnv ignoredContext PreApply

    it "does not launch a hook from a replaced state generation" $
      withTempDir $ \tmpDir _ -> do
        manifestFilename <- encodeFS "dojang.toml"
        intermediateDir <- encodeFS ".dojang"
        envFilename <- encodeFS "dojang-env.toml"
        stateDir <- encodeFS ".state"
        command <- encodeFS "hook-command"
        let Right identifier = parseHookId "prepare"
        let hook =
              Hook
                { hookId = Just identifier
                , policy = HookOnce
                , changeKey = Nothing
                , command = command
                , args = []
                , condition = Always
                , workingDirectory = Nothing
                , ignoreFailure = False
                }
        let Right repositoryId =
              parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        let Right machineId =
              parseMachineId "223e4567-e89b-42d3-a456-426614174000"
        let manifest' =
              Manifest (Just repositoryId) empty Map.empty Map.empty $
                Map.singleton PreApply [hook]
        let repository = Repository tmpDir (tmpDir </> intermediateDir) manifest'
        let environment = Environment "linux" "x86_64" $ Kernel "Linux" "6.0.0"
        let context = Context repository environment (const $ pure Nothing)
        let stateRootPath = tmpDir </> stateDir
        let createdTime = read "2026-07-15 00:00:00 UTC"
        let recreatedTime = read "2026-07-15 01:00:00 UTC"
        let oldState =
              MachineState
                3
                repositoryId
                machineId
                tmpDir
                (tmpDir </> manifestFilename)
                (tmpDir </> intermediateDir)
                (managedTargetSnapshotRoot stateRootPath repositoryId)
                createdTime
                createdTime
                False
                []
                Map.empty
                Map.empty
                Map.empty
        let recreatedState =
              oldState
                { createdTime = recreatedTime
                , updatedTime = recreatedTime
                }
        let appEnv =
              AppEnv
                tmpDir
                False
                (Just intermediateDir)
                stateRootPath
                manifestFilename
                envFilename
                False
                False
        runAppWithoutLogging appEnv $ do
          FileSystem.createDirectories $
            repositoryStateDirectory stateRootPath repositoryId
          FileSystem.writeFile
            (repositoryStatePath stateRootPath repositoryId)
            (encodeUtf8 $ encodeMachineState recreatedState)
        launches <- newIORef (0 :: Int)
        let hookEnv =
              HookEnv
                tmpDir
                (tmpDir </> manifestFilename)
                False
                "linux"
                "x86_64"
                "Linux"
                "6.0.0"
                "apply"
                []
                oldState
                stateRootPath
                ( \_ -> do
                    modifyIORef' launches (+ 1)
                    pure $ Right ExitSuccess
                )
        (runAppWithoutLogging appEnv $ executeHooks hookEnv context PreApply)
          `shouldThrow` (== machineStateError)
        readIORef launches `shouldReturn` 0

    it "hook environment probe" $ do
      arguments <- getArgs
      when (probePattern `elem` arguments && probeSeed `elem` arguments) $ do
        lookupEnv "DOJANG_TEST_PARENT" >>= (`shouldBe` Just "inherited")
        lookupEnv "DOJANG_OS" >>= (`shouldBe` Just "hook-test-os")
        lookupEnv "DOJANG_HOOK_STACK"
          >>= ( `shouldBe`
                  Just
                    "123e4567-e89b-42d3-a456-426614174000/pre-apply/shared"
              )

    it "removed post-hook probe" $ do
      event <- lookupEnv "DOJANG_HOOK_EVENT"
      when (event == Just "post-status") $ do
        repository <- lookupEnv "DOJANG_REPOSITORY"
        case repository of
          Nothing -> fail "DOJANG_REPOSITORY is missing"
          Just path ->
            ByteString.writeFile
              (path FilePath.</> "stale-post-hook-ran")
              ""

#ifdef mingw32_HOST_OS
posixNonUtf8HookScopeSpec :: Spec
posixNonUtf8HookScopeSpec = pure ()
#else
posixNonUtf8HookScopeSpec :: Spec
posixNonUtf8HookScopeSpec =
  it "distinguishes non-UTF-8 path scopes for on-change hooks" $ hedgehog $ do
    firstByte <- forAll $ Gen.word8 (Range.linear 0x80 0xff)
    secondByte <-
      forAll $ Gen.filter (/= firstByte) $ Gen.word8 (Range.linear 0x80 0xff)
    liftIO $ withTempDir $ \tmpDir _ -> do
      manifestFilename <- encodeFS "dojang.toml"
      intermediateDir <- encodeFS ".dojang"
      envFilename <- encodeFS "dojang-env.toml"
      stateDir <- encodeFS ".state"
      command <- encodeFS "hook-command"
      let Right identifier = parseHookId "native-path"
      let hook =
            Hook
              { hookId = Just identifier
              , policy = HookOnChange
              , changeKey = Nothing
              , command = command
              , args = []
              , condition = Always
              , workingDirectory = Nothing
              , ignoreFailure = False
              }
      let Right repositoryId =
            parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
      let Right machineId =
            parseMachineId "223e4567-e89b-42d3-a456-426614174000"
      let manifest' =
            Manifest (Just repositoryId) empty Map.empty Map.empty $
              Map.singleton PreApply [hook]
      let repository = Repository tmpDir (tmpDir </> intermediateDir) manifest'
      let environment = Environment "linux" "x86_64" $ Kernel "Linux" "6.0.0"
      let context = Context repository environment (const $ pure Nothing)
      let stateRootPath = tmpDir </> stateDir
      let timestamp = read "2026-07-15 00:00:00 UTC"
      let state =
            MachineState
              3
              repositoryId
              machineId
              tmpDir
              (tmpDir </> manifestFilename)
              (tmpDir </> intermediateDir)
              (managedTargetSnapshotRoot stateRootPath repositoryId)
              timestamp
              timestamp
              False
              []
              Map.empty
              Map.empty
              Map.empty
      let appEnv =
            AppEnv
              tmpDir
              False
              (Just intermediateDir)
              stateRootPath
              manifestFilename
              envFilename
              False
              False
      runAppWithoutLogging appEnv $ do
        FileSystem.createDirectories $
          repositoryStateDirectory stateRootPath repositoryId
        FileSystem.writeFile
          (repositoryStatePath stateRootPath repositoryId)
          (encodeUtf8 $ encodeMachineState state)
      launches <- newIORef (0 :: Int)
      let path byte =
            OsString.pack
              [OsString.unsafeFromChar $ chr $ 0xdc00 + fromIntegral byte]
      let hookEnv selectedPath =
            HookEnv
              tmpDir
              (tmpDir </> manifestFilename)
              False
              "linux"
              "x86_64"
              "Linux"
              "6.0.0"
              "apply"
              [selectedPath]
              state
              stateRootPath
              ( \_ -> do
                  modifyIORef' launches (+ 1)
                  pure $ Right ExitSuccess
              )
      runAppWithoutLogging appEnv $
        executeHooks (hookEnv $ path firstByte) context PreApply
      runAppWithoutLogging appEnv $
        executeHooks (hookEnv $ path secondByte) context PreApply
      readIORef launches `shouldReturn` 2
#endif


hookCommand :: IO (OsPath, [Text])
hookCommand = hookCommandFor probePattern probeSeed


hookCommandFor :: String -> String -> IO (OsPath, [Text])
hookCommandFor pattern' seed = do
  command <- getExecutablePath >>= encodeFS
  pure
    ( command
    ,
      [ "--match"
      , pack pattern'
      , "--seed"
      , pack seed
      ]
    )


probePattern :: String
probePattern = "/Dojang.Commands.Hook/executeHooks/hook environment probe/"


probeSeed :: String
probeSeed = "250025"


postHookProbePattern :: String
postHookProbePattern =
  "/Dojang.Commands.Hook/executeHooks/removed post-hook probe/"


postHookProbeSeed :: String
postHookProbeSeed = "250026"


withEnvVars :: [(String, Maybe String)] -> IO a -> IO a
withEnvVars [] action = action
withEnvVars ((name, value) : rest) action = do
  oldValue <- lookupEnv name
  bracket_
    (setOrUnset value)
    (setOrUnset oldValue)
    (withEnvVars rest action)
 where
  setOrUnset (Just value') = setEnv name value'
  setOrUnset Nothing = unsetEnv name
