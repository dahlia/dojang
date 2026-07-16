{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative (Alternative ((<|>)), optional, (<**>))
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function ((&))
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.String (fromString)
import Data.Text (pack)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (lookupEnv, setEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import System.IO.CodePage (withCP65001)
import System.IO.Unsafe (unsafePerformIO)
import System.Info (os)

import Control.Monad.Except (MonadError (catchError))
import Options.Applicative
  ( Parser
  , ParserInfo
  , ParserPrefs
  , ParserResult
  , action
  , argument
  , command
  , commandGroup
  , customExecParser
  , defaultPrefs
  , execParserPure
  , flag
  , flag'
  , footer
  , fullDesc
  , handleParseResult
  , header
  , help
  , helper
  , info
  , long
  , many
  , metavar
  , progDesc
  , short
  , showDefault
  , str
  , strOption
  , subparser
  , switch
  , value
  )
import System.OsPath (OsPath, encodeFS)
import TextShow (TextShow (showt))

import Dojang.App
  ( App
  , AppEnv (..)
  , applyAutomaticRepositorySelection
  , automaticSelectionUsesCheckoutManifest
  , runAppWithStderrLogging
  , runAppWithoutLogging
  , validateRepositoryCheckout
  )
import Dojang.Cli.CommandMode
  ( CommandMode (..)
  , ambiguousRepositoryExitCode
  , environmentCommandMode
  , initializationCommandMode
  , metaCommandMode
  , migrationCommandMode
  , repositoryCommandMode
  )
import Dojang.Commands
  ( Admonition (Error, Hint, Note)
  , codeStyleFor
  , pathStyleFor
  , printStderr'
  )
import Dojang.Commands.Apply qualified (apply)
import Dojang.Commands.Diff (DiffMode (..))
import Dojang.Commands.Diff qualified (diff)
import Dojang.Commands.Edit qualified (edit)
import Dojang.Commands.Env qualified (env)
import Dojang.Commands.Init (InitPreset (..), initPresetName)
import Dojang.Commands.Init qualified (initWithFacts)
import Dojang.Commands.Migrate qualified (migrate)
import Dojang.Commands.Reflect qualified (reflect)
import Dojang.Commands.Status (StatusOptions (..))
import Dojang.Commands.Status qualified (status)
import Dojang.Commands.TargetLifecycle qualified (forget, unmanage)
import Dojang.ExitCodes (machineStateError, unhandledError)
import Dojang.MonadFileSystem
  ( DryRunIO
  , MonadFileSystem
  , dryRunIO'
  )
import Dojang.Types.Environment (OperatingSystem (..))
import Dojang.Types.MachineState
  ( MachineState (..)
  , RepositorySelection (..)
  , StateRootInputs (..)
  , canonicalizeStateRoot
  , formatStateError
  , listRepositoryStates
  , readMachineId
  , resolveStateRoot
  , selectRepositoryState
  , validateMachineStateStore
  )
import Dojang.Types.Registry (Registry (..), readRegistry, registryFilename)
import Dojang.Version (toString, version)
import Options.Applicative.Path (hyphen, pathArgument, pathOption, period)
import System.Directory (getHomeDirectory)
import System.OsPath qualified as OsPath


manifestFilename :: OsPath
manifestFilename = unsafePerformIO $ encodeFS "dojang.toml"
{-# NOINLINE manifestFilename #-}


envFilename :: OsPath
envFilename = unsafePerformIO $ encodeFS "dojang-env.toml"
{-# NOINLINE envFilename #-}


data ParsedApp i
  = ParsedApp CommandMode Bool Bool AppEnv (App i ExitCode)


appP
  :: (MonadFileSystem i, MonadIO i)
  => OsPath
  -- ^ The platform-native machine-state root.
  -> OsPath
  -- ^ The default repository path (from registry or current directory).
  -> Parser (ParsedApp i)
appP stateRoot defaultRepoPath =
  makeApp
    <$> optional repositoryOption
    <*> optional intermediateOption
    <*> optional manifestOption
    <*> environmentOption
    <*> dryRunOption
    <*> debugOption
    <*> allowHookRecursionOption
    <*> cmdP stateRoot defaultRepoPath
 where
  makeApp repository intermediate manifest envFile' dryRun' debug' allowRecursion command' =
    ParsedApp
      (fst command')
      (case manifest of Just _ -> True; Nothing -> False)
      allowRecursion
      ( AppEnv
          (fromMaybe defaultRepoPath repository)
          (case repository of Just _ -> True; Nothing -> False)
          intermediate
          stateRoot
          (fromMaybe manifestFilename manifest)
          envFile'
          dryRun'
          debug'
      )
      (snd command')
  repositoryOption =
    pathOption
      ( long "repository-dir"
          <> short 'r'
          <> metavar "PATH"
          <> action "directory"
          <> help "Repository (i.e., source tree) directory"
      )
  intermediateOption =
    pathOption
      ( long "intermediate-dir"
          <> short 'i'
          <> metavar "PATH"
          <> action "directory"
          <> help
            ( "Intermediate directory which is managed by Dojang.  "
                ++ "Relative to repository directory if not absolute.  "
                ++ "The selected path is persisted"
            )
      )
  manifestOption =
    pathOption
      ( long "manifest-file"
          <> short 'm'
          <> metavar "PATH"
          <> action "file"
          <> help
            ( "Manifest file, relative to the repository if not absolute.  "
                ++ "Defaults to dojang.toml"
            )
      )
  environmentOption =
    pathOption
      ( long "env-file"
          <> short 'e'
          <> metavar "PATH"
          <> value envFilename
          <> showDefault
          <> action "file"
          <> help
            ( "Environment file, relative to the repository if not absolute.  "
                ++ "A missing file is ignored"
            )
      )
  dryRunOption =
    switch
      ( long "dry-run"
          <> help
            ( "Do not actually perform actions, but just print them.  "
                ++ "This option probably lets the program run much slower"
            )
      )
  debugOption =
    switch (long "debug" <> short 'd' <> help "Enable debug logging")
  allowHookRecursionOption =
    switch
      ( long "allow-hook-recursion"
          <> help "Allow hooks in one nested Dojang invocation"
      )


initPresetP :: Parser [InitPreset]
initPresetP =
  (\a b c d e f' g -> catMaybes [a, b, c, d, e, f', g])
    <$> f Amd64Linux ("linux-x86_64", "linux-amd64")
    <*> f Arm64Linux ("linux-aarch64", "linux-arm64")
    <*> f AppleSiliconMac ("macos-aarch64", "apple-silicon-mac")
    <*> f IntelMac ("macos-amd64", "intel-mac")
    <*> f Win64 ("windows-amd64", "win64")
    <*> f WinArm64 ("windows-aarch64", "win-arm64")
    <*> f Wsl2 ("linux-wsl2", "wsl2")
 where
  f :: InitPreset -> (String, String) -> Parser (Maybe InitPreset)
  f preset (longOpt, alias) =
    flag
      Nothing
      (Just preset)
      ( long longOpt
          <> long alias
          <> help ("Use " ++ initPresetName preset ++ " preset")
      )


cmdP
  :: (MonadFileSystem i, MonadIO i)
  => OsPath
  -> OsPath
  -- ^ The default repository path (from registry or current directory).
  -> Parser (CommandMode, App i ExitCode)
cmdP stateRoot defaultRepoPath =
  subparser
    ( commandGroup "Managing commands:"
        <> command
          "init"
          ( info
              ( Dojang.Commands.Init.initWithFacts
                  <$> initPresetP
                  <*> switch
                    ( long "no-interactive"
                        <> short 'I'
                        <> help "Do not prompt for anything"
                    )
                  <*> optional
                    ( pathOption
                        ( long "facts-file"
                            <> metavar "PATH"
                            <> action "file"
                            <> help
                              "Associate a TOML file containing a [facts] table"
                        )
                    )
                  <*> many
                    ( pack
                        <$> strOption
                          ( long "fact"
                              <> metavar "KEY=VALUE"
                              <> help "Persist a repository-specific machine fact"
                          )
                    )
                  <**> helper
                  & initializationCommandP
              )
              (progDesc "Initialize repository")
          )
        <> command
          "migrate"
          ( info
              (pure Dojang.Commands.Migrate.migrate <**> helper & migrationCommandP)
              (progDesc "Migrate repository to machine-scoped state")
          )
        <> command
          "env"
          ( info
              ( ( \ignoreEnvFile outputPath ->
                    ( environmentCommandMode ignoreEnvFile
                    , Dojang.Commands.Env.env ignoreEnvFile outputPath
                    )
                )
                  <$> switch
                    ( long "ignore-env-file"
                        <> short 'i'
                        <> help "Ignore environment file"
                    )
                  <*> pathOption
                    ( long "output-file"
                        <> short 'o'
                        <> metavar "PATH"
                        <> value hyphen
                        <> showDefault
                        <> action "file"
                        <> help
                          ( "Output file path.  Use - for stdout, "
                              ++ "and ./- for a file named -"
                          )
                    )
                  <**> helper
              )
              ( progDesc $
                  "Show environment in TOML format, "
                    ++ "which can be used with -e/--env-file"
              )
          )
        <> command
          "status"
          ( info
              ( Dojang.Commands.Status.status
                  <$> ( StatusOptions
                          <$> switch
                            ( long "no-trailing-slash"
                                <> short 'S'
                                <> help
                                  ( "Do not append trailing slash (or backslash "
                                      ++ "on Windows) to directory paths"
                                  )
                            )
                          <*> switch
                            ( long "only-changes"
                                <> short 'c'
                                <> help "Only show changed files"
                            )
                          <*> switch
                            ( long "show-destination-path"
                                <> short 'd'
                                <> help
                                  ( "Show destination (target) path "
                                      ++ "instead of source path"
                                  )
                            )
                      )
                  <**> helper
                  & repositoryCommandP
              )
              ( progDesc
                  "Show status of repository and target (destination) tree"
              )
          )
        <> command
          "diff"
          ( info
              ( Dojang.Commands.Diff.diff
                  <$> ( flag'
                          Source
                          ( long "source"
                              <> short 's'
                              <> help
                                ( "Two-way diff between source tree and base "
                                    ++ "(intermediate) tree"
                                )
                          )
                          <|> flag'
                            Destination
                            ( long "destination"
                                <> long "target"
                                <> short 'd'
                                <> help
                                  ( "Two-way diff between destination (target) "
                                      ++ "tree and base (intermediate) tree"
                                  )
                            )
                          <|> pure Both
                      )
                  <*> optional
                    ( pathOption
                        ( long "diff-program"
                            <> metavar "PROGRAM"
                            <> action "command"
                            <> help
                              ( "A program to use for two-way diff.  "
                                  ++ "If not specified, fall back to a program "
                                  ++ "specified by the environment variable "
                                  ++ "DOJANG_DIFF"
                              )
                        )
                    )
                  <*> many (pathArgument $ metavar "FILE" <> action "file")
                  <**> helper
                  & repositoryCommandP
              )
              ( progDesc $
                  "Show changes between source tree and target "
                    ++ "(destination) tree"
              )
          )
        <> command
          "reflect"
          ( info
              ( Dojang.Commands.Reflect.reflect
                  <$> switch
                    ( long "force"
                        <> short 'f'
                        <> help
                          "Enforce reflecting if there are ignorable errors"
                    )
                  <*> switch
                    ( long "all"
                        <> short 'a'
                        <> help
                          "Reflect all changed files without prompting"
                    )
                  <*> switch
                    ( long "include-unregistered"
                        <> short 'u'
                        <> help
                          "Include unregistered files (prompts for route)"
                    )
                  <*> optional
                    ( pathOption
                        ( long "source"
                            <> short 's'
                            <> metavar "PATH"
                            <> action "file"
                            <> help
                              "Explicitly specify source path (skip disambiguation)"
                        )
                    )
                  <*> many (pathArgument $ metavar "PATH" <> action "file")
                  <**> helper
                  & repositoryCommandP
              )
              (progDesc "Let the repository reflect the target file")
          )
        <> command
          "apply"
          ( info
              ( Dojang.Commands.Apply.apply
                  <$> switch
                    ( long "force"
                        <> short 'f'
                        <> help
                          ( "Apply changes even if there are undergone "
                              ++ "changes in the target tree too"
                          )
                    )
                  <*> many (pathArgument $ metavar "FILE" <> action "file")
                  <**> helper
                  & repositoryCommandP
              )
              (progDesc "Apply changes to target tree")
          )
        <> command
          "unmanage"
          ( info
              ( Dojang.Commands.TargetLifecycle.unmanage
                  <$> optional
                    ( pathOption
                        ( long "route"
                            <> metavar "ROUTE"
                            <> action "file"
                            <> help "Select orphan records produced by this route"
                        )
                    )
                  <*> many
                    (pathArgument $ metavar "DESTINATION" <> action "file")
                  <*> switch
                    ( long "force"
                        <> short 'f'
                        <> help
                          "Discard records even when destinations have diverged"
                    )
                  <**> helper
                  & repositoryCommandP
              )
              (progDesc "Stop managing orphan destinations without deleting them")
          )
        <> command
          "forget"
          ( info
              ( Dojang.Commands.TargetLifecycle.forget
                  <$> switch
                    ( long "force"
                        <> short 'f'
                        <> help
                          "Forget state even when destinations have diverged"
                    )
                  <**> helper
                  & repositoryCommandP
              )
              (progDesc "Forget this repository's machine-local state")
          )
        <> command
          "edit"
          ( info
              ( Dojang.Commands.Edit.edit
                  <$> optional
                    ( strOption
                        ( long "editor"
                            <> short 'E'
                            <> metavar "PROGRAM"
                            <> action "command"
                            <> help
                              ( "Editor program to use "
                                  ++ "(overrides $VISUAL and $EDITOR)"
                              )
                        )
                    )
                  <*> switch
                    ( long "no-apply"
                        <> short 'n'
                        <> help "Do not apply changes after editing"
                    )
                  <*> switch
                    ( long "force"
                        <> short 'f'
                        <> help "Skip conflict warnings and proceed"
                    )
                  <*> switch
                    ( long "sequential"
                        <> short 'S'
                        <> help "Edit files one at a time instead of all at once"
                    )
                  <*> switch
                    ( long "all"
                        <> short 'a'
                        <> help
                          "Edit all changed source files without prompting"
                    )
                  <*> switch
                    ( long "include-unregistered"
                        <> short 'u'
                        <> help
                          "Include unregistered files (prompts for route)"
                    )
                  <*> optional
                    ( pathOption
                        ( long "source"
                            <> short 's'
                            <> metavar "PATH"
                            <> action "file"
                            <> help "Explicit source path (skip disambiguation)"
                        )
                    )
                  <*> many (pathArgument $ metavar "PATH" <> action "file")
                  <**> helper
                  & repositoryCommandP
              )
              (progDesc "Edit source file of a target file and apply changes")
          )
    )
    <|> subparser
      ( commandGroup "Meta commands:"
          <> command
            "version"
            ( info
                (pure versionCmd <**> helper & metaCommandP)
                (progDesc "Show version")
            )
          <> command
            "help"
            ( info
                (helpP stateRoot defaultRepoPath & metaCommandP)
                (progDesc "Show help")
            )
      )


repositoryCommandP :: Parser a -> Parser (CommandMode, a)
repositoryCommandP = fmap ((,) repositoryCommandMode)


initializationCommandP :: Parser a -> Parser (CommandMode, a)
initializationCommandP = fmap ((,) initializationCommandMode)


migrationCommandP :: Parser a -> Parser (CommandMode, a)
migrationCommandP = fmap ((,) migrationCommandMode)


metaCommandP :: Parser a -> Parser (CommandMode, a)
metaCommandP = fmap ((,) metaCommandMode)


helpP
  :: (MonadFileSystem i, MonadIO i)
  => OsPath
  -> OsPath
  -- ^ The default repository path (from registry or current directory).
  -> Parser (App i ExitCode)
helpP stateRoot defaultRepoPath =
  helpCmd stateRoot defaultRepoPath
    <$> optional (argument str (metavar "COMMAND"))
    <**> helper


parser
  :: (MonadFileSystem i, MonadIO i)
  => OsPath
  -> OsPath
  -- ^ The default repository path (from registry or current directory).
  -> ParserInfo (ParsedApp i)
parser stateRoot defaultRepoPath =
  info
    (appP stateRoot defaultRepoPath <**> helper)
    ( fullDesc
        <> progDesc "Manage dotfiles"
        <> header "Dojang: A cross-platform dotfiles manager"
        <> footer "Distributed under the GPL 3.0 or later."
    )


parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs


versionCmd :: (MonadIO i) => App i ExitCode
versionCmd = do
  liftIO $ putStrLn $ "dojang " <> toString version
  return ExitSuccess


helpCmd
  :: forall i
   . (MonadFileSystem i, MonadIO i)
  => OsPath
  -> OsPath
  -- ^ The default repository path (from registry or current directory).
  -> Maybe String
  -> App i ExitCode
helpCmd stateRoot defaultRepoPath cmdString = do
  void $ liftIO $ handleParseResult result
  return ExitSuccess
 where
  args = maybeToList cmdString ++ ["--help"]
  result :: ParserResult (ParsedApp i)
  result = execParserPure parserPrefs (parser stateRoot defaultRepoPath) args


main :: IO ()
main = withCP65001 $ do
  when (System.Info.os == "mingw32") $ setLocaleEncoding utf8
  unresolvedStateRoot <- nativeStateRoot
  ParsedApp commandMode manifestExplicit allowRecursion unresolvedAppEnv _ <-
    liftIO $ customExecParser parserPrefs (parser unresolvedStateRoot period)
      :: IO (ParsedApp DryRunIO)
  stateRoot <-
    if commandMode.readsMachineState
      then canonicalizeStateRootOrExit unresolvedStateRoot
      else return unresolvedStateRoot
  let parsedAppEnv = unresolvedAppEnv{stateDirectory = stateRoot}
  when allowRecursion $ setEnv "DOJANG_ALLOW_HOOK_RECURSION" "1"
  when commandMode.readsMachineState $ validateMachineStateStoreOrExit stateRoot
  appEnv <-
    if not commandMode.autoSelectsRepository || parsedAppEnv.repositoryExplicit
      then return parsedAppEnv
      else do
        (selectedState, ambiguous) <-
          defaultRepository stateRoot
        let defaultRepoPath = maybe period (.checkoutPath) selectedState
        when ambiguous $ do
          printStderr' Error "More than one repository is registered on this machine."
          printStderr' Hint "Select one explicitly with -r/--repository-dir."
          exitWith ambiguousRepositoryExitCode
        when (defaultRepoPath == period) $ do
          homeDir <- OsPath.encodeFS =<< getHomeDirectory
          let registryPath = homeDir OsPath.</> registryFilename
          legacyRegistry <- readRegistry registryPath
          case legacyRegistry of
            Nothing -> return ()
            Just registry -> do
              pathStyle <- pathStyleFor stderr
              printStderr' Note $
                "The legacy registry points to "
                  <> pathStyle registry.repositoryPath
                  <> ", but it is no longer used for automatic selection."
              printStderr' Hint "Run `dojang -r PATH migrate' for that repository."
        case selectedState of
          Just state | manifestExplicit -> do
            manifestInsideCheckout <-
              automaticSelectionUsesCheckoutManifest state parsedAppEnv
            when (not manifestInsideCheckout) $ do
              printStderr' Error $
                "Cannot use a manifest outside an automatically selected "
                  <> "repository checkout."
              printStderr' Hint $
                "Select the repository explicitly with "
                  <> "-r/--repository-dir."
              exitWith ambiguousRepositoryExitCode
          _ -> return ()
        return $ case selectedState of
          Nothing -> parsedAppEnv
          Just state ->
            applyAutomaticRepositorySelection
              manifestExplicit
              state
              parsedAppEnv
  (exitCode, ops) <-
    if appEnv.dryRun
      then do
        ParsedApp _ _ _ _ cmd <-
          liftIO $
            customExecParser
              parserPrefs
              (parser stateRoot appEnv.sourceDirectory)
            :: IO (ParsedApp DryRunIO)
        dryRunIO' $ run appEnv cmd
      else do
        ParsedApp _ _ _ _ cmd <-
          liftIO $
            customExecParser
              parserPrefs
              (parser stateRoot appEnv.sourceDirectory)
            :: IO (ParsedApp IO)
        exitCode' <- run appEnv cmd
        return (exitCode', -1)
  codeColor <- codeStyleFor stderr
  when (appEnv.dryRun && ops > 0) $ do
    printStderr' Note $
      "Since "
        <> codeColor "--dry-run"
        <> " was specified, those "
        <> showt ops
        <> " changes were not actually committed to the filesystem."
  exitWith exitCode
 where
  run
    :: (MonadFileSystem i, MonadIO i)
    => AppEnv
    -> App i ExitCode
    -> i ExitCode
  run appEnv cmd = do
    (if appEnv.debug then runAppWithStderrLogging else runAppWithoutLogging)
      appEnv
      $ cmd
        `catchError` \e -> do
          printStderr' Error $
            "An unexpected error occurred; you may report this as a bug: "
              <> showt e
          return unhandledError


nativeStateRoot :: IO OsPath
nativeStateRoot = do
  home <- OsPath.encodeFS =<< getHomeDirectory
  xdg <- traverse OsPath.encodeFS =<< lookupEnv "XDG_DATA_HOME"
  local <- traverse OsPath.encodeFS =<< lookupEnv "LOCALAPPDATA"
  let operatingSystem = case System.Info.os of
        "linux" -> Linux
        "darwin" -> MacOS
        "mingw32" -> Windows
        other -> OtherOS $ fromString other
  return $ resolveStateRoot operatingSystem $ StateRootInputs home xdg local


defaultRepository :: OsPath -> IO (Maybe MachineState, Bool)
defaultRepository stateRoot = do
  machine <- readMachineId stateRoot
  case machine of
    Left err -> do
      printStderr' Error $ formatStateError err
      exitWith machineStateError
    Right Nothing -> return (Nothing, False)
    Right (Just machineId) -> do
      loaded <- listRepositoryStates stateRoot machineId
      states <- case loaded of
        Left err -> do
          printStderr' Error $ formatStateError err
          exitWith machineStateError
        Right values -> return values
      case selectRepositoryState states of
        NoRepositoryState -> return (Nothing, False)
        AmbiguousRepositoryStates -> return (Nothing, True)
        SelectedRepositoryState state -> do
          validated <- validateRepositoryCheckout state
          case validated of
            Left err -> do
              printStderr' Error $ formatStateError err
              exitWith machineStateError
            Right _ -> return (Just state, False)


validateMachineStateStoreOrExit :: OsPath -> IO ()
validateMachineStateStoreOrExit stateRoot = do
  validated <- validateMachineStateStore stateRoot
  case validated of
    Left err -> do
      printStderr' Error $ formatStateError err
      exitWith machineStateError
    Right () -> return ()


canonicalizeStateRootOrExit :: OsPath -> IO OsPath
canonicalizeStateRootOrExit stateRoot = do
  resolved <- canonicalizeStateRoot stateRoot
  case resolved of
    Left err -> do
      printStderr' Error $ formatStateError err
      exitWith machineStateError
    Right path' -> return path'
