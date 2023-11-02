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
import Data.Maybe (catMaybes, maybeToList)
import Data.Version (showVersion)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
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
  , argument
  , command
  , commandGroup
  , customExecParser
  , defaultPrefs
  , execParserPure
  , flag
  , footer
  , fullDesc
  , handleParseResult
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , progDesc
  , short
  , showDefault
  , str
  , subparser
  , switch
  , value
  )
import Options.Applicative.Path (hyphen, pathOption, period)
import System.OsPath (OsPath, encodeFS)
import TextShow (TextShow (showt))

import Dojang.App
  ( App
  , AppEnv (..)
  , runAppWithStderrLogging
  , runAppWithoutLogging
  )
import Dojang.Commands (Admonition (Error, Note), codeStyleFor, printStderr')
import Dojang.Commands.Env qualified (env)
import Dojang.Commands.Init (InitPreset (..), initPresetName)
import Dojang.Commands.Init qualified (init)
import Dojang.Commands.Status qualified (status)
import Dojang.ExitCodes (unhandledError)
import Dojang.MonadFileSystem (DryRunIO, MonadFileSystem, dryRunIO)
import Paths_dojang qualified as Meta


intermediateDirname :: OsPath
intermediateDirname = unsafePerformIO $ encodeFS ".dojang"
{-# NOINLINE intermediateDirname #-}


manifestFilename :: OsPath
manifestFilename = unsafePerformIO $ encodeFS "dojang.toml"
{-# NOINLINE manifestFilename #-}


envFilename :: OsPath
envFilename = unsafePerformIO $ encodeFS "dojang-env.toml"
{-# NOINLINE envFilename #-}


appP :: (MonadFileSystem i, MonadIO i) => Parser (AppEnv, App i ExitCode)
appP = do
  sourceDirectory' <-
    pathOption
      ( long "source-dir"
          <> short 's'
          <> metavar "PATH"
          <> value period
          <> showDefault
          <> help "Source tree directory"
      )
  intermediateDirectory' <-
    pathOption
      ( long "intermediate-dir"
          <> short 'i'
          <> metavar "PATH"
          <> value intermediateDirname
          <> showDefault
          <> help
            ( "Intermediate directory which is managed by Dojang.  "
                ++ "Relative to source tree directory (-s/--source-dir) "
                ++ "if not absolute"
            )
      )
  manifestFile' <-
    pathOption
      ( long "manifest-file"
          <> short 'm'
          <> metavar "PATH"
          <> value manifestFilename
          <> showDefault
          <> help
            ( "Manifest file.  Relative to source tree directory "
                ++ "(-s/--source-dir) if not absolute"
            )
      )
  envFile' <-
    pathOption
      ( long "env-file"
          <> short 'e'
          <> metavar "PATH"
          <> value envFilename
          <> showDefault
          <> help
            ( "Environment file.  Relative to source tree directory "
                ++ "(-s/--source-dir) if not absolute.  It will be silently "
                ++ "ignored if the file does not exist"
            )
      )
  dryRun' <-
    switch
      ( long "dry-run"
          <> help
            ( "Do not actually perform actions, but just print them.  "
                ++ "This option probably lets the program run much slower"
            )
      )
  debug' <- switch (long "debug" <> short 'd' <> help "Enable debug logging")
  cmd <- cmdP
  return
    ( AppEnv
        sourceDirectory'
        intermediateDirectory'
        manifestFile'
        envFile'
        dryRun'
        debug'
    , cmd
    )


initPresetP :: Parser [InitPreset]
initPresetP =
  (\a b c d e f' -> catMaybes [a, b, c, d, e, f'])
    <$> f Amd64Linux ("linux-amd64", "linux-x86-64")
    <*> f Arm64Linux ("linux-aarch64", "linux-arm64")
    <*> f AppleSiliconMac ("macos-aarch64", "apple-silicon-mac")
    <*> f IntelMac ("macos-amd64", "intel-mac")
    <*> f Win64 ("windows-amd64", "win64")
    <*> f WinArm64 ("windows-arm64", "win-arm64")
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


cmdP :: (MonadFileSystem i, MonadIO i) => Parser (App i ExitCode)
cmdP =
  subparser
    ( commandGroup "Managing commands:"
        <> command
          "init"
          ( info
              ( Dojang.Commands.Init.init
                  <$> initPresetP
                  <*> switch
                    ( long "no-interactive"
                        <> short 'I'
                        <> help "Do not prompt for anything"
                    )
                  <**> helper
              )
              (progDesc "Initialize repository")
          )
        <> command
          "env"
          ( info
              ( Dojang.Commands.Env.env
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
                        <> help
                          ( "Output file path.  Use - for stdout, "
                              ++ "and ./- for a file named -"
                          )
                    )
                  <**> helper
              )
              ( progDesc
                  $ "Show environment in TOML format, "
                  ++ "which can be used with -e/--env-file"
              )
          )
        <> command
          "status"
          ( info
              ( Dojang.Commands.Status.status
                  <$> switch
                    ( long "no-trailing-slash"
                        <> short 'S'
                        <> help
                          ( "Do not append trailing slash (or backslash "
                              ++ "on Windows) to directory paths"
                          )
                    )
                  <**> helper
              )
              (progDesc "Show status of repository and target tree")
          )
    )
    <|> subparser
      ( commandGroup "Meta commands:"
          <> command
            "version"
            ( info
                (pure versionCmd <**> helper)
                (progDesc "Show version")
            )
          <> command "help" (info helpP (progDesc "Show help"))
      )


helpP :: (MonadFileSystem i, MonadIO i) => Parser (App i ExitCode)
helpP =
  helpCmd
    <$> optional (argument str (metavar "COMMAND"))
    <**> helper


parser :: (MonadFileSystem i, MonadIO i) => ParserInfo (AppEnv, App i ExitCode)
parser =
  info
    (appP <**> helper)
    ( fullDesc
        <> progDesc "Manage dotfiles"
        <> header "Dojang: A cross-platform dotfiles manager"
        <> footer "Distributed under the GPL 3.0 or later."
    )


parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs


versionCmd :: (MonadIO i) => App i ExitCode
versionCmd = do
  liftIO $ putStrLn $ "dojang " <> showVersion Meta.version
  return ExitSuccess


helpCmd
  :: forall i. (MonadFileSystem i, MonadIO i) => Maybe String -> App i ExitCode
helpCmd cmdString = do
  void $ liftIO $ handleParseResult result
  return ExitSuccess
 where
  args = maybeToList cmdString ++ ["--help"]
  result :: ParserResult (AppEnv, App i ExitCode)
  result = execParserPure parserPrefs parser args


main :: IO ()
main = withCP65001 $ do
  when (System.Info.os == "mingw32") $ setLocaleEncoding utf8
  (appEnv, _) <-
    liftIO $ customExecParser parserPrefs parser
      :: IO (AppEnv, App DryRunIO ExitCode)
  exitCode <- if appEnv.dryRun then dryRunIO $ run appEnv else run appEnv
  codeColor <- codeStyleFor stderr
  when appEnv.dryRun
    $ printStderr' Note
    $ "Since `"
    <> codeColor "--dry-run"
    <> "' was specified, no changes were committed "
    <> "to the file system."
  exitWith exitCode
 where
  run :: (MonadFileSystem i, MonadIO i) => AppEnv -> i ExitCode
  run appEnv = do
    (_, cmd) <- liftIO $ customExecParser parserPrefs parser
    (if appEnv.debug then runAppWithStderrLogging else runAppWithoutLogging)
      appEnv
      $ cmd
      `catchError` \e -> do
        printStderr' Error
          $ "An unexpected error occurred; you may report this as a bug: "
          <> showt e
        return unhandledError
