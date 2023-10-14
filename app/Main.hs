{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative (Alternative ((<|>)), optional, (<**>))
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (catMaybes, maybeToList)
import Data.Version (showVersion)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Exit (ExitCode (..), exitWith)
import System.IO.CodePage (withCP65001)
import System.Info (os)

import Options.Applicative
  ( Parser
  , ParserInfo
  , ParserPrefs
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
  , strOption
  , subparser
  , switch
  , value
  )
import Paths_dojang qualified as Meta

import Dojang.App
  ( App
  , AppEnv (..)
  , runAppWithStderrLogging
  , runAppWithoutLogging
  )
import Dojang.Commands.Env qualified (env)
import Dojang.Commands.Init (InitPreset (..), initPresetName)
import Dojang.Commands.Init qualified (init)


appP :: Parser (AppEnv, App ExitCode)
appP = do
  sourceDirectory' <-
    strOption
      ( long "source-dir"
          <> short 's'
          <> metavar "PATH"
          <> value "."
          <> showDefault
          <> help "Source tree directory"
      )
  manifestFile' <-
    strOption
      ( long "manifest-file"
          <> short 'm'
          <> metavar "PATH"
          <> value "dojang.toml"
          <> showDefault
          <> help
            ( "Manifest file.  Relative to source tree directory "
                ++ "(-s/--source-dir) if not absolute"
            )
      )
  envFile' <-
    strOption
      ( long "env-file"
          <> short 'e'
          <> metavar "PATH"
          <> value "dojang-env.toml"
          <> showDefault
          <> help
            ( "Environment file.  Relative to source tree directory "
                ++ "(-s/--source-dir) if not absolute.  It will be silently "
                ++ "ignored if the file does not exist"
            )
      )
  debug' <- switch (long "debug" <> short 'd' <> help "Enable debug logging")
  cmd <- cmdP
  return (AppEnv sourceDirectory' manifestFile' envFile' debug', cmd)


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


cmdP :: Parser (App ExitCode)
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
                  <*> strOption
                    ( long "output-file"
                        <> short 'o'
                        <> metavar "PATH"
                        <> value "-"
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


helpP :: Parser (App ExitCode)
helpP =
  helpCmd
    <$> optional (argument str (metavar "COMMAND"))
    <**> helper


parser :: ParserInfo (AppEnv, App ExitCode)
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


versionCmd :: App ExitCode
versionCmd = do
  liftIO $ putStrLn $ "dojang " <> showVersion Meta.version
  return ExitSuccess


helpCmd :: Maybe String -> App ExitCode
helpCmd cmdString = do
  let args = maybeToList cmdString ++ ["--help"]
  let result = execParserPure parserPrefs parser args
  void $ liftIO $ handleParseResult result
  return ExitSuccess


main :: IO ()
main = withCP65001 $ do
  when (System.Info.os == "mingw32") $ setLocaleEncoding utf8
  (appEnv, cmd) <- customExecParser parserPrefs parser
  exitCode <-
    (if appEnv.debug then runAppWithStderrLogging else runAppWithoutLogging)
      appEnv
      cmd
  exitWith exitCode
