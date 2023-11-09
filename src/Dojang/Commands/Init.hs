{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.Init
  ( InitPreset (..)
  , init
  , initPresetEnvironment
  , initPresetName
  ) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (second))
import Data.Either (rights)
import Data.String (IsString)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import System.Info qualified (os)
import Prelude hiding (init)

import Control.Monad.Logger (logDebugSH, logInfo)
import Control.Monad.Reader (asks)
import Data.CaseInsensitive (CI (original))
import Data.HashMap.Strict qualified as HashMap (fromList)
import Data.Map.Strict
  ( Map
  , fromList
  , fromListWith
  , keys
  , member
  , toAscList
  , (!)
  )
import Data.Map.Strict qualified as Map (empty, singleton, union)
import Data.Set (singleton, size, toAscList, union)
import Data.Text (Text, lines, pack, unlines)
import FortyTwo.Prompts.Multiselect (multiselect)
import System.OsPath ((</>))

import Dojang.App
  ( App
  , AppEnv (debug, dryRun, sourceDirectory)
  , doesManifestExist
  , saveManifest
  )
import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , die'
  , pathStyleFor
  , printStderr
  , printStderr'
  )
import Dojang.ExitCodes (manifestAlreadyExists, unsupportedOnEnvError)
import Dojang.MonadFileSystem (FileType (..), MonadFileSystem (..))
import Dojang.Syntax.Manifest.Writer (writeManifest)
import Dojang.Types.Environment (Architecture (..), OperatingSystem (..))
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.FilePathExpression (FilePathExpression (..), (+/+))
import Dojang.Types.FileRoute (FileRoute (fileType), fileRoute)
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.MonikerName (parseMonikerName)


data InitPreset
  = Amd64Linux
  | Arm64Linux
  | AppleSiliconMac
  | IntelMac
  | Win64
  | WinArm64
  deriving (Show, Eq, Ord, Enum, Bounded)


initPresetName :: (IsString a) => InitPreset -> a
initPresetName = \case
  Amd64Linux -> "Linux (AMD64)"
  Arm64Linux -> "Linux (ARM64)"
  AppleSiliconMac -> "Apple silicon Mac (M series)"
  IntelMac -> "Intel Mac"
  Win64 -> "Windows (64-bit)"
  WinArm64 -> "Windows (ARM64)"


initPresetEnvironment :: InitPreset -> (OperatingSystem, Architecture)
initPresetEnvironment = \case
  Amd64Linux -> (Linux, X86_64)
  Arm64Linux -> (Linux, AArch64)
  AppleSiliconMac -> (MacOS, AArch64)
  IntelMac -> (MacOS, X86_64)
  Win64 -> (Windows, X86_64)
  WinArm64 -> (Windows, AArch64)


init :: (MonadFileSystem i, MonadIO i) => [InitPreset] -> Bool -> App i ExitCode
init presets noInteractive = do
  manifestExists <- doesManifestExist
  when (manifestExists) $ do
    die' manifestAlreadyExists "Manifest already exists."
  $(logInfo) "No manifest found."
  when (System.Info.os == "mingw32" && not noInteractive) $ do
    codeStyle <- codeStyleFor stderr
    printStderr' Error
      $ "We are sorry, but interactive mode is currently not supported on "
      <> "Windows.  See the relevant issue:\n"
      <> "\n  https://github.com/dahlia/dojang/issues/4\n"
    printStderr' Hint
      $ ("Please use the " <> codeStyle "--linux-*" <> "/")
      <> (codeStyle "--macos-*" <> "/" <> codeStyle "--windows-*")
      <> " options with the "
      <> (codeStyle "-I" <> "/" <> codeStyle "--no-interactive")
      <> " flag.  See also "
      <> (codeStyle "-h" <> "/" <> codeStyle "--help")
      <> " for command-line options."
    liftIO $ exitWith unsupportedOnEnvError
  presets' <-
    if null presets && not noInteractive
      then askPresets
      else pure presets
  $(logDebugSH) presets'
  -- FIXME: The below code is overly complicated.  It should be simplified,
  -- through letting it be more imperative, I think?
  let environments = second singleton . initPresetEnvironment <$> presets'
  let oses = (`fromListWith` environments) union
  let monikerNames =
        fromList
          [ ((os', arch'), name)
          | (os', arch', Right name) <-
              [ (os, Nothing, parseMonikerName $ original os.identifier)
              | os <- keys oses
              ]
                ++ [ ( os
                     , Just arch
                     , parseMonikerName
                        $ original
                        $ os.identifier
                        <> "-"
                        <> arch.identifier
                     )
                   | (os, archSet) <- Data.Map.Strict.toAscList oses
                   , arch <- Data.Set.toAscList archSet
                   ]
          ]
  let posixName = head $ rights [parseMonikerName "posix"]
  let needsPosix = Linux `member` oses && MacOS `member` oses
  let monikers =
        HashMap.fromList
          $ [ (monikerNames ! (os, Nothing), OperatingSystem os)
            | os <- keys oses
            ]
          ++ [ ( monikerNames ! (os, Just arch)
               , And
                  [ Moniker $ monikerNames ! (os, Nothing)
                  , Architecture arch
                  ]
               )
             | (os, archSet) <- Data.Map.Strict.toAscList oses
             , Data.Set.size archSet > 1
             , arch <- Data.Set.toAscList archSet
             ]
          ++ [ ( posixName
               , Or
                  [ Moniker $ monikerNames ! (Linux, Nothing)
                  , Moniker $ monikerNames ! (MacOS, Nothing)
                  ]
               )
             | needsPosix
             ]
  let route = fileRoute monikers
  let dirRoute = (`route` Directory)
  homePath <- encodePath "HOME"
  let homeRoutes =
        Map.singleton
          homePath
          ( dirRoute
              $ [ ( monikerNames ! (Windows, Nothing)
                  , Just windowsUserProfile
                  )
                | Windows `member` oses
                ]
              ++ [ (posixName, Just posixHome)
                 | Linux `member` oses || MacOS `member` oses
                 ]
          )
  xdgConfigHomePath <- encodePath "XDG_CONFIG_HOME"
  let xdgConfigHomRoutes =
        if Linux `member` oses || MacOS `member` oses
          then
            Map.singleton
              xdgConfigHomePath
              ( dirRoute
                  $ [ ( monikerNames ! (Windows, Nothing)
                      , Just windowsXdgConfigHome
                      )
                    | Windows `member` oses
                    ]
                  ++ [ (posixName, Just posixXdgConfigHome)
                     | Linux `member` oses || MacOS `member` oses
                     ]
              )
          else Map.empty
  applicationSupportPath <- encodePath "ApplicationSupport"
  let applicationSupportRoutes =
        if MacOS `member` oses
          then
            Map.singleton
              applicationSupportPath
              ( dirRoute
                  $ [ ( monikerNames ! (MacOS, Nothing)
                      , Just macosApplicationSupport
                      )
                    | MacOS `member` oses
                    ]
                  ++ [ ( monikerNames ! (Linux, Nothing)
                       , Just posixXdgConfigHome
                       )
                     | Linux `member` oses
                     ]
                  ++ [ ( monikerNames ! (Windows, Nothing)
                       , Just windowsAppData
                       )
                     | Windows `member` oses
                     ]
              )
          else Map.empty
  appDataPath <- encodePath "AppData"
  let appDataRoutes =
        if Windows `member` oses
          then
            Map.singleton
              appDataPath
              ( dirRoute
                  $ [ ( monikerNames ! (Windows, Nothing)
                      , Just windowsAppData
                      )
                    | Windows `member` oses
                    ]
                  ++ [ (posixName, Just posixXdgConfigHome)
                     | Linux `member` oses || MacOS `member` oses
                     ]
              )
          else Map.empty
  let fileRoutes =
        homeRoutes
          `Map.union` xdgConfigHomRoutes
          `Map.union` applicationSupportRoutes
          `Map.union` appDataRoutes
  let manifest = Manifest monikers fileRoutes [(homePath, ["*"])]
  repoDir <- asks (.sourceDirectory)
  pathStyle <- pathStyleFor stderr
  forM_ (Data.Map.Strict.toAscList manifest.fileRoutes) $ \(path, route') -> do
    when (route'.fileType == Directory) $ do
      let dirPath = repoDir </> path
      createDirectories dirPath
      dirPath' <- decodePath dirPath
      printStderr $ "Directory created: " <> pathStyle (pack dirPath') <> "."
  filename <- saveManifest manifest
  filename' <- decodePath filename
  printStderr $ "Manifest created: " <> pathStyle (pack filename') <> "."
  debug' <- asks (.debug)
  dryRun' <- asks (.dryRun)
  when (debug' || dryRun') $ do
    let manifestText = indent $ writeManifest manifest
    printStderr' Note
      $ "The manifest file looks like below:\n\n"
      <> manifestText
      <> "\n"
  return ExitSuccess


posixHome :: FilePathExpression
posixHome = Substitution "HOME"


windowsUserProfile :: FilePathExpression
windowsUserProfile = Substitution "UserProfile"


posixXdgConfigHome :: FilePathExpression
posixXdgConfigHome =
  SubstitutionWithDefault "XDG_CONFIG_HOME" $ posixHome +/+ ".config"


windowsXdgConfigHome :: FilePathExpression
windowsXdgConfigHome =
  SubstitutionWithDefault "XDG_CONFIG_HOME" $ windowsUserProfile +/+ ".config"


macosApplicationSupport :: FilePathExpression
macosApplicationSupport =
  Substitution "HOME" +/+ "Library" +/+ "Application Support"


windowsAppData :: FilePathExpression
windowsAppData = Substitution "AppData"


askPresets :: (MonadIO i) => App i [InitPreset]
askPresets = do
  -- FIXME: It doesn't seem to work on Windows...
  -- https://github.com/GianlucaGuarini/fortytwo/issues/7
  presets <-
    multiselect
      "What operating systems and architectures do you use?"
      (map initPresetName [minBound .. maxBound])
  return [namesToPresets ! name | name <- presets]
 where
  namesToPresets :: Map String InitPreset
  namesToPresets =
    fromList
      [(initPresetName preset, preset) | preset <- [minBound .. maxBound]]


indent :: Text -> Text
indent = Data.Text.unlines . map ("  " <>) . Data.Text.lines
