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

import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (Bifunctor (second))
import Data.Either (rights)
import Data.String (IsString)
import System.Exit (ExitCode (..))
import Prelude hiding (init)

import Control.Monad.Logger (logDebugSH, logInfo)
import Data.CaseInsensitive (CI (original))
import Data.Map.Strict
  ( Map
  , fromList
  , fromListWith
  , keys
  , member
  , toAscList
  , (!)
  )
import Data.Set (singleton, size, toAscList, union)
import Data.Text (pack)
import FortyTwo.Prompts.Multiselect (multiselect)

import Dojang.App (App, doesManifestExist, saveManifest)
import Dojang.Commands (Admonition (Error), printStderr, printStderr')
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Environment (Architecture (..), OperatingSystem (..))
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.FilePathExpression (FilePathExpression (..), (+/+))
import Dojang.Types.FileRoute (FileType (..), fileRoute)
import Dojang.Types.FileRouteMap qualified as FileRouteMap
  ( empty
  , singleton
  , union
  )
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.MonikerMap qualified as MonikerMap (fromList)
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
  if manifestExists
    then do
      printStderr' Error "Manifest already exists."
      return $ ExitFailure 1
    else do
      $(logInfo) "No manifest found."
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
            MonikerMap.fromList
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
            FileRouteMap.singleton
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
                FileRouteMap.singleton
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
              else FileRouteMap.empty
      applicationSupportPath <- encodePath "ApplicationSupport"
      let applicationSupportRoutes =
            if MacOS `member` oses
              then
                FileRouteMap.singleton
                  applicationSupportPath
                  ( dirRoute
                      $ [ ( monikerNames ! (MacOS, Nothing)
                          , Just macosApplicationSupport
                          )
                        | MacOS `member` oses
                        ]
                      ++ [ (monikerNames ! (Linux, Nothing), Nothing)
                         | Windows `member` oses
                         ]
                      ++ [ ( monikerNames ! (Windows, Nothing)
                           , Just windowsAppData
                           )
                         | Windows `member` oses
                         ]
                  )
              else FileRouteMap.empty
      appDataPath <- encodePath "AppData"
      let appDataRoutes =
            if Windows `member` oses
              then
                FileRouteMap.singleton
                  appDataPath
                  ( dirRoute
                      $ [ ( monikerNames ! (Windows, Nothing)
                          , Just windowsAppData
                          )
                        | Windows `member` oses
                        ]
                      ++ [ (posixName, Nothing)
                         | Linux `member` oses || MacOS `member` oses
                         ]
                  )
              else FileRouteMap.empty
      let fileRoutes =
            homeRoutes
              `FileRouteMap.union` xdgConfigHomRoutes
              `FileRouteMap.union` applicationSupportRoutes
              `FileRouteMap.union` appDataRoutes
      let manifest = Manifest monikers fileRoutes
      filename <- saveManifest manifest
      filename' <- decodePath filename
      printStderr $ "Manifest created: " <> pack filename'
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


askPresets :: (MonadFileSystem i, MonadIO i) => App i [InitPreset]
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
