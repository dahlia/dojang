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
  , initPresetName
  ) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function ((&))
import Data.List (maximumBy, sortOn)
import Data.List.NonEmpty as NonEmpty (NonEmpty ((:|)), toList)
import Data.Ord (comparing)
import Data.String (IsString)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Info qualified (os)
import Prelude hiding (init, writeFile)

import Control.Monad.Logger (logDebugSH, logInfo)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as HashMap
  ( filterWithKey
  , lookup
  , member
  , toList
  )
import Data.Map.Strict
  ( Map
  , findWithDefault
  , fromList
  , fromListWith
  , keys
  , toAscList
  , (!)
  )
import Data.Map.Strict qualified as Map (toList)
import Data.Set (Set)
import Data.Set qualified as Set (member, null, toAscList)
import Data.Text (Text, lines, unlines)
import FortyTwo.Prompts.Multiselect (multiselect)
import System.FilePattern (FilePattern)
import System.OsPath (OsPath, encodeFS, (</>))

import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
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
import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (..)
  , Kernel (..)
  , OperatingSystem (..)
  )
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.EnvironmentPredicate.Evaluate (evaluate)
import Dojang.Types.EnvironmentPredicate.Specificity (specificity)
import Dojang.Types.FilePathExpression (FilePathExpression (..), (+/+))
import Dojang.Types.FileRoute (FileRoute (fileType), fileRoute)
import Dojang.Types.FileRouteMap (FileRouteMap)
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (MonikerName, parseMonikerName)


data InitPreset
  = Amd64Linux
  | Arm64Linux
  | AppleSiliconMac
  | IntelMac
  | Win64
  | WinArm64
  | Wsl2
  deriving (Show, Eq, Ord, Enum, Bounded)


initPresetName :: (IsString a) => InitPreset -> a
initPresetName = \case
  Amd64Linux -> "Linux (AMD64)"
  Arm64Linux -> "Linux (ARM64)"
  AppleSiliconMac -> "Apple silicon Mac (M series)"
  IntelMac -> "Intel Mac"
  Win64 -> "Windows (64-bit)"
  WinArm64 -> "Windows (ARM64)"
  Wsl2 -> "Linux (WSL2)"


toEnvironment :: InitPreset -> Environment
toEnvironment = \case
  Amd64Linux -> Environment Linux X86_64 (Kernel "Linux" "0.0.0.0")
  Arm64Linux -> Environment Linux AArch64 (Kernel "Linux" "0.0.0.0")
  AppleSiliconMac -> Environment MacOS AArch64 (Kernel "Darwin" "0.0.0")
  IntelMac -> Environment MacOS X86_64 (Kernel "Darwin" "0.0.0")
  Win64 -> Environment Windows X86_64 (Kernel "Windows" "0.0.0.0")
  WinArm64 -> Environment Windows AArch64 (Kernel "Windows" "0.0.0.0")
  Wsl2 ->
    Environment
      Linux
      X86_64
      (Kernel "Linux" "0.0.0.0-microsoft-standard-WSL2")


monikers :: MonikerMap
monikers =
  [ (moniker "linux", OperatingSystem Linux)
  , (moniker "macos", OperatingSystem MacOS)
  , (moniker "windows", OperatingSystem Windows)
  , (moniker "posix", Or [OperatingSystem Linux, OperatingSystem MacOS])
  , (moniker "linux-amd64", And [OperatingSystem Linux, Architecture X86_64])
  , (moniker "linux-arm64", And [OperatingSystem Linux, Architecture AArch64])
  , (moniker "macos-intel", And [OperatingSystem MacOS, Architecture X86_64])
  ,
    ( moniker "macos-apple-silicon"
    , And [OperatingSystem MacOS, Architecture AArch64]
    )
  ,
    ( moniker "windows-amd64"
    , And [OperatingSystem Windows, Architecture X86_64]
    )
  ,
    ( moniker "windows-arm64"
    , And [OperatingSystem Windows, Architecture AArch64]
    )
  ,
    ( moniker "linux-wsl"
    , And
        [ OperatingSystem Linux
        , KernelReleaseSuffix "-microsoft-standard-WSL2"
        ]
    )
  ]


moniker :: Text -> MonikerName
moniker name = case parseMonikerName name of
  Right n -> n
  Left _ -> error $ "Invalid moniker name: " <> show name


selectMonikers :: Environment -> [MonikerName]
selectMonikers env =
  [ (name, predicate)
  | (name, predicate) <- HashMap.toList monikers
  , fst $ evaluate env monikers predicate
  ]
    & sortOn (specificity (`HashMap.lookup` monikers) . snd)
    & map fst


listNeededMonikers :: [InitPreset] -> [(InitPreset, NonEmpty MonikerName)]
listNeededMonikers presets =
  [ (preset, m :| ms'')
  | (preset, ms) <- selectedMonikers
  , let (havingDups, notHavingDups) = span (hasDuplicateMonikers preset) ms
  , let ms' = havingDups ++ take 1 notHavingDups
  , (m : ms'') <- [ms']
  ]
 where
  selectedMonikers :: [(InitPreset, [MonikerName])]
  selectedMonikers =
    [(preset, selectMonikers $ toEnvironment preset) | preset <- presets]
  hasDuplicateMonikers :: InitPreset -> MonikerName -> Bool
  hasDuplicateMonikers preset moniker' =
    (not . null)
      [ True
      | (p, ms) <- selectedMonikers
      , p /= preset
      , m <- ms
      , m == moniker'
      ]


path :: String -> OsPath
path = unsafePerformIO . encodeFS


routes
  :: Map OsPath ([(Set MonikerName, FilePathExpression)], Set OperatingSystem)
routes =
  [
    ( path "HOME"
    ,
      (
        [ ([moniker "posix", moniker "linux", moniker "macos"], posixHome)
        , ([moniker "windows"], windowsUserProfile)
        ]
      , [Linux, MacOS, Linux]
      )
    )
  ,
    ( path "XDG_CONFIG_HOME"
    ,
      (
        [
          ( [moniker "posix", moniker "linux", moniker "macos"]
          , posixXdgConfigHome
          )
        , ([moniker "windows"], windowsXdgConfigHome)
        ]
      , [Linux, MacOS]
      )
    )
  ,
    ( path "Application Support"
    ,
      (
        [ ([moniker "macos"], macosApplicationSupport)
        , ([moniker "linux"], posixXdgConfigHome)
        , ([moniker "windows"], windowsAppData)
        ]
      , [MacOS]
      )
    )
  ,
    ( path "AppData"
    ,
      (
        [ ([moniker "windows"], windowsAppData)
        ,
          ( [moniker "posix", moniker "linux", moniker "macos"]
          , posixXdgConfigHome
          )
        ]
      , [Windows]
      )
    )
  ]


ignorePatterns' :: Map OsPath [FilePattern]
ignorePatterns' =
  [ (path "HOME", ["*"])
  , (path "Application Support", ["*"])
  , (path "AppData", ["*"])
  ]


makeRouteMap
  :: [(InitPreset, NonEmpty MonikerName)] -> MonikerMap -> FileRouteMap
makeRouteMap neededMonikers monikers' =
  fromList
    [ (path', fileRoute monikers' (makePairs route) Directory)
    | (path', (route, requiredOses)) <- Map.toList routes
    , any
        ((`Set.member` requiredOses) . (.operatingSystem) . toEnvironment . fst)
        neededMonikers
    ]
 where
  monikerScoreTable :: Map MonikerName Int
  monikerScoreTable =
    fromListWith (+)
      $ zip (concatMap (NonEmpty.toList . snd) neededMonikers) [1, 1 ..]
  makePairs
    :: [(Set MonikerName, FilePathExpression)]
    -> [(MonikerName, Maybe FilePathExpression)]
  makePairs route =
    [ ( maximumBy
          ( comparing $ \k ->
              ( findWithDefault 0 k monikerScoreTable
              , specificity (`HashMap.lookup` monikers') $ Moniker k
              )
          )
          (Set.toAscList names)
      , Just expr
      )
    | (names, expr) <- route
    , not $ Set.null names
    , any (`HashMap.member` monikers') $ Set.toAscList names
    ]


makeManifest :: [InitPreset] -> Manifest
makeManifest presets =
  Manifest monikers' routeMap
    $ fromList
      [ (p, ignores)
      | p <- keys routeMap
      , let ignores = findWithDefault [] p ignorePatterns'
      , not $ null ignores
      ]
 where
  neededMonikers :: [(InitPreset, NonEmpty MonikerName)]
  neededMonikers = listNeededMonikers presets
  monikers' :: MonikerMap
  monikers' =
    monikers
      & HashMap.filterWithKey
        ( \k _ ->
            k `elem` [m | (_, ms) <- neededMonikers, m <- NonEmpty.toList ms]
        )
  routeMap :: FileRouteMap
  routeMap = makeRouteMap neededMonikers monikers'


init :: (MonadFileSystem i, MonadIO i) => [InitPreset] -> Bool -> App i ExitCode
init presets noInteractive = do
  manifestExists <- doesManifestExist
  when manifestExists $ do
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
  let manifest = makeManifest presets'
  repoDir <- asks (.sourceDirectory)
  pathStyle <- pathStyleFor stderr
  forM_ (Data.Map.Strict.toAscList manifest.fileRoutes) $ \(path', route') -> do
    when (route'.fileType == Directory) $ do
      let dirPath = repoDir </> path'
      createDirectories dirPath
      printStderr $ "Directory created: " <> pathStyle dirPath <> "."
  filename <- saveManifest manifest
  printStderr $ "Manifest created: " <> pathStyle filename <> "."
  generateGitIgnore
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


generateGitIgnore :: (MonadFileSystem i, MonadIO i) => App i ()
generateGitIgnore = do
  sourceDir <- asks (.sourceDirectory)
  gitDir <- encodePath ".git"
  gitDirExists <- isDirectory (sourceDir </> gitDir)
  gitIgnore <- encodePath ".gitignore"
  gitIgnoreExists <- isFile (sourceDir </> gitIgnore)
  when (gitDirExists && not gitIgnoreExists) $ do
    writeFile gitIgnore gitIgnoreContents
    printStderr ".gitignore created"


gitIgnoreContents :: ByteString
gitIgnoreContents = encodeUtf8 "# Generated by Dojang\n.dojang/\n"
