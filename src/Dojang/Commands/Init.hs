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
  , initWithFacts
  , initPresetName
  , referencedMachineFacts
  , requiredMachineFacts
  ) where

import Control.Monad (foldM, forM, forM_, void, when)
import Control.Monad.Except (MonadError (catchError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function ((&))
import Data.List (maximumBy, sortOn)
import Data.List.NonEmpty as NonEmpty (NonEmpty ((:|)), toList)
import Data.Ord (comparing)
import Data.String (IsString, fromString)
import Data.Time (getCurrentTime)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Info qualified (os)
import Prelude hiding (init)

import Control.Monad.Logger (logDebugSH, logInfo, logWarn)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as HashMap
  ( filterWithKey
  , lookup
  , member
  , toList
  )
import Data.Map.Strict
  ( Map
  , empty
  , findWithDefault
  , fromList
  , fromListWith
  , keys
  , toAscList
  , (!)
  )
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, lines, unlines)
import Data.Text qualified as Text
import FortyTwo.Prompts.Input (input)
import FortyTwo.Prompts.Multiselect (multiselect)
import System.FilePattern (FilePattern)
import System.OsPath
  ( OsPath
  , encodeFS
  , isAbsolute
  , makeRelative
  , normalise
  , (</>)
  )
import TextShow (FromStringShow (FromStringShow), TextShow (showt))

import Dojang.App
  ( App
  , AppEnv (debug, dryRun, sourceDirectory, stateDirectory)
  , currentEnvironmentWithFacts
  , doesManifestExist
  , ensureManifest
  , ensureNoLegacySnapshotForInitialization
  , prepareMachineState
  , prepareNewMachineStateBeforeMigration
  , readExistingMachineState
  , readValidatedLegacyRegistry
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
import Dojang.ExitCodes
  ( cliError
  , envFileReadError
  , fileWriteError
  , machineStateError
  , manifestAlreadyExists
  , missingMachineFactError
  , unsupportedOnEnvError
  )
import Dojang.MonadFileSystem (FileType (..), MonadFileSystem (..))
import Dojang.Syntax.Env (readFactsFile)
import Dojang.Syntax.Manifest.Writer (formatWriteError, writeManifest)
import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (..)
  , FactKey
  , FactMap
  , Kernel (..)
  , OperatingSystem (..)
  , emptyEnvironment
  , factKeyText
  , isBuiltInFact
  , lookupFact
  , parseFactKey
  )
import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (..)
  , normalizePredicate
  , referencedFacts
  )
import Dojang.Types.EnvironmentPredicate.Evaluate (evaluate)
import Dojang.Types.EnvironmentPredicate.Specificity (specificity)
import Dojang.Types.FilePathExpression (FilePathExpression (..), (+/+))
import Dojang.Types.FileRoute (FileRoute (fileType, predicates), fileRoute)
import Dojang.Types.FileRouteMap (FileRouteMap)
import Dojang.Types.Hook (Hook (condition))
import Dojang.Types.MachineState
  ( MachineState (..)
  , catchStateIOErrors
  , formatStateError
  , manifestIdentityLockPath
  , updateMachineFacts
  , withStateFileLock
  )
import Dojang.Types.ManagedTarget (isSafeManagedRelativePath)
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.ManifestVariable (ManifestVariable (branches))
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (MonikerName, parseMonikerName)
import Dojang.Types.RepositoryId (RepositoryId, newRepositoryId)


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
  Amd64Linux -> emptyEnvironment Linux X86_64 (Kernel "Linux" "0.0.0.0")
  Arm64Linux -> emptyEnvironment Linux AArch64 (Kernel "Linux" "0.0.0.0")
  AppleSiliconMac -> emptyEnvironment MacOS AArch64 (Kernel "Darwin" "0.0.0")
  IntelMac -> emptyEnvironment MacOS X86_64 (Kernel "Darwin" "0.0.0")
  Win64 -> emptyEnvironment Windows X86_64 (Kernel "Windows" "0.0.0.0")
  WinArm64 -> emptyEnvironment Windows AArch64 (Kernel "Windows" "0.0.0.0")
  Wsl2 ->
    emptyEnvironment
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
    fromListWith (+) $
      zip (concatMap (NonEmpty.toList . snd) neededMonikers) [1, 1 ..]
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


makeManifest :: RepositoryId -> [InitPreset] -> Manifest
makeManifest repositoryId presets =
  Manifest (Just repositoryId) monikers' empty routeMap ignorePatterns'' empty
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
  ignorePatterns'' =
    fromList
      [ (p, ignores)
      | p <- keys routeMap
      , let ignores = findWithDefault [] p ignorePatterns'
      , not $ null ignores
      ]


init :: (MonadFileSystem i, MonadIO i) => [InitPreset] -> Bool -> App i ExitCode
init presets noInteractive = initWithFacts presets noInteractive Nothing []


-- | Initializes a repository and enrolls its machine-specific facts.
--
-- Existing repositories validate and enroll the requested facts before
-- returning.  New repositories validate fact inputs before publishing their
-- manifest and state.
initWithFacts
  :: (MonadFileSystem i, MonadIO i)
  => [InitPreset]
  -- ^ Platform presets used when creating a new manifest.
  -> Bool
  -- ^ Whether prompts are disabled and every required fact must be supplied.
  -> Maybe OsPath
  -- ^ An optional facts file to associate with this repository.
  -> [Text]
  -- ^ @key=value@ facts that override associated profile values.
  -> App i ExitCode
  -- ^ 'ExitSuccess' after initialization and enrollment complete.
initWithFacts presets noInteractive factsFile assignments = do
  manifestExists <- doesManifestExist
  if manifestExists
    then do
      manifest <- ensureManifest
      prevalidateExistingMachineFacts
        manifest
        noInteractive
        factsFile
        assignments
      state <- prepareMachineState manifest
      _ <- enrollMachineFacts state manifest noInteractive factsFile assignments
      return ExitSuccess
    else initializeNew presets noInteractive factsFile assignments


initializeNew
  :: (MonadFileSystem i, MonadIO i)
  => [InitPreset]
  -> Bool
  -> Maybe OsPath
  -> [Text]
  -> App i ExitCode
initializeNew presets noInteractive factsFile assignments = do
  manifestExists <- doesManifestExist
  when manifestExists $ do
    die' manifestAlreadyExists "Manifest already exists."
  $(logInfo) "No manifest found."
  when (System.Info.os == "mingw32" && not noInteractive) $ do
    codeStyle <- codeStyleFor stderr
    printStderr' Error $
      "We are sorry, but interactive mode is currently not supported on "
        <> "Windows.  See the relevant issue:\n"
        <> "\n  https://github.com/dahlia/dojang/issues/4\n"
    printStderr' Hint $
      ("Please use the " <> codeStyle "--linux-*" <> "/")
        <> (codeStyle "--macos-*" <> "/" <> codeStyle "--windows-*")
        <> " options with the "
        <> (codeStyle "-I" <> "/" <> codeStyle "--no-interactive")
        <> " flag.  See also "
        <> (codeStyle "-h" <> "/" <> codeStyle "--help")
        <> " for command-line options."
    liftIO $ exitWith unsupportedOnEnvError
  _ <- readValidatedLegacyRegistry
  ensureNoLegacySnapshotForInitialization
  presets' <-
    if null presets && not noInteractive
      then askPresets
      else pure presets
  $(logDebugSH) presets'
  prevalidateNewMachineFacts factsFile assignments
  stateRoot <- asks (.stateDirectory)
  stateRootResult <- catchStateIOErrors $ do
    createDirectories stateRoot
    return $ Right ()
  case stateRootResult of
    Left err -> die' machineStateError $ formatStateError err
    Right () -> return ()
  initialized <- withStateFileLock (manifestIdentityLockPath stateRoot) $ do
    manifestCreated <- doesManifestExist
    when manifestCreated $ do
      die' manifestAlreadyExists "Manifest already exists."
    repositoryId <- newRepositoryId
    let manifest = makeManifest repositoryId presets'
    state <- prepareNewMachineStateBeforeMigration manifest $ do
      repoDir <- asks (.sourceDirectory)
      pathStyle <- pathStyleFor stderr
      forM_ (Data.Map.Strict.toAscList manifest.fileRoutes) $ \(path', route') -> do
        when (route'.fileType == Directory) $ do
          let dirPath = repoDir </> path'
          createDirectories dirPath
          printStderr $ "Directory created: " <> pathStyle dirPath <> "."
      filename <- saveManifest manifest
      printStderr $ "Manifest created: " <> pathStyle filename <> "."
    _ <- enrollMachineFacts state manifest noInteractive factsFile assignments
    debug' <- asks (.debug)
    dryRun' <- asks (.dryRun)
    when (debug' || dryRun') $ do
      manifestText <- case writeManifest manifest of
        Left err -> die' fileWriteError $ formatWriteError err
        Right source -> pure $ indent source
      printStderr' Note $
        "The manifest file looks like below:\n\n"
          <> manifestText
          <> "\n"
    return ExitSuccess
  case initialized of
    Left err -> die' machineStateError $ formatStateError err
    Right exitCode -> return exitCode


prevalidateMachineFactInputs
  :: (MonadFileSystem i, MonadIO i)
  => Maybe OsPath
  -> [Text]
  -> App i (FactMap, Maybe FactMap)
prevalidateMachineFactInputs requestedFactsFile assignments = do
  supplied <- parseFactAssignments assignments
  checkout <- asks (.sourceDirectory)
  fileFacts <- case requestedFactsFile of
    Just configured -> do
      normalized <- normalizeFactsFile checkout configured
      Just
        <$> readFactsSource
          ( if isAbsolute normalized
              then normalized
              else checkout </> normalized
          )
    Nothing -> return Nothing
  return (supplied, fileFacts)


readSelectedFacts
  :: (MonadFileSystem i, MonadIO i)
  => OsPath
  -> Maybe MachineState
  -> App i FactMap
readSelectedFacts checkout state =
  case state >>= (.factsFile) of
    Just factsFile ->
      readFactsSource $
        if isAbsolute factsFile
          then factsFile
          else checkout </> factsFile
    Nothing -> snd <$> findDefaultFactsSource checkout


prevalidateExistingMachineFacts
  :: (MonadFileSystem i, MonadIO i)
  => Manifest
  -> Bool
  -> Maybe OsPath
  -> [Text]
  -> App i ()
prevalidateExistingMachineFacts
  manifest
  noInteractive
  requestedFactsFile
  assignments = do
    (supplied, requestedFacts) <-
      prevalidateMachineFactInputs requestedFactsFile assignments
    checkout <- asks (.sourceDirectory)
    state <- readExistingMachineState manifest
    fileFacts <-
      maybe (readSelectedFacts checkout state) return requestedFacts
    let declared =
          maybe supplied (Map.union supplied . (.declaredFacts)) state
    when (noInteractive || System.Info.os == "mingw32") $ do
      missing <-
        missingMachineFacts manifest $ Map.union declared fileFacts
      reportMissingMachineFacts missing


prevalidateNewMachineFacts
  :: (MonadFileSystem i, MonadIO i)
  => Maybe OsPath
  -> [Text]
  -> App i ()
prevalidateNewMachineFacts requestedFactsFile assignments = do
  void $ prevalidateMachineFactInputs requestedFactsFile assignments
  case requestedFactsFile of
    Just _ -> return ()
    Nothing -> do
      checkout <- asks (.sourceDirectory)
      void $ findDefaultFactsSource checkout


missingMachineFacts
  :: (MonadFileSystem i, MonadIO i)
  => Manifest
  -> FactMap
  -> App i (Set FactKey)
missingMachineFacts manifest known = do
  let referenced = referencedMachineFacts manifest
  required <-
    if Set.null referenced
      then return Set.empty
      else do
        environment <- currentEnvironmentWithFacts known
        return $ requiredMachineFacts environment manifest
  return $ required `Set.difference` Map.keysSet known


reportMissingMachineFacts :: (MonadIO i) => Set FactKey -> App i ()
reportMissingMachineFacts missing =
  when (not $ Set.null missing) $ do
    codeStyle <- codeStyleFor stderr
    let rendered =
          Text.intercalate
            ", "
            [codeStyle $ factKeyText key | key <- Set.toAscList missing]
    die' missingMachineFactError $
      "Machine facts required by this repository are missing: "
        <> rendered
        <> ".  Supply them with `--facts-file` or `--fact`."


enrollMachineFacts
  :: (MonadFileSystem i, MonadIO i)
  => MachineState
  -> Manifest
  -> Bool
  -> Maybe OsPath
  -> [Text]
  -> App i MachineState
enrollMachineFacts state manifest noInteractive requestedFactsFile assignments = do
  supplied <- parseFactAssignments assignments
  (factsFile', fileFacts) <-
    resolveFactsSource state requestedFactsFile
  let declared = Map.union supplied state.declaredFacts
  let known = Map.union declared fileFacts
  missing <- missingMachineFacts manifest known
  prompted <-
    if Set.null missing
      then return Map.empty
      else
        if noInteractive || System.Info.os == "mingw32"
          then do
            reportMissingMachineFacts missing
            return Map.empty
          else do
            pairs <- forM (Set.toAscList missing) $ \key -> do
              value <- input $ "Value for fact." ++ Text.unpack (factKeyText key) ++ ":"
              return (key, fromString value)
            return $ Map.fromList pairs
  let declaredFactUpdates = Map.union prompted supplied
  let factsFileUpdate =
        if factsFile' == state.factsFile then Nothing else factsFile'
  if factsFileUpdate == Nothing && Map.null declaredFactUpdates
    then do
      $(logInfo) "Machine facts are already enrolled."
      return state
    else do
      stateRoot <- asks (.stateDirectory)
      now <- liftIO getCurrentTime
      updated <-
        updateMachineFacts
          stateRoot
          now
          state
          factsFileUpdate
          declaredFactUpdates
      case updated of
        Left err -> die' machineStateError $ formatStateError err
        Right state' -> do
          $(logInfo) "Machine facts enrolled."
          return state'


parseFactAssignments
  :: (MonadIO i) => [Text] -> App i FactMap
parseFactAssignments = foldM insertAssignment Map.empty
 where
  insertAssignment facts assignment = do
    let (keyText, separatorAndValue) = Text.breakOn "=" assignment
    when (Text.null separatorAndValue) $
      die' cliError $
        "Invalid machine fact assignment: " <> assignment <> "."
    key <- case parseFactKey keyText of
      Left message -> die' cliError message
      Right value -> return value
    when (isBuiltInFact key) $
      die' cliError $
        "Built-in machine facts cannot be persisted with `--fact`: "
          <> factKeyText key
          <> "."
    let value = fromString $ Text.unpack $ Text.drop 1 separatorAndValue
    return $ Map.insert key value facts


resolveFactsSource
  :: (MonadFileSystem i, MonadIO i)
  => MachineState
  -> Maybe OsPath
  -> App i (Maybe OsPath, FactMap)
resolveFactsSource state requested = do
  case requested of
    Just factsFile -> do
      configured <- normalizeFactsFile state.checkoutPath factsFile
      facts <-
        readFactsSource $
          if isAbsolute configured
            then configured
            else state.checkoutPath </> configured
      return (Just configured, facts)
    Nothing -> case state.factsFile of
      Just factsFile -> do
        facts <-
          readFactsSource $
            if isAbsolute factsFile
              then factsFile
              else state.checkoutPath </> factsFile
        return (Just factsFile, facts)
      Nothing -> findDefaultFactsSource state.checkoutPath


normalizeFactsFile
  :: (MonadFileSystem i, MonadIO i) => OsPath -> OsPath -> App i OsPath
normalizeFactsFile checkout configured =
  ( do
      checkout' <- canonicalizePath checkout
      resolved <-
        canonicalizePath $
          if isAbsolute configured then configured else checkout' </> configured
      let relative = normalise $ makeRelative checkout' resolved
      return $
        if not (isAbsolute relative) && isSafeManagedRelativePath relative
          then relative
          else normalise resolved
  )
    `catchError` \err ->
      die' envFileReadError $
        "Could not resolve the machine-facts file: " <> showt (FromStringShow err)


findDefaultFactsSource
  :: (MonadFileSystem i, MonadIO i)
  => OsPath
  -> App i (Maybe OsPath, FactMap)
findDefaultFactsSource checkout = do
  filename <- encodePath "dojang-env.toml"
  regular <- isRegularFile $ checkout </> filename
  if not regular
    then return (Nothing, Map.empty)
    else do
      facts <- readFactsSource $ checkout </> filename
      return (if Map.null facts then Nothing else Just filename, facts)


readFactsSource
  :: (MonadFileSystem i, MonadIO i) => OsPath -> App i FactMap
readFactsSource factsPath = do
  result <-
    readFactsFile factsPath `catchError` \err ->
      die' envFileReadError $
        "Could not read the machine-facts file: " <> showt (FromStringShow err)
  case result of
    Left errors ->
      die' envFileReadError $
        "Syntax errors in the machine-facts file:"
          <> Text.concat ["\n  " <> Text.pack error' | error' <- NonEmpty.toList errors]
    Right (facts, warnings) -> do
      forM_ warnings $ \warning -> $(logWarn) $ fromString warning
      return facts


-- | Collects custom machine facts referenced by reachable manifest branches.
referencedMachineFacts
  :: Manifest
  -- ^ Manifest whose reachable predicates are inspected.
  -> Set FactKey
  -- ^ Custom fact keys referenced by reachable branches.
referencedMachineFacts = machineFactsRequiredBy id


-- | Collects custom machine facts still required after specializing a
-- manifest for the known environment.
requiredMachineFacts
  :: Environment
  -- ^ Known environment used to specialize predicates.
  -> Manifest
  -- ^ Manifest whose reachable predicates are inspected.
  -> Set FactKey
  -- ^ Custom fact keys whose values are still required.
requiredMachineFacts environment =
  machineFactsRequiredBy $ specializePredicate environment


machineFactsRequiredBy
  :: (EnvironmentPredicate -> EnvironmentPredicate)
  -> Manifest
  -> Set FactKey
machineFactsRequiredBy specialize manifest =
  Set.filter (not . isBuiltInFact) $
    Set.unions $
      referencedFacts resolver
        <$> routePredicates <> variablePredicates <> hookPredicates
 where
  resolver = (`HashMap.lookup` manifest.monikers)
  routePredicates =
    [ predicate
    | route <- Map.elems manifest.fileRoutes
    , predicate <- reachablePredicates route.predicates
    ]
  hookPredicates =
    [ specialize $ resolvePredicate Set.empty hook.condition
    | hooks <- Map.elems manifest.hooks
    , hook <- hooks
    ]
  variablePredicates =
    [ predicate
    | variable <- Map.elems manifest.variables
    , predicate <- reachablePredicates $ NonEmpty.toList variable.branches
    ]
  reachablePredicates [] = []
  reachablePredicates ((predicate, _) : branches) =
    let resolved = specialize $ resolvePredicate Set.empty predicate
    in resolved
         : if resolved == Always
           then []
           else reachablePredicates branches
  resolvePredicate visited predicate =
    normalizePredicate $ case predicate of
      Moniker name
        | name `Set.member` visited -> predicate
        | otherwise ->
            maybe
              predicate
              (resolvePredicate $ Set.insert name visited)
              (resolver name)
      Not child -> Not $ resolvePredicate visited child
      And children -> And $ resolvePredicate visited <$> children
      Or children -> Or $ resolvePredicate visited <$> children
      _ -> predicate


specializePredicate
  :: Environment -> EnvironmentPredicate -> EnvironmentPredicate
specializePredicate environment predicate =
  normalizePredicate $ case predicate of
    Not child -> Not $ specializePredicate environment child
    And children -> And $ specializePredicate environment <$> children
    Or children -> Or $ specializePredicate environment <$> children
    Moniker _ -> predicate
    Fact key _
      | not (isBuiltInFact key) && lookupFact key environment == Nothing ->
          predicate
    FactDefined key
      | not (isBuiltInFact key) && lookupFact key environment == Nothing ->
          predicate
    _ ->
      if fst $ evaluate environment mempty predicate
        then Always
        else Not Always


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
