{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Hook execution for supported command lifecycle events.
module Dojang.Commands.Hook
  ( HookEnv (..)
  , HookScopePath (..)
  , commandHookTypes
  , disambiguatedHookScopePaths
  , effectiveHookWorkingDirectory
  , executeHooks
  , defaultHookProcessRunner
  , hookDueReason
  , hookIsDue
  , hookRecursionKey
  , hooksSuppressed
  , makeHookEnv
  , mergeHookEnvironment
  , normalizeHookScopePath
  , renderHookDryRun
  , shouldRunHook
  , withCommandHooks
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder
  ( byteString
  , toLazyByteString
  , word32BE
  , word64BE
  )
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isDigit, ord, toUpper)
import Data.List (nub, sort, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Numeric (showHex)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.Info (os)
import Prelude hiding (readFile)

import Control.Monad.Logger (logDebug, logError, logInfo)
import Data.CaseInsensitive (mk, original)
import Data.Text (Text, pack, unpack)
import System.OsPath
  ( OsPath
  , decodeFS
  , isAbsolute
  , makeRelative
  , normalise
  , splitDirectories
  , (</>)
  )
import System.OsPath qualified as OsPath
import System.Process
  ( CreateProcess (..)
  , createProcess
  , proc
  , waitForProcess
  )
import TextShow (FromStringShow (FromStringShow), TextShow (showt))

import Dojang.App
  ( App
  , AppEnv (..)
  , ensureContext
  , prepareMachineState
  )
import Dojang.Commands (Admonition (..), die', pathStyleFor, printStderr')
import Dojang.ExitCodes (hookFailedError, machineStateError)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Context (Context (..))
import Dojang.Types.Environment (Environment (..), Kernel (..))
import Dojang.Types.EnvironmentPredicate.Evaluate (evaluate)
import Dojang.Types.Hook
  ( Hook (..)
  , HookMap
  , HookPolicy (..)
  , HookType (..)
  , renderHookId
  , renderHookPolicy
  , renderHookType
  )
import Dojang.Types.MachineState
  ( HookExecution (..)
  , HookExecutionKey
  , HookExecutionPolicy (..)
  , MachineState (..)
  , formatStateError
  , hookExecutionFingerprint
  , hookExecutionKey
  , readRepositoryState
  , recordHookExecution
  , renderHookExecutionKey
  , repositoryStateDirectory
  , repositoryStatePath
  , validateRepositoryStateGeneration
  , withRepositoryStateGeneration
  , withStateFileLock
  )
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.Repository (Repository (..))
import Dojang.Types.RepositoryId (RepositoryId, repositoryIdText)

import System.IO (stderr)


-- | Environment information for hook execution.
data HookEnv = HookEnv
  { repositoryPath :: OsPath
  -- ^ The path to the repository.
  , manifestPath :: OsPath
  -- ^ The path to the manifest file.
  , isDryRun :: Bool
  -- ^ Whether we're in dry-run mode.
  , currentOS :: Text
  -- ^ The current operating system.
  , currentArch :: Text
  -- ^ The current architecture.
  , currentKernel :: Text
  -- ^ The current kernel name.
  , currentKernelRelease :: Text
  -- ^ The current kernel release.
  , commandName :: Text
  -- ^ The command whose lifecycle event is running.
  , selectedPaths :: [OsPath]
  -- ^ Normalized command path scope.
  , machineState :: MachineState
  -- ^ Repository-scoped state used by stateful policies.
  , stateRoot :: OsPath
  -- ^ Platform-native machine-state root.
  , processStarter
      :: CreateProcess
      -> IO (Either IOException (IO (Either IOException ExitCode)))
  -- ^ Injectable process start effect used by tests and the real command runner.
  }


-- | A selected hook-scope path together with its relative-path origin.
data HookScopePath
  = -- | A command path resolved from the caller's working directory.
    CallerRelativePath OsPath
  | -- | A route selector resolved from the repository root.
    RepositoryRelativePath OsPath
  deriving (Eq, Show)


-- | Builds hook scope for commands whose source option disambiguates targets.
disambiguatedHookScopePaths
  :: Maybe OsPath
  -- ^ Optional repository-relative source selector.
  -> [OsPath]
  -- ^ Caller-relative target paths.
  -> [HookScopePath]
  -- ^ Effective scope, excluding a source selector when no target uses it.
disambiguatedHookScopePaths _ [] = []
disambiguatedHookScopePaths explicitSource paths =
  maybe
    (CallerRelativePath <$> paths)
    ( \source ->
        RepositoryRelativePath source : (CallerRelativePath <$> paths)
    )
    explicitSource


-- | Builds the complete context supplied to hooks for one command.
makeHookEnv
  :: (MonadFileSystem i, MonadIO i)
  => Text
  -- ^ Command whose lifecycle event will run.
  -> [HookScopePath]
  -- ^ Paths selected by that command, with their relative-path origins.
  -> Context (App i)
  -- ^ Current repository and machine environment.
  -> MachineState
  -- ^ Repository-scoped state captured for the command.
  -> App i HookEnv
  -- ^ Complete normalized hook context.
makeHookEnv command paths ctx state = do
  configuredManifest <- asks (.manifestFile)
  dryRun' <- asks (.dryRun)
  root <- asks (.stateDirectory)
  repositoryRoot <- normalise <$> makeAbsolute ctx.repository.sourcePath
  callerRoot <- normalise <$> getCurrentDirectory
  manifest <-
    normalise
      <$> makeAbsolute
        ( if isAbsolute configuredManifest
            then configuredManifest
            else repositoryRoot </> configuredManifest
        )
  normalizedPaths <- mapM (normalizeHookScopePath repositoryRoot callerRoot) paths
  let environment = ctx.environment
  return $
    HookEnv
      repositoryRoot
      manifest
      dryRun'
      (original environment.operatingSystem.identifier)
      (original environment.architecture.identifier)
      (original environment.kernel.name)
      (original environment.kernel.release)
      command
      (sort $ nub normalizedPaths)
      state
      root
      defaultHookProcessRunner


-- | Normalizes one selected path without losing its relative-path origin.
--
-- Paths inside the repository are returned relative to the repository root;
-- external paths remain absolute.
normalizeHookScopePath
  :: (MonadFileSystem m)
  => OsPath
  -- ^ Absolute repository root.
  -> OsPath
  -- ^ Absolute caller working directory.
  -> HookScopePath
  -- ^ Selected path and its relative-path origin.
  -> m OsPath
  -- ^ Repository-relative path when inside the repository, otherwise absolute.
normalizeHookScopePath repositoryRoot callerRoot scopePath = do
  parent <- encodePath ".."
  let (base, candidate) = case scopePath of
        CallerRelativePath path -> (callerRoot, path)
        RepositoryRelativePath path -> (repositoryRoot, path)
      absolute =
        normalise $
          if isAbsolute candidate then candidate else base </> candidate
      relative = makeRelative repositoryRoot absolute
  return $
    if not (isAbsolute relative)
      && case splitDirectories relative of
        [] -> True
        first : _ -> first /= parent
      then relative
      else absolute


-- | Returns the lifecycle hook pair for a supported command.
commandHookTypes
  :: Text
  -- ^ Command name.
  -> Maybe (HookType, HookType)
  -- ^ Pre-hook and post-hook events, or 'Nothing' when hooks are unsupported.
commandHookTypes "apply" = Just (PreApply, PostApply)
commandHookTypes "reflect" = Just (PreReflect, PostReflect)
commandHookTypes "diff" = Just (PreDiff, PostDiff)
commandHookTypes "status" = Just (PreStatus, PostStatus)
commandHookTypes "edit" = Just (PreEdit, PostEdit)
commandHookTypes "unmanage" = Just (PreUnmanage, PostUnmanage)
commandHookTypes _ = Nothing


-- | Runs a command action between its pre and successful-post lifecycle hooks.
withCommandHooks
  :: (MonadFileSystem i, MonadIO i)
  => Text
  -- ^ Command name used to select lifecycle events.
  -> [HookScopePath]
  -- ^ Paths selected by the command.
  -> App i ExitCode
  -- ^ Command action to run between the lifecycle events.
  -> App i ExitCode
  -- ^ Command result after any applicable hooks have run.
withCommandHooks command paths action = case commandHookTypes command of
  Nothing -> action
  Just (preEvent, postEvent) -> do
    ctx <- ensureContext
    state <- prepareMachineState ctx.repository.manifest
    hookEnv <- makeHookEnv command paths ctx state
    executeHooks hookEnv ctx preEvent
    result <- action
    when (result == ExitSuccess) $ do
      postContext <- ensureContext
      postState <- prepareMachineState postContext.repository.manifest
      postHookEnv <- makeHookEnv command paths postContext postState
      executeHooks postHookEnv postContext postEvent
    return result


-- | Determines whether a hook's condition matches the current environment.
shouldRunHook
  :: MonikerMap
  -- ^ Monikers available to the condition.
  -> Environment
  -- ^ Current machine environment.
  -> Hook
  -- ^ Hook whose condition is evaluated.
  -> Bool
  -- ^ Whether the condition matches.
shouldRunHook monikers environment hook =
  let (result, _) = evaluate environment monikers hook.condition
  in result


-- | Overlay the variables supplied by Dojang on the parent environment.
mergeHookEnvironment
  :: String
  -- ^ The host platform identifier from 'System.Info.os'.
  -> [(String, String)]
  -- ^ The variables supplied by Dojang.
  -> [(String, String)]
  -- ^ The parent process environment.
  -> [(String, String)]
  -- ^ The combined hook environment.
mergeHookEnvironment platform envVars parentEnv =
  envVars
    ++ filter
      ( \(name, _) ->
          not (isManagedHookContext name)
            && all (not . sameName name . fst) envVars
      )
      parentEnv
 where
  sameName left right
    | platform == "mingw32" = mk left == mk right
    | otherwise = left == right
  isManagedHookContext name =
    any (sameName name) managedHookContextNames
      || case stripPrefix "DOJANG_PATH_" $ normalizeName name of
        Just suffix -> not (null suffix) && all isDigit suffix
        Nothing -> False
  normalizeName
    | platform == "mingw32" = fmap toUpper
    | otherwise = id


managedHookContextNames :: [String]
managedHookContextNames =
  [ "DOJANG_REPOSITORY"
  , "DOJANG_MANIFEST"
  , "DOJANG_DRY_RUN"
  , "DOJANG_OS"
  , "DOJANG_ARCH"
  , "DOJANG_KERNEL"
  , "DOJANG_KERNEL_RELEASE"
  , "DOJANG_COMMAND"
  , "DOJANG_HOOK_EVENT"
  , "DOJANG_HOOK_ID"
  , "DOJANG_HOOK_POLICY"
  , "DOJANG_REPOSITORY_ID"
  , "DOJANG_MACHINE_STATE"
  , "DOJANG_INTERMEDIATE"
  , "DOJANG_PATH_COUNT"
  , "DOJANG_HOOK_DEPTH"
  , "DOJANG_HOOK_STACK"
  , "DOJANG_COMMAND_OUTCOME"
  , "DOJANG_EXIT_CODE"
  ]


-- | Executes all matching hooks of a lifecycle event in manifest order.
executeHooks
  :: (MonadFileSystem i, MonadIO i)
  => HookEnv
  -- ^ Normalized command and machine-state context.
  -> Context (App i)
  -- ^ Repository manifest and current machine environment.
  -> HookType
  -- ^ Lifecycle event to execute.
  -> App i ()
  -- ^ Completed hook execution, or an application exit on a fatal failure.
executeHooks hookEnv ctx hookType = do
  let repo = ctx.repository :: Repository
  let manifest' = repo.manifest :: Manifest
  let hookMap = manifest'.hooks :: HookMap
  depth <- liftIO currentHookDepth
  allowRecursion <-
    liftIO $ (== Just "1") <$> lookupEnv "DOJANG_ALLOW_HOOK_RECURSION"
  if hooksSuppressed depth allowRecursion
    then $(logDebug) "Suppressing hooks for a nested Dojang invocation."
    else case Map.lookup hookType hookMap of
      Nothing -> return ()
      Just hookList -> do
        $(logDebug) $ "Executing " <> renderHookType hookType <> " hooks..."
        forM_ hookList $ \hook -> do
          let monikers = manifest'.monikers
          let environment = ctx.environment
          if shouldRunHook monikers environment hook
            then executeHook hookEnv hookType hook
            else
              $(logDebug) $
                "Skipping hook (condition not met): "
                  <> showt (FromStringShow hook)
                  <> "."


-- | Determines whether hooks are suppressed at a nested invocation depth.
hooksSuppressed
  :: Int
  -- ^ Current hook nesting depth.
  -> Bool
  -- ^ Whether one nested invocation was explicitly allowed.
  -> Bool
  -- ^ Whether hook execution is suppressed at this depth.
hooksSuppressed depth allowRecursion =
  depth > 0 && (not allowRecursion || depth >= 2)


-- | Builds a repository-scoped identity for recursion suppression.
hookRecursionKey
  :: RepositoryId
  -- ^ Repository that owns the hook.
  -> Text
  -- ^ Lifecycle event spelling.
  -> Text
  -- ^ Stable or derived hook identifier.
  -> Text
  -- ^ Recursion-stack identity.
hookRecursionKey repositoryId event identifier =
  repositoryIdText repositoryId <> "/" <> event <> "/" <> identifier


-- | Execute a single hook.
executeHook
  :: (MonadFileSystem i, MonadIO i)
  => HookEnv
  -> HookType
  -> Hook
  -> App i ()
executeHook hookEnv hookType hook = do
  dryRun' <- asks (.dryRun)
  let event = renderHookType hookType
  let identifier = effectiveHookId hookType hook
  let executionKey = hookExecutionKey hookType <$> hook.hookId
  let stateKey =
        maybe (event <> "/" <> identifier) renderHookExecutionKey executionKey
  let recursionKey =
        hookRecursionKey hookEnv.machineState.repositoryId event identifier
  let fingerprint = hookFingerprint hookEnv hookType hook
  ancestorStack <- liftIO $ fromMaybe "" <$> lookupEnv "DOJANG_HOOK_STACK"
  if recursionKey `elem` Text.splitOn "," (pack ancestorStack)
    then $(logDebug) $ "Suppressing recursive hook " <> recursionKey <> "."
    else
      if dryRun' || hookEnv.isDryRun
        then case hookDueReason
          hook.policy
          fingerprint
          (executionKey >>= (`Map.lookup` hookEnv.machineState.hookExecutions)) of
          Nothing -> return ()
          Just reason -> printDryRun event reason hook
        else case hook.policy of
          HookAlways -> do
            _ <-
              runHookProcess
                hookEnv
                hookType
                recursionKey
                identifier
                hook
                (liftIO . hookEnv.processStarter)
            return ()
          _ -> case executionKey of
            Nothing ->
              die'
                machineStateError
                "A stateful hook has no validated execution identity."
            Just typedKey -> do
              lockName <-
                encodePath $ "hook-" <> Text.unpack (digestText stateKey) <> ".lock"
              let lockDirectory =
                    repositoryStateDirectory
                      hookEnv.stateRoot
                      hookEnv.machineState.repositoryId
              createDirectories lockDirectory
              locked <-
                withStateFileLock (lockDirectory </> lockName) $ do
                  loaded <-
                    readRepositoryState
                      hookEnv.stateRoot
                      hookEnv.machineState.repositoryId
                      hookEnv.machineState.machineId
                  case loaded of
                    Left err -> die' machineStateError $ formatStateError err
                    Right Nothing ->
                      die'
                        machineStateError
                        "The repository machine-state record disappeared before hook execution."
                    Right (Just state) -> do
                      case validateRepositoryStateGeneration
                        hookEnv.machineState
                        state of
                        Left err -> die' machineStateError $ formatStateError err
                        Right () -> do
                          due <- isHookDue state hook typedKey fingerprint
                          when due $
                            runAndRecord
                              typedKey
                              recursionKey
                              identifier
                              fingerprint
              case locked of
                Left err -> die' machineStateError $ formatStateError err
                Right () -> return ()
 where
  printDryRun event reason hook' = do
    pathStyle <- pathStyleFor stderr
    workingDirectory <- effectiveHookWorkingDirectory hookEnv hook'
    printStderr' Note $
      renderHookDryRun
        event
        (pathStyle workingDirectory)
        reason
        (pathStyle hook'.command)
        hook'.args

  runAndRecord executionKey recursionKey identifier fingerprint = do
    succeeded <-
      runHookProcess
        hookEnv
        hookType
        recursionKey
        identifier
        hook
        startCurrentGeneration
    when (succeeded && hook.policy /= HookAlways) $ do
      now <- liftIO getCurrentTime
      case hookExecutionPolicy hook.policy fingerprint of
        Nothing ->
          die'
            machineStateError
            "A stateful hook has invalid execution policy data."
        Just policy -> do
          let execution = HookExecution policy now
          result <-
            recordHookExecution
              hookEnv.stateRoot
              now
              hookEnv.machineState
              executionKey
              execution
          case result of
            Left err -> die' machineStateError $ formatStateError err
            Right _ -> return ()

  hookExecutionPolicy HookOnce _ = Just HookOnceExecution
  hookExecutionPolicy HookOnChange (Just value) =
    Just $ HookOnChangeExecution value
  hookExecutionPolicy _ _ = Nothing

  startCurrentGeneration createProc = do
    guarded <-
      withRepositoryStateGeneration
        hookEnv.stateRoot
        hookEnv.machineState
        (liftIO $ hookEnv.processStarter createProc)
    case guarded of
      Left err -> die' machineStateError $ formatStateError err
      Right started -> return started


isHookDue
  :: (Applicative m)
  => MachineState
  -> Hook
  -> HookExecutionKey
  -> Maybe Text
  -> m Bool
isHookDue state hook key fingerprint =
  pure $ hookIsDue hook.policy fingerprint $ Map.lookup key state.hookExecutions


-- | Determines whether a hook policy is due from its previous successful run.
hookIsDue
  :: HookPolicy
  -- ^ Configured execution policy.
  -> Maybe Text
  -- ^ Current on-change fingerprint, when applicable.
  -> Maybe HookExecution
  -- ^ Previous successful execution, when present.
  -> Bool
  -- ^ Whether the hook is due.
hookIsDue policy fingerprint = isJust . hookDueReason policy fingerprint


-- | Explains why a hook policy is due, or returns 'Nothing' when it is not.
hookDueReason
  :: HookPolicy
  -- ^ Configured execution policy.
  -> Maybe Text
  -- ^ Current on-change fingerprint, when applicable.
  -> Maybe HookExecution
  -- ^ Previous successful execution, when present.
  -> Maybe Text
  -- ^ Human-readable reason when due.
hookDueReason HookAlways _ _ = Just "always policy"
hookDueReason HookOnce _ Nothing =
  Just "once policy has no successful execution"
hookDueReason HookOnce _ (Just _) = Nothing
hookDueReason HookOnChange _ Nothing =
  Just "on-change policy has no successful execution"
hookDueReason HookOnChange fingerprint (Just previous)
  | hookExecutionFingerprint previous /= fingerprint =
      Just "on-change fingerprint changed"
  | otherwise = Nothing


-- | Renders one dry-run hook report with its lifecycle and policy context.
renderHookDryRun
  :: Text
  -- ^ Lifecycle event spelling.
  -> Text
  -- ^ Styled effective working directory.
  -> Text
  -- ^ Reason the policy is due.
  -> Text
  -- ^ Styled executable path.
  -> [Text]
  -- ^ Command arguments.
  -> Text
  -- ^ Complete dry-run report.
renderHookDryRun event workingDirectory reason command args =
  "Would run hook (event: "
    <> event
    <> "; working directory: "
    <> workingDirectory
    <> "; reason: "
    <> reason
    <> "): "
    <> Text.unwords (command : args)


effectiveHookId :: HookType -> Hook -> Text
effectiveHookId hookType hook =
  maybe
    ( "anonymous-"
        <> Text.take 16 (digestText $ renderHookType hookType <> pack (show hook))
    )
    renderHookId
    hook.hookId


hookFingerprint :: HookEnv -> HookType -> Hook -> Maybe Text
hookFingerprint _ _ hook | hook.policy /= HookOnChange = Nothing
hookFingerprint hookEnv hookType hook =
  Just $
    digestParts $
      ( encodeUtf8
          <$> [ renderHookType hookType
              , pack $ show hook
              , fromMaybe "" hook.changeKey
              , hookEnv.currentOS
              , hookEnv.currentArch
              , hookEnv.currentKernel
              , hookEnv.currentKernelRelease
              ]
      )
        ++ (osPathBytes <$> hookEnv.selectedPaths)


digestText :: Text -> Text
digestText = digestBytes . SHA256.hash . encodeUtf8


digestParts :: [ByteString.ByteString] -> Text
digestParts =
  digestBytes
    . SHA256.hash
    . LazyByteString.toStrict
    . toLazyByteString
    . foldMap frame
 where
  frame bytes = word64BE (fromIntegral $ ByteString.length bytes) <> byteString bytes


osPathBytes :: OsPath -> ByteString.ByteString
osPathBytes =
  LazyByteString.toStrict
    . toLazyByteString
    . foldMap (word32BE . fromIntegral . ord . OsPath.toChar)
    . OsPath.unpack


digestBytes :: ByteString.ByteString -> Text
digestBytes = Text.pack . concatMap byteHex . ByteString.unpack
 where
  byteHex byte = case showHex byte "" of
    [digit] -> ['0', digit]
    digits -> digits


currentHookDepth :: IO Int
currentHookDepth = do
  value <- lookupEnv "DOJANG_HOOK_DEPTH"
  return $ case value >>= readMaybeInt of
    Just depth -> max 0 depth
    Nothing -> 0
 where
  readMaybeInt value = case reads value of
    [(number, "")] -> Just number
    _ -> Nothing


runHookProcess
  :: (MonadFileSystem i, MonadIO i)
  => HookEnv
  -> HookType
  -> Text
  -> Text
  -> Hook
  -> (CreateProcess -> App i (Either IOException (IO (Either IOException ExitCode))))
  -> App i Bool
runHookProcess hookEnv hookType recursionKey identifier hook start = do
  pathStyle <- pathStyleFor stderr
  cmdPath <- liftIO $ decodeFS hook.command
  let argsStr = unpack <$> hook.args
  $(logInfo) $
    "Running hook: "
      <> showt (FromStringShow hook.command)
      <> " "
      <> showt argsStr
      <> "..."
  workDirPath <- effectiveHookWorkingDirectory hookEnv hook
  workDir <- Just <$> decodePath workDirPath
  repoPath <- makeAbsolute hookEnv.repositoryPath >>= decodePath
  manifestPath' <- decodePath hookEnv.manifestPath
  statePath <-
    decodePath $
      repositoryStatePath
        hookEnv.stateRoot
        hookEnv.machineState.repositoryId
  intermediate <- decodePath hookEnv.machineState.intermediatePath
  paths <- mapM decodePath hookEnv.selectedPaths
  depth <- liftIO currentHookDepth
  parentEnv <- liftIO getEnvironment
  let environment =
        [ ("DOJANG_REPOSITORY", repoPath)
        , ("DOJANG_MANIFEST", manifestPath')
        , ("DOJANG_DRY_RUN", "0")
        , ("DOJANG_OS", unpack hookEnv.currentOS)
        , ("DOJANG_ARCH", unpack hookEnv.currentArch)
        , ("DOJANG_KERNEL", unpack hookEnv.currentKernel)
        , ("DOJANG_KERNEL_RELEASE", unpack hookEnv.currentKernelRelease)
        , ("DOJANG_COMMAND", unpack hookEnv.commandName)
        , ("DOJANG_HOOK_EVENT", unpack $ renderHookType hookType)
        , ("DOJANG_HOOK_ID", unpack identifier)
        , ("DOJANG_HOOK_POLICY", unpack $ renderHookPolicy hook.policy)
        ,
          ( "DOJANG_REPOSITORY_ID"
          , unpack $ repositoryIdText hookEnv.machineState.repositoryId
          )
        , ("DOJANG_MACHINE_STATE", statePath)
        , ("DOJANG_INTERMEDIATE", intermediate)
        , ("DOJANG_PATH_COUNT", show $ length paths)
        , ("DOJANG_HOOK_DEPTH", show $ depth + 1)
        , ("DOJANG_HOOK_STACK", unpack recursionKey <> stackSuffix parentEnv)
        ]
          <> zipWith
            (\index value -> ("DOJANG_PATH_" <> show index, value))
            [0 :: Int ..]
            paths
          <> postEnvironment
      postEnvironment =
        if "post-" `Text.isPrefixOf` renderHookType hookType
          then [("DOJANG_COMMAND_OUTCOME", "success"), ("DOJANG_EXIT_CODE", "0")]
          else []
      stackSuffix variables = case lookup "DOJANG_HOOK_STACK" variables of
        Just old | old /= "" -> "," <> old
        _ -> ""
      createProc =
        (proc cmdPath argsStr)
          { cwd = workDir
          , env = Just $ mergeHookEnvironment os environment parentEnv
          , delegate_ctlc = True
          }
  started <- start createProc
  result <- case started of
    Left err -> return $ Left err
    Right waitForExit -> liftIO waitForExit
  case result of
    Left err -> do
      $(logError) $
        "Hook could not be started: " <> showt (FromStringShow err) <> "."
      unless hook.ignoreFailure $
        die' hookFailedError $
          "Hook "
            <> pathStyle hook.command
            <> " could not be started: "
            <> showt (FromStringShow err)
            <> "."
      return False
    Right ExitSuccess -> do
      $(logInfo) $
        "Hook completed successfully: "
          <> showt (FromStringShow hook.command)
          <> "."
      return True
    Right (ExitFailure code) -> do
      $(logError) $
        "Hook failed with exit code "
          <> showt code
          <> ": "
          <> showt (FromStringShow hook.command)
          <> "."
      unless hook.ignoreFailure $
        die' hookFailedError $
          "Hook "
            <> pathStyle hook.command
            <> " failed with exit code "
            <> showt code
            <> "."
      return False


-- | Resolves the hook's effective working directory.
effectiveHookWorkingDirectory
  :: (MonadFileSystem i, MonadIO i)
  => HookEnv
  -- ^ Hook context whose repository is the relative-path base.
  -> Hook
  -- ^ Hook with an optional configured working directory.
  -> App i OsPath
  -- ^ Normalized absolute working directory.
effectiveHookWorkingDirectory hookEnv hook =
  normalise <$> makeAbsolute candidate
 where
  candidate = case hook.workingDirectory of
    Nothing -> hookEnv.repositoryPath
    Just configured
      | isAbsolute configured -> configured
      | otherwise -> hookEnv.repositoryPath </> configured


-- | Starts a hook process and returns an action that waits for its exit status.
defaultHookProcessRunner
  :: CreateProcess
  -- ^ Fully configured process specification.
  -> IO (Either IOException (IO (Either IOException ExitCode)))
  -- ^ Start error or an action that waits for the process result.
defaultHookProcessRunner createProc = do
  started <- try @IOException $ createProcess createProc
  return $ case started of
    Left err -> Left err
    Right (_, _, _, processHandle) ->
      Right $ try @IOException $ waitForProcess processHandle
