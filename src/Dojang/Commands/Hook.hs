{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Hook execution module for running pre/post apply hooks.
module Dojang.Commands.Hook
  ( HookEnv (..)
  , executeHooks
  , shouldRunHook
  ) where

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import System.Exit (ExitCode (..))
import Prelude hiding (readFile)

import Control.Monad.Logger (logDebug, logError, logInfo)
import Data.Map.Strict (lookup)
import Data.Text (Text, pack, unpack)
import System.Directory.OsPath (makeAbsolute)
import System.OsPath (OsPath, decodeFS)
import System.Process
  ( CreateProcess (..)
  , proc
  , waitForProcess
  , withCreateProcess
  )
import TextShow (FromStringShow (FromStringShow), TextShow (showt))

import Dojang.App (App, AppEnv (..))
import Dojang.Commands (Admonition (..), die', pathStyleFor, printStderr')
import Dojang.ExitCodes (hookFailedError)
import Dojang.MonadFileSystem (MonadFileSystem)
import Dojang.Types.Context (Context (..))
import Dojang.Types.Environment (Environment (..))
import Dojang.Types.EnvironmentPredicate.Evaluate (evaluate)
import Dojang.Types.Hook (Hook (..), HookMap, HookType (..))
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.Repository (Repository (..))

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
  }
  deriving (Show)


-- | Determine if a hook should run based on its condition.
shouldRunHook :: MonikerMap -> Environment -> Hook -> Bool
shouldRunHook monikers environment hook =
  let (result, _) = evaluate environment monikers hook.condition
  in result


-- | Execute all hooks of a given type.
executeHooks
  :: (MonadFileSystem i, MonadIO i)
  => HookEnv
  -> Context (App i)
  -> HookType
  -> App i ()
executeHooks hookEnv ctx hookType = do
  let repo = ctx.repository :: Repository
  let manifest' = repo.manifest :: Manifest
  let hookMap = manifest'.hooks :: HookMap
  case Data.Map.Strict.lookup hookType hookMap of
    Nothing -> return ()
    Just hookList -> do
      $(logDebug) $ "Executing " <> pack (show hookType) <> " hooks..."
      forM_ hookList $ \hook -> do
        let monikers = manifest'.monikers
        let environment = ctx.environment
        let shouldRun = shouldRunHook monikers environment hook
        if shouldRun
          then executeHook hookEnv hook
          else
            $(logDebug) $
              "Skipping hook (condition not met): " <> showt (FromStringShow hook)


-- | Execute a single hook.
executeHook
  :: (MonadIO i)
  => HookEnv
  -> Hook
  -> App i ()
executeHook hookEnv hook = do
  dryRun' <- asks (.dryRun)
  pathStyle <- pathStyleFor stderr
  cmdPath <- liftIO $ decodeFS hook.command
  let argsStr = unpack <$> hook.args

  if dryRun' || hookEnv.isDryRun
    then do
      -- In dry-run mode, just print what would be done
      printStderr' Note $
        "Would run hook: "
          <> pathStyle hook.command
          <> " "
          <> pack (unwords argsStr)
    else do
      -- Actually execute the hook
      $(logInfo) $
        "Running hook: "
          <> showt (FromStringShow hook.command)
          <> " "
          <> showt argsStr
          <> "..."

      workDir <- case hook.workingDirectory of
        Just wd -> Just <$> liftIO (decodeFS wd)
        Nothing -> Just <$> liftIO (decodeFS hookEnv.repositoryPath)

      -- Set up environment variables
      repoPath <- liftIO $ makeAbsolute hookEnv.repositoryPath >>= decodeFS
      manifestPath' <- liftIO $ decodeFS hookEnv.manifestPath
      let envVars =
            [ ("DOJANG_REPOSITORY", repoPath)
            , ("DOJANG_MANIFEST", manifestPath')
            , ("DOJANG_DRY_RUN", if hookEnv.isDryRun then "1" else "0")
            , ("DOJANG_OS", unpack hookEnv.currentOS)
            , ("DOJANG_ARCH", unpack hookEnv.currentArch)
            ]

      let createProc =
            (proc cmdPath argsStr)
              { cwd = workDir
              , env = Just envVars
              , delegate_ctlc = True
              }

      exitCode <- liftIO $ withCreateProcess createProc $ \_ _ _ ph ->
        waitForProcess ph

      case exitCode of
        ExitSuccess ->
          $(logInfo) $
            "Hook completed successfully: " <> showt (FromStringShow hook.command)
        ExitFailure code -> do
          $(logError) $
            "Hook failed with exit code "
              <> showt code
              <> ": "
              <> showt (FromStringShow hook.command)
          unless hook.ignoreFailure $ do
            die' hookFailedError $
              "Hook "
                <> pathStyle hook.command
                <> " failed with exit code "
                <> showt code
                <> "."
