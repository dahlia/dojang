{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | The application monad for Dojang.
module Dojang.App
  ( App
  , AppEnv (..)
  , Loc
  , LogLevel
  , LogSource
  , LogStr
  , applyAutomaticRepositorySelection
  , currentEnvironment'
  , doesManifestExist
  , ensureContext
  , ensureNoLegacySnapshotForInitialization
  , ensureRepository
  , loadManifest
  , loadRepository
  , markMachineStateApplied
  , prepareMachineState
  , prepareMachineStateBeforeMigration
  , prepareNewMachineState
  , prepareNewMachineStateBeforeMigration
  , readValidatedLegacyRegistry
  , lookupEnv'
  , runAppWithLogging
  , runAppWithStderrLogging
  , runAppWithoutLogging
  , saveManifest
  , validateRepositoryCheckout
  ) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty (toList)
import Data.String (IsString (fromString))
import Data.Time (getCurrentTime)
import Dojang.Types.FilePathExpression (EnvironmentVariable)
import System.Directory.OsPath (getHomeDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitWith)
import System.IO (stderr)
import System.IO.Error (isDoesNotExistError)
import Prelude hiding (readFile, writeFile)

import Control.Monad.Except (MonadError (..), tryError)
import Control.Monad.Logger
  ( Loc
  , LogLevel
  , LogSource
  , LogStr
  , LoggingT (..)
  , MonadLogger
  , logDebug
  , logDebugSH
  , logError
  , logInfo
  , logWarn
  , runStderrLoggingT
  )
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (concat, pack, unlines, unpack)
import System.OsPath
  ( OsPath
  , OsString
  , isAbsolute
  , makeRelative
  , normalise
  , splitDirectories
  , (</>)
  )
import TextShow (FromStringShow (FromStringShow), TextShow (showt))

import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , die'
  , dieWithErrors
  , pathStyleFor
  , printStderr'
  )
import Dojang.ExitCodes
  ( envFileReadError
  , machineStateError
  , manifestReadError
  , manifestUninitialized
  , noEnvFile
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Syntax.Env (readEnvFile)
import Dojang.Syntax.Manifest.Parser (Error, formatErrors, readManifestFile)
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.Types.Context (Context (..))
import Dojang.Types.Environment (Environment (..))
import Dojang.Types.Environment.Current
  ( MonadArchitecture (currentArchitecture)
  , MonadEnvironment (currentEnvironment)
  , MonadOperatingSystem (currentOperatingSystem)
  )
import Dojang.Types.MachineState
  ( MachineState (..)
  , StateError (StaleRepositoryCheckout)
  , catchStateIOErrors
  , ensureMachineId
  , formatStateError
  , markFirstApplied
  , prepareRepositoryStateWithOwnershipBeforeMigration
  , readRepositoryState
  , sameExistingPath
  )
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.Registry
  ( Registry (repositoryPath)
  , readRegistryStrict
  , registryFilename
  )
import Dojang.Types.Repository (Repository (..))
import Dojang.Types.RepositoryId (RepositoryId)


-- | The environment for the application monad.
data AppEnv = AppEnv
  { sourceDirectory :: OsPath
  -- ^ The source directory (i.e., repository).
  , repositoryExplicit :: Bool
  -- ^ Whether the repository path was explicitly supplied on the command line.
  , intermediateDirectory :: Maybe OsPath
  -- ^ An explicit intermediate directory override.  Relative paths are
  -- resolved from the source directory and persisted in machine state.
  , stateDirectory :: OsPath
  -- ^ Platform-native root directory for repository-scoped machine state.
  , manifestFile :: OsPath
  -- ^ The manifest file.
  , envFile :: OsPath
  -- ^ The environment file.
  , dryRun :: Bool
  -- ^ Whether to actually perform actions or just print what would be done.
  , debug :: Bool
  -- ^ Whether debug logging is enabled.
  }
  deriving (Show)


-- | The application monad for Dojang.
newtype (MonadFileSystem i, MonadError IOError i, MonadIO i) => App i v = App
  { unApp :: ReaderT AppEnv (LoggingT i) v
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppEnv
    , MonadLogger
    )


instance
  (MonadFileSystem i, MonadError IOError i, MonadIO i)
  => MonadError IOError (App i)
  where
  throwError = App . throwError
  catchError action handler = App $ catchError (unApp action) (unApp . handler)


instance (MonadFileSystem i, MonadIO i) => MonadFileSystem (App i) where
  encodePath = App . lift . lift . encodePath
  decodePath = App . lift . lift . decodePath
  getCurrentDirectory = App $ lift $ lift getCurrentDirectory
  exists = App . lift . lift . exists
  isFile = App . lift . lift . isFile
  isRegularFile = App . lift . lift . isRegularFile
  isDirectory = App . lift . lift . isDirectory
  isSymlink = App . lift . lift . isSymlink
  readFile = App . lift . lift . readFile
  writeFile dst = App . lift . lift . writeFile dst
  replaceFile src = App . lift . lift . replaceFile src
  writeTemporaryFile directory template contents =
    App $ lift $ lift $ writeTemporaryFile directory template contents
  withFileLock lockPath action = App $ ReaderT $ \appEnv ->
    LoggingT $ \logger ->
      withFileLock lockPath $
        runLoggingT (runReaderT (unApp action) appEnv) logger
  canonicalizePath = App . lift . lift . canonicalizePath
  readSymlinkTarget = App . lift . lift . readSymlinkTarget
  copyFile src = App . lift . lift . copyFile src
  copyFileWithMetadata src = App . lift . lift . copyFileWithMetadata src
  copyFilePermissions src = App . lift . lift . copyFilePermissions src
  createDirectory = App . lift . lift . createDirectory
  removeFile = App . lift . lift . removeFile
  removeDirectory = App . lift . lift . removeDirectory
  listDirectory = App . lift . lift . listDirectory
  getFileSize = App . lift . lift . getFileSize


currentEnvironment' :: (MonadFileSystem i, MonadIO i) => App i Environment
currentEnvironment' = do
  sourceDir <- asks (.sourceDirectory)
  envFile' <- asks (.envFile)
  let filePath = sourceDir </> envFile'
  $(logDebug) $ "Environment file path: " <> showt (FromStringShow filePath)
  result <-
    readEnvFile filePath `catchError` \e -> do
      if isDoesNotExistError e
        then do
          $(logWarn) $
            "Environment file not found: "
              <> showt (FromStringShow e)
          env <- liftIO currentEnvironment
          return $ Right (env, [])
        else die' noEnvFile $ showt e
  case result of
    Left errors -> do
      let formattedErrors =
            Data.Text.concat
              ["\n  " <> pack e | e <- toList errors]
      die' envFileReadError $ "Syntax errors in environment file:" <> formattedErrors
    Right (env, warnings) -> do
      $(logDebugSH) env
      forM_ warnings $ \w -> $(logWarn) $ fromString w
      return env


instance (MonadFileSystem i, MonadIO i) => MonadEnvironment (App i) where
  currentEnvironment = currentEnvironment'


instance (MonadFileSystem i, MonadIO i) => MonadOperatingSystem (App i) where
  currentOperatingSystem = (.operatingSystem) <$> currentEnvironment'


instance (MonadFileSystem i, MonadIO i) => MonadArchitecture (App i) where
  currentArchitecture = (.architecture) <$> currentEnvironment'


runAppWithoutLogging
  :: (MonadFileSystem i, MonadIO i) => AppEnv -> App i a -> i a
runAppWithoutLogging env app = runAppWithLogging env app (\_ _ _ _ -> pure ())


-- | Run the application monad with logging handled by the given function.
runAppWithLogging
  :: (MonadFileSystem i, MonadIO i)
  => AppEnv
  -> App i a
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> i a
runAppWithLogging env app = runLoggingT (runReaderT app.unApp env)


-- | Run the application monad with logging to stderr.
runAppWithStderrLogging
  :: (MonadFileSystem i, MonadIO i) => AppEnv -> App i a -> i a
runAppWithStderrLogging env = runStderrLoggingT . (`runReaderT` env) . unApp


manifestPath :: (MonadIO i) => App i OsPath
manifestPath = do
  sourceDir <- asks (.sourceDirectory)
  manifestFile' <- asks (.manifestFile)
  let path = sourceDir </> manifestFile'
  $(logDebug) $ "Manifest path: " <> showt (FromStringShow path)
  return path


loadManifest
  :: (MonadFileSystem i, MonadIO i) => App i (Either Error (Maybe Manifest))
loadManifest = do
  filename <- manifestPath
  result <- tryError (readManifestFile filename)
  case result of
    Left e
      | isDoesNotExistError e -> do
          $(logInfo) "Manifest file not found"
          return $ Right Nothing
    Left e -> do
      $(logError) $ "Error reading manifest file: " <> showt (FromStringShow e)
      throwError e
    Right (Left err) -> do
      $(logError) $
        "Error parsing manifest file: "
          <> Data.Text.unlines (formatErrors err)
      return $ Left err
    Right (Right (manifest, warnings)) -> do
      forM_ warnings $ \w -> $(logWarn) w
      $(logDebugSH) manifest
      return $ Right $ Just manifest


doesManifestExist :: (MonadFileSystem i, MonadIO i) => App i Bool
doesManifestExist = do
  filePath <- manifestPath
  exists' <- exists filePath
  $(logInfo) $
    "Manifest file "
      <> showt (FromStringShow filePath)
      <> " "
      <> if exists' then "exists" else "does not exist"
  return exists'


saveManifest :: (MonadFileSystem i, MonadIO i) => Manifest -> App i OsPath
saveManifest manifest = do
  filename <- manifestPath
  $(logDebugSH) manifest
  writeManifestFile manifest filename `catchError` \e -> do
    $(logError) $ "Error writing manifest file: " <> showt (FromStringShow e)
    throwError e
  $(logInfo) $
    "Manifest file "
      <> showt (FromStringShow filename)
      <> " written"
  return filename


loadRepository
  :: (MonadFileSystem i, MonadIO i) => App i (Either Error (Maybe Repository))
loadRepository = do
  sourceDir <- asks (normalise . (.sourceDirectory))
  result <- loadManifest
  case result of
    Left err -> return $ Left err
    Right Nothing -> return $ Right Nothing
    Right (Just manifest) -> do
      state <- prepareMachineState manifest
      return $
        Right $
          Just $
            Repository sourceDir state.intermediatePath manifest


-- | Validates that a recorded checkout still declares its repository identity.
--
-- Returns the checkout path only when its recorded manifest exists, parses,
-- and contains the identity stored in the machine-state record.
validateRepositoryCheckout
  :: (MonadFileSystem m)
  => MachineState
  -- ^ Selected machine-state record.
  -> m (Either StateError OsPath)
validateRepositoryCheckout state = catchStateIOErrors $ do
  ownsIdentity <-
    checkoutHasRepositoryIdentity
      state.manifestPath
      state.repositoryId
      state.checkoutPath
  return $
    if ownsIdentity
      then Right state.checkoutPath
      else Left $ StaleRepositoryCheckout state.repositoryId state.checkoutPath


-- | Applies a validated automatic repository selection to the application
-- environment.
--
-- The recorded manifest path is restored unless the user explicitly selected
-- a manifest on the command line.
applyAutomaticRepositorySelection
  :: Bool
  -- ^ Whether the manifest path was explicitly supplied.
  -> MachineState
  -- ^ The selected repository state.
  -> AppEnv
  -- ^ The parsed application environment.
  -> AppEnv
applyAutomaticRepositorySelection manifestExplicit state appEnv =
  appEnv
    { sourceDirectory = state.checkoutPath
    , manifestFile =
        if manifestExplicit then appEnv.manifestFile else state.manifestPath
    }


-- | Loads or creates machine state, preserving known legacy apply history.
prepareMachineState
  :: (MonadFileSystem i, MonadIO i) => Manifest -> App i MachineState
prepareMachineState = prepareMachineStateWithLegacyHistory True


-- | Loads or creates machine state after validating a new migration and then
-- running the supplied action before migration changes the snapshot layout.
prepareMachineStateBeforeMigration
  :: (MonadFileSystem i, MonadIO i)
  => Manifest
  -> App i ()
  -> App i MachineState
prepareMachineStateBeforeMigration =
  prepareMachineState' True


-- | Creates machine state for a newly initialized repository identity.
--
-- Unlike migration, initialization must not adopt a legacy snapshot or inherit
-- lifecycle history from a repository at the same checkout path.
prepareNewMachineState
  :: (MonadFileSystem i, MonadIO i) => Manifest -> App i MachineState
prepareNewMachineState manifest =
  prepareNewMachineStateBeforeMigration manifest (return ())


-- | Creates machine state after validating a new repository and running an
-- action that publishes its checkout files.
prepareNewMachineStateBeforeMigration
  :: (MonadFileSystem i, MonadIO i)
  => Manifest
  -> App i ()
  -> App i MachineState
prepareNewMachineStateBeforeMigration manifest beforeMigration = do
  ensureNoLegacySnapshotForInitialization
  prepareMachineState' False manifest beforeMigration


-- | Refuses initialization when the checkout already has legacy state.
--
-- Migration is the only command allowed to adopt and remove a worktree-local
-- snapshot.  This check runs before initialization output is written and is
-- repeated by the new-state preparation APIs to keep them safe on their own.
ensureNoLegacySnapshotForInitialization
  :: (MonadFileSystem i, MonadIO i) => App i ()
ensureNoLegacySnapshotForInitialization = do
  sourceDir' <- asks (.sourceDirectory)
  sourceDir <- normalise <$> makeAbsolute sourceDir'
  legacyName <- encodePath ".dojang"
  let legacy = sourceDir </> legacyName
  present <- exists legacy
  symbolicLink <- isSymlink legacy
  when (present || symbolicLink) $ do
    pathStyle <- pathStyleFor stderr
    die' machineStateError $
      "Cannot initialize a checkout that already contains legacy state at "
        <> pathStyle legacy
        <> ".  Run `dojang migrate` to adopt it explicitly."


prepareMachineStateWithLegacyHistory
  :: (MonadFileSystem i, MonadIO i)
  => Bool
  -> Manifest
  -> App i MachineState
prepareMachineStateWithLegacyHistory inheritLegacyHistory manifest =
  prepareMachineState' inheritLegacyHistory manifest (return ())


prepareMachineState'
  :: (MonadFileSystem i, MonadIO i)
  => Bool
  -> Manifest
  -> App i ()
  -> App i MachineState
prepareMachineState' inheritLegacyHistory manifest beforeMigration =
  case manifest.repositoryId of
    Nothing -> do
      codeStyle <- codeStyleFor stderr
      printStderr' Error "This repository has no stable repository identity."
      printStderr' Note $
        "Run `" <> codeStyle "dojang migrate" <> "' to migrate it safely."
      liftIO $ exitWith machineStateError
    Just repositoryId -> do
      stateDir <- asks (.stateDirectory)
      sourceDir' <- asks (.sourceDirectory)
      manifestFile' <- asks (.manifestFile)
      sourceDir <- normalise <$> makeAbsolute sourceDir'
      let configuredManifest =
            if isAbsolute manifestFile'
              then manifestFile'
              else sourceDir </> manifestFile'
      resolvedManifest <- normalise <$> makeAbsolute configuredManifest
      intermediate <- asks (.intermediateDirectory)
      machineResult <- ensureMachineId stateDir
      machineId <- case machineResult of
        Left err -> die' machineStateError $ formatStateError err
        Right identifier -> return identifier
      existingState <- readRepositoryState stateDir repositoryId machineId
      legacyFirstApplied <- case existingState of
        Left err -> die' machineStateError $ formatStateError err
        Right (Just _) -> return False
        Right Nothing
          | not inheritLegacyHistory -> return False
          | otherwise -> do
              legacyRegistry <- readValidatedLegacyRegistry
              case legacyRegistry of
                Nothing -> return False
                Just registry -> do
                  registryExists <- isDirectory registry.repositoryPath
                  sourceExists <- isDirectory sourceDir
                  if registryExists && sourceExists
                    then sameExistingPath registry.repositoryPath sourceDir
                    else return False
      now <- liftIO getCurrentTime
      stateResult <-
        catchStateIOErrors $
          prepareRepositoryStateWithOwnershipBeforeMigration
            stateDir
            repositoryId
            machineId
            sourceDir
            resolvedManifest
            intermediate
            ( \checkout storedManifest ->
                checkoutOwnsRecordedRepositoryIdentity
                  storedManifest
                  repositoryId
                  checkout
            )
            legacyFirstApplied
            now
            beforeMigration
      case stateResult of
        Left err -> die' machineStateError $ formatStateError err
        Right (state, _) -> return state


-- | Reads the legacy registry and reports malformed data as a machine-state
-- error instead of treating it as absent.
readValidatedLegacyRegistry
  :: (MonadFileSystem i, MonadIO i) => App i (Maybe Registry)
readValidatedLegacyRegistry = do
  homeDirectory <- liftIO getHomeDirectory
  registryRead <-
    catchStateIOErrors $
      Right <$> readRegistryStrict (homeDirectory </> registryFilename)
  legacyResult <- case registryRead of
    Left err -> die' machineStateError $ formatStateError err
    Right result -> return result
  case legacyResult of
    Left errors ->
      dieWithErrors machineStateError $
        "The legacy registry is malformed.  Repair or remove it before "
          <> "creating machine state."
          : errors
    Right value -> return value


checkoutHasRepositoryIdentity
  :: (MonadFileSystem m)
  => OsPath
  -> RepositoryId
  -> OsPath
  -> m Bool
checkoutHasRepositoryIdentity manifestFile' repositoryId checkout = do
  checkoutExists <- isDirectory checkout
  if not checkoutExists
    then return False
    else do
      let manifestCandidate = normalise $ checkout </> manifestFile'
      resolvedCheckout <- canonicalizePath checkout
      resolvedManifest <- canonicalizePath manifestCandidate
      parentComponent <- encodePath ".."
      let relative = makeRelative resolvedCheckout resolvedManifest
      let insideCheckout =
            not (isAbsolute relative)
              && case splitDirectories relative of
                [] -> True
                first : _ -> first /= parentComponent
      if not insideCheckout
        then return False
        else manifestDeclaresRepositoryIdentity manifestCandidate repositoryId


-- | Checks ownership of an existing checkout using the manifest path stored
-- in its state record.
--
-- Unlike automatic selection, duplicate detection accepts a recorded absolute
-- manifest outside the checkout.  A live old checkout with that manifest must
-- retain ownership rather than silently transferring its snapshot.
checkoutOwnsRecordedRepositoryIdentity
  :: (MonadFileSystem m)
  => OsPath
  -> RepositoryId
  -> OsPath
  -> m Bool
checkoutOwnsRecordedRepositoryIdentity manifestFile' repositoryId checkout = do
  checkoutExists <- isDirectory checkout
  if not checkoutExists
    then return False
    else do
      let manifestCandidate =
            normalise $
              if isAbsolute manifestFile'
                then manifestFile'
                else checkout </> manifestFile'
      manifestDeclaresRepositoryIdentity manifestCandidate repositoryId


manifestDeclaresRepositoryIdentity
  :: (MonadFileSystem m) => OsPath -> RepositoryId -> m Bool
manifestDeclaresRepositoryIdentity manifestCandidate repositoryId = do
  loaded <- tryError $ readManifestFile manifestCandidate
  return $ case loaded of
    Right (Right (manifest, _)) ->
      manifest.repositoryId == Just repositoryId
    _ -> False


-- | Marks a repository's first successful apply in machine-local state.
markMachineStateApplied
  :: (MonadFileSystem i, MonadIO i) => MachineState -> App i ()
markMachineStateApplied state = do
  stateDir <- asks (.stateDirectory)
  now <- liftIO getCurrentTime
  result <- markFirstApplied stateDir now state
  case result of
    Left err -> die' machineStateError $ formatStateError err
    Right _ -> return ()


ensureRepository :: (MonadFileSystem i, MonadIO i) => App i Repository
ensureRepository = do
  result <- loadRepository
  case result of
    Left e ->
      dieWithErrors manifestReadError $ formatErrors e
    Right Nothing -> do
      printStderr' Error "No manifest found."
      codeStyle <- codeStyleFor stderr
      printStderr'
        Note
        ("Run `" <> codeStyle "dojang init" <> "' to create one.")
      liftIO $ exitWith manifestUninitialized
    Right (Just repo) -> return repo


ensureContext :: (MonadFileSystem i, MonadIO i) => App i (Context (App i))
ensureContext = do
  repo <- ensureRepository
  currentEnv <- currentEnvironment'
  $(logDebugSH) currentEnv
  return $ Context repo currentEnv lookupEnv'


lookupEnv'
  :: (MonadFileSystem i, MonadIO i) => EnvironmentVariable -> i (Maybe OsString)
lookupEnv' env = do
  value <- liftIO $ lookupEnv $ unpack env
  case value of
    Just v -> do
      Just <$> encodePath v
    Nothing -> return Nothing
