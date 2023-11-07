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
  , currentEnvironment'
  , doesManifestExist
  , ensureContext
  , ensureRepository
  , loadManifest
  , loadRepository
  , runAppWithLogging
  , runAppWithStderrLogging
  , runAppWithoutLogging
  , saveManifest
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty (toList)
import Data.String (IsString (fromString))
import Dojang.Types.FilePathExpression (EnvironmentVariable)
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
  , LoggingT (runLoggingT)
  , MonadLogger
  , logDebug
  , logDebugSH
  , logError
  , logInfo
  , logWarn
  , runStderrLoggingT
  )
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (concat, pack, unlines, unpack)
import System.OsPath (normalise, (</>))
import System.OsPath.Types (OsPath)
import System.OsString (OsString)
import TextShow (FromStringShow (FromStringShow), TextShow (showt))

import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , die'
  , dieWithErrors
  , printStderr'
  )
import Dojang.ExitCodes
  ( envFileReadError
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
import Dojang.Types.Manifest (Manifest)
import Dojang.Types.Repository (Repository (..))


-- | The environment for the application monad.
data AppEnv = AppEnv
  { sourceDirectory :: OsPath
  -- ^ The source directory (i.e., repository).
  , intermediateDirectory :: OsPath
  -- ^ The intermediate directory which is managed by Dojang.  It's normally
  -- @.dojang@ in the source directory.
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
  exists = App . lift . lift . exists
  isFile = App . lift . lift . isFile
  isDirectory = App . lift . lift . isDirectory
  isSymlink = App . lift . lift . isSymlink
  readFile = App . lift . lift . readFile
  writeFile dst = App . lift . lift . writeFile dst
  readSymlinkTarget = App . lift . lift . readSymlinkTarget
  copyFile src = App . lift . lift . copyFile src
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
          $(logWarn)
            $ "Environment file not found: "
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
      $(logError)
        $ "Error parsing manifest file: "
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
  $(logInfo)
    $ "Manifest file "
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
  $(logInfo)
    $ "Manifest file "
    <> showt (FromStringShow filename)
    <> " written"
  return filename


loadRepository
  :: (MonadFileSystem i, MonadIO i) => App i (Either Error (Maybe Repository))
loadRepository = do
  sourceDir <- asks (normalise . (.sourceDirectory))
  intermediateDir <- asks (normalise . (.intermediateDirectory))
  result <- loadManifest
  case result of
    Left err -> return $ Left err
    Right Nothing -> return $ Right Nothing
    Right (Just manifest) -> do
      return
        $ Right
        $ Just
        $ Repository sourceDir (sourceDir </> intermediateDir) manifest


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
