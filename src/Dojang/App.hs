{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , loadManifest
  , runAppWithLogging
  , runAppWithStderrLogging
  , runAppWithoutLogging
  , saveManifest
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty (toList)
import Data.String (IsString (fromString))
import System.IO.Error (isDoesNotExistError, tryIOError)
import Prelude hiding (readFile, writeFile)

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
import Data.Text (concat, pack)
import System.OsPath ((</>))
import System.OsPath.Types (OsPath)
import TextShow (FromStringShow (FromStringShow), TextShow (showt))

import Control.Monad.Trans (MonadTrans (lift))
import Dojang.Commands (die)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Syntax.Env (EnvFileError (..), readEnvFile)
import Dojang.Syntax.Manifest.Parser (Error, formatError, readManifestFile)
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.Types.Environment (Environment (..))
import Dojang.Types.Environment.Current
  ( MonadArchitecture (currentArchitecture)
  , MonadEnvironment (currentEnvironment)
  , MonadOperatingSystem (currentOperatingSystem)
  )
import Dojang.Types.Manifest (Manifest)


-- | The environment for the application monad.
data AppEnv = AppEnv
  { sourceDirectory :: OsPath
  -- ^ The source directory (i.e., repository).
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
newtype (MonadFileSystem i, MonadIO i) => App i v = App
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


instance forall i. (MonadFileSystem i, MonadIO i) => MonadFileSystem (App i) where
  encodePath = App . lift . lift . encodePath
  decodePath = App . lift . lift . decodePath
  exists = App . lift . lift . exists
  isFile = App . lift . lift . isFile
  isDirectory = App . lift . lift . isDirectory
  readFile = App . lift . lift . readFile
  writeFile dst = App . lift . lift . writeFile dst
  copyFile src = App . lift . lift . copyFile src
  createDirectory = App . lift . lift . createDirectory
  removeFile = App . lift . lift . removeFile
  listDirectory = App . lift . lift . listDirectory
  getFileSize = App . lift . lift . getFileSize


currentEnvironment' :: (MonadFileSystem i, MonadIO i) => App i Environment
currentEnvironment' = do
  sourceDir <- asks (.sourceDirectory)
  envFile' <- asks (.envFile)
  let filePath = sourceDir </> envFile'
  $(logDebug) $ "Environment file path: " <> showt (FromStringShow filePath)
  result <- readEnvFile filePath
  case result of
    Left (IOError e) | isDoesNotExistError e -> do
      $(logWarn) $ "Environment file not found: " <> showt (FromStringShow e)
      liftIO currentEnvironment
    Left (IOError e) -> do
      die $ showt e
    Left (TomlErrors errors) -> do
      let formattedErrors =
            Data.Text.concat
              ["\n  " <> pack e | e <- toList errors]
      die $ "Syntax errors in environment file:" <> formattedErrors
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


manifestPath :: (MonadFileSystem i, MonadIO i) => App i OsPath
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
  result <- liftIO $ tryIOError (readManifestFile filename)
  case result of
    Left e
      | isDoesNotExistError e -> do
          $(logInfo) "Manifest file not found"
          return $ Right Nothing
    Left e -> do
      $(logError) $ "Error reading manifest file: " <> showt (FromStringShow e)
      liftIO $ ioError e
    Right (Left err) -> do
      $(logError) $ "Error parsing manifest file: " <> formatError err
      return $ Left err
    Right (Right (manifest, warnings)) -> do
      forM_ warnings $ \w -> $(logWarn) w
      $(logDebugSH) manifest
      return $ Right $ Just manifest


doesManifestExist :: (MonadFileSystem i, MonadIO i) => App i Bool
doesManifestExist = do
  filePath <- manifestPath
  exists' <- liftIO $ exists filePath
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
  writeResult <- writeManifestFile manifest filename
  case writeResult of
    Left e -> do
      $(logError) $ "Error writing manifest file: " <> showt (FromStringShow e)
      liftIO $ ioError e
    Right () -> do
      $(logInfo)
        $ "Manifest file "
        <> showt (FromStringShow filename)
        <> " written"
      return filename
