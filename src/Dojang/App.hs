{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.String (IsString (fromString))
import System.Exit (die)
import System.IO.Error (isDoesNotExistError, tryIOError)

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
import System.Directory (doesFileExist)
import System.OsPath (decodeFS, encodeFS, (</>))
import System.OsPath.Types (OsPath)
import TextShow (FromStringShow (FromStringShow), TextShow (showt))

import Dojang.Syntax.Manifest.Parser (Error, formatError, readManifestFile)
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.Types.Manifest (Manifest)

import Dojang.Syntax.Env (readEnvFile)
import Dojang.Types.Environment (Environment (..))
import Dojang.Types.Environment.Current
  ( MonadArchitecture (currentArchitecture)
  , MonadEnvironment (currentEnvironment)
  , MonadOperatingSystem (currentOperatingSystem)
  )


-- | The environment for the application monad.
data AppEnv = AppEnv
  { sourceDirectory :: FilePath
  -- ^ The source directory (i.e., repository).
  , manifestFile :: FilePath
  -- ^ The manifest file.
  , envFile :: FilePath
  -- ^ The environment file.
  , debug :: Bool
  -- ^ Whether debug logging is enabled.
  }
  deriving (Show)


-- | The application monad for Dojang.
newtype App a = App {unApp :: ReaderT AppEnv (LoggingT IO) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppEnv
    , MonadLogger
    )


die' :: String -> App a
die' = liftIO . die


currentEnvironment' :: App Environment
currentEnvironment' = do
  sourceDir <- asks (.sourceDirectory)
  envFile' <- asks (.envFile)
  sourceDirOP <- liftIO $ encodeFS sourceDir
  envFileOP <- liftIO $ encodeFS envFile'
  let filePath = sourceDirOP </> envFileOP
  $(logDebug) $ "Environment file path: " <> showt (FromStringShow filePath)
  result <- liftIO $ tryIOError $ readEnvFile filePath
  case result of
    Left e | isDoesNotExistError e -> do
      $(logWarn) $ "Environment file not found: " <> showt (FromStringShow e)
      liftIO currentEnvironment
    Left e -> do
      die' $ "Error: " <> show e
    Right (Left errors) -> do
      let formattedErrors = concatMap ("\n  " ++) errors
      die' $ "Error: Syntax errors in environment file:" <> formattedErrors
    Right (Right (env, warnings)) -> do
      $(logDebugSH) env
      forM_ warnings $ \w -> $(logWarn) $ fromString w
      return env


instance MonadEnvironment App where
  currentEnvironment = currentEnvironment'


instance MonadOperatingSystem App where
  currentOperatingSystem = (.operatingSystem) <$> currentEnvironment'


instance MonadArchitecture App where
  currentArchitecture = (.architecture) <$> currentEnvironment'


runAppWithoutLogging :: AppEnv -> App a -> IO a
runAppWithoutLogging env app = runAppWithLogging env app (\_ _ _ _ -> pure ())


-- | Run the application monad with logging handled by the given function.
runAppWithLogging
  :: AppEnv
  -> App a
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO a
runAppWithLogging env app = runLoggingT (runReaderT app.unApp env)


-- | Run the application monad with logging to stderr.
runAppWithStderrLogging :: AppEnv -> App a -> IO a
runAppWithStderrLogging env = runStderrLoggingT . (`runReaderT` env) . unApp


manifestPath :: App OsPath
manifestPath = do
  sourceDir <- asks (.sourceDirectory)
  manifestFile' <- asks (.manifestFile)
  sourceDirOP <- liftIO $ encodeFS sourceDir
  manifestOP <- liftIO $ encodeFS manifestFile'
  let path = sourceDirOP </> manifestOP
  $(logDebug) $ "Manifest path: " <> showt (FromStringShow path)
  return path


loadManifest :: App (Either Error (Maybe Manifest))
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


doesManifestExist :: App Bool
doesManifestExist = do
  filename <- manifestPath
  filePath <- liftIO $ decodeFS filename
  exists <- liftIO $ doesFileExist filePath
  $(logInfo)
    $ "Manifest file "
    <> showt (FromStringShow filename)
    <> " "
    <> if exists then "exists" else "does not exist"
  return exists


saveManifest :: Manifest -> App OsPath
saveManifest manifest = do
  filename <- manifestPath
  $(logDebugSH) manifest
  writeManifestFile manifest filename
  $(logInfo)
    $ "Manifest file "
    <> showt (FromStringShow filename)
    <> " written"
  return filename
