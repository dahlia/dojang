{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.Env (env) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Exit (ExitCode (..))
import Prelude hiding (putStr)

import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (logDebugSH, logError)
import Data.Text.IO (putStr)
import Options.Applicative.Path (hyphen)
import System.OsPath (OsPath)
import TextShow (TextShow (showt))

import Dojang.App (App, currentEnvironment')
import Dojang.MonadFileSystem (MonadFileSystem)
import Dojang.Syntax.Env (writeEnvFile, writeEnvironment)
import Dojang.Types.Environment.Current (MonadEnvironment (..))


env :: (MonadFileSystem i, MonadIO i) => Bool -> OsPath -> App i ExitCode
env ignoreEnvFile outputPath = do
  currentEnv <-
    if ignoreEnvFile then liftIO currentEnvironment else currentEnvironment'
  $(logDebugSH) currentEnv
  if outputPath == hyphen
    then do
      liftIO $ putStr $ writeEnvironment currentEnv
      return ExitSuccess
    else
      ( do
          () <- writeEnvFile currentEnv outputPath
          return ExitSuccess
      )
        `catchError` \err -> do
          $(logError) $ showt err
          return $ ExitFailure 1
