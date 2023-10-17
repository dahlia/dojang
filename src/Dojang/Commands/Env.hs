{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.Env (env) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Exit (ExitCode (..))
import Prelude hiding (putStr)

import Control.Monad.Logger (logDebugSH, logError)
import Data.Text.IO (putStr)
import System.OsPath (encodeFS)
import TextShow (TextShow (showt))

import Dojang.App (App, currentEnvironment')
import Dojang.Syntax.Env (writeEnvFile, writeEnvironment)
import Dojang.Types.Environment.Current (MonadEnvironment (..))


env :: Bool -> FilePath -> App ExitCode
env ignoreEnvFile outputFile = do
  currentEnv <-
    if ignoreEnvFile then liftIO currentEnvironment else currentEnvironment'
  $(logDebugSH) currentEnv
  if outputFile == "-"
    then do
      liftIO $ putStr $ writeEnvironment currentEnv
      return ExitSuccess
    else do
      outputPath' <- liftIO $ encodeFS outputFile
      writeResult <- writeEnvFile currentEnv outputPath'
      case writeResult of
        Left err -> do
          $(logError) $ showt err
          return $ ExitFailure 1
        Right () -> return ExitSuccess
