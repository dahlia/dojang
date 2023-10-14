{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.Env (env) where

import System.Exit (ExitCode (..))

import Control.Monad.Logger (logDebugSH)
import Data.Text.IO (putStr, writeFile)
import Prelude hiding (putStr, writeFile)

import Dojang.App (App, currentEnvironment')
import Dojang.Syntax.Env (writeEnvironment)
import Dojang.Types.Environment.Current (MonadEnvironment (..))
import Control.Monad.IO.Class (MonadIO (liftIO))


env :: Bool -> FilePath -> App ExitCode
env ignoreEnvFile outputFile = do
  currentEnv <-
    if ignoreEnvFile then liftIO currentEnvironment else currentEnvironment'
  $(logDebugSH) currentEnv
  let toml = writeEnvironment currentEnv
  liftIO
    $ if outputFile == "-"
      then putStr toml
      else writeFile outputFile toml
  return ExitSuccess
