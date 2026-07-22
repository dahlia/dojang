{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.Env (env) where

import Control.Monad.IO.Class (MonadIO)
import System.Exit (ExitCode (..))
import Prelude hiding (putStr)

import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (logDebugSH, logError)
import Options.Applicative.Path (hyphen)
import System.OsPath (OsPath)
import TextShow (TextShow (showt))

import Dojang.App (App, currentEnvironment')
import Dojang.CommandEffect
  ( MonadCommandEffect (detectEnvironment, writeStream)
  , OutputStream (OutputStandard)
  )
import Dojang.ExitCodes (fileWriteError)
import Dojang.MonadFileSystem (MonadFileSystem)
import Dojang.Syntax.Env (writeEnvFile, writeEnvironment)


env :: (MonadFileSystem i, MonadIO i) => Bool -> OsPath -> App i ExitCode
env ignoreEnvFile outputPath = do
  currentEnv <-
    if ignoreEnvFile then detectEnvironment else currentEnvironment'
  $(logDebugSH) currentEnv
  if outputPath == hyphen
    then do
      writeStream OutputStandard $ writeEnvironment currentEnv
      return ExitSuccess
    else
      ( do
          () <- writeEnvFile currentEnv outputPath
          return ExitSuccess
      )
        `catchError` \err -> do
          $(logError) $ showt err
          return fileWriteError
