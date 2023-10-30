{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Dojang.ExitCodes where

import System.Exit (ExitCode (..))


unhandledError :: ExitCode
unhandledError = ExitFailure $ -1


cliError :: ExitCode
cliError = ExitFailure 1


fileWriteError :: ExitCode
fileWriteError = ExitFailure 2


manifestUninitialized :: ExitCode
manifestUninitialized = ExitFailure 10


manifestReadError :: ExitCode
manifestReadError = ExitFailure 11


manifestAlreadyExists :: ExitCode
manifestAlreadyExists = ExitFailure 12


noEnvFile :: ExitCode
noEnvFile = ExitFailure 20


envFileReadError :: ExitCode
envFileReadError = ExitFailure 21
