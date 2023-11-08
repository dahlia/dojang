{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Dojang.ExitCodes where

import System.Exit (ExitCode (..))


unhandledError :: ExitCode
unhandledError = ExitFailure $ -1


cliError :: ExitCode
cliError = ExitFailure 1


fileWriteError :: ExitCode
fileWriteError = ExitFailure 2


fileNotFoundError :: ExitCode
fileNotFoundError = ExitFailure 3


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


conflictError :: ExitCode
conflictError = ExitFailure 30


sourceCannotBeTargetError :: ExitCode
sourceCannotBeTargetError = ExitFailure 31


fileNotRoutedError :: ExitCode
fileNotRoutedError = ExitFailure 32


ignoredFileError :: ExitCode
ignoredFileError = ExitFailure 33
