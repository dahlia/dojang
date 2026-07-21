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


externalProgramNonZeroExit :: ExitCode
externalProgramNonZeroExit = ExitFailure 4


unsupportedOnEnvError :: ExitCode
unsupportedOnEnvError = ExitFailure 9


manifestUninitialized :: ExitCode
manifestUninitialized = ExitFailure 10


manifestReadError :: ExitCode
manifestReadError = ExitFailure 11


manifestAlreadyExists :: ExitCode
manifestAlreadyExists = ExitFailure 12


machineStateError :: ExitCode
machineStateError = ExitFailure 13


noEnvFile :: ExitCode
noEnvFile = ExitFailure 20


envFileReadError :: ExitCode
envFileReadError = ExitFailure 21


missingMachineFactError :: ExitCode
missingMachineFactError = ExitFailure 22


conflictError :: ExitCode
conflictError = ExitFailure 30


sourceCannotBeTargetError :: ExitCode
sourceCannotBeTargetError = ExitFailure 31


fileNotRoutedError :: ExitCode
fileNotRoutedError = ExitFailure 32


ignoredFileError :: ExitCode
ignoredFileError = ExitFailure 33


accidentalDeletionWarning :: ExitCode
accidentalDeletionWarning = ExitFailure 34


ambiguousRouteError :: ExitCode
ambiguousRouteError = ExitFailure 35


userCancelledError :: ExitCode
userCancelledError = ExitFailure 36


lifecycleSelectionError :: ExitCode
lifecycleSelectionError = ExitFailure 37


-- | The route configuration cannot give every destination one owner.
routeOwnershipError :: ExitCode
routeOwnershipError = ExitFailure 38


-- | A route codec could not be validated or evaluated.
codecError :: ExitCode
codecError = ExitFailure 39


hookFailedError :: ExitCode
hookFailedError = ExitFailure 40
