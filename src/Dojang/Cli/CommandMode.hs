{-# LANGUAGE NoFieldSelectors #-}

-- | Repository and machine-state requirements for CLI commands.
module Dojang.Cli.CommandMode
  ( CommandMode (..)
  , ambiguousRepositoryExitCode
  , environmentCommandMode
  , initializationCommandMode
  , metaCommandMode
  , migrationCommandMode
  , repositoryCommandMode
  ) where

import System.Exit (ExitCode)

import Dojang.ExitCodes (machineStateError)


-- | Controls machine-state validation and automatic repository selection.
data CommandMode = CommandMode
  { readsMachineState :: Bool
  -- ^ Whether the command must validate the machine-state store before running.
  , autoSelectsRepository :: Bool
  -- ^ Whether the command may use a recorded checkout when @-r@ is omitted.
  }
  deriving (Eq, Show)


-- | Mode for commands that operate on an existing repository.
repositoryCommandMode :: CommandMode
repositoryCommandMode = CommandMode True True


-- | Mode for repository initialization.
initializationCommandMode :: CommandMode
initializationCommandMode = CommandMode True False


-- | Mode for migrating the repository selected by the command-line path.
migrationCommandMode :: CommandMode
migrationCommandMode = CommandMode True False


-- | Mode for environment inspection.
environmentCommandMode :: Bool -> CommandMode
environmentCommandMode ignoreEnvFile =
  if ignoreEnvFile then metaCommandMode else repositoryCommandMode


-- | Mode for commands that do not access repository or machine state.
metaCommandMode :: CommandMode
metaCommandMode = CommandMode False False


-- | Exit code used when repository state cannot select one checkout.
ambiguousRepositoryExitCode :: ExitCode
ambiguousRepositoryExitCode = machineStateError
