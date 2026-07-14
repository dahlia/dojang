{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Explicit migration to repository-scoped machine state.
module Dojang.Commands.Migrate (migrate) where

import Control.Monad (when)
import Control.Monad.Except (MonadError (catchError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.IO (stderr)
import System.OsPath ((</>))
import Prelude hiding (readFile, writeFile)

import Dojang.App
  ( App
  , AppEnv (manifestFile, sourceDirectory, stateDirectory)
  , loadManifest
  , prepareMachineState
  )
import Dojang.Commands
  ( Admonition (..)
  , codeStyleFor
  , die'
  , dieWithErrors
  , pathStyleFor
  , printStderr
  , printStderr'
  )
import Dojang.ExitCodes
  ( machineStateError
  , manifestReadError
  , manifestUninitialized
  )
import Dojang.MonadFileSystem
  ( MonadFileSystem (..)
  , writeFileAtomically
  )
import Dojang.Syntax.Manifest.Parser (formatErrors)
import Dojang.Syntax.Manifest.Writer (insertRepositoryId)
import Dojang.Types.MachineState
  ( MachineState (MachineState)
  , catchStateIOErrors
  , formatStateError
  , manifestIdentityLockPath
  , validateMigrationStateRoot
  , withStateFileLock
  )
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.RepositoryId (newRepositoryId)


-- | Migrates an existing manifest and its local intermediate snapshot.
migrate :: (MonadFileSystem i, MonadIO i) => App i ExitCode
migrate = do
  stateRoot <- asks (.stateDirectory)
  sourceDir <- asks (.sourceDirectory)
  validated <- validateMigrationStateRoot sourceDir stateRoot
  case validated of
    Left err -> die' machineStateError $ formatStateError err
    Right () -> return ()
  stateRootResult <- catchStateIOErrors $ do
    createDirectories stateRoot
    return $ Right ()
  case stateRootResult of
    Left err -> die' machineStateError $ formatStateError err
    Right () -> return ()
  lockedManifest <-
    withStateFileLock (manifestIdentityLockPath stateRoot) $ do
      manifest <- loadRequiredManifest
      case manifest.repositoryId of
        Just _ -> return manifest
        Nothing -> do
          manifestFile' <- asks (.manifestFile)
          let filename = sourceDir </> manifestFile'
          symbolicLink <- isSymlink filename
          when symbolicLink $ do
            pathStyle <- pathStyleFor stderr
            die' manifestReadError $
              "Cannot migrate symbolic-link manifest "
                <> pathStyle filename
                <> ".  Replace it with a regular file and retry."
          identifier <- newRepositoryId
          originalBytes <- readManifestBytes filename
          original <- case decodeUtf8' originalBytes of
            Left err ->
              dieWithErrors manifestReadError [invalidUtf8Message err]
            Right source -> return source
          ( writeFileAtomically filename "dojang.toml.tmp" $
              encodeUtf8 $
                insertRepositoryId identifier original
            )
            `catchError` manifestWriteIOError
          pathStyle <- pathStyleFor stderr
          printStderr $
            "Stable repository identity added to " <> pathStyle filename <> "."
          return manifest{repositoryId = Just identifier}
  migratedManifest <- case lockedManifest of
    Left err -> die' machineStateError $ formatStateError err
    Right manifest -> return manifest
  state <- prepareMachineState migratedManifest
  let MachineState _ _ _ _ _ intermediatePath _ _ _ = state
  pathStyle <- pathStyleFor stderr
  printStderr $
    "Repository machine state is ready at "
      <> pathStyle intermediatePath
      <> "."
  return ExitSuccess
 where
  loadRequiredManifest = do
    loaded <-
      loadManifest `catchError` manifestIOError
    case loaded of
      Left err -> dieWithErrors manifestReadError $ formatErrors err
      Right Nothing -> do
        printStderr' Error "No manifest found."
        codeStyle <- codeStyleFor stderr
        printStderr' Note $
          "Run `" <> codeStyle "dojang init" <> "' to create one."
        liftIO $ exitWith manifestUninitialized
      Right (Just value) -> return value

  readManifestBytes filename =
    readFile filename `catchError` manifestIOError

  manifestIOError err =
    die' manifestReadError $
      "Cannot read the manifest: " <> Text.pack (show err) <> "."

  manifestWriteIOError err =
    die' manifestReadError $
      "Cannot update the manifest: " <> Text.pack (show err) <> "."

  invalidUtf8Message :: (Show e) => e -> Text
  invalidUtf8Message err =
    "The manifest is not valid UTF-8: " <> Text.pack (show err)
