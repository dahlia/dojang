{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | End-to-end repository bootstrap orchestration.
module Dojang.Commands.Bootstrap
  ( bootstrap
  , initialize
  , makeTransportProcessRequest
  , normalizeBootstrapDestination
  ) where

import Control.Exception (displayException)
import Control.Monad (unless, when)
import Control.Monad.Catch (onException)
import Control.Monad.Except (MonadError (catchError))
import Control.Monad.Reader (asks, local)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import System.Exit (ExitCode (..))
import System.OsPath
  ( OsPath
  , dropTrailingPathSeparator
  , isAbsolute
  , normalise
  , takeDirectory
  , (</>)
  )
import Prelude hiding (readFile)

import Dojang.App
  ( App
  , AppEffects
  , AppEnv (..)
  , catchCommandExit
  , ensureManifest
  , liftApp
  )
import Dojang.Bootstrap
  ( AcquisitionError (..)
  , detectBuiltinSource
  , publishStagedDirectory
  , stageBuiltinSource
  )
import Dojang.CommandEffect
  ( MonadCommandEffect (..)
  , ProcessRequest (..)
  , ProcessResult (..)
  , confirmPrompt
  , emptyProcessRequest
  )
import Dojang.Commands
  ( Admonition (Note)
  , StandardStream (StandardError)
  , die'
  , pathStyleFor
  , printStderr
  , printStderr'
  )
import Dojang.Commands.Apply qualified as Apply
import Dojang.Commands.Init (InitPreset)
import Dojang.Commands.Init qualified as Init
import Dojang.ExitCodes
  ( cliError
  , externalProgramNonZeroExit
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Syntax.Transport qualified as TransportSyntax
import Dojang.Types.Transport
  ( EnvironmentNameCase (..)
  , TransportLookupError (..)
  , TransportSpec
  , expandTransportCommand
  , lookupTransport
  , resolveTransportEnvironment
  )


-- | Initializes a new repository or bootstraps an existing repository,
-- validating that acquisition-only options are used with @--from@.
initialize
  :: (MonadFileSystem i, AppEffects i)
  => Maybe Text
  -- ^ Optional bootstrap source.
  -> Maybe Text
  -- ^ Optional external transport name.
  -> Maybe OsPath
  -- ^ Optional external transport configuration file.
  -> [InitPreset]
  -- ^ New-repository platform presets.
  -> Bool
  -- ^ Whether prompts are disabled.
  -> Bool
  -- ^ Whether the first bootstrap apply is accepted.
  -> Maybe OsPath
  -- ^ Optional facts file.
  -> [Text]
  -- ^ Explicit machine facts.
  -> App i ExitCode
initialize source transport transportFile presets noInteractive acceptApply factsFile facts =
  case source of
    Nothing
      | transport /= Nothing ->
          die' cliError "--transport requires --from."
      | transportFile /= Nothing ->
          die' cliError "--transport-file requires --from."
      | acceptApply ->
          die' cliError "--yes requires --from."
      | otherwise ->
          Init.initWithFacts presets noInteractive factsFile facts
    Just source' ->
      bootstrap
        source'
        transport
        transportFile
        presets
        noInteractive
        acceptApply
        factsFile
        facts


-- | Acquires an existing repository and completes its initial enrollment and
-- apply flow.
bootstrap
  :: (MonadFileSystem i, AppEffects i)
  => Text
  -- ^ Local source path or opaque external-transport source.
  -> Maybe Text
  -- ^ Explicit external transport name.
  -> Maybe OsPath
  -- ^ Explicit transport configuration file.
  -> [InitPreset]
  -- ^ Presets retained for CLI compatibility; rejected for existing sources.
  -> Bool
  -- ^ Whether all prompts are disabled.
  -> Bool
  -- ^ Whether the first mutating apply is accepted non-interactively.
  -> Maybe OsPath
  -- ^ Optional facts file.
  -> [Text]
  -- ^ Explicit machine facts.
  -> App i ExitCode
bootstrap
  source
  requestedTransport
  requestedTransportFile
  presets
  noInteractive
  acceptApply
  factsFile
  facts = do
    unless (null presets) $
      die' cliError "Platform presets cannot be used together with --from."
    when (requestedTransport == Nothing && requestedTransportFile /= Nothing) $
      die' cliError "--transport-file requires --transport."
    when (noInteractive && not acceptApply) $
      die' cliError "--no-interactive bootstrap requires --yes."
    configuredDestination <- asks (.sourceDirectory)
    destination <-
      normalizeBootstrapDestination
        <$> makeAbsolute configuredDestination
    local
      (\environment -> environment{sourceDirectory = destination})
      ( bootstrapInto
          source
          requestedTransport
          requestedTransportFile
          noInteractive
          acceptApply
          factsFile
          facts
          destination
      )


-- | Normalizes an absolute bootstrap destination and removes a non-root
-- trailing separator so that staging is always created beside it.
normalizeBootstrapDestination :: OsPath -> OsPath
normalizeBootstrapDestination =
  dropTrailingPathSeparator . normalise


bootstrapInto
  :: (MonadFileSystem i, AppEffects i)
  => Text
  -> Maybe Text
  -> Maybe OsPath
  -> Bool
  -> Bool
  -> Maybe OsPath
  -> [Text]
  -> OsPath
  -> App i ExitCode
bootstrapInto
  source
  requestedTransport
  requestedTransportFile
  noInteractive
  acceptApply
  factsFile
  facts
  destination = do
    ensureAvailableDestination destination
    staging <- newStagingPath destination
    let cleanup = cleanupStaging staging
    catchCommandExit
      ( onException
          (bootstrapAction staging)
          cleanup
      )
      (\exitCode -> cleanup >> abortCommand exitCode)
   where
    bootstrapAction staging = do
      externalDryRun <-
        ( case requestedTransport of
            Nothing -> acquireBuiltin source staging
            Just name ->
              acquireExternal
                source
                name
                requestedTransportFile
                staging
        )
          `catchError` reportFilesystemError
      if externalDryRun
        then return ExitSuccess
        else do
          catchError
            (validateStaging staging >> publishStaging staging destination)
            reportFilesystemError
          enrolled <-
            catchCommandExit
              (Init.initWithFacts [] noInteractive factsFile facts)
              (\exitCode -> enrollmentFailed destination >> abortCommand exitCode)
          case enrolled of
            ExitFailure _ -> return enrolled
            ExitSuccess -> do
              shouldApply <-
                if acceptApply
                  then return True
                  else
                    confirmPrompt
                      "Apply this repository to the current machine now?"
              if shouldApply
                then Apply.apply False []
                else do
                  printStderr' Note $
                    "Repository acquired and enrolled without applying it."
                  return ExitSuccess


ensureAvailableDestination
  :: (MonadFileSystem i, AppEffects i) => OsPath -> App i ()
ensureAvailableDestination destination = do
  symbolicLink <- isSymlink destination
  when symbolicLink $ unavailable "is a symbolic link"
  present <- exists destination
  when present $ do
    directory <- isDirectory destination
    unless directory $ unavailable "is not a directory"
    entries <- listDirectory destination
    unless (null entries) $ unavailable "is not empty"
 where
  unavailable reason = do
    pathStyle <- pathStyleFor StandardError
    die' cliError $
      "Bootstrap destination " <> pathStyle destination <> " " <> reason <> "."


newStagingPath
  :: (MonadFileSystem i, AppEffects i) => OsPath -> App i OsPath
newStagingPath destination = do
  identifier <- newUUID
  name <-
    encodePath $
      ".dojang-bootstrap-" <> Text.unpack (UUID.toText identifier)
  return $ takeDirectory destination </> name


acquireBuiltin
  :: (MonadFileSystem i, AppEffects i) => Text -> OsPath -> App i Bool
acquireBuiltin source staging = do
  sourcePath <- encodePath $ Text.unpack source
  detected <- detectBuiltinSource sourcePath
  builtin <- either reportAcquisitionError return detected
  printAcquisition sourcePath staging
  staged <- liftApp $ stageBuiltinSource builtin staging
  either reportAcquisitionError (const $ return False) staged


acquireExternal
  :: (MonadFileSystem i, AppEffects i)
  => Text
  -> Text
  -> Maybe OsPath
  -> OsPath
  -> App i Bool
acquireExternal source name requestedConfig staging = do
  platform <- hostPlatform
  configPath <- maybe (defaultTransportConfigPath platform) return requestedConfig
  configResult <-
    TransportSyntax.readTransportConfigFile configPath
      `catchError` \err -> do
        configStyle <- pathStyleFor StandardError
        die' cliError $
          "Could not read transport configuration "
            <> configStyle configPath
            <> ": "
            <> Text.pack (show err)
            <> "."
  config <-
    either (die' cliError . TransportSyntax.formatError) return configResult
  transport <-
    case lookupTransport name config of
      Left (InvalidLookupTransportName err) ->
        die' cliError $
          "Invalid transport name '" <> name <> "': " <> Text.pack (show err) <> "."
      Left (UnknownTransportName _) ->
        die' cliError $ "Unknown transport '" <> name <> "'."
      Right value -> return value
  hostEnvironment <- processEnvironment
  stagingPath <- decodePath staging
  let request =
        makeTransportProcessRequest
          platform
          hostEnvironment
          transport
          source
          stagingPath
  dryRunEnabled <- asks (.dryRun)
  if dryRunEnabled
    then do
      printStderr $
        "Would run external transport: " <> Text.pack (show request) <> "."
      return True
    else do
      printStderr $
        "Running external transport '" <> name <> "' into staging..."
      result <- runProcess request
      ensureTransportSucceeded name result
      symbolicLink <- isSymlink staging
      when symbolicLink $
        die' cliError "External transport created a symbolic-link staging path."
      directory <- isDirectory staging
      unless directory $
        die' cliError "External transport did not create its staging directory."
      return False


-- | Builds a shell-free process request with an explicit child environment.
makeTransportProcessRequest
  :: String
  -- ^ Host platform identifier.
  -> [(String, String)]
  -- ^ Complete host environment.
  -> TransportSpec
  -- ^ Validated external transport.
  -> Text
  -- ^ Opaque source.
  -> FilePath
  -- ^ Staging destination.
  -> ProcessRequest
makeTransportProcessRequest platform hostEnvironment transport source destination =
  emptyProcessRequest
    { executable = Text.unpack executable
    , arguments = Text.unpack <$> arguments
    , environment =
        Just $
          fmap
            (\(name, value) -> (Text.unpack name, Text.unpack value))
            resolvedEnvironment
    }
 where
  (executable, arguments) =
    expandTransportCommand transport source $ Text.pack destination
  resolvedEnvironment =
    resolveTransportEnvironment
      ( if platform == "mingw32"
          then CaseInsensitiveEnvironment
          else CaseSensitiveEnvironment
      )
      (fmap (\(name, value) -> (Text.pack name, Text.pack value)) hostEnvironment)
      transport


defaultTransportConfigPath
  :: (MonadFileSystem i, AppEffects i) => String -> App i OsPath
defaultTransportConfigPath platform
  | platform == "mingw32" = do
      appData <- lookupEnvironmentVariable "APPDATA"
      base <- case appData of
        Just value -> encodePath value
        Nothing -> do
          home <- getHomeDirectory
          suffix <- encodePath "AppData/Roaming"
          return $ home </> suffix
      appendConfigPath base
  | platform == "darwin" = do
      home <- getHomeDirectory
      suffix <- encodePath "Library/Application Support"
      appendConfigPath $ home </> suffix
  | otherwise = do
      configured <- lookupEnvironmentVariable "XDG_CONFIG_HOME"
      base <- case configured of
        Just value -> do
          path <- encodePath value
          if isAbsolute path
            then return path
            else defaultPosixBase
        Nothing -> defaultPosixBase
      appendConfigPath base
 where
  defaultPosixBase = do
    home <- getHomeDirectory
    suffix <- encodePath ".config"
    return $ home </> suffix
  appendConfigPath base = do
    suffix <- encodePath "dojang/transports.toml"
    return $ base </> suffix


validateStaging
  :: (MonadFileSystem i, AppEffects i) => OsPath -> App i ()
validateStaging staging = do
  _ <-
    local
      (\environment -> environment{sourceDirectory = staging})
      ensureManifest
  printStderr "Staged repository manifest validated."


publishStaging
  :: (MonadFileSystem i, AppEffects i)
  => OsPath
  -> OsPath
  -> App i ()
publishStaging staging destination = do
  publishStagedDirectory staging destination
  pathStyle <- pathStyleFor StandardError
  printStderr $ "Repository published: " <> pathStyle destination <> "."


cleanupStaging
  :: (MonadFileSystem i, AppEffects i) => OsPath -> App i ()
cleanupStaging staging = do
  symbolicLink <- isSymlink staging
  directory <- isDirectory staging
  present <- exists staging
  if directory && not symbolicLink
    then removeDirectoryRecursively staging `catchError` const (return ())
    else
      when (symbolicLink || present) $
        removeFile staging `catchError` const (return ())


reportFilesystemError :: (AppEffects i) => IOError -> App i a
reportFilesystemError err =
  die' cliError $
    "Could not bootstrap repository: "
      <> Text.pack (displayException err)
      <> "."


enrollmentFailed
  :: (AppEffects i) => OsPath -> App i ()
enrollmentFailed destination = do
  pathStyle <- pathStyleFor StandardError
  printStderr' Note $
    "The acquired repository was kept at "
      <> pathStyle destination
      <> ".  Resolve the enrollment error there with `dojang init` or "
      <> "`dojang migrate`."


printAcquisition
  :: (AppEffects i) => OsPath -> OsPath -> App i ()
printAcquisition source staging = do
  pathStyle <- pathStyleFor StandardError
  printStderr $
    "Staging repository from "
      <> pathStyle source
      <> " at "
      <> pathStyle staging
      <> "."


reportAcquisitionError
  :: (AppEffects i) => AcquisitionError -> App i a
reportAcquisitionError err =
  die' cliError $ case err of
    SourceDoesNotExist path ->
      "Bootstrap source does not exist or is not a file or directory: "
        <> Text.pack path
        <> "."
    UnsupportedArchiveFormat path ->
      "Unsupported local archive format: " <> Text.pack path <> "."
    InvalidArchive message -> "Invalid bootstrap archive: " <> message <> "."
    UnsafeArchiveEntry path ->
      "Unsafe archive entry path: " <> Text.pack path <> "."
    UnsupportedArchiveEntry path ->
      "Unsupported archive entry: " <> Text.pack path <> "."
    ConflictingArchiveEntry path ->
      "Conflicting archive entry: " <> Text.pack path <> "."


ensureTransportSucceeded
  :: (AppEffects i) => Text -> ProcessResult -> App i ()
ensureTransportSucceeded _ (ProcessCompleted ExitSuccess _ _) = return ()
ensureTransportSucceeded name (ProcessCompleted code _ _) =
  failure name $ "exited with " <> Text.pack (show code)
ensureTransportSucceeded name (ProcessStartFailed message) =
  failure name $ "could not start: " <> message
ensureTransportSucceeded name (ProcessWaitFailed message) =
  failure name $ "could not be awaited: " <> message
ensureTransportSucceeded name (ProcessIOFailed message) =
  failure name $ "failed: " <> message
ensureTransportSucceeded name (ProcessUnavailable _) =
  failure name "is unavailable"


failure :: (AppEffects i) => Text -> Text -> App i a
failure name reason =
  die' externalProgramNonZeroExit $
    "Transport '" <> name <> "' " <> reason <> "."
