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
  , redactTransportSource
  ) where

import Control.Exception (displayException)
import Control.Monad (unless, when)
import Control.Monad.Catch (onException)
import Control.Monad.Except (MonadError (catchError))
import Control.Monad.Reader (asks, local)
import Data.List (isPrefixOf)
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
  , BuiltinSource (..)
  , StagedMetadata
  , detectBuiltinSource
  , emptyStagedMetadata
  , publishStagedDirectoryWithMetadata
  , stageBuiltinSourceWithMetadata
  )
import Dojang.CommandEffect
  ( MonadCommandEffect (..)
  , ProcessRequest (..)
  , ProcessResult (..)
  , confirmPrompt
  , emptyProcessRequest
  )
import Dojang.Commands
  ( Admonition (Error, Hint, Note, Warning)
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
import Dojang.Types.PathIdentity (pathIdentityComponents)
import Dojang.Types.Transport
  ( EnvironmentNameCase (..)
  , TransportLookupError (..)
  , TransportSpec
  , expandTransportCommandNative
  , lookupTransport
  , resolveTransportEnvironmentNative
  )


-- | Initializes a new repository or bootstraps an existing repository,
-- validating that acquisition-only options are used with @--from@.
initialize
  :: (MonadFileSystem i, AppEffects i)
  => Maybe FilePath
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
  => FilePath
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
  => FilePath
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
    preparedBuiltin <-
      ( case requestedTransport of
          Nothing -> Just <$> prepareBuiltin source destination
          Just _ -> return Nothing
      )
        `catchError` reportFilesystemError
    stagingRoot <- newStagingPath destination
    repositoryName <- encodePath "repository"
    let staging = stagingRoot </> repositoryName
        cleanup = cleanupStaging stagingRoot
    catchCommandExit
      ( onException
          (bootstrapAction preparedBuiltin stagingRoot staging)
          cleanup
      )
      (\exitCode -> cleanup >> abortCommand exitCode)
   where
    bootstrapAction preparedBuiltin stagingRoot staging = do
      createPrivateDirectory stagingRoot
        `catchError` reportFilesystemError
      acquired <-
        ( case requestedTransport of
            Nothing ->
              case preparedBuiltin of
                Just builtin -> acquireBuiltin builtin staging
                Nothing -> die' cliError "Bootstrap source was not prepared."
            Just name ->
              acquireExternal
                source
                name
                requestedTransportFile
                staging
        )
          `catchError` reportFilesystemError
      case acquired of
        Nothing -> cleanupStaging stagingRoot >> return ExitSuccess
        Just metadata -> do
          catchError
            ( validateStaging staging
                >> publishStaging metadata staging destination
                >> cleanupStaging stagingRoot
            )
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
  when present $ unavailable "already exists"
 where
  unavailable reason = do
    pathStyle <- pathStyleFor StandardError
    printStderr' Error $
      "Bootstrap destination " <> pathStyle destination <> " " <> reason <> "."
    printStderr' Hint $
      "Choose a repository directory that does not exist yet with "
        <> "`-r`/`--repository-dir`."
    abortCommand cliError


newStagingPath
  :: (MonadFileSystem i, AppEffects i) => OsPath -> App i OsPath
newStagingPath destination = do
  identifier <- newUUID
  name <-
    encodePath $
      ".dojang-bootstrap-" <> Text.unpack (UUID.toText identifier)
  return $ takeDirectory destination </> name


acquireBuiltin
  :: (MonadFileSystem i, AppEffects i)
  => BuiltinSource
  -> OsPath
  -> App i (Maybe StagedMetadata)
acquireBuiltin builtin staging = do
  let sourcePath = case builtin of
        DirectorySource path -> path
        ArchiveSource _ path -> path
  printAcquisition sourcePath staging
  staged <- liftApp $ stageBuiltinSourceWithMetadata builtin staging
  either reportAcquisitionError (return . Just) staged


prepareBuiltin
  :: (MonadFileSystem i, AppEffects i)
  => FilePath
  -> OsPath
  -> App i BuiltinSource
prepareBuiltin source destination = do
  sourcePath <- encodePath source
  detected <- detectBuiltinSource sourcePath
  builtin <- either reportAcquisitionError return detected
  case builtin of
    DirectorySource directory -> do
      canonicalSource <- canonicalizePath directory
      canonicalDestination <- canonicalizePath destination
      let sourceComponents = pathIdentityComponents canonicalSource
          destinationComponents =
            pathIdentityComponents canonicalDestination
      when
        ( sourceComponents /= destinationComponents
            && sourceComponents `isPrefixOf` destinationComponents
        )
        $ die'
          cliError
          "Bootstrap destination cannot be inside its directory source."
    ArchiveSource _ _ -> return ()
  return builtin


acquireExternal
  :: (MonadFileSystem i, AppEffects i)
  => FilePath
  -> Text
  -> Maybe OsPath
  -> OsPath
  -> App i (Maybe StagedMetadata)
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
        "Would run external transport: "
          <> Text.pack (show $ redactTransportSource source request)
          <> "."
      return Nothing
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
      return $ Just emptyStagedMetadata


-- | Builds a shell-free process request with an explicit child environment.
makeTransportProcessRequest
  :: String
  -- ^ Host platform identifier.
  -> [(String, String)]
  -- ^ Complete host environment.
  -> TransportSpec
  -- ^ Validated external transport.
  -> FilePath
  -- ^ Opaque source.
  -> FilePath
  -- ^ Staging destination.
  -> ProcessRequest
makeTransportProcessRequest platform hostEnvironment transport source destination =
  emptyProcessRequest
    { executable = executable
    , arguments = arguments
    , environment = Just resolvedEnvironment
    }
 where
  (executable, arguments) =
    expandTransportCommandNative transport source destination
  resolvedEnvironment =
    resolveTransportEnvironmentNative
      ( if platform == "mingw32"
          then CaseInsensitiveEnvironment
          else CaseSensitiveEnvironment
      )
      hostEnvironment
      transport


-- | Redacts an expanded transport source before rendering a process request.
--
-- Validated transports contain the source as exactly one whole argument, so
-- replacing exact matches hides embedded credentials without altering the
-- request that will actually be executed.
redactTransportSource :: FilePath -> ProcessRequest -> ProcessRequest
redactTransportSource source request =
  request
    { arguments =
        fmap
          (\argument -> if argument == source then "<redacted>" else argument)
          request.arguments
    }


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
  => StagedMetadata
  -> OsPath
  -> OsPath
  -> App i ()
publishStaging metadata staging destination = do
  modeFailures <-
    publishStagedDirectoryWithMetadata metadata staging destination
  unless (null modeFailures) $
    printStderr' Warning $
      "Could not restore stored permissions for "
        <> Text.pack (show $ length modeFailures)
        <> " repository "
        <> if length modeFailures == 1 then "entry." else "entries."
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
    UnsupportedSourceEntry path ->
      "Unsupported directory source entry: " <> Text.pack path <> "."
    ConflictingArchiveEntry path ->
      "Conflicting archive entry: " <> Text.pack path <> "."
    ArchiveResourceLimitExceeded ->
      "Bootstrap archive exceeds the safe size or entry-count limit."
    ArchiveChangedDuringAcquisition ->
      "Bootstrap archive changed during acquisition.  Retry with a stable source."


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
