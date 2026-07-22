{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Explicit effects used by command orchestration.
--
-- Commands depend on this interface instead of lifting arbitrary 'IO'.  The
-- production, dry-run, and test interpreters can therefore make different
-- guarantees about which effects are allowed to reach the host.
module Dojang.CommandEffect
  ( BinaryProcessRequest (..)
  , BinaryProcessResult (..)
  , CommandEffect (..)
  , CommandEffectError (..)
  , CommandEffectKind (..)
  , CommandEffectResponse (..)
  , CommandEffectTest
  , MonadCommandEffect (..)
  , MonadProcessControl (..)
  , OutputStream (..)
  , ProcessRequest (..)
  , ProcessResult (..)
  , RedactedProcessBytes
  , PromptRequest (..)
  , PromptResult (..)
  , StandardStream (..)
  , StartedProcess
  , awaitProcess
  , cancelStartedProcess
  , confirmPrompt
  , currentTimeIO
  , detectEnvironmentIO
  , emptyProcessRequest
  , hostPlatformIO
  , hoistStartedProcess
  , isTerminalIO
  , lookupEnvironmentVariableIO
  , newUUIDIO
  , processEnvironmentIO
  , promptIO
  , runCommandEffectTest
  , runProcessIO
  , runBinaryProcessIO
  , redactedProcessBytes
  , revealProcessBytes
  , selectPrompt
  , startProcessIO
  , startedProcess
  , writeStreamIO
  ) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (mask, onException, throwIO)
import Control.Monad (void)
import Control.Monad.Catch qualified as MonadCatch
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as TextIO
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import System.Environment qualified as Environment
import System.Exit (ExitCode (ExitSuccess))
import System.IO (Handle, hClose, stderr, stdin, stdout)
import System.IO.Error (tryIOError)
import System.Info qualified
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (CreatePipe, Inherit)
  , createProcess
  , getPid
  , proc
  , readCreateProcessWithExitCode
  , terminateProcess
  , waitForProcess
  )
import System.Timeout (timeout)
import Prelude hiding (readFile, writeFile)

import FortyTwo.Prompts.Confirm qualified as FortyTwo
import FortyTwo.Prompts.Input qualified as FortyTwo
import FortyTwo.Prompts.Multiselect qualified as FortyTwo
import FortyTwo.Prompts.Select qualified as FortyTwo
import System.IO.Extra (hIsTerminalDevice)

import Dojang.MonadFileSystem
  ( DryRunIO
  , MonadFileSystem (..)
  , dryRunIO
  )
import Dojang.Types.Environment (Environment)
import Dojang.Types.Environment.Current qualified as CurrentEnvironment


#ifndef mingw32_HOST_OS
import System.Posix.Signals (sigKILL, signalProcess)
#endif


-- | A standard process stream exposed to command code.
data StandardStream = StandardInput | StandardOutput | StandardError
  deriving (Eq, Ord, Show)


-- | A standard stream that command code may write to.
data OutputStream = OutputStandard | OutputError
  deriving (Eq, Ord, Show)


-- | An interactive request made by a command.
data PromptRequest
  = ConfirmPrompt Text
  | InputPrompt Text
  | SelectPrompt Text [Text]
  | MultiselectPrompt Text [Text]
  deriving (Eq, Show)


-- | The answer to a 'PromptRequest'.
data PromptResult
  = Confirmed Bool
  | InputValue Text
  | SelectedValue Text
  | MultiselectedValues [Text]
  | PromptUnavailable
  deriving (Eq, Show)


-- | A process invocation without shell interpolation.
data ProcessRequest = ProcessRequest
  { executable :: FilePath
  -- ^ Executable path or name.
  , arguments :: [String]
  -- ^ Individual command-line arguments.
  , workingDirectory :: Maybe FilePath
  -- ^ Working directory, or the current directory when absent.
  , environment :: Maybe [(String, String)]
  -- ^ Complete child environment, or inheritance when absent.
  , captureOutput :: Bool
  -- ^ Whether stdout and stderr should be captured.
  }
  deriving (Eq, Show)


-- | A process request with inherited environment and streams.
emptyProcessRequest :: ProcessRequest
emptyProcessRequest = ProcessRequest "" [] Nothing Nothing False


-- | The observable result of a process request.
data ProcessResult
  = ProcessCompleted ExitCode Text Text
  | ProcessStartFailed Text
  | ProcessWaitFailed Text
  | ProcessIOFailed Text
  | ProcessUnavailable CommandEffectKind
  deriving (Eq, Show)


-- | Binary process bytes whose 'Show' instance reveals only their length.
newtype RedactedProcessBytes = RedactedProcessBytes ByteString
  deriving (Eq, Ord)


instance Show RedactedProcessBytes where
  show (RedactedProcessBytes bytes) =
    "<redacted bytes: " <> show (ByteString.length bytes) <> ">"


-- | Protects bytes before they enter a command-effect request or response.
redactedProcessBytes :: ByteString -> RedactedProcessBytes
redactedProcessBytes = RedactedProcessBytes


-- | Reveals bytes only at the backend protocol boundary.
revealProcessBytes :: RedactedProcessBytes -> ByteString
revealProcessBytes (RedactedProcessBytes bytes) = bytes


-- | A shell-free, binary process request for a codec backend.
data BinaryProcessRequest = BinaryProcessRequest
  { binaryExecutable :: FilePath
  -- ^ Absolute executable path.
  , binaryWorkingDirectory :: Maybe FilePath
  -- ^ Repository root used as the child working directory.
  , binaryEnvironment :: [(String, String)]
  -- ^ Complete child environment.  An empty list is intentionally hermetic.
  , binaryStandardInput :: RedactedProcessBytes
  -- ^ Protocol header and payload written to standard input.
  , binaryTimeoutSeconds :: Int
  -- ^ Positive execution timeout in seconds.
  }
  deriving (Eq, Show)


-- | Result of a binary codec backend process.
data BinaryProcessResult
  = BinaryProcessCompleted
      ExitCode
      RedactedProcessBytes
      RedactedProcessBytes
  | BinaryProcessStartFailed Text
  | BinaryProcessIOFailed Text
  | BinaryProcessTimedOut
  | BinaryProcessUnavailable CommandEffectKind
  deriving (Eq, Show)


-- | A child process started by @m@ that can be awaited or cancelled.
data StartedProcess m = StartedProcess (m ProcessResult) (m ())


-- | Creates an interpreter-specific started-process handle.
startedProcess :: m ProcessResult -> m () -> StartedProcess m
startedProcess = StartedProcess


-- | Maps a started process's lifecycle actions into another monad.
hoistStartedProcess
  :: (forall a. m a -> n a) -> StartedProcess m -> StartedProcess n
hoistStartedProcess transform (StartedProcess await cancel) =
  StartedProcess (transform await) (transform cancel)


-- | An effect that cannot be silently simulated.
data CommandEffectKind
  = ProcessExecution
  | CodecBackendExecution
  | PromptInteraction
  deriving (Eq, Ord, Show)


-- | One command effect, suitable for deterministic test assertions.
data CommandEffect
  = EnvironmentLookup String
  | ProcessEnvironmentRead
  | HostEnvironmentRead
  | HostPlatformRead
  | CurrentTimeRead
  | UUIDGenerated
  | TerminalChecked StandardStream
  | StreamWrite OutputStream Text
  | Prompted PromptRequest
  | ProcessRun ProcessRequest
  | BinaryProcessRun BinaryProcessRequest
  | ProcessStart ProcessRequest
  | ProcessAwait Int
  | ProcessCancel Int
  deriving (Eq, Show)


-- | A scripted response consumed by the test interpreter.
data CommandEffectResponse
  = EnvironmentValue (Maybe String)
  | ProcessEnvironmentValue [(String, String)]
  | HostEnvironmentValue Environment
  | PlatformValue String
  | CurrentTimeValue UTCTime
  | UUIDValue UUID
  | TerminalValue Bool
  | PromptValue PromptResult
  | ProcessValue ProcessResult
  | BinaryProcessValue BinaryProcessResult
  | ProcessStartedValue
  | ProcessCancellationConfirmed
  | ExecutionRequired CommandEffectKind
  deriving (Eq, Show)


-- | A deterministic script mismatch.
data CommandEffectError
  = UnexpectedEffect CommandEffect
  | UnexpectedResponse CommandEffect CommandEffectResponse
  | UnusedResponses [CommandEffectResponse]
  | UnfinishedProcesses [Int]
  | CommandAborted ExitCode
  | CommandAbortedWithUnfinishedProcesses ExitCode [Int]
  deriving (Eq, Show)


-- | Effects that commands may perform outside the filesystem abstraction.
class (Monad m) => MonadCommandEffect m where
  -- | Looks up one process environment variable.
  lookupEnvironmentVariable :: String -> m (Maybe String)


  -- | Reads the complete process environment for a child process.
  processEnvironment :: m [(String, String)]


  -- | Detects the current host environment.
  detectEnvironment :: m Environment


  -- | Reads the host platform identifier used by the Haskell runtime.
  hostPlatform :: m String


  -- | Reads the current wall-clock time.
  currentTime :: m UTCTime


  -- | Generates a random UUID.
  newUUID :: m UUID


  -- | Tests whether a standard stream is attached to a terminal.
  isTerminal :: StandardStream -> m Bool


  -- | Writes text to a standard output stream.
  writeStream :: OutputStream -> Text -> m ()


  -- | Requests interactive input.
  prompt :: PromptRequest -> m PromptResult


  -- | Runs a process according to a structured request.
  runProcess :: ProcessRequest -> m ProcessResult


  -- | Runs a binary codec backend through the dedicated process boundary.
  runBinaryProcess :: BinaryProcessRequest -> m BinaryProcessResult


  -- | Aborts the current command with a public exit status.
  abortCommand :: ExitCode -> m a


-- | Two-phase child-process control for lock-sensitive orchestration.
class (Monad m) => MonadProcessControl m where
  -- | Starts a process whose streams are inherited from the parent.
  --
  -- The returned handle may be awaited after a surrounding lock is released.
  -- Callers that split startup from waiting must mask that handoff and arrange
  -- cancellation on exceptional exits.
  startProcess :: ProcessRequest -> m (Either ProcessResult (StartedProcess m))


-- | Waits for a process that was started successfully.
awaitProcess :: StartedProcess m -> m ProcessResult
awaitProcess (StartedProcess await _) = await


-- | Cancels a process that was started successfully.
cancelStartedProcess :: StartedProcess m -> m ()
cancelStartedProcess (StartedProcess _ cancel) = cancel


-- | Requests confirmation, returning 'False' when prompting is unavailable.
confirmPrompt :: (MonadCommandEffect m) => Text -> m Bool
confirmPrompt message = do
  response <- prompt $ ConfirmPrompt message
  return $ case response of
    Confirmed value -> value
    _ -> False


-- | Requests one selected value, or 'Nothing' when prompting is unavailable.
selectPrompt :: (MonadCommandEffect m) => Text -> [Text] -> m (Maybe Text)
selectPrompt message values = do
  response <- prompt $ SelectPrompt message values
  return $ case response of
    SelectedValue value -> Just value
    _ -> Nothing


data CommandEffectTestState = CommandEffectTestState
  { responses :: [CommandEffectResponse]
  , effects :: [CommandEffect]
  , nextProcessId :: Int
  , activeProcesses :: [Int]
  }


newtype CommandEffectTest a = CommandEffectTest
  { unCommandEffectTest
      :: ReaderT
           (IORef CommandEffectTestState)
           (ExceptT CommandEffectError DryRunIO)
           a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadCatch.MonadThrow
    , MonadCatch.MonadCatch
    , MonadCatch.MonadMask
    )


liftCommandEffectBase :: DryRunIO a -> CommandEffectTest a
liftCommandEffectBase = CommandEffectTest . lift . lift


instance MonadError IOError CommandEffectTest where
  throwError = liftCommandEffectBase . throwError
  catchError action handler = CommandEffectTest $ ReaderT $ \stateRef ->
    ExceptT $
      runExceptT (runReaderT action.unCommandEffectTest stateRef)
        `catchError` \err ->
          runExceptT $
            runReaderT (handler err).unCommandEffectTest stateRef


instance MonadFileSystem CommandEffectTest where
  encodePath = liftCommandEffectBase . encodePath
  decodePath = liftCommandEffectBase . decodePath
  getCurrentDirectory = liftCommandEffectBase getCurrentDirectory
  getHomeDirectory = liftCommandEffectBase getHomeDirectory
  exists = liftCommandEffectBase . exists
  isFile = liftCommandEffectBase . isFile
  isRegularFile = liftCommandEffectBase . isRegularFile
  isDirectory = liftCommandEffectBase . isDirectory
  isSymlink = liftCommandEffectBase . isSymlink
  readFile = liftCommandEffectBase . readFile
  writeFile path = liftCommandEffectBase . writeFile path
  replaceFile source = liftCommandEffectBase . replaceFile source
  writeTemporaryFile directory template =
    liftCommandEffectBase . writeTemporaryFile directory template
  withFileLock _ action = action
  canonicalizePath = liftCommandEffectBase . canonicalizePath
  readSymlinkTarget = liftCommandEffectBase . readSymlinkTarget
  copyFile source = liftCommandEffectBase . copyFile source
  copyFileWithMetadata source =
    liftCommandEffectBase . copyFileWithMetadata source
  copyFilePermissions source =
    liftCommandEffectBase . copyFilePermissions source
  createDirectory = liftCommandEffectBase . createDirectory
  removeFile = liftCommandEffectBase . removeFile
  removeDirectory = liftCommandEffectBase . removeDirectory
  listDirectory = liftCommandEffectBase . listDirectory
  getFileSize = liftCommandEffectBase . getFileSize
  getPortableMode = liftCommandEffectBase . getPortableMode
  setPortableMode path = liftCommandEffectBase . setPortableMode path
  setPortableWritable path = liftCommandEffectBase . setPortableWritable path
  createSymbolicLink target link =
    liftCommandEffectBase . createSymbolicLink target link


record :: CommandEffect -> CommandEffectTest ()
record effect = modifyCommandEffectState $ \state ->
  state{effects = effect : state.effects}


readCommandEffectState :: CommandEffectTest CommandEffectTestState
readCommandEffectState = CommandEffectTest $ do
  stateRef <- ask
  liftIO $ readIORef stateRef


modifyCommandEffectState
  :: (CommandEffectTestState -> CommandEffectTestState)
  -> CommandEffectTest ()
modifyCommandEffectState transform = CommandEffectTest $ do
  stateRef <- ask
  liftIO $ modifyIORef' stateRef transform


throwCommandEffectError :: CommandEffectError -> CommandEffectTest a
throwCommandEffectError = CommandEffectTest . lift . throwError


respond
  :: CommandEffect
  -> (CommandEffectResponse -> Maybe a)
  -> CommandEffectTest a
respond effect decode = do
  record effect
  state <- readCommandEffectState
  case state.responses of
    [] -> throwCommandEffectError $ UnexpectedEffect effect
    response : rest -> case decode response of
      Nothing ->
        throwCommandEffectError $ UnexpectedResponse effect response
      Just value -> do
        modifyCommandEffectState $ \current -> current{responses = rest}
        return value


instance MonadCommandEffect CommandEffectTest where
  lookupEnvironmentVariable name =
    respond (EnvironmentLookup name) $ \response -> case response of
      EnvironmentValue value -> Just value
      _ -> Nothing
  processEnvironment =
    respond ProcessEnvironmentRead $ \response -> case response of
      ProcessEnvironmentValue value -> Just value
      _ -> Nothing
  detectEnvironment = respond HostEnvironmentRead $ \response -> case response of
    HostEnvironmentValue value -> Just value
    _ -> Nothing
  hostPlatform = respond HostPlatformRead $ \response -> case response of
    PlatformValue value -> Just value
    _ -> Nothing
  currentTime = respond CurrentTimeRead $ \response -> case response of
    CurrentTimeValue value -> Just value
    _ -> Nothing
  newUUID = respond UUIDGenerated $ \response -> case response of
    UUIDValue value -> Just value
    _ -> Nothing
  isTerminal stream = respond (TerminalChecked stream) $ \response ->
    case response of
      TerminalValue value -> Just value
      _ -> Nothing
  writeStream stream value = record $ StreamWrite stream value
  prompt request = respond (Prompted request) $ \response -> case response of
    PromptValue value -> Just value
    ExecutionRequired PromptInteraction -> Just PromptUnavailable
    _ -> Nothing
  runProcess request = respond (ProcessRun request) $ \response -> case response of
    ProcessValue value -> Just value
    ExecutionRequired ProcessExecution -> Just $ ProcessUnavailable ProcessExecution
    _ -> Nothing
  runBinaryProcess request =
    respond (BinaryProcessRun request) $ \response -> case response of
      BinaryProcessValue value -> Just value
      ExecutionRequired CodecBackendExecution ->
        Just $ BinaryProcessUnavailable CodecBackendExecution
      _ -> Nothing
  abortCommand = throwCommandEffectError . CommandAborted


instance MonadProcessControl CommandEffectTest where
  startProcess request = do
    result <- respond (ProcessStart request) $ \response -> case response of
      ProcessStartedValue -> Just $ Right ()
      ProcessValue value@ProcessStartFailed{} -> Just $ Left value
      ProcessValue value@ProcessUnavailable{} -> Just $ Left value
      ExecutionRequired ProcessExecution ->
        Just $ Left $ ProcessUnavailable ProcessExecution
      _ -> Nothing
    case result of
      Left failure -> return $ Left failure
      Right () -> do
        state <- readCommandEffectState
        let processId = state.nextProcessId
        modifyCommandEffectState $ \current ->
          current
            { nextProcessId = processId + 1
            , activeProcesses = processId : current.activeProcesses
            }
        return $
          Right $
            StartedProcess
              (awaitTestProcess processId)
              (cancelTestProcess processId)


awaitTestProcess :: Int -> CommandEffectTest ProcessResult
awaitTestProcess processId = do
  result <- respond (ProcessAwait processId) $ \response -> case response of
    ProcessValue value -> Just value
    _ -> Nothing
  finishProcess processId
  return result


cancelTestProcess :: Int -> CommandEffectTest ()
cancelTestProcess processId = do
  respond (ProcessCancel processId) $ \response -> case response of
    ProcessCancellationConfirmed -> Just ()
    _ -> Nothing
  finishProcess processId


finishProcess :: Int -> CommandEffectTest ()
finishProcess processId = modifyCommandEffectState $ \state ->
  state{activeProcesses = filter (/= processId) state.activeProcesses}


instance MonadCommandEffect IO where
  lookupEnvironmentVariable = lookupEnvironmentVariableIO
  processEnvironment = processEnvironmentIO
  detectEnvironment = detectEnvironmentIO
  hostPlatform = hostPlatformIO
  currentTime = currentTimeIO
  newUUID = newUUIDIO
  isTerminal = isTerminalIO
  writeStream = writeStreamIO
  prompt = promptIO
  runProcess = runProcessIO
  runBinaryProcess = runBinaryProcessIO
  abortCommand = throwIO


instance MonadProcessControl IO where
  startProcess = startProcessIO


instance (MonadProcessControl m) => MonadProcessControl (ReaderT r m) where
  startProcess request = do
    started <- lift $ startProcess request
    return $ hoistStartedProcess lift <$> started


instance (MonadProcessControl m) => MonadProcessControl (ExceptT e m) where
  startProcess request = do
    started <- lift $ startProcess request
    return $ hoistStartedProcess lift <$> started


instance (MonadProcessControl m) => MonadProcessControl (StateT s m) where
  startProcess request = do
    started <- lift $ startProcess request
    return $ hoistStartedProcess lift <$> started


instance (MonadCommandEffect m) => MonadCommandEffect (ReaderT r m) where
  lookupEnvironmentVariable = lift . lookupEnvironmentVariable
  processEnvironment = lift processEnvironment
  detectEnvironment = lift detectEnvironment
  hostPlatform = lift hostPlatform
  currentTime = lift currentTime
  newUUID = lift newUUID
  isTerminal = lift . isTerminal
  writeStream stream = lift . writeStream stream
  prompt = lift . prompt
  runProcess = lift . runProcess
  runBinaryProcess = lift . runBinaryProcess
  abortCommand = lift . abortCommand


instance (MonadCommandEffect m) => MonadCommandEffect (ExceptT e m) where
  lookupEnvironmentVariable = lift . lookupEnvironmentVariable
  processEnvironment = lift processEnvironment
  detectEnvironment = lift detectEnvironment
  hostPlatform = lift hostPlatform
  currentTime = lift currentTime
  newUUID = lift newUUID
  isTerminal = lift . isTerminal
  writeStream stream = lift . writeStream stream
  prompt = lift . prompt
  runProcess = lift . runProcess
  runBinaryProcess = lift . runBinaryProcess
  abortCommand = lift . abortCommand


instance (MonadCommandEffect m) => MonadCommandEffect (StateT s m) where
  lookupEnvironmentVariable = lift . lookupEnvironmentVariable
  processEnvironment = lift processEnvironment
  detectEnvironment = lift detectEnvironment
  hostPlatform = lift hostPlatform
  currentTime = lift currentTime
  newUUID = lift newUUID
  isTerminal = lift . isTerminal
  writeStream stream = lift . writeStream stream
  prompt = lift . prompt
  runProcess = lift . runProcess
  runBinaryProcess = lift . runBinaryProcess
  abortCommand = lift . abortCommand


-- | Runs a child process to completion in the production interpreter.
runProcessIO :: ProcessRequest -> IO ProcessResult
runProcessIO request
  | request.captureOutput = do
      result <-
        tryIOError $
          readCreateProcessWithExitCode (toCreateProcess request) ""
      return $ case result of
        Left err -> ProcessIOFailed $ pack $ show err
        Right (code, out, err) -> ProcessCompleted code (pack out) (pack err)
  | otherwise = mask $ \restore -> do
      started <- tryIOError $ createProcess $ toCreateProcess request
      case started of
        Left err -> return $ ProcessStartFailed $ pack $ show err
        Right (_, _, _, handle) -> do
          waited <-
            tryIOError (restore $ waitForProcess handle)
              `onException` cleanupProcess handle
          case waited of
            Left _ -> cleanupProcess handle
            Right _ -> return ()
          return $ case waited of
            Left err -> ProcessWaitFailed $ pack $ show err
            Right code -> ProcessCompleted code "" ""


-- | Runs a binary codec backend with captured streams and a hard timeout.
runBinaryProcessIO :: BinaryProcessRequest -> IO BinaryProcessResult
runBinaryProcessIO request
  | request.binaryTimeoutSeconds <= 0 =
      return $ BinaryProcessIOFailed "Backend timeout must be positive."
runBinaryProcessIO request = mask $ \restore -> do
  started <- tryIOError $ createProcess $ toBinaryCreateProcess request
  case started of
    Left err -> return $ BinaryProcessStartFailed $ pack $ show err
    Right (Just input, Just output, Just errors, handle) -> do
      let process = (input, output, errors, handle)
          writeInput = do
            written <-
              tryIOError $
                ByteString.hPut input $
                  revealProcessBytes request.binaryStandardInput
            closed <- tryIOError $ hClose input
            return $ written >> closed
          run = do
            (written, (out, err)) <-
              concurrently
                writeInput
                ( concurrently
                    (ByteString.hGetContents output)
                    (ByteString.hGetContents errors)
                )
            code <- waitForProcess handle
            return (code, written, out, err)
      result <-
        tryIOError
          (restore $ timeout (request.binaryTimeoutSeconds * 1000000) run)
          `onException` cleanupBinaryProcess process
      case result of
        Left err -> do
          cleanupBinaryProcess process
          return $ BinaryProcessIOFailed $ pack $ show err
        Right Nothing -> do
          cleanupBinaryProcess process
          return BinaryProcessTimedOut
        Right (Just (code, written, out, err)) -> case (code, written) of
          (ExitSuccess, Left writeError) ->
            return $ BinaryProcessIOFailed $ pack $ show writeError
          _ ->
            return $
              BinaryProcessCompleted
                code
                (redactedProcessBytes out)
                (redactedProcessBytes err)
    Right _ -> return $ BinaryProcessIOFailed "Failed to capture backend streams."


cleanupBinaryProcess
  :: (Handle, Handle, Handle, ProcessHandle)
  -> IO ()
cleanupBinaryProcess (input, output, errors, handle) = do
  processId <- getPid handle
  mapM_ closeBinaryHandle [input, output, errors]
  void $ tryIOError $ terminateProcess handle
  stopped <- timeout 2000000 $ tryIOError $ waitForProcess handle
  case stopped of
    Just _ -> return ()
    Nothing -> do
#ifndef mingw32_HOST_OS
      mapM_ (signalProcess sigKILL) processId
      void $ timeout 2000000 $ tryIOError $ waitForProcess handle
#else
      return ()
#endif


closeBinaryHandle :: Handle -> IO ()
closeBinaryHandle = void . tryIOError . hClose


instance MonadCommandEffect DryRunIO where
  lookupEnvironmentVariable = liftIO . lookupEnvironmentVariableIO
  processEnvironment = liftIO processEnvironmentIO
  detectEnvironment = liftIO detectEnvironmentIO
  hostPlatform = liftIO hostPlatformIO
  currentTime = liftIO currentTimeIO
  newUUID = liftIO newUUIDIO
  isTerminal = liftIO . isTerminalIO
  writeStream stream = liftIO . writeStreamIO stream
  prompt = liftIO . promptIO
  runProcess _ = return $ ProcessUnavailable ProcessExecution
  runBinaryProcess _ =
    return $ BinaryProcessUnavailable CodecBackendExecution
  abortCommand = liftIO . throwIO


instance MonadProcessControl DryRunIO where
  startProcess _ = return $ Left $ ProcessUnavailable ProcessExecution


streamHandle :: StandardStream -> Handle
streamHandle StandardInput = stdin
streamHandle StandardOutput = stdout
streamHandle StandardError = stderr


-- | Looks up an environment variable in the production interpreter.
lookupEnvironmentVariableIO :: String -> IO (Maybe String)
lookupEnvironmentVariableIO = Environment.lookupEnv


-- | Reads the child-process environment in the production interpreter.
processEnvironmentIO :: IO [(String, String)]
processEnvironmentIO = Environment.getEnvironment


-- | Detects the host environment in the production interpreter.
detectEnvironmentIO :: IO Environment
detectEnvironmentIO = CurrentEnvironment.currentEnvironment


-- | Reads the runtime platform identifier in the production interpreter.
hostPlatformIO :: IO String
hostPlatformIO = return System.Info.os


-- | Reads wall-clock time in the production interpreter.
currentTimeIO :: IO UTCTime
currentTimeIO = getCurrentTime


-- | Generates a UUID in the production interpreter.
newUUIDIO :: IO UUID
newUUIDIO = UUID.nextRandom


-- | Tests terminal attachment in the production interpreter.
isTerminalIO :: StandardStream -> IO Bool
isTerminalIO = hIsTerminalDevice . streamHandle


-- | Writes to a standard stream in the production interpreter.
writeStreamIO :: OutputStream -> Text -> IO ()
writeStreamIO OutputStandard = TextIO.hPutStr stdout
writeStreamIO OutputError = TextIO.hPutStr stderr


-- | Runs an interactive prompt in the production interpreter.
promptIO :: PromptRequest -> IO PromptResult
promptIO request = case request of
  ConfirmPrompt message -> Confirmed <$> FortyTwo.confirm (unpack message)
  InputPrompt message -> InputValue . pack <$> FortyTwo.input (unpack message)
  SelectPrompt message values ->
    SelectedValue . pack
      <$> FortyTwo.select (unpack message) (unpack <$> values)
  MultiselectPrompt message values ->
    MultiselectedValues . map pack
      <$> FortyTwo.multiselect (unpack message) (unpack <$> values)


toCreateProcess :: ProcessRequest -> CreateProcess
toCreateProcess request =
  (proc request.executable request.arguments)
    { cwd = request.workingDirectory
    , env = request.environment
    , std_out = if request.captureOutput then CreatePipe else Inherit
    , std_err = if request.captureOutput then CreatePipe else Inherit
    , delegate_ctlc = True
    }


toBinaryCreateProcess :: BinaryProcessRequest -> CreateProcess
toBinaryCreateProcess request =
  (proc request.binaryExecutable [])
    { cwd = request.binaryWorkingDirectory
    , env = Just request.binaryEnvironment
    , std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    , delegate_ctlc = True
    }


cleanupProcess :: ProcessHandle -> IO ()
cleanupProcess handle = do
  _ <- tryIOError $ terminateProcess handle
  _ <- tryIOError $ waitForProcess handle
  return ()


startProcessHandleIO
  :: ProcessRequest -> IO (Either ProcessResult ProcessHandle)
-- Mask process creation itself.  The caller remains responsible for masking
-- the handoff after this function returns a handle.
startProcessHandleIO request = mask $ \_ -> do
  started <- tryIOError $ createProcess $ toCreateProcess request
  return $ case started of
    Left err -> Left $ ProcessStartFailed $ pack $ show err
    Right (_, _, _, handle) -> Right handle


-- | Starts an inherited-stream child in the production interpreter.
startProcessIO
  :: ProcessRequest -> IO (Either ProcessResult (StartedProcess IO))
startProcessIO request
  | request.captureOutput =
      return $ Left $ ProcessStartFailed "captured processes cannot be split"
  | otherwise = do
      started <- startProcessHandleIO request
      return $ case started of
        Left result -> Left result
        Right handle ->
          Right $
            StartedProcess
              (waitStartedProcessIO handle)
              (cleanupProcess handle)


waitStartedProcessIO :: ProcessHandle -> IO ProcessResult
waitStartedProcessIO handle = mask $ \restore -> do
  waited <-
    tryIOError (restore $ waitForProcess handle)
      `onException` cleanupProcess handle
  case waited of
    Left _ -> cleanupProcess handle
    Right _ -> return ()
  return $ case waited of
    Left err -> ProcessWaitFailed $ pack $ show err
    Right code -> ProcessCompleted code "" ""


-- | Runs an action against an exact effect script.
--
-- Every response must be consumed, and every requested effect must match the
-- next response's type.  The returned log is in execution order.
runCommandEffectTest
  :: [CommandEffectResponse]
  -> CommandEffectTest a
  -> IO (Either CommandEffectError (a, [CommandEffect]))
runCommandEffectTest scripted action = do
  stateRef <- newIORef $ CommandEffectTestState scripted [] 0 []
  result <-
    dryRunIO $
      runExceptT $
        runReaderT action.unCommandEffectTest stateRef
  finalState <- readIORef stateRef
  return $ case result of
    Left (CommandAborted exitCode) ->
      case reverse finalState.activeProcesses of
        [] -> Left $ CommandAborted exitCode
        remaining ->
          Left $ CommandAbortedWithUnfinishedProcesses exitCode remaining
    Left err -> Left err
    Right value -> case finalState.responses of
      [] -> case reverse finalState.activeProcesses of
        [] -> Right (value, reverse finalState.effects)
        remaining -> Left $ UnfinishedProcesses remaining
      remaining -> Left $ UnusedResponses remaining
