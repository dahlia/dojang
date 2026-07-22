{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Explicit effects used by command orchestration.
--
-- Commands depend on this interface instead of lifting arbitrary 'IO'.  The
-- production, dry-run, and test interpreters can therefore make different
-- guarantees about which effects are allowed to reach the host.
module Dojang.CommandEffect
  ( CommandEffect (..)
  , CommandEffectError (..)
  , CommandEffectKind (..)
  , CommandEffectResponse (..)
  , CommandEffectTest
  , MonadCommandEffect (..)
  , MonadProcessControl (..)
  , OutputStream (..)
  , ProcessRequest (..)
  , ProcessResult (..)
  , PromptRequest (..)
  , PromptResult (..)
  , StandardStream (..)
  , StartedProcess (..)
  , confirmPrompt
  , currentTimeIO
  , detectEnvironmentIO
  , emptyProcessRequest
  , hostPlatformIO
  , isTerminalIO
  , lookupEnvironmentVariableIO
  , newUUIDIO
  , processEnvironmentIO
  , promptIO
  , runCommandEffectTest
  , runProcessIO
  , selectPrompt
  , startProcessIO
  , writeStreamIO
  ) where

import Control.Exception (mask, onException, throwIO)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict
  ( MonadState (get, put)
  , StateT
  , modify'
  , runStateT
  )
import Control.Monad.Trans.Class (lift)
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as TextIO
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import System.Environment qualified as Environment
import System.Exit (ExitCode)
import System.IO (Handle, stderr, stdin, stdout)
import System.IO.Error (tryIOError)
import System.Info qualified
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (CreatePipe, Inherit)
  , createProcess
  , proc
  , readCreateProcessWithExitCode
  , terminateProcess
  , waitForProcess
  )

import FortyTwo.Prompts.Confirm qualified as FortyTwo
import FortyTwo.Prompts.Input qualified as FortyTwo
import FortyTwo.Prompts.Multiselect qualified as FortyTwo
import FortyTwo.Prompts.Select qualified as FortyTwo
import System.IO.Extra (hIsTerminalDevice)

import Dojang.MonadFileSystem (DryRunIO)
import Dojang.Types.Environment (Environment)
import Dojang.Types.Environment.Current qualified as CurrentEnvironment


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


-- | A child process that has started and can be awaited or cancelled.
data StartedProcess m = StartedProcess
  { awaitProcess :: m ProcessResult
  -- ^ Waits for the child and returns its observable result.
  , cancelStartedProcess :: m ()
  -- ^ Terminates the child and waits for it to release its resources.
  }


-- | An effect that cannot be silently simulated.
data CommandEffectKind = ProcessExecution | PromptInteraction
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
  | ProcessStart ProcessRequest
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
  | ExecutionRequired CommandEffectKind
  deriving (Eq, Show)


-- | A deterministic script mismatch.
data CommandEffectError
  = UnexpectedEffect CommandEffect
  | UnexpectedResponse CommandEffect CommandEffectResponse
  | UnusedResponses [CommandEffectResponse]
  | CommandAborted ExitCode
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


  -- | Aborts the current command with a public exit status.
  abortCommand :: ExitCode -> m a


-- | Two-phase child-process control for lock-sensitive orchestration.
class (Monad m) => MonadProcessControl m where
  -- | Starts a process whose streams are inherited from the parent.
  --
  -- The returned wait action may be run after a surrounding lock is released.
  startProcess :: ProcessRequest -> m (Either ProcessResult (StartedProcess m))


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
  }


newtype CommandEffectTest a = CommandEffectTest
  { unCommandEffectTest
      :: ExceptT CommandEffectError (StateT CommandEffectTestState IO) a
  }
  deriving (Functor, Applicative, Monad)


record :: CommandEffect -> CommandEffectTest ()
record effect = CommandEffectTest $ modify' $ \state ->
  state{effects = effect : state.effects}


respond
  :: CommandEffect
  -> (CommandEffectResponse -> Maybe a)
  -> CommandEffectTest a
respond effect decode = do
  record effect
  state <- CommandEffectTest get
  case state.responses of
    [] -> CommandEffectTest $ throwError $ UnexpectedEffect effect
    response : rest -> case decode response of
      Nothing ->
        CommandEffectTest $ throwError $ UnexpectedResponse effect response
      Just value -> do
        CommandEffectTest $ put state{responses = rest}
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
  abortCommand = CommandEffectTest . throwError . CommandAborted


instance MonadProcessControl CommandEffectTest where
  startProcess request = do
    result <- respond (ProcessStart request) $ \response -> case response of
      ProcessValue value -> Just value
      ExecutionRequired ProcessExecution ->
        Just $ ProcessUnavailable ProcessExecution
      _ -> Nothing
    return $ case result of
      failed@ProcessStartFailed{} -> Left failed
      unavailable@ProcessUnavailable{} -> Left unavailable
      other -> Right $ StartedProcess (return other) (return ())


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
  abortCommand = throwIO


instance MonadProcessControl IO where
  startProcess = startProcessIO


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


cleanupProcess :: ProcessHandle -> IO ()
cleanupProcess handle = do
  _ <- tryIOError $ terminateProcess handle
  _ <- tryIOError $ waitForProcess handle
  return ()


startProcessHandleIO
  :: ProcessRequest -> IO (Either ProcessResult ProcessHandle)
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
  (result, finalState) <-
    runStateT
      (runExceptT action.unCommandEffectTest)
      (CommandEffectTestState scripted [])
  return $ case result of
    Left err -> Left err
    Right value -> case finalState.responses of
      [] -> Right (value, reverse finalState.effects)
      remaining -> Left $ UnusedResponses remaining
