{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , OutputStream (..)
  , ProcessRequest (..)
  , ProcessResult (..)
  , PromptRequest (..)
  , PromptResult (..)
  , StandardStream (..)
  , emptyProcessRequest
  , runCommandEffectTest
  ) where

import Control.Exception (mask, onException)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict
  ( MonadState (get, put)
  , StateT
  , modify'
  , runStateT
  )
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as TextIO
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import System.Environment qualified as Environment
import System.Exit (ExitCode)
import System.IO (Handle, stderr, stdin, stdout)
import System.IO.Error (tryIOError)
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


-- | An effect that cannot be silently simulated.
data CommandEffectKind = ProcessExecution | PromptInteraction
  deriving (Eq, Ord, Show)


-- | One command effect, suitable for deterministic test assertions.
data CommandEffect
  = EnvironmentLookup String
  | ProcessEnvironmentRead
  | CurrentTimeRead
  | UUIDGenerated
  | TerminalChecked StandardStream
  | StreamWrite OutputStream Text
  | Prompted PromptRequest
  | ProcessRun ProcessRequest
  deriving (Eq, Show)


-- | A scripted response consumed by the test interpreter.
data CommandEffectResponse
  = EnvironmentValue (Maybe String)
  | ProcessEnvironmentValue [(String, String)]
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
  deriving (Eq, Show)


-- | Effects that commands may perform outside the filesystem abstraction.
class (Monad m) => MonadCommandEffect m where
  -- | Looks up one process environment variable.
  lookupEnvironmentVariable :: String -> m (Maybe String)


  -- | Reads the complete process environment for a child process.
  processEnvironment :: m [(String, String)]


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


instance MonadCommandEffect IO where
  lookupEnvironmentVariable = Environment.lookupEnv
  processEnvironment = Environment.getEnvironment
  currentTime = getCurrentTime
  newUUID = UUID.nextRandom
  isTerminal stream = hIsTerminalDevice $ streamHandle stream
  writeStream OutputStandard value = TextIO.hPutStr stdout value
  writeStream OutputError value = TextIO.hPutStr stderr value
  prompt request = case request of
    ConfirmPrompt message -> Confirmed <$> FortyTwo.confirm (unpack message)
    InputPrompt message -> InputValue . pack <$> FortyTwo.input (unpack message)
    SelectPrompt message values ->
      SelectedValue . pack
        <$> FortyTwo.select (unpack message) (unpack <$> values)
    MultiselectPrompt message values ->
      MultiselectedValues . map pack
        <$> FortyTwo.multiselect (unpack message) (unpack <$> values)
  runProcess request
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
  lookupEnvironmentVariable = liftIO . Environment.lookupEnv
  processEnvironment = liftIO Environment.getEnvironment
  currentTime = liftIO getCurrentTime
  newUUID = liftIO UUID.nextRandom
  isTerminal stream = liftIO $ hIsTerminalDevice $ streamHandle stream
  writeStream OutputStandard value = liftIO $ TextIO.hPutStr stdout value
  writeStream OutputError value = liftIO $ TextIO.hPutStr stderr value
  prompt _ = return PromptUnavailable
  runProcess _ = return $ ProcessUnavailable ProcessExecution


streamHandle :: StandardStream -> Handle
streamHandle StandardInput = stdin
streamHandle StandardOutput = stdout
streamHandle StandardError = stderr


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
