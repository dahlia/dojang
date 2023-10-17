{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands
  ( Admonition (..)
  , die
  , die'
  , printStderr
  , printStderr'
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hIsTerminalDevice, stderr)

import Data.Text (Text)
import Data.Text.IO (hPutStrLn)
import System.Console.Pretty (Color (..), color)
import TextShow (FromStringShow (FromStringShow), TextShow (showt))


printStderr :: (MonadIO m) => Text -> m ()
printStderr = liftIO . hPutStrLn stderr


data Admonition = Hint | Note | Warning | Error
  deriving (Show, Eq, Ord, Enum, Bounded)


printStderr' :: (MonadIO m) => Admonition -> Text -> m ()
printStderr' admonition message = do
  isATty <- liftIO $ hIsTerminalDevice stderr
  printStderr
    $ (if isATty then colorPrefix else plainPrefix)
    <> message
 where
  color' :: Color
  color' = case admonition of
    Hint -> Cyan
    Note -> Yellow
    Warning -> Yellow
    Error -> Red
  colorPrefix :: Text
  colorPrefix = color color' plainPrefix
  plainPrefix :: Text
  plainPrefix = showt (FromStringShow admonition) <> ": "


die' :: (MonadIO m) => Int -> Text -> m a
die' exitCode message = do
  printStderr' Error message
  liftIO $ exitWith $ ExitFailure exitCode


die :: (MonadIO m) => Text -> m a
die = die' 1
