{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dojang.Commands
  ( Admonition (..)
  , die'
  , dieWithErrors
  , printStderr
  , printStderr'
  , printTable
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Exit (ExitCode, exitWith)
import System.IO.Extra (Handle, hIsTerminalDevice, stderr, stdout)
import Prelude hiding (putStr, replicate)

import Data.List (transpose)
import Data.Text (Text, length, replicate)
import Data.Text.IO (hPutStr, hPutStrLn, putStr)
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


die' :: (MonadIO m) => ExitCode -> Text -> m a
die' exitCode message = do
  printStderr' Error message
  liftIO $ exitWith exitCode


dieWithErrors :: (MonadIO m) => ExitCode -> [Text] -> m a
dieWithErrors exitCode errors = do
  forM_ errors $ printStderr' Error
  liftIO $ exitWith exitCode


printTable :: forall m. (MonadIO m) => [Text] -> [[(Color, Text)]] -> m ()
printTable headers rows = do
  -- Headers are printed to stderr, so that they can be piped to another
  -- program without interfering with the table.
  forM_ (zip headers columnWidths) $ \(header, width) -> do
    putCol stderr Cyan header width
    liftIO $ hPutStr stderr " "
  putLnStderr
  forM_ columnWidths $ \width -> do
    putCol stderr Cyan (replicate width "-") width
    liftIO $ hPutStr stderr " "
  putLnStderr
  forM_ rows $ \row -> do
    forM_ (zip row columnWidths) $ \((color', value), width) -> do
      putCol stdout color' value width
      liftIO $ putStr " "
    putLn
 where
  valueWidths :: [[Int]]
  valueWidths =
    [Data.Text.length c | c <- headers]
      : [[Data.Text.length v | (_, v) <- row] | row <- rows]
  columnWidths :: [Int]
  columnWidths = map maximum $ transpose valueWidths
  putCol :: Handle -> Color -> Text -> Int -> m ()
  putCol h color' value width = do
    isATty <- liftIO $ hIsTerminalDevice h
    if isATty
      then liftIO $ hPutStr h $ color color' value
      else liftIO $ hPutStr h value
    liftIO $ hPutStr h $ replicate (width - Data.Text.length value) " "
  putLn :: m ()
  putLn = liftIO $ putStrLn mempty
  putLnStderr :: m ()
  putLnStderr = liftIO $ hPutStrLn stderr mempty
