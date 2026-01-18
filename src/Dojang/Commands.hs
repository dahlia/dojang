{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dojang.Commands
  ( Admonition (..)
  , Color (..)
  , codeStyleFor
  , colorFor
  , die'
  , dieWithErrors
  , pathStyleFor
  , pathStyleFor'
  , printStderr
  , printStderr'
  , printTable
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Environment (lookupEnv)
import System.Exit (ExitCode, exitWith)
import System.IO.Extra (Handle, hIsTerminalDevice, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (putStr, replicate)

import Data.List (transpose)
import Data.Text (Text, length, pack, replicate)
import Data.Text.IO (hPutStr, hPutStrLn, putStr)
import System.Console.Pretty
  ( Color (..)
  , Pretty
  , Style (..)
  , bgColor
  , style
  )
import System.Console.Pretty qualified (color)
import System.OsPath (OsPath, decodeFS)
import TextShow (FromStringShow (FromStringShow), TextShow (showt))


isColorAvailable :: (MonadIO m) => Handle -> m Bool
isColorAvailable handle = liftIO $ do
  term <- lookupEnv "TERM"
  noColor <- lookupEnv "NO_COLOR"
  case (term, noColor) of
    (Just "dumb", _) -> return False
    (_, Just (_ : _)) -> return False
    _ -> hIsTerminalDevice handle


colorFor
  :: forall a m. (Pretty a, MonadIO m) => Handle -> m (Color -> Color -> a -> a)
colorFor handle = do
  colorAvailable <- isColorAvailable handle
  return $ if colorAvailable then color else dumb
 where
  dumb :: Color -> Color -> a -> a
  dumb _ _ = id
  color :: Color -> Color -> a -> a
  color bg text v = bgColor bg $ System.Console.Pretty.color text v


codeStyleFor :: forall a m. (Pretty a, MonadIO m) => Handle -> m (a -> a)
codeStyleFor handle = do
  colorAvailable <- isColorAvailable handle
  return $
    if colorAvailable
      then style Bold
      else id


pathStyleFor :: forall m. (MonadIO m) => Handle -> m (OsPath -> Text)
pathStyleFor handle = do
  pathStyle <- pathStyleFor' handle
  return $ \path -> pathStyle $ decodeFS' path
 where
  decodeFS' :: OsPath -> Text
  decodeFS' = pack . unsafePerformIO . decodeFS


pathStyleFor' :: forall a m. (Pretty a, MonadIO m) => Handle -> m (a -> a)
pathStyleFor' handle = do
  colorAvailable <- isColorAvailable handle
  return $
    if colorAvailable
      then style Italic
      else id


printStderr :: (MonadIO m) => Text -> m ()
printStderr = liftIO . hPutStrLn stderr


data Admonition = Hint | Note | Warning | Error
  deriving (Show, Eq, Ord, Enum, Bounded)


printStderr' :: (MonadIO m) => Admonition -> Text -> m ()
printStderr' admonition message = do
  color <- colorFor stderr
  printStderr $ color Default color' prefix <> message
 where
  color' :: Color
  color' = case admonition of
    Hint -> Cyan
    Note -> Yellow
    Warning -> Yellow
    Error -> Red
  prefix :: Text
  prefix = showt (FromStringShow admonition) <> ": "


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
    putCol stderr Cyan (replicate width "\x2500") width
    liftIO $ hPutStr stderr " "
  putLnStderr
  forM_ rows $ \row -> do
    forM_ (zip3 row columnWidths [1 ..]) $ \((color', value), width, i) -> do
      putCol stdout color' value $
        if i < Prelude.length row then width else Data.Text.length value
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
    color <- colorFor stdout
    liftIO $ hPutStr h $ color Default color' value
    liftIO $ hPutStr h $ replicate (width - Data.Text.length value) " "
  putLn :: m ()
  putLn = liftIO $ putStrLn mempty
  putLnStderr :: m ()
  putLnStderr = liftIO $ hPutStrLn stderr mempty
