{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dojang.Commands
  ( Admonition (..)
  , Color (..)
  , StandardStream (..)
  , codeStyleFor
  , colorFor
  , die'
  , dieWithErrors
  , ensureRouteOwnership
  , pathStyleFor
  , pathStyleFor'
  , printModeRestoreFailure
  , printSkippedReconciliation
  , printStderr
  , printStderr'
  , printTable
  ) where

import Control.Monad (forM_)
import System.Exit (ExitCode)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (putStr, replicate)

import Data.List (transpose)
import Data.Text (Text, length, pack, replicate)
import System.Console.Pretty
  ( Color (..)
  , Pretty
  , Style (..)
  , bgColor
  , style
  )
import System.Console.Pretty qualified (color)
import System.OsPath (OsPath, decodeFS)

import Dojang.CommandEffect
  ( MonadCommandEffect (..)
  , OutputStream (..)
  , StandardStream (..)
  )
import Dojang.ExitCodes (routeOwnershipError)
import Dojang.Types.Reconciliation (ReconciliationSkipReason (..))
import Dojang.Types.RouteOwnership
  ( OwnershipError
  , formatOwnershipError
  )
import TextShow (FromStringShow (FromStringShow), TextShow (showt))


isColorAvailable :: (MonadCommandEffect m) => StandardStream -> m Bool
isColorAvailable stream = do
  term <- lookupEnvironmentVariable "TERM"
  noColor <- lookupEnvironmentVariable "NO_COLOR"
  case (term, noColor) of
    (Just "dumb", _) -> return False
    (_, Just (_ : _)) -> return False
    _ -> isTerminal stream


colorFor
  :: forall a m
   . (Pretty a, MonadCommandEffect m)
  => StandardStream
  -> m (Color -> Color -> a -> a)
colorFor stream = do
  colorAvailable <- isColorAvailable stream
  return $ if colorAvailable then color else dumb
 where
  dumb :: Color -> Color -> a -> a
  dumb _ _ = id
  color :: Color -> Color -> a -> a
  color bg text v = bgColor bg $ System.Console.Pretty.color text v


codeStyleFor
  :: forall a m. (Pretty a, MonadCommandEffect m) => StandardStream -> m (a -> a)
codeStyleFor stream = do
  colorAvailable <- isColorAvailable stream
  return $
    if colorAvailable
      then style Bold
      else id


pathStyleFor
  :: forall m. (MonadCommandEffect m) => StandardStream -> m (OsPath -> Text)
pathStyleFor stream = do
  pathStyle <- pathStyleFor' stream
  return $ \path -> pathStyle $ decodeFS' path
 where
  decodeFS' :: OsPath -> Text
  decodeFS' = pack . unsafePerformIO . decodeFS


pathStyleFor'
  :: forall a m
   . (Pretty a, MonadCommandEffect m)
  => StandardStream
  -> m (a -> a)
pathStyleFor' stream = do
  colorAvailable <- isColorAvailable stream
  return $
    if colorAvailable
      then style Italic
      else id


printStderr :: (MonadCommandEffect m) => Text -> m ()
printStderr message = writeStream OutputError $ message <> "\n"


data Admonition = Hint | Note | Warning | Error
  deriving (Show, Eq, Ord, Enum, Bounded)


printStderr' :: (MonadCommandEffect m) => Admonition -> Text -> m ()
printStderr' admonition message = do
  color <- colorFor StandardError
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


die' :: (MonadCommandEffect m) => ExitCode -> Text -> m a
die' exitCode message = do
  printStderr' Error message
  abortCommand exitCode


-- | Unwraps a route-ownership result, dying with 'routeOwnershipError'
-- and a formatted message when the route configuration is unsafe.
ensureRouteOwnership
  :: (MonadCommandEffect m) => Either OwnershipError a -> m a
ensureRouteOwnership (Right value) = return value
ensureRouteOwnership (Left err) = do
  pathStyle <- pathStyleFor StandardError
  die' routeOwnershipError $ formatOwnershipError pathStyle err


dieWithErrors :: (MonadCommandEffect m) => ExitCode -> [Text] -> m a
dieWithErrors exitCode errors = do
  forM_ errors $ printStderr' Error
  abortCommand exitCode


printTable
  :: forall m. (MonadCommandEffect m) => [Text] -> [[(Color, Text)]] -> m ()
printTable headers rows = do
  -- Headers are printed to stderr, so that they can be piped to another
  -- program without interfering with the table.
  forM_ (zip headers columnWidths) $ \(header, width) -> do
    putCol OutputError StandardError Cyan header width
    writeStream OutputError " "
  putLnStderr
  forM_ columnWidths $ \width -> do
    putCol OutputError StandardError Cyan (replicate width "\x2500") width
    writeStream OutputError " "
  putLnStderr
  forM_ rows $ \row -> do
    forM_ (zip3 row columnWidths [1 ..]) $ \((color', value), width, i) -> do
      putCol OutputStandard StandardOutput color' value $
        if i < Prelude.length row then width else Data.Text.length value
      writeStream OutputStandard " "
    putLn
 where
  valueWidths :: [[Int]]
  valueWidths =
    [Data.Text.length c | c <- headers]
      : [[Data.Text.length v | (_, v) <- row] | row <- rows]
  columnWidths :: [Int]
  columnWidths = map maximum $ transpose valueWidths
  putCol :: OutputStream -> StandardStream -> Color -> Text -> Int -> m ()
  putCol output stream color' value width = do
    color <- colorFor stream
    writeStream output $ color Default color' value
    writeStream output $ replicate (width - Data.Text.length value) " "
  putLn :: m ()
  putLn = writeStream OutputStandard "\n"
  putLnStderr :: m ()
  putLnStderr = writeStream OutputError "\n"


-- | Explains why one planned correspondence was skipped.  The first path
-- names the authoritative side of the correspondence for unsupported
-- symbolic links, which differs between apply and reflect; every other
-- reason names the destination path.
printSkippedReconciliation
  :: (MonadCommandEffect m)
  => (OsPath -> Text)
  -- ^ Path renderer.
  -> OsPath
  -- ^ Authoritative path named for 'UnsupportedSymlink'.
  -> OsPath
  -- ^ Destination path named for the other reasons.
  -> ReconciliationSkipReason
  -- ^ Why the correspondence was skipped.
  -> m ()
printSkippedReconciliation pathStyle authoritativePath destinationPath reason =
  case reason of
    IgnoredDestination _ pattern ->
      printStderr' Warning $
        "Skipping "
          <> pathStyle destinationPath
          <> " because it is ignored by pattern "
          <> pack (show pattern)
          <> "."
    UnsupportedSymlink ->
      printStderr' Warning $
        "Skipping "
          <> pathStyle authoritativePath
          <> " because symbolic link synchronization is not supported."
    DeploymentLinkNotReflectable ->
      printStderr' Note $
        "Skipping "
          <> pathStyle destinationPath
          <> " because deployment links are never reflected."
    ProtectedSubtreeReplacement ->
      printStderr' Warning $
        "Skipping "
          <> pathStyle destinationPath
          <> " because entries inside it are owned by nested routes."


-- | Warns that a prior read-only mode could not be restored while a
-- failed reconciliation plan was being undone.
printModeRestoreFailure :: (MonadCommandEffect m) => IOError -> m ()
printModeRestoreFailure err =
  printStderr' Warning $
    "Failed to restore a prior mode while undoing changes: "
      <> pack (show err)
      <> "."
