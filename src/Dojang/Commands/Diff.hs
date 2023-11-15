{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dojang.Commands.Diff (DiffMode (..), diff) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (find, nub)
import Data.Maybe (maybeToList)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (stderr, stdout)
import System.Info (os)
import Prelude hiding (lines, putStrLn, readFile)
import Prelude qualified (putStrLn)

import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput
  ( DiffOperation (..)
  , LineRange (..)
  , diffToLineRanges
  )
import Data.Text (Text, cons, lines, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (putStrLn)
import System.Directory.OsPath (makeAbsolute)
import System.OsPath (OsPath)
import System.Process (spawnProcess, waitForProcess)
import TextShow (TextShow (showt))

import Dojang.App (App, ensureContext)
import Dojang.Commands
  ( Admonition (..)
  , Color (..)
  , colorFor
  , die'
  , pathStyleFor
  , pathStyleFor'
  , printStderr'
  )
import Dojang.Commands.Status (printWarnings)
import Dojang.ExitCodes (externalProgramNonZeroExit, fileNotRoutedError)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Context
  ( FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , makeCorrespond
  )


data DiffMode = Both | Source | Destination deriving (Show)


diff
  :: (MonadFileSystem i, MonadIO i)
  => DiffMode
  -> Maybe OsPath
  -> [OsPath]
  -> App i ExitCode
diff mode diffProgram files = do
  ctx <- ensureContext
  (corresponds, ws) <- makeCorrespond ctx
  routedFiles <- forM corresponds $ \c -> do
    sourcePath <- liftIO $ makeAbsolute c.source.path
    destinationPath <- liftIO $ makeAbsolute c.destination.path
    return (sourcePath, destinationPath)
  pathStyle <- pathStyleFor stderr
  nonExistents <- (`filterM` files) $ \file -> do
    file' <- liftIO $ makeAbsolute file
    if any (\(src, dst) -> file' == src || file' == dst) routedFiles
      then return False
      else do
        printWarnings ws
        printStderr' Error $ pathStyle file <> " is not a routed file."
        return True
  unless (null nonExistents) $ do
    printWarnings ws
    liftIO $ exitWith fileNotRoutedError
  files' <-
    nub <$> case files of
      [] -> return corresponds
      _ -> do
        fs <- forM files $ \file -> do
          file' <- liftIO $ makeAbsolute file
          let found =
                find (\((src, dst), _) -> file' == src || file' == dst)
                  $ zip routedFiles corresponds
          return $ maybeToList $ fmap snd found
        return $ concat fs
  case mode of
    Source -> twoWay diffProgram (.source) (.intermediate) files'
    Destination -> twoWay diffProgram (.destination) (.intermediate) files'
    Both -> twoWay diffProgram (.source) (.destination) files'
  printWarnings ws
  return ExitSuccess


nullPath :: FilePath
nullPath = if os == "mingw32" then "NUL" else "/dev/null"


getPath :: (MonadFileSystem m) => FileEntry -> m FilePath
getPath entry = case entry.stat of
  Missing -> return nullPath
  Directory -> return nullPath
  _ -> decodePath entry.path


twoWay
  :: (MonadFileSystem i, MonadIO i)
  => Maybe OsPath
  -> (FileCorrespondence -> FileEntry)
  -> (FileCorrespondence -> FileEntry)
  -> [FileCorrespondence]
  -> App i ()
twoWay program' srcSelector dstSelector files = do
  program <- case program' of
    Just p -> Just <$> decodePath p
    Nothing -> liftIO $ lookupEnv "DOJANG_DIFF"
  forM_ files $ \file -> when (hasChange file) $ do
    let src = srcSelector file
    let dst = dstSelector file
    case program of
      Nothing -> do
        builtinDiff src dst
      Just prog -> do
        srcPath <- getPath src
        dstPath <- getPath dst
        handle <- liftIO $ spawnProcess prog [srcPath, dstPath]
        exitCode <- liftIO $ waitForProcess handle
        when (exitCode /= ExitSuccess && exitCode /= ExitFailure 1) $ do
          cmdStyle <- pathStyleFor' stderr
          die' externalProgramNonZeroExit
            $ cmdStyle (pack prog)
            <> " terminated with exit code "
            <> showt exitCode
            <> "."


hasChange :: FileCorrespondence -> Bool
hasChange c = case (c.sourceDelta, c.destinationDelta) of
  (Unchanged, Unchanged) -> False
  _ -> True


builtinDiff
  :: forall i. (MonadFileSystem i, MonadIO i) => FileEntry -> FileEntry -> App i ()
builtinDiff a b = do
  color <- colorFor stdout
  color' <- colorFor stdout
  pathStyle <- pathStyleFor stdout
  liftIO $ putStrLn $ color Default Red "--- " <> pathStyle a.path <> case a.stat of
    Missing -> " (missing)"
    Directory -> " (directory)"
    _ -> ""
  liftIO $ putStrLn $ color Default Green "+++ " <> pathStyle b.path <> case b.stat of
    Missing -> " (missing)"
    Directory -> " (directory)"
    _ -> ""
  textA <- readFileEntry a
  textB <- readFileEntry b
  let linesA = lines textA
  let linesB = lines textB
  let diffOps =
        diffToLineRanges
          $ getGroupedDiff (unpack <$> linesA) (unpack <$> linesB)
  forM_ diffOps $ \op -> liftIO $ do
    case op of
      Deletion aRange bOffset -> do
        let ( addRange
              , delRange
              , (headOffset, headLines)
              , (tailOffset, tailLines)
              ) = calcRange linesB (bOffset + 1, bOffset) linesA aRange.lrNumbers
        printRange delRange addRange
        forM_ (take headLines (drop headOffset linesB)) $ \line ->
          putStrLn $ ' ' `cons` line
        forM_ aRange.lrContents $ \line ->
          Prelude.putStrLn $ color' Default Red ('-' : line)
        forM_ (take tailLines (drop tailOffset linesB)) $ \line ->
          putStrLn $ ' ' `cons` line
      Addition bRange aOffset -> do
        let ( addRange
              , delRange
              , (headOffset, headLines)
              , (tailOffset, tailLines)
              ) = calcRange linesB bRange.lrNumbers linesA (aOffset + 1, aOffset)
        printRange delRange addRange
        forM_ (take headLines (drop headOffset linesB)) $ \line ->
          putStrLn $ ' ' `cons` line
        forM_ bRange.lrContents $ \line ->
          Prelude.putStrLn $ color' Default Green ('+' : line)
        forM_ (take tailLines (drop tailOffset linesB)) $ \line ->
          putStrLn $ ' ' `cons` line
      Change aRange bRange -> do
        let ( addRange
              , delRange
              , (headOffset, headLines)
              , (tailOffset, tailLines)
              ) = calcRange linesB bRange.lrNumbers linesA aRange.lrNumbers
        printRange delRange addRange
        forM_ (take headLines (drop headOffset linesB)) $ \line ->
          putStrLn $ ' ' `cons` line
        forM_ aRange.lrContents $ \line ->
          Prelude.putStrLn $ color' Default Red ('-' : line)
        forM_ bRange.lrContents $ \line ->
          Prelude.putStrLn $ color' Default Green ('+' : line)
        forM_ (take tailLines (drop tailOffset linesB)) $ \line ->
          putStrLn $ ' ' `cons` line
 where
  context :: Int
  context = 3
  calcRange
    :: [Text]
    -> (Int, Int)
    -> [Text]
    -> (Int, Int)
    -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
  calcRange addContentLines (addBegin, addEnd) delContentLines (delBegin, delEnd) =
    let headOffset = max 0 (addBegin - 1 - context)
        headLines = min context (addBegin - 1 - headOffset)
        tailOffset = addEnd
        tailLines = min context (length addContentLines - tailOffset)
        addOffset =
          min (length addContentLines) (1 + max 0 (addBegin - 1 - context))
        addLines =
          min
            (length addContentLines)
            (addEnd - (addBegin - 1) + headLines + tailLines)
        delOffset =
          min (length delContentLines) (1 + max 0 (delBegin - 1 - context))
        delLines =
          min
            (length delContentLines)
            (delEnd - (delBegin - 1) + headLines + tailLines)
    in ( (addOffset, addLines)
       , (delOffset, delLines)
       , (headOffset, headLines)
       , (tailOffset, tailLines)
       )
  printRange :: (Int, Int) -> (Int, Int) -> IO ()
  printRange (delOffset, delLines) (addOffset, addLines) = do
    color <- colorFor stdout
    putStrLn
      $ color Default Cyan
      $ "@@ -"
      <> showt delOffset
      <> ","
      <> showt delLines
      <> " +"
      <> showt addOffset
      <> ","
      <> showt addLines
      <> " @@"
  readFileEntry :: FileEntry -> App i Text
  readFileEntry entry = case entry.stat of
    Missing -> return ""
    Directory -> return ""
    _ ->
      -- FIXME: It is assuming that the file is encoded in UTF-8, but it is not
      --        always the case:
      decodeUtf8 <$> readFile entry.path
