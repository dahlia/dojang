{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dojang.Commands.Diff (DiffMode (..), diff, diffWithCodecRuntime) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.List (find, nub)
import Data.Maybe (catMaybes)
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
import Data.ByteString (ByteString)
import Data.Text (Text, cons, lines, pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.IO (putStrLn)
import System.OsPath (OsPath)
import System.Process (spawnProcess, waitForProcess)
import TextShow (TextShow (showt))

import Dojang.App
  ( App
  , AppEnv (dryRun)
  , ensureContext
  , prepareMachineState
  )
import Dojang.Commands
  ( Admonition (..)
  , Color (..)
  , colorFor
  , die'
  , ensureRouteOwnership
  , pathStyleFor
  , pathStyleFor'
  , printStderr'
  )
import Dojang.Commands.Hook
  ( HookScopePath (CallerRelativePath)
  , withCommandHooks
  )
import Dojang.Commands.Status (printWarnings)
import Dojang.ExitCodes
  ( codecError
  , externalProgramNonZeroExit
  , fileNotRoutedError
  )
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Codec.BuiltIn (builtInCodecRuntime)
import Dojang.Types.Codec.Context
  ( EvaluatedManagedCorrespondence (..)
  , evaluateManagedCorrespondencesWithCache
  , evaluationWarnings
  , loadCodecCacheEntries
  , renderedSourceFor
  )
import Dojang.Types.Codec.Evaluate
  ( CodecRuntime
  , EvaluationMode (DryRunEvaluation, NormalEvaluation)
  , OpaqueBytes
  , formatCodecError
  , revealBytes
  )
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , ManagedCorrespondence (..)
  , makeManagedCorrespond
  )
import Dojang.Types.Repository (Repository (..))


data DiffMode = Both | Source | Destination deriving (Show)


diff
  :: (MonadFileSystem i, MonadIO i)
  => DiffMode
  -> Maybe OsPath
  -> [OsPath]
  -> App i ExitCode
diff mode diffProgram files =
  do
    dryRun' <- asks (.dryRun)
    let evaluationMode = if dryRun' then DryRunEvaluation else NormalEvaluation
    diffWithCodecRuntime
      (builtInCodecRuntime evaluationMode)
      mode
      diffProgram
      files


-- | Displays differences using an explicit codec runtime.
diffWithCodecRuntime
  :: (MonadFileSystem i, MonadIO i)
  => CodecRuntime (App i)
  -> DiffMode
  -> Maybe OsPath
  -> [OsPath]
  -> App i ExitCode
diffWithCodecRuntime runtime mode diffProgram files =
  withCommandHooks "diff" (CallerRelativePath <$> files) $
    diffCore runtime mode diffProgram files


diffCore
  :: (MonadFileSystem i, MonadIO i)
  => CodecRuntime (App i)
  -> DiffMode
  -> Maybe OsPath
  -> [OsPath]
  -> App i ExitCode
diffCore runtime mode diffProgram files = do
  ctx <- ensureContext
  (managed, ws) <- makeManagedCorrespond ctx >>= ensureRouteOwnership
  routedManaged <- forM managed $ \item -> do
    sourcePath <- makeAbsolute item.correspondence.source.path
    destinationPath <- makeAbsolute item.correspondence.destination.path
    return ((sourcePath, destinationPath), item)
  pathStyle <- pathStyleFor stderr
  nonExistents <- (`filterM` files) $ \file -> do
    file' <- makeAbsolute file
    if any
      ( \((src, dst), _) ->
          file' == src || file' == dst
      )
      routedManaged
      then return False
      else do
        printWarnings ws
        printStderr' Error $ pathStyle file <> " is not a routed file."
        return True
  unless (null nonExistents) $ do
    printWarnings ws
    liftIO $ exitWith fileNotRoutedError
  selected <- case files of
    [] -> return managed
    _ -> fmap (nub . catMaybes) $ forM files $ \file -> do
      file' <- makeAbsolute file
      return $
        snd
          <$> find
            ( \((src, dst), _) ->
                file' == src || file' == dst
            )
            routedManaged
  files' <- case mode of
    Destination ->
      return [(item.correspondence, Nothing) | item <- selected]
    _ -> do
      machineState <- prepareMachineState ctx.repository.manifest
      cache <- loadCodecCacheEntries ctx machineState selected
      evaluatedResult <-
        evaluateManagedCorrespondencesWithCache runtime ctx cache selected
      evaluated <- case evaluatedResult of
        Left err -> die' codecError $ formatCodecError err
        Right value -> return value
      printWarnings $ evaluationWarnings evaluated
      return
        [ (item.managed.correspondence, renderedSourceFor item)
        | item <- evaluated
        ]
  case mode of
    Source ->
      twoWay
        diffProgram
        True
        ((/= Unchanged) . (.sourceDelta))
        (.source)
        (.intermediate)
        files'
    Destination ->
      twoWay
        diffProgram
        False
        ((/= Unchanged) . (.destinationDelta))
        (.destination)
        (.intermediate)
        files'
    Both ->
      twoWay diffProgram True hasChange (.source) (.destination) files'
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
  -> Bool
  -> (FileCorrespondence -> Bool)
  -> (FileCorrespondence -> FileEntry)
  -> (FileCorrespondence -> FileEntry)
  -> [(FileCorrespondence, Maybe OpaqueBytes)]
  -> App i ()
twoWay program' usesRenderedSource selectedHasChange srcSelector dstSelector files = do
  program <- case program' of
    Just p -> Just <$> decodePath p
    Nothing -> liftIO $ lookupEnv "DOJANG_DIFF"
  case program of
    Just _
      | usesRenderedSource
      , any
          ( \(file, renderedSource) ->
              selectedHasChange file && renderedSource /= Nothing
          )
          files ->
          die'
            codecError
            "An external diff program cannot inspect a rendered codec source."
    _ -> return ()
  forM_ files $ \(file, renderedSource) -> when (selectedHasChange file) $ do
    let src = srcSelector file
    let dst = dstSelector file
    case program of
      Nothing -> do
        builtinDiff
          (if usesRenderedSource then renderedSource else Nothing)
          src
          dst
      Just prog -> do
        srcPath <- getPath src
        dstPath <- getPath dst
        handle <- liftIO $ spawnProcess prog [srcPath, dstPath]
        exitCode <- liftIO $ waitForProcess handle
        when (exitCode /= ExitSuccess && exitCode /= ExitFailure 1) $ do
          cmdStyle <- pathStyleFor' stderr
          die' externalProgramNonZeroExit $
            cmdStyle (pack prog)
              <> " terminated with exit code "
              <> showt exitCode
              <> "."


hasChange :: FileCorrespondence -> Bool
hasChange c = case (c.sourceDelta, c.destinationDelta) of
  (Unchanged, Unchanged) -> False
  _ -> True


builtinDiff
  :: forall i
   . (MonadFileSystem i, MonadIO i)
  => Maybe OpaqueBytes
  -> FileEntry
  -> FileEntry
  -> App i ()
builtinDiff renderedA a b = do
  bytesA <- case renderedA of
    Just content -> return $ revealBytes content
    Nothing -> readFileEntryBytes a
  bytesB <- readFileEntryBytes b
  unless (a.stat == b.stat && bytesA == bytesB) $ do
    color <- colorFor stdout
    color' <- colorFor stdout
    pathStyle <- pathStyleFor stdout
    liftIO $
      putStrLn $
        color Default Red "--- " <> pathStyle a.path <> case a.stat of
          Missing -> " (missing)"
          Directory -> " (directory)"
          _ -> ""
    liftIO $
      putStrLn $
        color Default Green "+++ " <> pathStyle b.path <> case b.stat of
          Missing -> " (missing)"
          Directory -> " (directory)"
          _ -> ""
    case (decodeUtf8' bytesA, decodeUtf8' bytesB) of
      (Right textA, Right textB) -> do
        let linesA = lines textA
        let linesB = lines textB
        let diffOps =
              diffToLineRanges $
                getGroupedDiff (unpack <$> linesA) (unpack <$> linesB)
        forM_ diffOps $ \op -> liftIO $ case op of
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
      _ -> liftIO $ putStrLn "Binary files differ."
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
    putStrLn $
      color Default Cyan $
        "@@ -"
          <> showt delOffset
          <> ","
          <> showt delLines
          <> " +"
          <> showt addOffset
          <> ","
          <> showt addLines
          <> " @@"
  readFileEntryBytes :: FileEntry -> App i ByteString
  readFileEntryBytes entry = case entry.stat of
    Missing -> return ""
    Directory -> return ""
    _ -> readFile entry.path
