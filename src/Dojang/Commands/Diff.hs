{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dojang.Commands.Diff (DiffMode (..), diff, diffWithCodecRuntime) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (asks)
import Data.List (find, nub)
import Data.Maybe (catMaybes)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Prelude hiding (lines, readFile)

import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput
  ( DiffOperation (..)
  , LineRange (..)
  , diffToLineRanges
  )
import Data.ByteString (ByteString)
import Data.Text (Text, cons, lines, pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import System.OsPath (OsPath)
import TextShow (TextShow (showt))

import Dojang.App
  ( App
  , AppEnv (dryRun)
  , ensureContext
  , prepareMachineState
  )
import Dojang.CommandEffect
  ( MonadCommandEffect
      ( abortCommand
      , hostPlatform
      , lookupEnvironmentVariable
      , runProcess
      , writeStream
      )
  , OutputStream (OutputStandard)
  , ProcessRequest (..)
  , ProcessResult (..)
  , emptyProcessRequest
  )
import Dojang.Commands
  ( Admonition (..)
  , Color (..)
  , StandardStream (..)
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
  pathStyle <- pathStyleFor StandardError
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
    abortCommand fileNotRoutedError
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


nullPath :: (MonadCommandEffect m) => m FilePath
nullPath = do
  platform <- hostPlatform
  return $ if platform == "mingw32" then "NUL" else "/dev/null"


getPath :: (MonadFileSystem m, MonadCommandEffect m) => FileEntry -> m FilePath
getPath entry = case entry.stat of
  Missing -> nullPath
  Directory -> nullPath
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
    Nothing -> lookupEnvironmentVariable "DOJANG_DIFF"
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
        result <-
          runProcess
            emptyProcessRequest
              { executable = prog
              , arguments = [srcPath, dstPath]
              }
        case result of
          ProcessCompleted exitCode _ _
            | exitCode == ExitSuccess || exitCode == ExitFailure 1 -> return ()
            | otherwise -> processFailure prog $ "exit code " <> showt exitCode
          ProcessStartFailed err -> processFailure prog err
          ProcessWaitFailed err -> processFailure prog err
          ProcessIOFailed err -> processFailure prog err
          ProcessUnavailable _ ->
            processFailure prog "execution is unavailable in this mode"
 where
  processFailure prog reason = do
    cmdStyle <- pathStyleFor' StandardError
    die' externalProgramNonZeroExit $
      cmdStyle (pack prog) <> " terminated with " <> reason <> "."


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
    color <- colorFor StandardOutput
    color' <- colorFor StandardOutput
    pathStyle <- pathStyleFor StandardOutput
    putOutputLine $
      color Default Red "--- " <> pathStyle a.path <> case a.stat of
        Missing -> " (missing)"
        Directory -> " (directory)"
        _ -> ""
    putOutputLine $
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
        forM_ diffOps $ \op -> case op of
          Deletion aRange bOffset -> do
            let ( addRange
                  , delRange
                  , (headOffset, headLines)
                  , (tailOffset, tailLines)
                  ) = calcRange linesB (bOffset + 1, bOffset) linesA aRange.lrNumbers
            printRange delRange addRange
            forM_ (take headLines (drop headOffset linesB)) $ \line ->
              putOutputLine $ ' ' `cons` line
            forM_ aRange.lrContents $ \line ->
              putOutputLine $ pack $ color' Default Red ('-' : line)
            forM_ (take tailLines (drop tailOffset linesB)) $ \line ->
              putOutputLine $ ' ' `cons` line
          Addition bRange aOffset -> do
            let ( addRange
                  , delRange
                  , (headOffset, headLines)
                  , (tailOffset, tailLines)
                  ) = calcRange linesB bRange.lrNumbers linesA (aOffset + 1, aOffset)
            printRange delRange addRange
            forM_ (take headLines (drop headOffset linesB)) $ \line ->
              putOutputLine $ ' ' `cons` line
            forM_ bRange.lrContents $ \line ->
              putOutputLine $ pack $ color' Default Green ('+' : line)
            forM_ (take tailLines (drop tailOffset linesB)) $ \line ->
              putOutputLine $ ' ' `cons` line
          Change aRange bRange -> do
            let ( addRange
                  , delRange
                  , (headOffset, headLines)
                  , (tailOffset, tailLines)
                  ) = calcRange linesB bRange.lrNumbers linesA aRange.lrNumbers
            printRange delRange addRange
            forM_ (take headLines (drop headOffset linesB)) $ \line ->
              putOutputLine $ ' ' `cons` line
            forM_ aRange.lrContents $ \line ->
              putOutputLine $ pack $ color' Default Red ('-' : line)
            forM_ bRange.lrContents $ \line ->
              putOutputLine $ pack $ color' Default Green ('+' : line)
            forM_ (take tailLines (drop tailOffset linesB)) $ \line ->
              putOutputLine $ ' ' `cons` line
      _ -> putOutputLine "Binary files differ."
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
  printRange :: (Int, Int) -> (Int, Int) -> App i ()
  printRange (delOffset, delLines) (addOffset, addLines) = do
    color <- colorFor StandardOutput
    putOutputLine $
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


putOutputLine :: (MonadCommandEffect m) => Text -> m ()
putOutputLine value = writeStream OutputStandard $ value <> "\n"
