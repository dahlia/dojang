{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Safe, transport-independent repository acquisition primitives.
module Dojang.Bootstrap
  ( AcquisitionError (..)
  , ArchiveFormat (..)
  , BuiltinSource (..)
  , archiveFormatFromFilePath
  , detectBuiltinSource
  , normalizeArchiveEntryPath
  , publishStagedDirectory
  , stageBuiltinSource
  ) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as TarEntry
import Codec.Archive.Zip qualified as Zip
import Codec.Compression.GZip qualified as GZip
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, displayException, evaluate)
import Control.Monad (forM_, unless, when)
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isAlpha, isDigit, toLower)
import Data.List (dropWhileEnd, inits, isSuffixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import GHC.Generics (Generic)
import System.FilePath.Posix qualified as Posix
import System.OsPath
  ( OsPath
  , takeDirectory
  , (</>)
  )
import Text.Read (readMaybe)
import Prelude hiding (readFile, writeFile)

import Dojang.MonadFileSystem
  ( FileType (..)
  , MonadFileSystem (..)
  )


-- | Archive formats supported without an external transport.
data ArchiveFormat
  = -- | A ZIP archive.
    ZipArchive
  | -- | An uncompressed tar archive.
    TarArchive
  | -- | A gzip-compressed tar archive.
    TarGzipArchive
  deriving (Eq, Show, Generic, NFData)


-- | A local source handled by a built-in transport.
data BuiltinSource
  = -- | An existing local directory.
    DirectorySource OsPath
  | -- | An existing local archive and its detected format.
    ArchiveSource ArchiveFormat OsPath
  deriving (Eq, Show)


-- | A failure to validate or decode a built-in source.
data AcquisitionError
  = -- | The local source is neither a directory nor a regular file.
    SourceDoesNotExist FilePath
  | -- | The local file extension does not identify a supported archive.
    UnsupportedArchiveFormat FilePath
  | -- | The archive cannot be decoded.
    InvalidArchive Text
  | -- | An entry path could escape or vary across supported platforms.
    UnsafeArchiveEntry FilePath
  | -- | An entry uses links, encryption, or another unsupported type.
    UnsupportedArchiveEntry FilePath
  | -- | Two entries conflict by path or file type.
    ConflictingArchiveEntry FilePath
  deriving (Eq, Show, Generic, NFData)


data StagedEntry
  = StagedDirectory FilePath
  | StagedFile FilePath ByteString
  deriving (Eq, Show, Generic, NFData)


-- | Maps a local filename to a supported archive format.
archiveFormatFromFilePath :: FilePath -> Maybe ArchiveFormat
archiveFormatFromFilePath path
  | ".tar.gz" `isSuffixOf` lower = Just TarGzipArchive
  | ".tgz" `isSuffixOf` lower = Just TarGzipArchive
  | ".tar" `isSuffixOf` lower = Just TarArchive
  | ".zip" `isSuffixOf` lower = Just ZipArchive
  | otherwise = Nothing
 where
  lower = toLower <$> path


-- | Detects a local directory or supported archive source.
detectBuiltinSource
  :: (MonadFileSystem m)
  => OsPath
  -> m (Either AcquisitionError BuiltinSource)
detectBuiltinSource source = do
  directory <- isDirectory source
  if directory
    then return $ Right $ DirectorySource source
    else do
      regularFile <- isFile source
      sourceName <- decodePath source
      if not regularFile
        then return $ Left $ SourceDoesNotExist sourceName
        else return $ case archiveFormatFromFilePath sourceName of
          Nothing -> Left $ UnsupportedArchiveFormat sourceName
          Just format -> Right $ ArchiveSource format source


-- | Validates and normalizes an archive path without consulting the host
-- filesystem. Backslashes are rejected so the same archive cannot acquire a
-- different meaning on Windows.
normalizeArchiveEntryPath
  :: FilePath -> Either AcquisitionError FilePath
normalizeArchiveEntryPath original
  | null path = Left unsafe
  | '\0' `elem` path = Left unsafe
  | '\\' `elem` path = Left unsafe
  | ':' `elem` path = Left unsafe
  | Posix.isAbsolute path = Left unsafe
  | windowsDrive path = Left unsafe
  | any (== "..") components = Left unsafe
  | null normalizedComponents = Left unsafe
  | any windowsUnsafeComponent normalizedComponents = Left unsafe
  | otherwise = Right $ Posix.joinPath normalizedComponents
 where
  unsafe = UnsafeArchiveEntry original
  path = dropWhileEnd (== '/') original
  components = Posix.splitDirectories path
  normalizedComponents = filter (/= ".") components
  windowsDrive (letter : ':' : _) = isAlpha letter
  windowsDrive _ = False
  windowsUnsafeComponent component =
    null component
      || last component == '.'
      || last component == ' '
      || reservedWindowsName component
  reservedWindowsName component =
    let base = fmap toLower $ takeWhile (/= '.') component
    in base `elem` ["con", "prn", "aux", "nul", "clock$"]
         || base `elem` (("com" <>) . show <$> [1 :: Int .. 9])
         || base `elem` (("lpt" <>) . show <$> [1 :: Int .. 9])


-- | Copies or extracts a built-in source into a new staging directory.
--
-- Archive entries are all decoded and validated before the staging directory
-- is created, so an invalid archive cannot partially populate the filesystem.
stageBuiltinSource
  :: (MonadFileSystem m, MonadCatch m, MonadIO m)
  => BuiltinSource
  -> OsPath
  -> m (Either AcquisitionError ())
stageBuiltinSource (DirectorySource source) staging = do
  copyDirectoryTree source staging
  return $ Right ()
stageBuiltinSource (ArchiveSource format source) staging = do
  bytes <- readFile source
  decoded <-
    try $
      liftIO $
        evaluate $
          force $
            decodeArchive format bytes
  case decoded of
    Left (err :: SomeException) ->
      return $ Left $ InvalidArchive $ Text.pack $ displayException err
    Right (Left err) -> return $ Left err
    Right (Right entries) -> do
      createDirectory staging
      cleanupStagingOnError staging $
        forM_ entries $ \case
          StagedDirectory relative -> do
            encoded <- encodePath relative
            createDirectories $ staging </> encoded
          StagedFile relative contents -> do
            encoded <- encodePath relative
            let destination = staging </> encoded
            createDirectories $ takeDirectory destination
            writeFile destination contents
      return $ Right ()


-- | Publishes a fully validated staging tree. A missing destination is created
-- with an atomic directory rename. An existing empty destination keeps its
-- directory identity, which is required when it is the process's current
-- working directory.
publishStagedDirectory
  :: (MonadFileSystem m) => OsPath -> OsPath -> m ()
publishStagedDirectory staging destination = do
  destinationExists <- exists destination
  if not destinationExists
    then renameDirectory staging destination
    else do
      destinationEntries <- listDirectory destination
      unless (null destinationEntries) $
        throwError $
          userError "bootstrap destination is not empty"
      copyDirectoryContents staging destination


copyDirectoryTree
  :: (MonadFileSystem m) => OsPath -> OsPath -> m ()
copyDirectoryTree source destination = do
  entries <- listDirectoryRecursively source []
  createDirectory destination
  cleanupStagingOnError destination $
    copyDirectoryEntries source destination entries


copyDirectoryContents
  :: (MonadFileSystem m) => OsPath -> OsPath -> m ()
copyDirectoryContents source destination = do
  entries <- listDirectoryRecursively source []
  topLevelEntries <- listDirectory source
  cleanupDestinationOnError destination topLevelEntries $ do
    copyDirectoryEntries source destination entries
    removeDirectoryRecursively source


copyDirectoryEntries
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> [(FileType, OsPath)]
  -> m ()
copyDirectoryEntries source destination entries =
  forM_ entries $ \(fileType, relative) -> do
    let sourceEntry = source </> relative
        destinationEntry = destination </> relative
    case fileType of
      Directory -> createDirectory destinationEntry
      File -> copyFileWithMetadata sourceEntry destinationEntry
      Symlink -> do
        target <- readSymlinkTarget sourceEntry
        directoryLink <- isDirectory sourceEntry
        createSymbolicLink
          target
          destinationEntry
          (if directoryLink then Directory else File)


cleanupDestinationOnError
  :: (MonadFileSystem m) => OsPath -> [OsPath] -> m a -> m a
cleanupDestinationOnError destination entries action =
  action `catchError` \err -> do
    forM_ entries $ \entry ->
      removeAnyEntry (destination </> entry)
        `catchError` const (return ())
    throwError err


removeAnyEntry :: (MonadFileSystem m) => OsPath -> m ()
removeAnyEntry path = do
  symbolicLink <- isSymlink path
  directory <- isDirectory path
  present <- exists path
  if directory && not symbolicLink
    then removeDirectoryRecursively path
    else when (symbolicLink || present) $ removeFile path


cleanupStagingOnError
  :: (MonadFileSystem m) => OsPath -> m a -> m a
cleanupStagingOnError staging action =
  action `catchError` \err -> do
    removeDirectoryRecursively staging `catchError` const (return ())
    throwError err


decodeArchive
  :: ArchiveFormat -> ByteString -> Either AcquisitionError [StagedEntry]
decodeArchive format bytes = do
  entries <- case format of
    ZipArchive -> decodeZip lazyBytes
    TarArchive -> decodeTar lazyBytes
    TarGzipArchive -> decodeTar $ GZip.decompress lazyBytes
  validateEntryLayout entries
  return entries
 where
  lazyBytes = LazyByteString.fromStrict bytes


validateEntryLayout
  :: [StagedEntry] -> Either AcquisitionError ()
validateEntryLayout entries =
  case duplicatePaths <> fileAncestors of
    conflict : _ -> Left $ ConflictingArchiveEntry conflict
    [] -> Right ()
 where
  paths = entryPath <$> entries
  filePaths =
    Set.fromList
      [ path
      | StagedFile path _ <- entries
      ]
  duplicatePaths =
    Map.keys $
      Map.filter (> (1 :: Int)) $
        Map.fromListWith (+) ((\path -> (path, 1 :: Int)) <$> paths)
  fileAncestors =
    [ ancestor
    | path <- paths
    , ancestor <- properAncestors path
    , Set.member ancestor filePaths
    ]
  entryPath (StagedDirectory path) = path
  entryPath (StagedFile path _) = path
  properAncestors path =
    Posix.joinPath
      <$> take
        (length components - 1)
        (drop 1 $ inits components)
   where
    components = Posix.splitDirectories path


decodeZip
  :: LazyByteString.ByteString
  -> Either AcquisitionError [StagedEntry]
decodeZip bytes = do
  archive <-
    case Zip.toArchiveOrFail bytes of
      Left err -> Left $ InvalidArchive $ Text.pack err
      Right value -> Right value
  traverse decodeEntry $ Zip.zEntries archive
 where
  decodeEntry entry
    | Zip.isEncryptedEntry entry =
        Left $ UnsupportedArchiveEntry $ Zip.eRelativePath entry
    | Zip.isEntrySymbolicLink entry =
        Left $ UnsupportedArchiveEntry $ Zip.eRelativePath entry
    | otherwise = do
        relative <- normalizeArchiveEntryPath $ Zip.eRelativePath entry
        if "/" `isSuffixOf` Zip.eRelativePath entry
          then Right $ StagedDirectory relative
          else
            Right $
              StagedFile relative $
                LazyByteString.toStrict $
                  Zip.fromEntry entry


decodeTar
  :: LazyByteString.ByteString
  -> Either AcquisitionError [StagedEntry]
decodeTar = go Nothing . Tar.decodeLongNames . Tar.read
 where
  go _ Tar.Done = Right []
  go _ (Tar.Fail err) = Left $ InvalidArchive $ Text.pack $ show err
  go pendingPath (Tar.Next entry remaining) =
    case Tar.entryContent entry of
      Tar.OtherEntryType 'g' _ _ -> go pendingPath remaining
      Tar.OtherEntryType 'x' contents _ -> do
        headers <- parsePaxHeaders contents
        let path = Text.unpack <$> Map.lookup "path" headers
        go (path <|> pendingPath) remaining
      _ -> do
        decoded <-
          decodeEntry
            (fromMaybe (TarEntry.entryTarPath entry) pendingPath)
            entry
        (decoded :) <$> go Nothing remaining
  decodeEntry archivePath entry = do
    relative <- normalizeArchiveEntryPath archivePath
    case Tar.entryContent entry of
      Tar.NormalFile contents _ ->
        Right $ StagedFile relative $ LazyByteString.toStrict contents
      Tar.Directory -> Right $ StagedDirectory relative
      _ -> Left $ UnsupportedArchiveEntry archivePath


parsePaxHeaders
  :: LazyByteString.ByteString
  -> Either AcquisitionError (Map.Map Text Text)
parsePaxHeaders = go Map.empty . LazyByteString.toStrict
 where
  go headers bytes
    | ByteString.null bytes = Right headers
    | otherwise = do
        let (digits, afterDigits) = ByteString.Char8.span isDigit bytes
        recordLength <-
          case readMaybe $ ByteString.Char8.unpack digits of
            Nothing -> malformed
            Just value -> Right value
        if
          | ByteString.null digits -> malformed
          | ByteString.null afterDigits -> malformed
          | ByteString.head afterDigits /= 0x20 -> malformed
          | recordLength <= ByteString.length digits + 2 -> malformed
          | recordLength > ByteString.length bytes -> malformed
          | otherwise -> do
              let (record, remaining) =
                    ByteString.splitAt recordLength bytes
                  bodyWithNewline =
                    ByteString.drop (ByteString.length digits + 1) record
              body <- case ByteString.unsnoc bodyWithNewline of
                Just (value, 0x0a) -> Right value
                _ -> malformed
              let (keyBytes, valueWithEquals) =
                    ByteString.Char8.break (== '=') body
              valueBytes <- case ByteString.uncons valueWithEquals of
                Just (0x3d, value) -> Right value
                _ -> malformed
              key <- decode keyBytes
              value <- decode valueBytes
              go (Map.insert key value headers) remaining
  decode bytes =
    case decodeUtf8' bytes of
      Left _ -> malformed
      Right value -> Right value
  malformed = Left $ InvalidArchive "Malformed pax extended header."
