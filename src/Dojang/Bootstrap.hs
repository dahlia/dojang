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
  , StagedMetadata
  , archiveFormatFromFilePath
  , detectBuiltinSource
  , emptyStagedMetadata
  , normalizeArchiveEntryPath
  , publishStagedDirectory
  , publishStagedDirectoryWithMetadata
  , stageBuiltinSource
  , stageBuiltinSourceWithMetadata
  ) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as TarEntry
import Codec.Archive.Zip qualified as Zip
import Codec.Compression.GZip qualified as GZip
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, displayException, evaluate)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isAlpha, isDigit, toLower)
import Data.List (dropWhileEnd, inits, isSuffixOf, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Normalize qualified as Unicode
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
import Dojang.Types.RouteMetadata (PortableMode (..))


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
  = StagedDirectory FilePath (Maybe Word)
  | StagedFile FilePath ByteString (Maybe Word)
  deriving (Eq, Show, Generic, NFData)


data StagedMode = StagedMode FilePath FileType Word
  deriving (Eq, Show, Generic)


-- | Permission metadata retained until a staged source is published.
newtype StagedMetadata = StagedMetadata [StagedMode]
  deriving (Eq, Show, Generic)


-- | Empty metadata for sources whose permissions already live in staging.
emptyStagedMetadata :: StagedMetadata
emptyStagedMetadata = StagedMetadata []


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
stageBuiltinSource source staging =
  fmap (fmap $ const ()) $ stageBuiltinSourceWithMetadata source staging


-- | Copies or extracts a built-in source and retains archive permissions until
-- publication, so staging stays readable and removable during validation.
stageBuiltinSourceWithMetadata
  :: (MonadFileSystem m, MonadCatch m, MonadIO m)
  => BuiltinSource
  -> OsPath
  -> m (Either AcquisitionError StagedMetadata)
stageBuiltinSourceWithMetadata (DirectorySource source) staging = do
  metadata <- copyDirectoryTree source staging
  return $ Right metadata
stageBuiltinSourceWithMetadata (ArchiveSource format source) staging = do
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
      cleanupStagingOnError staging $ do
        forM_ entries $ \case
          StagedDirectory relative _ -> do
            encoded <- encodePath relative
            createDirectories $ staging </> encoded
          StagedFile relative contents _ -> do
            encoded <- encodePath relative
            let destination = staging </> encoded
            createDirectories $ takeDirectory destination
            writeFile destination contents
      return $ Right $ metadataFromEntries entries


-- | Publishes a fully validated staging tree. A missing destination is created
-- with an atomic directory rename. An existing empty destination keeps its
-- directory identity, which is required when it is the process's current
-- working directory.
publishStagedDirectory
  :: (MonadFileSystem m) => OsPath -> OsPath -> m ()
publishStagedDirectory staging destination =
  void $
    publishStagedDirectoryWithMetadata
      emptyStagedMetadata
      staging
      destination


-- | Publishes a fully validated staging tree and applies retained permissions.
--
-- A missing destination is still published with one atomic rename.  An
-- existing empty destination keeps its identity while receiving the same file
-- and directory modes as the staging tree, with retained archive modes taking
-- precedence.  The returned paths identify entries whose stored permissions
-- the destination filesystem could not represent; their contents are still
-- published.
publishStagedDirectoryWithMetadata
  :: (MonadFileSystem m)
  => StagedMetadata
  -> OsPath
  -> OsPath
  -> m [FilePath]
publishStagedDirectoryWithMetadata metadata staging destination = do
  destinationExists <- exists destination
  if not destinationExists
    then do
      modeFailures <- applyStagedMetadata staging metadata
      renameDirectory staging destination
        `catchError` cleanupRestricted staging metadata
      return modeFailures
    else do
      destinationEntries <- listDirectory destination
      unless (null destinationEntries) $
        throwError $
          userError "bootstrap destination is not empty"
      copyDirectoryContents metadata staging destination


copyDirectoryTree
  :: (MonadFileSystem m) => OsPath -> OsPath -> m StagedMetadata
copyDirectoryTree source destination = do
  entries <- listDirectoryRecursively source []
  metadata <- captureStagedMetadata source entries
  createDirectory destination
  cleanupStagingOnError destination $
    copyDirectoryEntries source destination entries
  return metadata


copyDirectoryContents
  :: (MonadFileSystem m)
  => StagedMetadata
  -> OsPath
  -> OsPath
  -> m [FilePath]
copyDirectoryContents retainedMetadata source destination = do
  entries <- listDirectoryRecursively source []
  sourceMetadata <- captureStagedMetadata source entries
  let publishedMetadata =
        overlayStagedMetadata retainedMetadata sourceMetadata
  topLevelEntries <- listDirectory source
  cleanupDestinationOnError
    destination
    topLevelEntries
    publishedMetadata
    $ do
      copyDirectoryEntries source destination entries
      modeFailures <- applyStagedMetadata destination publishedMetadata
      widenStagedMetadata source sourceMetadata
      removeDirectoryRecursively source
      return modeFailures


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
      File -> copyFile sourceEntry destinationEntry
      Symlink -> do
        target <- readSymlinkTarget sourceEntry
        directoryLink <- isDirectory sourceEntry
        createSymbolicLink
          target
          destinationEntry
          (if directoryLink then Directory else File)


cleanupDestinationOnError
  :: (MonadFileSystem m)
  => OsPath
  -> [OsPath]
  -> StagedMetadata
  -> m a
  -> m a
cleanupDestinationOnError destination entries metadata action =
  action `catchError` \err -> do
    widenStagedMetadata destination metadata
      `catchError` const (return ())
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
    Map.fromList
      [ (archivePathKey path, path)
      | StagedFile path _ _ <- entries
      ]
  duplicatePaths =
    [ path
    | group <- Map.elems pathGroups
    , length group > 1
    , path : _ <- [group]
    ]
  pathGroups =
    Map.fromListWith (<>) $
      fmap (\path -> (archivePathKey path, [path])) paths
  fileAncestors =
    [ filePath
    | path <- paths
    , ancestor <- properAncestors path
    , Just filePath <- [Map.lookup (archivePathKey ancestor) filePaths]
    ]
  properAncestors path =
    Posix.joinPath
      <$> take
        (length components - 1)
        (drop 1 $ inits components)
   where
    components = Posix.splitDirectories path


archivePathKey :: FilePath -> Text
archivePathKey =
  Unicode.normalize Unicode.NFC
    . Text.toCaseFold
    . Unicode.normalize Unicode.NFC
    . Text.pack


entryPath :: StagedEntry -> FilePath
entryPath (StagedDirectory path _) = path
entryPath (StagedFile path _ _) = path


entryMode :: StagedEntry -> Maybe Word
entryMode (StagedDirectory _ mode) = mode
entryMode (StagedFile _ _ mode) = mode


metadataFromEntries :: [StagedEntry] -> StagedMetadata
metadataFromEntries entries =
  StagedMetadata
    [ StagedMode (entryPath entry) (entryType entry) mode
    | entry <- entries
    , Just mode <- [entryMode entry]
    ]


entryType :: StagedEntry -> FileType
entryType (StagedDirectory _ _) = Directory
entryType (StagedFile _ _ _) = File


captureStagedMetadata
  :: (MonadFileSystem m)
  => OsPath
  -> [(FileType, OsPath)]
  -> m StagedMetadata
captureStagedMetadata source entries =
  StagedMetadata . concat
    <$> traverse capture entries
 where
  capture (Symlink, _) = return []
  capture (fileType, relative) = do
    PortableMode posixBits writable <-
      getPortableMode $ source </> relative
    path <- decodePath relative
    let mode = fromMaybe (if writable then 0o200 else 0) posixBits
    return [StagedMode path fileType mode]


overlayStagedMetadata
  :: StagedMetadata -> StagedMetadata -> StagedMetadata
overlayStagedMetadata (StagedMetadata retained) (StagedMetadata captured) =
  StagedMetadata $
    retained
      <> filter
        ( \(StagedMode path _ _) ->
            Map.notMember (portableMetadataPathKey path) retainedPaths
        )
        captured
 where
  retainedPaths =
    Map.fromList
      [ (portableMetadataPathKey path, ())
      | StagedMode path _ _ <- retained
      ]


portableMetadataPathKey :: FilePath -> Text
portableMetadataPathKey =
  archivePathKey
    . fmap (\character -> if character == '\\' then '/' else character)


applyStagedMetadata
  :: (MonadFileSystem m) => OsPath -> StagedMetadata -> m [FilePath]
applyStagedMetadata root (StagedMetadata modes) =
  catMaybes
    <$> forM
      (sortOn (Down . stagedModeDepth) modes)
      ( \(StagedMode path _ mode) -> do
          encoded <- encodePath path
          (setPortableMode (root </> encoded) mode >> return Nothing)
            `catchError` const (return $ Just path)
      )


widenStagedMetadata
  :: (MonadFileSystem m) => OsPath -> StagedMetadata -> m ()
widenStagedMetadata root (StagedMetadata modes) =
  forM_ (sortOn stagedModeDepth modes) $ \(StagedMode path fileType mode) -> do
    encoded <- encodePath path
    setPortableMode
      (root </> encoded)
      (mode .|. ownerAccess fileType)
      `catchError` const (return ())
 where
  ownerAccess Directory = 0o700
  ownerAccess _ = 0o600


stagedModeDepth :: StagedMode -> Int
stagedModeDepth (StagedMode path _ _) =
  length $ Posix.splitDirectories path


cleanupRestricted
  :: (MonadFileSystem m)
  => OsPath
  -> StagedMetadata
  -> IOError
  -> m a
cleanupRestricted root metadata err = do
  widenStagedMetadata root metadata `catchError` const (return ())
  throwError err


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
          then Right $ StagedDirectory relative $ zipEntryMode entry
          else
            Right $
              StagedFile
                relative
                (LazyByteString.toStrict $ Zip.fromEntry entry)
                (zipEntryMode entry)


zipEntryMode :: Zip.Entry -> Maybe Word
zipEntryMode entry
  | creatorSystem `elem` [3, 19]
      && permissionBits /= 0 =
      Just $
        fromIntegral permissionBits
  | otherwise = Nothing
 where
  creatorSystem = Zip.eVersionMadeBy entry `shiftR` 8
  permissionBits =
    (Zip.eExternalFileAttributes entry `shiftR` 16) .&. 0o777


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
        Right $
          StagedFile
            relative
            (LazyByteString.toStrict contents)
            (Just $ archivePermissions entry)
      Tar.Directory ->
        Right $ StagedDirectory relative $ Just $ archivePermissions entry
      _ -> Left $ UnsupportedArchiveEntry archivePath
  archivePermissions entry =
    fromIntegral (TarEntry.entryPermissions entry) .&. 0o777


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
