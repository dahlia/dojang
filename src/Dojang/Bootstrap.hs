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
  , publishStagedDirectoryWithMetadataChecked
  , stageBuiltinSource
  , stageBuiltinSourceWithMetadata
  ) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as TarEntry
import Codec.Archive.Zip qualified as Zip
import Codec.Compression.GZip qualified as GZip
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData, force)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , displayException
  , evaluate
  , fromException
  , throwIO
  )
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Catch
  ( MonadCatch
  , MonadMask
  , catch
  , mask
  , throwM
  , try
  )
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isAlpha, isDigit, ord, toLower)
import Data.List (dropWhileEnd, inits, isSuffixOf, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (Down))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Normalize qualified as Unicode
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import System.FilePath.Posix qualified as Posix
import System.IO.Error (ioeGetFileName, isDoesNotExistError)
import System.OsPath
  ( OsPath
  , takeDirectory
  , (</>)
  )
import Text.Read (readMaybe)
import Prelude hiding (readFile, writeFile)

import Dojang.MonadFileSystem
  ( BoundedFileRead (..)
  , FileIdentity
  , FileModeSnapshot (..)
  , FileSnapshot
  , FileType (..)
  , MonadFileSystem (..)
  , fileSnapshotIdentity
  )
import Dojang.Types.RouteMetadata
  ( PortableMode (..)
  , portableModeFromBits
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
  | -- | A directory source contains a non-regular, non-link entry.
    UnsupportedSourceEntry FilePath
  | -- | Two directory-source entries collide by portable path identity.
    ConflictingSourceEntry FilePath
  | -- | A directory source changed while it was being validated.
    SourceChangedDuringAcquisition FilePath
  | -- | Two entries conflict by path or file type.
    ConflictingArchiveEntry FilePath
  | -- | The archive exceeds the bounded acquisition resource limits.
    ArchiveResourceLimitExceeded
  | -- | The archive changed while its bytes were being read.
    ArchiveChangedDuringAcquisition
  deriving (Eq, Show, Generic, NFData)


data StagedEntry
  = StagedDirectory FilePath (Maybe Word)
  | StagedFile FilePath ByteString (Maybe Word)
  deriving (Eq, Show, Generic, NFData)


data StagedMode = StagedMode FilePath FileType PortableMode
  deriving (Eq, Show, Generic)


data DirectoryEntrySnapshot
  = DirectoryEntryIdentity FileIdentity (Maybe PortableMode)
  | DirectoryFileSnapshot FileSnapshot PortableMode


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
      regularFile <- resolvesToRegularFile source
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
  | length original > maximumArchivePathCharacters =
      Left ArchiveResourceLimitExceeded
  | null path = Left unsafe
  | '\0' `elem` path = Left unsafe
  | '\\' `elem` path = Left unsafe
  | ':' `elem` path = Left unsafe
  | Posix.isAbsolute path = Left unsafe
  | windowsDrive path = Left unsafe
  | any (== "..") components = Left unsafe
  | null normalizedComponents = Left unsafe
  | length normalizedComponents > maximumArchivePathDepth =
      Left ArchiveResourceLimitExceeded
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
      || any windowsForbiddenCharacter component
      || reservedWindowsName component
  windowsForbiddenCharacter character =
    ord character <= 31 || character `elem` ("<>:\"/\\|?*" :: String)
  reservedWindowsName component =
    let base = fmap toLower $ takeWhile (/= '.') component
        numberedDevices =
          [prefix <> [suffix] | prefix <- ["com", "lpt"], suffix <- deviceDigits]
    in base
         `elem` ( ["con", "prn", "aux", "nul", "clock$", "conin$", "conout$"]
                    <> numberedDevices
                )
  deviceDigits = ['1' .. '9'] <> ['\185', '\178', '\179']


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
  copyDirectoryTree source staging
stageBuiltinSourceWithMetadata (ArchiveSource format source) staging = do
  sourceName <- decodePath source
  readRegularFileBounded maximumArchiveBytes source >>= \case
    NotRegularFile -> return $ Left $ SourceDoesNotExist sourceName
    FileSizeLimitExceeded ->
      return $ Left ArchiveResourceLimitExceeded
    FileChangedDuringRead ->
      return $ Left ArchiveChangedDuringAcquisition
    BoundedFileContents bytes -> do
      decoded <-
        try $
          liftIO $
            evaluate $
              force $
                decodeArchive format bytes
      case decoded of
        Left (err :: SomeException)
          | Just (_ :: SomeAsyncException) <- fromException err ->
              liftIO $ throwIO err
          | otherwise ->
              return $ Left $ InvalidArchive $ Text.pack $ displayException err
        Right (Left err) -> return $ Left err
        Right (Right entries) -> do
          stagingIdentity <- createOwnedPrivateDirectory staging
          cleanupStagingOnError staging stagingIdentity $ do
            forM_ entries $ \case
              StagedDirectory "" _ -> return ()
              StagedDirectory relative _ -> do
                encoded <- encodePath relative
                createDirectories $ staging </> encoded
              StagedFile relative contents _ -> do
                encoded <- encodePath relative
                let destination = staging </> encoded
                createDirectories $ takeDirectory destination
                writeFile destination contents
          return $ Right $ metadataFromEntries entries


-- | Publishes a fully validated staging tree to a missing destination with an
-- atomic no-replace directory rename.
publishStagedDirectory
  :: (MonadFileSystem m, MonadMask m) => OsPath -> OsPath -> m ()
publishStagedDirectory staging destination =
  void $
    publishStagedDirectoryWithMetadata
      emptyStagedMetadata
      staging
      destination


-- | Publishes a fully validated staging tree and applies retained permissions.
--
-- The destination must not exist. File and directory modes come from the
-- staging tree, with retained archive modes taking precedence. The returned
-- paths identify entries whose stored permissions the destination filesystem
-- could not represent; their contents are still published.
publishStagedDirectoryWithMetadata
  :: (MonadFileSystem m, MonadMask m)
  => StagedMetadata
  -> OsPath
  -> OsPath
  -> m [FilePath]
publishStagedDirectoryWithMetadata metadata staging destination =
  do
    result <-
      publishStagedDirectoryWithMetadataChecked
        metadata
        staging
        destination
        (return (Right () :: Either Void ()))
    return $ either absurd id result


-- | Publishes a staged tree after applying retained permissions and running a
-- final private validation.
--
-- The validation action runs after stored permissions are applied but before
-- the atomic no-replace rename.  A 'Left' result or a raised exception widens
-- restrictive staging permissions for cleanup and leaves the destination
-- absent.
publishStagedDirectoryWithMetadataChecked
  :: (MonadFileSystem m, MonadMask m)
  => StagedMetadata
  -- ^ Permissions retained while staging.
  -> OsPath
  -- ^ Private staging directory.
  -> OsPath
  -- ^ Missing publication destination.
  -> m (Either validationError ())
  -- ^ Final validation result under the retained permissions.
  -> m (Either validationError [FilePath])
  -- ^ A validation rejection, or entries whose exact permissions could not be
  -- restored.
publishStagedDirectoryWithMetadataChecked
  metadata
  staging
  destination
  validate = do
    destinationSymlink <- isSymlink destination
    when destinationSymlink $
      throwError $
        userError "bootstrap destination is a symbolic link"
    destinationExists <- exists destination
    when destinationExists $
      throwError $
        userError "bootstrap destination already exists"
    protectRestrictedStaging staging metadata $ do
      modeFailures <- applyStagedMetadata staging metadata
      validation <- validate
      case validation of
        Left err -> do
          widenDirectoryForCleanup staging
            `catchError` const (return ())
          widenStagedMetadata staging metadata
            `catchError` const (return ())
          return $ Left err
        Right () -> do
          renameDirectory staging destination
          return $ Right modeFailures


resolvesToRegularFile :: (MonadFileSystem m) => OsPath -> m Bool
resolvesToRegularFile path = do
  regularFile <- isRegularFile path
  if regularFile
    then return True
    else do
      symbolicLink <- isSymlink path
      if not symbolicLink
        then return False
        else do
          resolved <- canonicalizePath path
          isRegularFile resolved


copyDirectoryTree
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> m (Either AcquisitionError StagedMetadata)
copyDirectoryTree source destination = do
  entriesResult <- listDirectorySourceSnapshot source
  case entriesResult of
    Left path -> return $ Left $ SourceChangedDuringAcquisition path
    Right entries -> do
      layout <- validateDirectoryEntryLayout entries
      case layout of
        Left err -> return $ Left err
        Right () -> do
          validation <- identifyDirectoryEntries source entries
          case validation of
            Left err -> return $ Left err
            Right
              ( sourceIdentity
                , resolvedSource
                , rootModeSnapshot
                , entryIdentities
                ) -> do
                metadata <-
                  captureStagedMetadata
                    rootModeSnapshot
                    entryIdentities
                    entries
                stagingIdentity <- createOwnedPrivateDirectory destination
                cleanupStagingOnError destination stagingIdentity $ do
                  copyDirectoryEntries
                    source
                    destination
                    entryIdentities
                    entries
                  verifyDirectorySource
                    source
                    sourceIdentity
                    resolvedSource
                    rootModeSnapshot
                    entryIdentities
                    entries
                return $ Right metadata


listDirectorySourceSnapshot
  :: (MonadFileSystem m)
  => OsPath
  -> m (Either FilePath [(FileType, OsPath)])
listDirectorySourceSnapshot source =
  (Right <$> listDirectoryRecursivelyStrict source [])
    `catchError` \err ->
      if isDoesNotExistError err
        then do
          path <- maybe (decodePath source) return $ ioeGetFileName err
          return $ Left path
        else throwError err


validateDirectoryEntryLayout
  :: (MonadFileSystem m)
  => [(FileType, OsPath)]
  -> m (Either AcquisitionError ())
validateDirectoryEntryLayout entries = do
  decoded <- traverse (decodePath . snd) entries
  let groups =
        Map.fromListWith (<>) $
          fmap (\path -> (archivePathKey path, [path])) decoded
  return $
    case [ path
         | group <- Map.elems groups
         , length group > 1
         , path : _ <- [group]
         ] of
      conflict : _ -> Left $ ConflictingSourceEntry conflict
      [] -> Right ()


identifyDirectoryEntries
  :: (MonadFileSystem m)
  => OsPath
  -> [(FileType, OsPath)]
  -> m
       ( Either
           AcquisitionError
           ( FileIdentity
           , OsPath
           , FileModeSnapshot
           , Map.Map OsPath DirectoryEntrySnapshot
           )
       )
identifyDirectoryEntries source entries = do
  sourceIdentity <- getFileIdentity source
  case sourceIdentity of
    Nothing -> do
      path <- decodePath source
      return $ Left $ UnsupportedSourceEntry path
    Just identity -> do
      resolvedSource <- canonicalizePath source
      rootModeSnapshot <- getFileModeSnapshot resolvedSource
      supportedRoot <- isDirectory resolvedSource
      identified <- go Map.empty entries
      sourceIdentityAfter <- getFileIdentity source
      rootModeSnapshotAfter <- getFileModeSnapshot resolvedSource
      case (rootModeSnapshot, identified) of
        (_, Left err) -> return $ Left err
        (Nothing, _) -> unsupported source
        (Just _, _)
          | not supportedRoot -> unsupported source
        (Just rootSnapshot, Right entryIdentities)
          | sourceIdentityAfter == Just identity
              && rootModeSnapshotAfter == Just rootSnapshot ->
              return $
                Right
                  ( identity
                  , resolvedSource
                  , rootSnapshot
                  , entryIdentities
                  )
        _ -> sourceChanged source
 where
  go identities [] = return $ Right identities
  go identities ((fileType, relative) : remaining) = do
    let path = source </> relative
    identified <- case fileType of
      File -> identifyFile path
      Directory -> identifyDirectory path
      Symlink ->
        identifyEntry path (isSymlink path) $
          flip DirectoryEntryIdentity Nothing
    case identified of
      Just snapshot ->
        go (Map.insert relative snapshot identities) remaining
      Nothing -> unsupported relative

  identifyFile path = do
    fileSnapshot <- getFileSnapshot path
    modeSnapshot <- getFileModeSnapshot path
    return $ case (fileSnapshot, modeSnapshot) of
      (Just snapshot, Just (FileModeSnapshot identity mode))
        | fileSnapshotIdentity snapshot == identity ->
            Just $ DirectoryFileSnapshot snapshot mode
      _ -> Nothing

  identifyDirectory path = do
    modeSnapshotBefore <- getFileModeSnapshot path
    supported <-
      (&&) <$> isDirectory path <*> (not <$> isSymlink path)
    modeSnapshotAfter <- getFileModeSnapshot path
    return $ case modeSnapshotBefore of
      Just (FileModeSnapshot identity mode)
        | supported && modeSnapshotAfter == modeSnapshotBefore ->
            Just $ DirectoryEntryIdentity identity $ Just mode
      _ -> Nothing

  identifyEntry path supported makeSnapshot = do
    identityBefore <- getFileIdentity path
    supported' <- supported
    identityAfter <- getFileIdentity path
    return $ case identityBefore of
      Just identity
        | supported' && identityAfter == Just identity ->
            Just $ makeSnapshot identity
      _ -> Nothing

  unsupported relative = do
    decoded <- decodePath relative
    return $ Left $ UnsupportedSourceEntry decoded

  sourceChanged path = do
    decoded <- decodePath path
    return $ Left $ SourceChangedDuringAcquisition decoded


copyDirectoryEntries
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> Map.Map OsPath DirectoryEntrySnapshot
  -> [(FileType, OsPath)]
  -> m ()
copyDirectoryEntries source destination identities entries =
  forM_ entries $ \(fileType, relative) -> do
    let sourceEntry = source </> relative
        destinationEntry = destination </> relative
    expectedSnapshot <- expectedEntrySnapshot identities relative
    case fileType of
      Directory -> do
        expectedIdentity <-
          expectedDirectoryEntryIdentity relative expectedSnapshot
        verifyEntryIdentity source relative expectedIdentity
        createDirectory destinationEntry
      File -> do
        expectedFileSnapshot <-
          expectedDirectoryFileSnapshot relative expectedSnapshot
        copied <-
          copyRegularFileWithSnapshot
            expectedFileSnapshot
            sourceEntry
            destinationEntry
        unless copied $ do
          throwSourceEntryChanged relative
      Symlink -> do
        expectedIdentity <-
          expectedDirectoryEntryIdentity relative expectedSnapshot
        verifyEntryIdentity source relative expectedIdentity
        target <- readSymlinkTarget sourceEntry
        linkType <- getSymbolicLinkType sourceEntry
        verifyEntryIdentity source relative expectedIdentity
        createSymbolicLink
          target
          destinationEntry
          linkType


verifyDirectorySource
  :: (MonadFileSystem m)
  => OsPath
  -> FileIdentity
  -> OsPath
  -> FileModeSnapshot
  -> Map.Map OsPath DirectoryEntrySnapshot
  -> [(FileType, OsPath)]
  -> m ()
verifyDirectorySource
  source
  sourceIdentity
  resolvedSource
  rootModeSnapshot
  identities
  entries = do
    verifyPathIdentity source source sourceIdentity
    currentRootModeSnapshot <- getFileModeSnapshot resolvedSource
    unless (currentRootModeSnapshot == Just rootModeSnapshot) $
      throwSourceEntryChanged source
    currentEntriesResult <- listDirectorySourceSnapshot source
    currentEntries <- case currentEntriesResult of
      Left path -> throwSourceEntryChangedPath path
      Right value -> return value
    let expectedMembership = directoryMembership entries
        currentMembership = directoryMembership currentEntries
        allPaths =
          Map.keysSet expectedMembership
            `Set.union` Map.keysSet currentMembership
    case [ relative
         | relative <- Set.toAscList allPaths
         , Map.lookup relative expectedMembership
             /= Map.lookup relative currentMembership
         ] of
      changed : _ -> throwSourceEntryChanged changed
      [] -> return ()
    forM_ entries $ \(_, relative) -> do
      expectedSnapshot <- expectedEntrySnapshot identities relative
      verifyEntrySnapshot source relative expectedSnapshot
    verifyPathIdentity source source sourceIdentity
    finalRootModeSnapshot <- getFileModeSnapshot resolvedSource
    unless (finalRootModeSnapshot == Just rootModeSnapshot) $
      throwSourceEntryChanged source


directoryMembership :: [(FileType, OsPath)] -> Map.Map OsPath FileType
directoryMembership =
  Map.fromList . fmap (\(fileType, relative) -> (relative, fileType))


verifyEntrySnapshot
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> DirectoryEntrySnapshot
  -> m ()
verifyEntrySnapshot source relative expectedSnapshot =
  case expectedSnapshot of
    DirectoryEntryIdentity expectedIdentity Nothing ->
      verifyEntryIdentity source relative expectedIdentity
    DirectoryEntryIdentity expectedIdentity (Just expectedMode) -> do
      actualSnapshot <- getFileModeSnapshot $ source </> relative
      unless
        ( actualSnapshot
            == Just (FileModeSnapshot expectedIdentity expectedMode)
        )
        $ throwSourceEntryChanged relative
    DirectoryFileSnapshot expectedFileSnapshot _ -> do
      actualSnapshot <- getFileSnapshot $ source </> relative
      unless (actualSnapshot == Just expectedFileSnapshot) $
        throwSourceEntryChanged relative


verifyEntryIdentity
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> FileIdentity
  -> m ()
verifyEntryIdentity source relative =
  verifyPathIdentity relative $ source </> relative


verifyPathIdentity
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> FileIdentity
  -> m ()
verifyPathIdentity displayPath path expectedIdentity = do
  actualIdentity <- getFileIdentity path
  unless (actualIdentity == Just expectedIdentity) $
    throwSourceEntryChanged displayPath


throwSourceEntryChanged :: (MonadFileSystem m) => OsPath -> m a
throwSourceEntryChanged path = do
  decoded <- decodePath path
  throwSourceEntryChangedPath decoded


throwSourceEntryChangedPath
  :: (MonadFileSystem m) => FilePath -> m a
throwSourceEntryChangedPath path =
  throwError $
    userError $
      "bootstrap source entry changed during acquisition: " <> path


expectedEntrySnapshot
  :: (MonadFileSystem m)
  => Map.Map OsPath DirectoryEntrySnapshot
  -> OsPath
  -> m DirectoryEntrySnapshot
expectedEntrySnapshot identities relative =
  case Map.lookup relative identities of
    Just snapshot -> return snapshot
    Nothing -> do
      decoded <- decodePath relative
      throwError $
        userError $
          "filesystem cannot identify bootstrap source entry: " <> decoded


expectedDirectoryEntryIdentity
  :: (MonadFileSystem m)
  => OsPath
  -> DirectoryEntrySnapshot
  -> m FileIdentity
expectedDirectoryEntryIdentity _ (DirectoryEntryIdentity identity _) =
  return identity
expectedDirectoryEntryIdentity relative _ =
  throwSourceEntryTypeChanged relative


expectedDirectoryFileSnapshot
  :: (MonadFileSystem m)
  => OsPath
  -> DirectoryEntrySnapshot
  -> m FileSnapshot
expectedDirectoryFileSnapshot _ (DirectoryFileSnapshot snapshot _) =
  return snapshot
expectedDirectoryFileSnapshot relative _ =
  throwSourceEntryTypeChanged relative


throwSourceEntryTypeChanged :: (MonadFileSystem m) => OsPath -> m a
throwSourceEntryTypeChanged path = do
  decoded <- decodePath path
  throwError $
    userError $
      "bootstrap source entry type changed during acquisition: " <> decoded


cleanupStagingOnError
  :: (MonadFileSystem m) => OsPath -> FileIdentity -> m a -> m a
cleanupStagingOnError staging identity action =
  action `catchError` \err -> do
    cleanupResult <-
      (removeDirectoryRecursivelyIfIdentity staging identity >> return Nothing)
        `catchError` (return . Just)
    case cleanupResult of
      Nothing -> throwError err
      Just cleanupError ->
        throwError $
          userError $
            displayException err
              <> "; additionally, staging cleanup failed: "
              <> displayException cleanupError


createOwnedPrivateDirectory
  :: (MonadFileSystem m) => OsPath -> m FileIdentity
createOwnedPrivateDirectory path = do
  createPrivateDirectory path
  identity <- getFileIdentity path
  case identity of
    Just value -> return value
    Nothing -> do
      decoded <- decodePath path
      -- A filesystem-backed interpreter returns 'Nothing' only when the path
      -- disappeared.  Do not issue another pathname-based removal here: an
      -- unidentifiable replacement must be preserved.
      throwError $
        userError $
          "filesystem cannot identify private staging directory: " <> decoded


decodeArchive
  :: ArchiveFormat -> ByteString -> Either AcquisitionError [StagedEntry]
decodeArchive format bytes = do
  entries <- case format of
    ZipArchive -> decodeZip lazyBytes
    TarArchive -> decodeTar lazyBytes
    TarGzipArchive -> do
      decompressed <-
        boundedLazyBytes maximumDecodedTarBytes $
          GZip.decompress lazyBytes
      decodeTar decompressed
  validateEntryLayout entries
  return entries
 where
  lazyBytes = LazyByteString.fromStrict bytes


maximumArchiveBytes :: Int
maximumArchiveBytes = 16 * 1024 * 1024


maximumExpandedArchiveBytes :: Integer
maximumExpandedArchiveBytes = 64 * 1024 * 1024


maximumArchiveEntries :: Int
maximumArchiveEntries = 10000


maximumArchivePathCharacters :: Int
maximumArchivePathCharacters = 4096


maximumArchivePathDepth :: Int
maximumArchivePathDepth = 256


maximumDecodedTarBytes :: Int
maximumDecodedTarBytes =
  fromIntegral maximumExpandedArchiveBytes
    + maximumArchiveEntries * 1024
    + 1024


boundedLazyBytes
  :: Int
  -> LazyByteString.ByteString
  -> Either AcquisitionError LazyByteString.ByteString
boundedLazyBytes limit bytes =
  let bounded = LazyByteString.take (fromIntegral limit + 1) bytes
  in if LazyByteString.length bounded > fromIntegral limit
       then Left ArchiveResourceLimitExceeded
       else Right bounded


validateEntryLayout
  :: [StagedEntry] -> Either AcquisitionError ()
validateEntryLayout entries =
  case duplicatePaths <> directoryAliases <> fileAncestors of
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
  directoryAliases =
    [ path
    | group <- Map.elems directoryGroups
    , let spellings = Set.fromList group
    , Set.size spellings > 1
    , path : _ <- [Set.toAscList spellings]
    ]
  directoryGroups =
    Map.fromListWith (<>) $
      [ (archivePathKey path, [path])
      | entry <- entries
      , path <- entryDirectoryPaths entry
      ]
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
  entryDirectoryPaths (StagedDirectory path _) =
    path : properAncestors path
  entryDirectoryPaths (StagedFile path _ _) =
    properAncestors path


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
    , Just bits <- [entryMode entry]
    , let mode = portableModeFromBits bits
    ]


entryType :: StagedEntry -> FileType
entryType (StagedDirectory _ _) = Directory
entryType (StagedFile _ _ _) = File


captureStagedMetadata
  :: (MonadFileSystem m)
  => FileModeSnapshot
  -> Map.Map OsPath DirectoryEntrySnapshot
  -> [(FileType, OsPath)]
  -> m StagedMetadata
captureStagedMetadata
  (FileModeSnapshot _ rootMode)
  identities
  entries = do
    descendants <- concat <$> traverse capture entries
    return $
      StagedMetadata $
        StagedMode "" Directory rootMode : descendants
   where
    capture (Symlink, _) = return []
    capture (fileType, relative) = do
      snapshot <- expectedEntrySnapshot identities relative
      mode <- case snapshot of
        DirectoryEntryIdentity _ (Just value) -> return value
        DirectoryFileSnapshot _ value -> return value
        _ -> throwSourceEntryTypeChanged relative
      path <- decodePath relative
      return [StagedMode path fileType mode]


applyStagedMetadata
  :: (MonadFileSystem m) => OsPath -> StagedMetadata -> m [FilePath]
applyStagedMetadata root (StagedMetadata modes) =
  catMaybes
    <$> forM
      (sortOn (Down . stagedModeDepth) modes)
      ( \(StagedMode path _ mode) -> do
          encoded <- encodePath path
          catchError
            ( do
                restored <- restorePortableMode (root </> encoded) mode
                return $ if restored then Nothing else Just path
            )
            (const $ return $ Just path)
      )


widenStagedMetadata
  :: (MonadFileSystem m) => OsPath -> StagedMetadata -> m ()
widenStagedMetadata root (StagedMetadata modes) =
  forM_ (sortOn stagedModeDepth modes) $ \(StagedMode path fileType mode) -> do
    encoded <- encodePath path
    let entry = root </> encoded
    case mode of
      PortableMode (Just bits) _ ->
        setPortableMode entry (bits .|. ownerAccess fileType)
          `catchError` const (return ())
      PortableMode Nothing _ ->
        setPortableWritable entry True
          `catchError` const (return ())
 where
  ownerAccess Directory = 0o700
  ownerAccess _ = 0o600


stagedModeDepth :: StagedMode -> Int
stagedModeDepth (StagedMode path _ _) =
  length $ Posix.splitDirectories path


restorePortableMode
  :: (MonadFileSystem m) => OsPath -> PortableMode -> m Bool
restorePortableMode path mode = do
  case mode of
    PortableMode (Just bits) _ -> setPortableMode path bits
    PortableMode Nothing writable -> setPortableWritable path writable
  restored <- getPortableMode path
  return $ restored == mode


widenDirectoryForCleanup :: (MonadFileSystem m) => OsPath -> m ()
widenDirectoryForCleanup path = do
  mode <- getPortableMode path
  case mode of
    PortableMode (Just bits) _ ->
      setPortableMode path $ bits .|. 0o700
    PortableMode Nothing _ ->
      setPortableWritable path True


protectRestrictedStaging
  :: (MonadFileSystem m, MonadMask m)
  => OsPath
  -> StagedMetadata
  -> m a
  -> m a
protectRestrictedStaging root metadata action =
  mask $ \_ ->
    (action `catch` handleException)
      `catchError` handleFilesystemError
 where
  widen = do
    widenDirectoryForCleanup root `catchError` const (return ())
    widenStagedMetadata root metadata `catchError` const (return ())
  handleException (err :: SomeException) = widen >> throwM err
  handleFilesystemError err = widen >> throwError err


decodeZip
  :: LazyByteString.ByteString
  -> Either AcquisitionError [StagedEntry]
decodeZip bytes = do
  archive <-
    case Zip.toArchiveOrFail bytes of
      Left err -> Left $ InvalidArchive $ Text.pack err
      Right value -> Right value
  let entries = Zip.zEntries archive
  validateResourceUsage $
    fromIntegral . Zip.eUncompressedSize <$> entries
  decodeEntries maximumExpandedArchiveBytes entries
 where
  decodeEntries _ [] = Right []
  decodeEntries remaining (entry : entries) = do
    (decoded, consumed) <- decodeEntry remaining entry
    (decoded :) <$> decodeEntries (remaining - consumed) entries
  decodeEntry remaining entry
    | Zip.isEncryptedEntry entry =
        Left $ UnsupportedArchiveEntry $ Zip.eRelativePath entry
    | Zip.isEntrySymbolicLink entry =
        Left $ UnsupportedArchiveEntry $ Zip.eRelativePath entry
    | otherwise = do
        relative <- normalizeArchiveEntryPath $ Zip.eRelativePath entry
        case zipEntryKind entry of
          ZipDirectory ->
            Right (StagedDirectory relative (zipEntryMode entry), 0)
          ZipRegularFile -> do
            bounded <-
              boundedLazyBytes
                (fromIntegral remaining)
                (Zip.fromEntry entry)
            let contents = LazyByteString.toStrict bounded
            Right
              ( StagedFile relative contents $ zipEntryMode entry
              , fromIntegral $ ByteString.length contents
              )
          ZipUnsupported ->
            Left $ UnsupportedArchiveEntry $ Zip.eRelativePath entry


data ZipEntryKind
  = ZipRegularFile
  | ZipDirectory
  | ZipUnsupported


zipEntryKind :: Zip.Entry -> ZipEntryKind
zipEntryKind entry
  | zipCreatorSystem entry `notElem` [3, 19] = inferredKind
  | unixFileType == 0 = inferredKind
  | unixFileType == 0o100000 && not directoryPath = ZipRegularFile
  | unixFileType == 0o040000 = ZipDirectory
  | otherwise = ZipUnsupported
 where
  directoryPath = "/" `isSuffixOf` Zip.eRelativePath entry
  inferredKind
    | directoryPath = ZipDirectory
    | dosDirectory = ZipDirectory
    | otherwise = ZipRegularFile
  dosDirectory =
    Zip.eExternalFileAttributes entry .&. 0x10 /= 0
  unixFileType =
    (Zip.eExternalFileAttributes entry `shiftR` 16) .&. 0o170000


zipEntryMode :: Zip.Entry -> Maybe Word
zipEntryMode entry
  | zipCreatorSystem entry `elem` [3, 19]
      && unixMode /= 0 =
      Just $
        fromIntegral permissionBits
  | otherwise = Nothing
 where
  unixMode =
    (Zip.eExternalFileAttributes entry `shiftR` 16) .&. 0xffff
  permissionBits =
    unixMode .&. 0o777


zipCreatorSystem :: Zip.Entry -> Word
zipCreatorSystem entry =
  fromIntegral $ Zip.eVersionMadeBy entry `shiftR` 8


decodeTar
  :: LazyByteString.ByteString
  -> Either AcquisitionError [StagedEntry]
decodeTar = go 0 0 Nothing . Tar.decodeLongNames . Tar.read
 where
  go _ _ _ Tar.Done = Right []
  go _ _ _ (Tar.Fail err) = Left $ InvalidArchive $ Text.pack $ show err
  go count expanded pendingPath (Tar.Next entry remaining) = do
    let nextCount = count + 1
        nextExpanded = expanded + tarEntrySize entry
    whenResourceLimit nextCount nextExpanded
    case Tar.entryContent entry of
      Tar.OtherEntryType 'g' _ _ ->
        go nextCount nextExpanded pendingPath remaining
      Tar.OtherEntryType 'x' contents _ -> do
        headers <- parsePaxHeaders contents
        let path = Text.unpack <$> Map.lookup "path" headers
        go nextCount nextExpanded (path <|> pendingPath) remaining
      Tar.Directory
        | isTarRootDirectory
            (fromMaybe (TarEntry.entryTarPath entry) pendingPath) ->
            ( StagedDirectory
                ""
                (Just $ archivePermissions entry)
                :
            )
              <$> go nextCount nextExpanded Nothing remaining
      _ -> do
        decoded <-
          decodeEntry
            (fromMaybe (TarEntry.entryTarPath entry) pendingPath)
            entry
        (decoded :) <$> go nextCount nextExpanded Nothing remaining
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
  isTarRootDirectory path = path == "." || path == "./"
  tarEntrySize entry =
    case Tar.entryContent entry of
      Tar.NormalFile _ size -> fromIntegral size
      Tar.OtherEntryType _ _ size -> fromIntegral size
      _ -> 0


validateResourceUsage :: [Integer] -> Either AcquisitionError ()
validateResourceUsage sizes =
  whenResourceLimit (length sizes) (sum sizes)


whenResourceLimit :: Int -> Integer -> Either AcquisitionError ()
whenResourceLimit count expanded
  | count > maximumArchiveEntries =
      Left ArchiveResourceLimitExceeded
  | expanded > maximumExpandedArchiveBytes =
      Left ArchiveResourceLimitExceeded
  | otherwise = Right ()


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
