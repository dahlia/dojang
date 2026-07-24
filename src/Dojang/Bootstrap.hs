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
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , displayException
  , evaluate
  , fromException
  , throwIO
  )
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Catch (MonadCatch, MonadMask, catch, mask, throwM, try)
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
import GHC.Generics (Generic)
import System.FilePath.Posix qualified as Posix
import System.IO.Error (isDoesNotExistError)
import System.OsPath
  ( OsPath
  , joinPath
  , splitDirectories
  , takeDirectory
  , (</>)
  )
import Text.Read (readMaybe)
import Prelude hiding (readFile, writeFile)

import Dojang.MonadFileSystem
  ( BoundedFileRead (..)
  , FileIdentity
  , FileType (..)
  , MonadFileSystem (..)
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
  | -- | Two entries conflict by path or file type.
    ConflictingArchiveEntry FilePath
  | -- | The archive exceeds the bounded acquisition resource limits.
    ArchiveResourceLimitExceeded
  deriving (Eq, Show, Generic, NFData)


data StagedEntry
  = StagedDirectory FilePath (Maybe Word)
  | StagedFile FilePath ByteString (Maybe Word)
  deriving (Eq, Show, Generic, NFData)


data StagedMode = StagedMode FilePath FileType PortableMode
  deriving (Eq, Show, Generic)


data PublishedEntry
  = PublishedEntry FileType OsPath OsPath OsPath FileIdentity


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
          createPrivateDirectory staging
          cleanupStagingOnError staging $ do
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


-- | Publishes a fully validated staging tree. A missing or existing empty
-- destination is published with an atomic directory rename. When the
-- destination is the process's current working directory, its directory
-- identity is preserved instead.
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
-- A missing or existing empty destination is published with an atomic rename,
-- except that the current working directory keeps its identity.  File and
-- directory modes come from the staging tree, with retained archive modes
-- taking precedence.  The returned paths identify entries whose stored
-- permissions the destination filesystem could not represent; their contents
-- are still published.
publishStagedDirectoryWithMetadata
  :: (MonadFileSystem m, MonadMask m)
  => StagedMetadata
  -> OsPath
  -> OsPath
  -> m [FilePath]
publishStagedDirectoryWithMetadata metadata staging destination = do
  destinationSymlink <- isSymlink destination
  when destinationSymlink $
    throwError $
      userError "bootstrap destination is a symbolic link"
  destinationExists <- exists destination
  if not destinationExists
    then do
      protectRestrictedStaging staging metadata $ do
        modeFailures <- applyStagedMetadata staging metadata
        renameDirectory staging destination
        return modeFailures
    else do
      destinationEntries <- listDirectory destination
      unless (null destinationEntries) $
        throwError $
          userError "bootstrap destination is not empty"
      currentDirectory <- canonicalizePath =<< getCurrentDirectory
      absoluteDestination <- canonicalizePath =<< makeAbsolute destination
      if absoluteDestination == currentDirectory
        then do
          current <- encodePath "."
          copyDirectoryContents metadata staging current
        else do
          destinationMode <- getPortableMode destination
          destinationIdentity <- getFileIdentity destination
          protectRestrictedStaging staging metadata $ do
            modeFailures <-
              applyStagedMetadata staging $ withoutRootMetadata metadata
            rootModeRestored <- restorePortableMode staging destinationMode
            unless rootModeRestored $
              throwError $
                userError
                  "bootstrap destination permissions could not be preserved"
            didExchange <- case destinationIdentity of
              Nothing -> return False
              Just _ -> exchangeDirectories staging destination
            unless didExchange $
              throwError $
                userError
                  "filesystem cannot atomically exchange the bootstrap destination"
            validateExchangedDestination
              destinationIdentity
              staging
              destination
            widenDirectoryForCleanup staging
            removeDirectory staging `catchError` \err -> do
              restoreExchangedDestinationMode
                destinationMode
                staging
                destination
              throwError err
            return modeFailures


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


validateExchangedDestination
  :: (MonadFileSystem m, MonadCatch m)
  => Maybe FileIdentity
  -> OsPath
  -> OsPath
  -> m ()
validateExchangedDestination expectedIdentity staging destination = do
  inspected <-
    catchError
      ( try $
          (,)
            <$> getFileIdentity staging
            <*> listDirectory staging
      )
      (return . Left)
  case inspected of
    Left (err :: IOError) -> do
      restoreExchangedDestination staging destination
      throwError err
    Right (actualIdentity, exchangedEntries) ->
      unless
        (actualIdentity == expectedIdentity && null exchangedEntries)
        $ do
          restoreExchangedDestination staging destination
          throwError $
            userError "bootstrap destination changed during publication"


restoreExchangedDestination
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> m ()
restoreExchangedDestination staging destination = do
  restored <- exchangeDirectories staging destination
  unless restored $
    throwError $
      userError "bootstrap destination exchange could not be reversed"


restoreExchangedDestinationMode
  :: (MonadFileSystem m, MonadCatch m)
  => PortableMode
  -> OsPath
  -> OsPath
  -> m ()
restoreExchangedDestinationMode mode staging destination = do
  modeResult <-
    catchError
      (try $ restorePortableMode staging mode)
      (return . Left)
  restoreExchangedDestination staging destination
  case modeResult of
    Left (err :: IOError) -> throwError err
    Right False ->
      throwError $
        userError
          "bootstrap destination permissions could not be restored"
    Right True -> return ()


copyDirectoryTree
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> m (Either AcquisitionError StagedMetadata)
copyDirectoryTree source destination = do
  entries <- listDirectoryRecursively source []
  validation <- identifyDirectoryEntries source entries
  case validation of
    Left err -> return $ Left err
    Right (sourceIdentity, entryIdentities) -> do
      metadata <- captureStagedMetadata source entries
      createPrivateDirectory destination
      cleanupStagingOnError destination $ do
        copyDirectoryEntries
          source
          destination
          entryIdentities
          entries
        verifyDirectorySource
          source
          sourceIdentity
          entryIdentities
          entries
      return $ Right metadata


identifyDirectoryEntries
  :: (MonadFileSystem m)
  => OsPath
  -> [(FileType, OsPath)]
  -> m
       ( Either
           AcquisitionError
           (FileIdentity, Map.Map OsPath FileIdentity)
       )
identifyDirectoryEntries source entries = do
  sourceIdentity <- getFileIdentity source
  case sourceIdentity of
    Nothing -> do
      path <- decodePath source
      return $ Left $ UnsupportedSourceEntry path
    Just identity -> do
      identified <- go Map.empty entries
      return $ fmap (\entryIdentities -> (identity, entryIdentities)) identified
 where
  go identities [] = return $ Right identities
  go identities ((fileType, relative) : remaining) = do
    let path = source </> relative
    identityBefore <- getFileIdentity path
    supported <- case fileType of
      File -> isRegularFile path
      Directory -> (&&) <$> isDirectory path <*> (not <$> isSymlink path)
      Symlink -> isSymlink path
    identityAfter <- getFileIdentity path
    case identityBefore of
      Just identity
        | supported && identityAfter == Just identity ->
            go (Map.insert relative identity identities) remaining
      _ -> do
        decoded <- decodePath relative
        return $ Left $ UnsupportedSourceEntry decoded


copyDirectoryContents
  :: (MonadFileSystem m, MonadMask m)
  => StagedMetadata
  -> OsPath
  -> OsPath
  -> m [FilePath]
copyDirectoryContents retainedMetadata source destination = do
  entries <- listDirectoryRecursively source []
  sourceMetadata <- captureStagedMetadata source entries
  let publishedMetadata =
        withoutRootMetadata $
          overlayStagedMetadata retainedMetadata sourceMetadata
  withDirectoryEntriesNoReplace
    source
    destination
    entries
    (applyStagedMetadata source publishedMetadata)
    $ \modeFailures _ -> do
      removeDirectory source
      return modeFailures


copyDirectoryEntries
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> Map.Map OsPath FileIdentity
  -> [(FileType, OsPath)]
  -> m ()
copyDirectoryEntries source destination identities entries =
  forM_ entries $ \(fileType, relative) -> do
    let sourceEntry = source </> relative
        destinationEntry = destination </> relative
    expectedIdentity <- expectedEntryIdentity identities relative
    case fileType of
      Directory -> do
        verifyEntryIdentity source relative expectedIdentity
        createDirectory destinationEntry
      File -> do
        copied <-
          copyRegularFileWithIdentity
            expectedIdentity
            sourceEntry
            destinationEntry
        unless copied $ do
          throwSourceEntryChanged relative
      Symlink -> do
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
  -> Map.Map OsPath FileIdentity
  -> [(FileType, OsPath)]
  -> m ()
verifyDirectorySource source sourceIdentity identities entries = do
  verifyPathIdentity source source sourceIdentity
  forM_ entries $ \(_, relative) -> do
    expectedIdentity <- expectedEntryIdentity identities relative
    verifyEntryIdentity source relative expectedIdentity


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
  throwError $
    userError $
      "bootstrap source entry changed during acquisition: " <> decoded


expectedEntryIdentity
  :: (MonadFileSystem m)
  => Map.Map OsPath FileIdentity
  -> OsPath
  -> m FileIdentity
expectedEntryIdentity identities relative =
  case Map.lookup relative identities of
    Just identity -> return identity
    Nothing -> do
      decoded <- decodePath relative
      throwError $
        userError $
          "filesystem cannot identify bootstrap source entry: " <> decoded


withDirectoryEntriesNoReplace
  :: (MonadFileSystem m, MonadMask m)
  => OsPath
  -> OsPath
  -> [(FileType, OsPath)]
  -> m preparation
  -> (preparation -> [PublishedEntry] -> m a)
  -> m a
withDirectoryEntriesNoReplace source destination entries prepare action = do
  rollbackDirectory <- createRollbackDirectory
  identified <-
    identifyEntries rollbackDirectory $ zip [0 ..] entries
  prepared <-
    prepare `catchError` \err -> do
      removeDirectory rollbackDirectory `catchError` const (return ())
      throwError err
  moveTopLevelEntries rollbackDirectory prepared [] identified
 where
  createRollbackDirectory = do
    temporary <-
      writeTemporaryFile
        (takeDirectory source)
        ".dojang-bootstrap-rollback-"
        ""
    removeFile temporary
    createPrivateDirectory temporary
    return temporary

  identifyEntries rollbackDirectory =
    mapM $ \(index, (fileType, relative)) -> do
      quarantineName <- encodePath $ show (index :: Int)
      getFileIdentity (source </> relative) >>= \case
        Just identity ->
          return
            ( fileType
            , relative
            , rollbackDirectory </> quarantineName
            , identity
            )
        Nothing -> do
          path <- decodePath relative
          throwError $
            userError $
              "filesystem cannot identify staged bootstrap entry: " <> path

  topLevel relative =
    case splitDirectories relative of
      component : _ -> component
      [] -> relative

  topLevelEntries identified =
    [ (fileType, relative)
    | (fileType, relative, _, _) <- identified
    , relative == topLevel relative
    ]

  publishedEntries identified relative =
    [ PublishedEntry
        fileType
        entry
        (destination </> entry)
        quarantine
        identity
    | (fileType, entry, quarantine, identity) <- identified
    , topLevel entry == relative
    ]

  moveTopLevelEntries rollbackDirectory prepared created identified =
    moveEntries
      rollbackDirectory
      prepared
      created
      identified
      (topLevelEntries identified)

  moveEntries rollbackDirectory prepared created _ [] = do
    result <- action prepared created
    removeDirectory rollbackDirectory
    return result
  moveEntries
    rollbackDirectory
    prepared
    created
    identified
    ((fileType, relative) : remaining) =
      mask $ \restore -> do
        let published = publishedEntries identified relative
            cleanup = cleanupPublishedEntries published
            continue =
              restore $
                moveEntries
                  rollbackDirectory
                  prepared
                  (created <> published)
                  identified
                  remaining
            handleException (err :: SomeException)
              | Just (_ :: IOError) <- fromException err = throwM err
              | otherwise = cleanup >> throwM err
        renameEntry
          fileType
          (source </> relative)
          (destination </> relative)
        (continue `catch` handleException)
          `catchError` \err -> cleanup >> throwError err


cleanupPublishedEntries
  :: (MonadFileSystem m) => [PublishedEntry] -> m ()
cleanupPublishedEntries [] = return ()
cleanupPublishedEntries
  ( root@(PublishedEntry rootType rootRelative rootPath rootQuarantine rootIdentity)
      : descendants
    ) = do
    quarantined <- quarantineEntry root
    when quarantined $ do
      currentIdentity <- getFileIdentity rootQuarantine
      if currentIdentity /= Just rootIdentity
        then renameEntry rootType rootQuarantine rootPath
        else case rootType of
          Directory -> do
            rootMode <- getPortableMode rootQuarantine
            widenDirectoryForCleanup rootQuarantine
            (safeDirectories, directoryModes) <-
              preparePublishedDirectories
                rootRelative
                rootQuarantine
                (Set.singleton rootRelative)
                [(rootRelative, rootMode)]
                descendants
            forM_ (reverse descendants) $ \entry ->
              when
                (publishedEntryParent entry `Set.member` safeDirectories)
                $ cleanupPublishedEntry
                  rootRelative
                  rootQuarantine
                  directoryModes
                  entry
            removePublishedEntry
              Directory
              rootPath
              rootQuarantine
              (Just rootMode)
          File ->
            removePublishedEntry File rootPath rootQuarantine Nothing
          Symlink ->
            removePublishedEntry Symlink rootPath rootQuarantine Nothing


preparePublishedDirectories
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> Set.Set OsPath
  -> [(OsPath, PortableMode)]
  -> [PublishedEntry]
  -> m (Set.Set OsPath, [(OsPath, PortableMode)])
preparePublishedDirectories _ _ safe modes [] =
  return (safe, modes)
preparePublishedDirectories
  rootRelative
  rootQuarantine
  safe
  modes
  (entry@(PublishedEntry fileType relative _ quarantine expectedIdentity) : rest)
    | fileType /= Directory
        || publishedEntryParent entry `Set.notMember` safe =
        preparePublishedDirectories
          rootRelative
          rootQuarantine
          safe
          modes
          rest
    | otherwise = do
        let path =
              relocatedPublishedPath rootRelative rootQuarantine relative
        quarantined <-
          renameEntryIfPresent Directory path quarantine
        if not quarantined
          then continue safe modes
          else do
            currentIdentity <- getFileIdentity quarantine
            if currentIdentity /= Just expectedIdentity
              then renameEntry Directory quarantine path >> continue safe modes
              else do
                mode <- getPortableMode quarantine
                widenDirectoryForCleanup quarantine
                renameEntry Directory quarantine path
                continue
                  (Set.insert relative safe)
                  ((relative, mode) : modes)
   where
    continue safe' modes' =
      preparePublishedDirectories
        rootRelative
        rootQuarantine
        safe'
        modes'
        rest


publishedEntryParent :: PublishedEntry -> OsPath
publishedEntryParent (PublishedEntry _ relative _ _ _) =
  takeDirectory relative


relocatedPublishedPath :: OsPath -> OsPath -> OsPath -> OsPath
relocatedPublishedPath rootRelative rootQuarantine relative =
  case drop
    (length $ splitDirectories rootRelative)
    (splitDirectories relative) of
    [] -> rootQuarantine
    components -> rootQuarantine </> joinPath components


quarantineEntry :: (MonadFileSystem m) => PublishedEntry -> m Bool
quarantineEntry (PublishedEntry fileType _ path quarantine _) =
  renameEntryIfPresent fileType path quarantine


renameEntryIfPresent
  :: (MonadFileSystem m)
  => FileType
  -> OsPath
  -> OsPath
  -> m Bool
renameEntryIfPresent fileType source destination =
  catchError
    (renameEntry fileType source destination >> return True)
    $ \err ->
      if isDoesNotExistError err
        then return False
        else throwError err


cleanupPublishedEntry
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> [(OsPath, PortableMode)]
  -> PublishedEntry
  -> m ()
cleanupPublishedEntry
  rootRelative
  rootQuarantine
  directoryModes
  (PublishedEntry fileType relative _ quarantine expectedIdentity) = do
    let path =
          relocatedPublishedPath rootRelative rootQuarantine relative
    quarantined <-
      renameEntryIfPresent fileType path quarantine
    when quarantined $ do
      currentIdentity <- getFileIdentity quarantine
      if currentIdentity == Just expectedIdentity
        then
          removePublishedEntry
            fileType
            path
            quarantine
            (lookup relative directoryModes)
        else renameEntry fileType quarantine path


removePublishedEntry
  :: (MonadFileSystem m)
  => FileType
  -> OsPath
  -> OsPath
  -> Maybe PortableMode
  -> m ()
removePublishedEntry fileType published quarantine originalMode =
  case fileType of
    Directory -> do
      mode <- maybe (getPortableMode quarantine) return originalMode
      widenDirectoryForCleanup quarantine
      entries <- listDirectory quarantine
      if null entries
        then removeDirectory quarantine
        else do
          restored <- restorePortableMode quarantine mode
          unless restored $
            throwError $
              userError
                "bootstrap rollback could not restore directory permissions"
          renameEntry Directory quarantine published
    File -> do
      setPortableWritable quarantine True
      removeFile quarantine
    Symlink -> removeFile quarantine


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
  => OsPath
  -> [(FileType, OsPath)]
  -> m StagedMetadata
captureStagedMetadata source entries =
  do
    resolvedSource <- canonicalizePath source
    rootMode <- getPortableMode resolvedSource
    descendants <- concat <$> traverse capture entries
    return $
      StagedMetadata $
        StagedMode "" Directory rootMode : descendants
 where
  capture (Symlink, _) = return []
  capture (fileType, relative) = do
    mode <- getPortableMode $ source </> relative
    path <- decodePath relative
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


withoutRootMetadata :: StagedMetadata -> StagedMetadata
withoutRootMetadata (StagedMetadata modes) =
  StagedMetadata
    [ mode
    | mode@(StagedMode path _ _) <- modes
    , not $ null path
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
      && permissionBits /= 0 =
      Just $
        fromIntegral permissionBits
  | otherwise = Nothing
 where
  permissionBits =
    (Zip.eExternalFileAttributes entry `shiftR` 16) .&. 0o777


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
