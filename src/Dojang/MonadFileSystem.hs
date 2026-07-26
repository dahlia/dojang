{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Dojang.MonadFileSystem
  ( BoundedFileRead (..)
  , DirectoryPathIdentity
  , DryRunIO
  , FileIdentity
  , FileModeSnapshot (..)
  , FileSnapshot
  , FileType (..)
  , MonadFileSystem (..)
  , dryRunIO
  , dryRunIO'
  , captureDirectoryPathIdentity
  , fileSnapshotIdentity
  , isNoReplaceUnsupportedError
  , matchesDirectoryPathIdentity
  , noReplaceUnsupportedError
  , tryDryRunIO
  , writeFileAtomically
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception qualified as Exception
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bits (complement, (.&.), (.|.))
import Data.List (inits, isPrefixOf, sort, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)), filter, singleton, toList)
import Data.Ord (Down (Down))
import GHC.IO.Exception
  ( IOErrorType
      ( InappropriateType
      , InvalidArgument
      , UnsupportedOperation
      )
  )
import GHC.Stack (HasCallStack)
import System.IO.Error
  ( alreadyExistsErrorType
  , doesNotExistErrorType
  , ioeGetErrorType
  , ioeGetLocation
  , ioeSetErrorString
  , ioeSetFileName
  , ioeSetLocation
  , isAlreadyExistsError
  , isDoesNotExistError
  , isPermissionError
  , mkIOError
  )
import System.Info (os)
import Prelude hiding (filter, readFile, writeFile)

import Control.Monad.Except (MonadError (..), tryError)
import Control.Monad.Extra (partitionM)
import Control.Monad.State.Strict
  ( MonadState
  , StateT
  , gets
  , modify'
  , runStateT
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified
  ( concat
  , hGetContents
  , hGetSome
  , hPut
  , length
  , null
  , readFile
  , unpack
  , writeFile
  )
import Data.Map.Strict (Map, alter, fromList, keys, toAscList, (!?))
import Numeric (showHex)
import System.Directory qualified as Directory
import System.Directory.OsPath
  ( doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , getSymbolicLinkTarget
  , pathIsSymbolicLink
  , removeDirectoryRecursive
  )
import System.Directory.OsPath qualified as OsDirectory
import System.Entropy (getEntropy)
import System.FileLock qualified as FileLock

import Dojang.Types.RouteMetadata
  ( PortableMode (..)
  , portableModeFromBits
  )


#ifdef mingw32_HOST_OS
import Foreign
  ( Ptr
  , alloca
  , allocaBytes
  , castPtr
  , nullPtr
  , peek
  , peekByteOff
  , poke
  , sizeOf
  )
import Foreign.C.Types (CInt (CInt))
import Data.Int (Int64)
import System.IO (IOMode (ReadMode), hIsSeekable)
import System.Win32.File qualified as Win32
import System.Win32.String qualified as Win32String
import System.Win32.Time qualified as Win32Time
import System.Win32.Types qualified as Win32
#else
import Foreign.C.Error qualified as CError
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (CInt), CSize (CSize))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
#if defined(linux_HOST_OS) || defined(darwin_HOST_OS)
import Foreign.C.Types (CUInt (CUInt))
#endif
import System.Posix.Directory qualified as PosixDirectory
import System.Posix.Files qualified as Posix
import System.Posix.IO qualified as Posix
import System.Posix.Internals qualified as PosixInternal
import System.Posix.Types (Fd)
#endif
import System.FilePattern (FilePattern, Step (stepApply, stepDone), step_)
import System.IO
  ( Handle
  , IOMode (WriteMode)
  , hClose
  , hFlush
  , openBinaryFile
  , openBinaryTempFile
  , openBinaryTempFileWithDefaultPermissions
  )
import System.OsPath
  ( OsPath
  , decodeFS
  , encodeFS
  , isAbsolute
  , joinPath
  , normalise
  , splitDirectories
  , takeDirectory
  , takeFileName
  , (</>)
  )


#ifdef mingw32_HOST_OS
foreign import ccall unsafe "ConvertStringSecurityDescriptorToSecurityDescriptorW"
  c_convertStringSecurityDescriptor
    :: Win32.LPCTSTR
    -> Win32.DWORD
    -> Ptr Win32.LPVOID
    -> Win32.LPDWORD
    -> IO Win32.BOOL

foreign import ccall unsafe "LocalFree"
  c_localFree :: Win32.LPVOID -> IO Win32.LPVOID

foreign import ccall unsafe "GetVolumePathNameW"
  c_getVolumePathName
    :: Win32.LPCTSTR
    -> Win32.LPTSTR
    -> Win32.DWORD
    -> IO Win32.BOOL

foreign import ccall unsafe "GetVolumeInformationW"
  c_getVolumeInformation
    :: Win32.LPCTSTR
    -> Win32.LPTSTR
    -> Win32.DWORD
    -> Win32.LPDWORD
    -> Win32.LPDWORD
    -> Win32.LPDWORD
    -> Win32.LPTSTR
    -> Win32.DWORD
    -> IO Win32.BOOL

foreign import ccall unsafe "GetFileInformationByHandleEx"
  c_getFileInformationByHandleEx
    :: Win32.HANDLE
    -> CInt
    -> Ptr ()
    -> Win32.DWORD
    -> IO Win32.BOOL

#endif


-- | A type that represents a file or directory.
data FileType
  = -- | A directory.
    Directory
  | -- | A file.
    File
  | -- | A symbolic link.
    Symlink
  deriving (Eq, Ord, Show)


-- | A stable identity for one filesystem entry.
--
-- Symbolic links are identified without following them.  The representation
-- is intentionally opaque so callers can compare identities but cannot depend
-- on platform-specific device, volume, inode, or file-index details.
data FileIdentity = FileIdentity Integer Integer
  deriving (Eq, Ord, Show)


-- | Stable identities for every directory from a filesystem root through a
-- selected directory path.
--
-- The constructor is intentionally hidden so callers can only compare a later
-- observation with the complete captured chain.
data DirectoryPathIdentity
  = DirectoryPathIdentity OsPath [(OsPath, FileIdentity)]
  deriving (Eq, Show)


-- | The identity and change metadata of a regular file at one instant.
--
-- The representation is intentionally opaque.  A snapshot can be passed back
-- to 'copyRegularFileWithSnapshot' to reject both pathname replacements and
-- in-place changes that the filesystem reports through size or change
-- metadata since the snapshot was captured.
data FileSnapshot
  = FileSnapshot FileIdentity Integer Rational (Maybe Rational)
  deriving (Eq, Show)


-- | A filesystem entry's identity and portable mode observed together.
--
-- Filesystem-backed implementations capture both values from one stat result
-- or open handle so a pathname replacement cannot pair one entry's identity
-- with another entry's permissions.
data FileModeSnapshot
  = FileModeSnapshot FileIdentity PortableMode
  deriving (Eq, Show)


-- | Gets the stable entry identity retained by a regular-file snapshot.
fileSnapshotIdentity :: FileSnapshot -> FileIdentity
fileSnapshotIdentity (FileSnapshot identity _ _ _) = identity


-- | Captures a non-symbolic-link directory and all of its ancestors.
--
-- The chain is observed twice and accepted only when both observations agree,
-- so a replacement during capture fails closed.
captureDirectoryPathIdentity
  :: (MonadFileSystem m) => OsPath -> m (Maybe DirectoryPathIdentity)
captureDirectoryPathIdentity path = do
  first <- observe
  second <- observe
  return $
    if first == second
      then DirectoryPathIdentity normalized <$> first
      else Nothing
 where
  normalized = normalise path
  paths =
    map joinPath $
      drop 1 $
        inits $
          splitDirectories normalized
  observe = do
    entries <-
      forM paths $ \entry -> do
        symbolicLink <- isSymlink entry
        directory <- isDirectory entry
        if symbolicLink || not directory
          then return Nothing
          else do
            identity <- getFileIdentity entry
            return $ fmap (\value -> (entry, value)) identity
    return $ sequence entries


-- | Checks whether a directory and its complete ancestor chain still match a
-- previously captured observation.
matchesDirectoryPathIdentity
  :: (MonadFileSystem m) => DirectoryPathIdentity -> m Bool
matchesDirectoryPathIdentity expected@(DirectoryPathIdentity path _) =
  (== Just expected) <$> captureDirectoryPathIdentity path


-- | The result of reading a regular file with an explicit byte limit.
data BoundedFileRead
  = -- | The opened path was not a regular file.
    NotRegularFile
  | -- | The regular file was larger than the requested limit.
    FileSizeLimitExceeded
  | -- | The regular file changed while its contents were being read.
    FileChangedDuringRead
  | -- | Complete contents of a regular file within the requested limit.
    BoundedFileContents ByteString
  deriving (Eq, Show)


-- | A monad that can perform filesystem operations.  It's also based on
-- 'OsPath' instead of 'FilePath'.
class (MonadError IOError m) => MonadFileSystem m where
  -- | Encodes a 'FilePath' into an 'OsPath'.
  encodePath :: (HasCallStack) => FilePath -> m OsPath


  -- | Decodes a 'OsPath' into a 'FilePath'.
  decodePath :: (HasCallStack) => OsPath -> m FilePath


  -- | Gets the process current working directory.
  getCurrentDirectory :: (HasCallStack) => m OsPath


  -- | Gets the current user's home directory.
  getHomeDirectory :: (HasCallStack) => m OsPath


  -- | Makes a path absolute using the interpreter's current working directory.
  makeAbsolute :: (HasCallStack) => OsPath -> m OsPath
  makeAbsolute path
    | isAbsolute path = return $ normalise path
    | otherwise = normalise . (</> path) <$> getCurrentDirectory


  -- | Checks if a file (or directory) exists.  If a path is a symbolic link,
  -- then it tells whether the target of the symbolic link exists.
  exists :: (HasCallStack) => OsPath -> m Bool


  -- | Checks if a path exists and is a file.  If a path is a symbolic link,
  -- then it tells whether the target of the symbolic link is a file.
  isFile :: (HasCallStack) => OsPath -> m Bool


  -- | Checks if a path exists and is a regular, non-symbolic-link file.
  isRegularFile :: (HasCallStack) => OsPath -> m Bool


  -- | Checks if a path exists and is a directory.  If a path is a symbolic
  -- link, then it tells whether the target of the symbolic link is a directory.
  isDirectory :: (HasCallStack) => OsPath -> m Bool


  -- | Checks if a path exists and is a symbolic link.
  isSymlink :: (HasCallStack) => OsPath -> m Bool


  -- | Reads contents from a file.
  readFile :: (HasCallStack) => OsPath -> m ByteString


  -- | Reads a regular file, returning 'Nothing' for every other file type.
  --
  -- Symbolic links to regular files are accepted. Filesystem-backed
  -- implementations should validate the opened file rather than a pathname so
  -- that a concurrent replacement cannot redirect the read to a special file.
  readRegularFile :: (HasCallStack) => OsPath -> m (Maybe ByteString)
  readRegularFile path = do
    regularFile <- isRegularFile path
    if regularFile
      then Just <$> readFile path
      else do
        symbolicLink <- isSymlink path
        if not symbolicLink
          then return Nothing
          else do
            resolved <- canonicalizePath path
            resolvedRegularFile <- isRegularFile resolved
            if resolvedRegularFile
              then Just <$> readFile path
              else return Nothing


  -- | Reads a regular file without consuming more than the given number of
  -- bytes.
  --
  -- The source-validation guarantees are the same as 'readRegularFile'.
  -- Filesystem-backed implementations should enforce the bound while reading
  -- from the validated handle, rather than inspecting the pathname first, and
  -- return 'FileChangedDuringRead' when its change metadata no longer matches
  -- after a complete read.
  readRegularFileBounded
    :: (HasCallStack) => Int -> OsPath -> m BoundedFileRead
  readRegularFileBounded limit path = do
    result <- readRegularFile path
    case result of
      Nothing -> return NotRegularFile
      Just contents
        | Data.ByteString.length contents > limit ->
            return FileSizeLimitExceeded
        | otherwise -> return $ BoundedFileContents contents


  -- | Copies a regular file without following a concurrent replacement to a
  -- special file.
  --
  -- Symbolic links to regular files are accepted. Filesystem-backed
  -- implementations should validate the opened source and stream from that
  -- same handle. Returns 'False' without creating the destination when the
  -- opened source is not a regular file.
  copyRegularFile
    :: (HasCallStack)
    => OsPath
    -- ^ Source path.
    -> OsPath
    -- ^ Destination path.
    -> m Bool
  copyRegularFile source destination = do
    result <- readRegularFile source
    case result of
      Nothing -> return False
      Just contents -> writeFile destination contents >> return True


  -- | Copies a regular file only when the opened source matches a snapshot.
  --
  -- Filesystem-backed implementations must compare identity and change
  -- metadata obtained from the same handle used for copying.  This prevents
  -- pathname replacements and filesystem-reported in-place changes between
  -- directory-source validation and acquisition from redirecting or corrupting
  -- the copy.  They must also recheck change metadata from that handle after
  -- copying and discard the destination if the source changed in place.
  -- Returns 'False' without retaining the destination when the identity
  -- differs, the source changes, or the opened source is not a regular file.
  copyRegularFileWithSnapshot
    :: (HasCallStack)
    => FileSnapshot
    -- ^ Snapshot captured while validating the source tree.
    -> OsPath
    -- ^ Source path.
    -> OsPath
    -- ^ Destination path.
    -> m Bool
  copyRegularFileWithSnapshot expectedSnapshot source destination = do
    actualSnapshot <- getFileSnapshot source
    if actualSnapshot == Just expectedSnapshot
      then copyRegularFile source destination
      else return False


  -- | Writes contents into a file.
  writeFile :: (HasCallStack) => OsPath -> ByteString -> m ()


  -- | Creates a complete file through an atomic sibling rename.
  --
  -- The new file must receive the platform's default permissions, and the
  -- operation must fail rather than replace a destination created
  -- concurrently.  Virtual and test filesystems may use the default
  -- single-write implementation.
  createFileAtomicallyWithDefaultPermissions
    :: (HasCallStack) => OsPath -> FilePath -> ByteString -> m ()
  createFileAtomicallyWithDefaultPermissions destination _ contents = do
    destinationExists <- exists destination
    if not destinationExists
      then writeFile destination contents
      else do
        destination' <- decodePath destination
        throwError $
          mkIOError
            alreadyExistsErrorType
            "createFileAtomicallyWithDefaultPermissions"
            Nothing
            (Just destination')


  -- | Replaces the destination file with the source file.
  --
  -- Both paths must be on the same filesystem.  Implementations should use
  -- the platform's atomic replacement operation where one is available.
  replaceFile :: (HasCallStack) => OsPath -> OsPath -> m ()


  -- | Renames a directory without replacing an existing destination.
  --
  -- Filesystem-backed implementations should use an atomic same-filesystem
  -- rename.  The default implementation preserves observable behavior for
  -- virtual filesystems by copying the tree and removing the source.
  renameDirectory
    :: (HasCallStack)
    => OsPath
    -- ^ Existing source directory.
    -> OsPath
    -- ^ Destination path, which must not exist.
    -> m ()
  renameDirectory source destination = do
    entries <- listDirectoryRecursively source []
    rootMode <- getPortableMode source
    nestedDirectoryModes <-
      forM
        [relative | (Directory, relative) <- entries]
        $ \relative -> do
          let sourceEntry = source </> relative
              destinationEntry = destination </> relative
          mode <- getPortableMode sourceEntry
          return (destinationEntry, mode)
    createDirectory destination
    forM_ entries $ \(fileType, relative) -> do
      let sourceEntry = source </> relative
          destinationEntry = destination </> relative
      case fileType of
        Directory -> createDirectory destinationEntry
        File -> copyFileWithMetadata sourceEntry destinationEntry
        Symlink -> do
          target <- readSymlinkTarget sourceEntry
          linkType <- getSymbolicLinkType sourceEntry
          createSymbolicLink
            target
            destinationEntry
            linkType
    let directoryModes =
          (destination, rootMode) : nestedDirectoryModes
    forM_
      ( sortOn
          (Down . length . splitDirectories . fst)
          directoryModes
      )
      $ \(path, mode) ->
        case mode.posixBits of
          Just bits -> setPortableMode path bits
          Nothing -> setPortableWritable path mode.writable
    removeDirectoryRecursively source


  -- | Writes a uniquely named temporary file in the given directory.
  --
  -- The returned path belongs to the caller, which should replace or remove
  -- it after use.
  writeTemporaryFile
    :: (HasCallStack)
    => OsPath
    -- ^ Parent directory.
    -> FilePath
    -- ^ Filename template.
    -> ByteString
    -- ^ File contents.
    -> m OsPath


  -- | Runs an action while holding an exclusive inter-process file lock.
  withFileLock :: (HasCallStack) => OsPath -> m a -> m a


  -- | Resolves symbolic links and other filesystem aliases in a path.
  canonicalizePath :: (HasCallStack) => OsPath -> m OsPath
  canonicalizePath = return . normalise


  -- | Tells the target path of a symbolic link.  If the path is not a symbolic
  -- link, then it throws an 'IOError'.  The target path is relative to the
  -- symbolic link (i.e., resolved from the directory that contains the
  -- symbolic link).
  readSymlinkTarget :: OsPath -> m OsPath


  -- | Gets the intrinsic file-or-directory type of a symbolic link.
  --
  -- This matters on Windows, where the link type is recorded independently
  -- of whether its target currently exists.  The path must be a symbolic link.
  getSymbolicLinkType :: (HasCallStack) => OsPath -> m FileType
  getSymbolicLinkType path = do
    directoryLink <- isDirectory path
    return $ if directoryLink then Directory else File


  -- | Copies a file from one path to another.
  copyFile
    :: (HasCallStack)
    => OsPath
    -- ^ Source path.
    -> OsPath
    -- ^ Destination path.
    -> m ()


  -- | Copies a file together with its filesystem metadata.
  --
  -- The default implementation copies only the contents.  Filesystem-backed
  -- implementations should preserve permissions and other supported metadata.
  copyFileWithMetadata
    :: (HasCallStack)
    => OsPath
    -- ^ Source path.
    -> OsPath
    -- ^ Destination path.
    -> m ()
  copyFileWithMetadata = copyFile


  -- | Copies filesystem permissions without replacing file contents.
  --
  -- The default implementation does nothing.  Filesystem-backed
  -- implementations should preserve every supported permission bit or
  -- attribute.
  copyFilePermissions
    :: (HasCallStack)
    => OsPath
    -- ^ Source path.
    -> OsPath
    -- ^ Destination path.
    -> m ()
  copyFilePermissions _ _ = return ()


  -- | Creates a directory at the given path.
  createDirectory :: (HasCallStack) => OsPath -> m ()


  -- | Creates a directory that is accessible only to the current user.
  --
  -- Filesystem-backed implementations must apply the restrictive mode or ACL
  -- atomically with creation so that no wider-permission window is observable.
  -- They must reject filesystems that cannot enforce this restriction.
  createPrivateDirectory :: (HasCallStack) => OsPath -> m ()
  createPrivateDirectory path = do
    createDirectory path
    setPortableMode path 0o700


  -- | Creates a directory at the given path, including all parent directories.
  createDirectories :: (HasCallStack) => OsPath -> m ()
  createDirectories path =
    ( do
        forM_ ancestors $ \ancestor -> do
          isSymlink' <- isSymlink ancestor
          when isSymlink' $ do
            ancestor' <- decodePath ancestor
            throwError $ symlinkError ancestor'
          isDir <- isDirectory ancestor
          unless isDir $ do
            exists' <- isFile ancestor
            if exists'
              then do
                ancestor' <- decodePath ancestor
                throwError $ fileError ancestor'
              else
                createDirectory ancestor `catchError` \err ->
                  if isAlreadyExistsError err
                    then do
                      createdByPeer <- isDirectory ancestor
                      unless createdByPeer $ throwError err
                    else throwError err
    )
      `mapError` (`ioePrependLocation` "createDirectories")
   where
    split :: [OsPath]
    split = splitDirectories path
    ancestors :: [OsPath]
    ancestors = map joinPath $ drop 1 (inits split)
    fileError :: FilePath -> IOError
    fileError path' =
      mkIOError InappropriateType "createDirectories" Nothing (Just path')
        `ioeSetErrorString` "one of its ancestors is a non-directory file"
    symlinkError :: FilePath -> IOError
    symlinkError path' =
      mkIOError InappropriateType "createDirectories" Nothing (Just path')
        `ioeSetErrorString` "one of its ancestors is a symbolic link"


  -- | Removes a regular file.
  removeFile :: (HasCallStack) => OsPath -> m ()


  -- | Removes a directory.  It must be empty.
  removeDirectory :: (HasCallStack) => OsPath -> m ()


  -- | Removes a directory entirely, including all its contents.
  removeDirectoryRecursively :: (HasCallStack) => OsPath -> m ()
  removeDirectoryRecursively path =
    ( do
        entries <- listDirectoryRecursively path []
        forM_ (sortOn (Down . snd) entries) $ \(fileType, entry) ->
          case fileType of
            Directory -> removeDirectoryRecursively $ path </> entry
            File -> removeFile $ path </> entry
            Symlink -> removeFile $ path </> entry
        removeDirectory path
    )
      `mapError` (`ioePrependLocation` "removeDirectoryRecursively")


  -- | Removes a directory root only when it has the expected identity.
  --
  -- Returns 'False' without removing anything when the path is absent, its
  -- identity differs, or the interpreter cannot verify identities.
  -- Filesystem-backed implementations must bind the final identity check to
  -- the entry being removed, for example by atomically moving it to an
  -- exclusively allocated quarantine path before checking and recursively
  -- deleting it.  A raced replacement may be moved briefly and restored.
  -- Restoration failures must name the preserved quarantine path in the
  -- resulting error.
  removeDirectoryRecursivelyIfIdentity
    :: (HasCallStack) => OsPath -> FileIdentity -> m Bool
  removeDirectoryRecursivelyIfIdentity path expectedIdentity = do
    actualIdentity <- getFileIdentity path
    if actualIdentity == Just expectedIdentity
      then removeDirectoryRecursively path >> return True
      else return False


  -- | Lists all files and directories in a directory except for @.@ and @..@,
  -- without recursing into subdirectories.
  listDirectory :: (HasCallStack) => OsPath -> m [OsPath]


  -- | Lists all files and directories in a directory recursively.  It doesn't
  -- include @.@ and @..@.  Paths are relative to the given directory,
  -- and directories always go before their contents.
  --
  -- Note that it doesn't follow symbolic links.  Instead, it returns the
  -- symbolic links themselves with the 'Symlink' file type.
  -- Filesystem-backed implementations must keep each traversed directory
  -- pinned and open child directories without following links, so a concurrent
  -- pathname replacement cannot redirect traversal.
  -- Entries that vanish during traversal may be omitted; use
  -- 'listDirectoryRecursivelyStrict' when membership must be consistent.
  listDirectoryRecursively
    :: (HasCallStack)
    => OsPath
    -- ^ The directory to list recursively.
    -> [FilePattern]
    -- ^ The file patterns to ignore.  If a directory matches one of these
    -- patterns, then its contents will not be listed recursively either.
    -> m [(FileType, OsPath)]
    -- ^ The list of pairs of file types and paths.  The paths are relative
    -- to the given directory.
  listDirectoryRecursively path ignorePatterns =
    listDirectoryRecursively'
      TolerateVanishedEntries
      path
      (step_ ignorePatterns)
      `mapError` (`ioePrependLocation` "listDirectoryRecursively")


  -- | Lists a directory recursively while rejecting names that vanish after
  -- enumeration.
  --
  -- This is the strict counterpart of 'listDirectoryRecursively' for callers
  -- that require a consistent membership snapshot.  Implementations must
  -- throw a does-not-exist 'IOError' when a name returned by directory
  -- enumeration disappears before it can be classified or opened.
  listDirectoryRecursivelyStrict
    :: (HasCallStack)
    => OsPath
    -- ^ The directory to list recursively.
    -> [FilePattern]
    -- ^ The file patterns to ignore.
    -> m [(FileType, OsPath)]
  listDirectoryRecursivelyStrict path ignorePatterns =
    listDirectoryRecursively'
      RejectVanishedEntries
      path
      (step_ ignorePatterns)
      `mapError` (`ioePrependLocation` "listDirectoryRecursivelyStrict")


  -- | Gets the size of a file in bytes.  If the file doesn't exist or is
  -- a directory, then it throws an 'IOError'.
  getFileSize :: (HasCallStack) => OsPath -> m Integer


  -- | Gets a stable identity for an entry without following symbolic links.
  --
  -- Returns 'Nothing' when the entry does not exist or the interpreter cannot
  -- represent filesystem identities.
  getFileIdentity :: (HasCallStack) => OsPath -> m (Maybe FileIdentity)
  getFileIdentity _ = return Nothing


  -- | Captures a regular file's identity and change metadata without following
  -- symbolic links.
  --
  -- Returns 'Nothing' when the entry is not a regular file, does not exist, or
  -- the interpreter cannot represent a stable snapshot.
  getFileSnapshot :: (HasCallStack) => OsPath -> m (Maybe FileSnapshot)
  getFileSnapshot _ = return Nothing


  -- | Captures an entry's identity and portable mode in one observation.
  --
  -- Filesystem-backed implementations must derive both values from the same
  -- stat result or open handle.  Returns 'Nothing' when the entry does not
  -- exist or the interpreter cannot provide a stable identity.
  getFileModeSnapshot
    :: (HasCallStack) => OsPath -> m (Maybe FileModeSnapshot)
  getFileModeSnapshot path = do
    identityBefore <- getFileIdentity path
    case identityBefore of
      Nothing -> return Nothing
      Just identity -> do
        mode <- getPortableMode path
        identityAfter <- getFileIdentity path
        return $
          if identityAfter == Just identity
            then Just $ FileModeSnapshot identity mode
            else Nothing


  -- | Observes the portable permission state of a filesystem entry without
  -- following symbolic links.  Throws an 'IOError' when the entry does not
  -- exist.  Fields the platform cannot observe are 'Nothing'.
  getPortableMode :: (HasCallStack) => OsPath -> m PortableMode


  -- | Applies exact POSIX permission bits to a filesystem entry.  On
  -- platforms without POSIX permission bits (Windows), only the writable
  -- distinction is applied via the read-only attribute; callers that need
  -- a stronger guarantee must warn or fail themselves rather than assume
  -- the whole mode was enforced.  Throws an 'IOError' when the entry does
  -- not exist.
  setPortableMode :: (HasCallStack) => OsPath -> Word -> m ()


  -- | Sets or clears only the owner-write permission (the read-only
  -- attribute on Windows), leaving every other permission bit unchanged.
  -- This is the primitive used to temporarily widen a read-only entry
  -- before mutating it and to restore the previous state afterwards.
  -- Throws an 'IOError' when the entry does not exist.
  setPortableWritable :: (HasCallStack) => OsPath -> Bool -> m ()


  -- | Creates a symbolic link.  The target may be absolute or relative;
  -- a relative target is resolved from the directory containing the link.
  -- The 'FileType' selects the intrinsic link type on Windows (file link
  -- or directory link) and is ignored on POSIX.  Throws an 'IOError' when
  -- the link path already exists, including when it is a broken symbolic
  -- link.
  createSymbolicLink
    :: (HasCallStack)
    => OsPath
    -- ^ The path the link points at.
    -> OsPath
    -- ^ The path of the symbolic link to create.
    -> FileType
    -- ^ The intrinsic link type ('Directory' for a directory link;
    -- everything else creates a file link).
    -> m ()


-- | Writes a sibling temporary file and atomically replaces the destination.
writeFileAtomically
  :: (HasCallStack, MonadFileSystem m)
  => OsPath
  -- ^ Destination file.
  -> FilePath
  -- ^ Temporary filename template.
  -> ByteString
  -- ^ Complete replacement contents.
  -> m ()
writeFileAtomically destination template contents = do
  let directory = takeDirectory destination
  temporary <- writeTemporaryFile directory template contents
  ( do
      destinationExists <- exists destination
      when destinationExists $
        copyFilePermissions destination temporary
      replaceFile temporary destination
    )
    `catchError` \err -> do
      temporaryExists <- exists temporary
      when temporaryExists $ removeFile temporary
      throwError err


createFileAtomicallyWithDefaultPermissionsIO
  :: OsPath -> FilePath -> ByteString -> IO ()
createFileAtomicallyWithDefaultPermissionsIO destination template contents = do
  directory <- decodeFS $ takeDirectory destination
  Exception.bracketOnError
    (openBinaryTempFileWithDefaultPermissions directory template)
    discardTemporary
    publishTemporary
 where
  discardTemporary (temporary, handle) = do
    hClose handle `catchError` const (return ())
    Directory.removeFile temporary `catchError` const (return ())

  publishTemporary (temporary, handle) = do
    Data.ByteString.hPut handle contents
    hFlush handle
    hClose handle
    temporaryPath <- encodeFS temporary
    renameDirectoryNoReplaceIO temporaryPath destination
      `catchError` \err ->
        if isNoReplaceUnsupportedError err
          then replaceFileIO temporaryPath destination
          else throwError err


-- | Tests whether a no-replace rename is unsupported by the filesystem.
--
-- This classification is shared by atomic file creation and the platform
-- no-replace implementation so unsupported filesystems can retain atomic
-- publication through their ordinary rename operation.
isNoReplaceUnsupportedError :: IOError -> Bool
isNoReplaceUnsupportedError err =
  ioeGetErrorType err == UnsupportedOperation


-- | Constructs the error used when atomic no-replace rename is unavailable.
--
-- The supplied path is attached to the error for diagnostics.
noReplaceUnsupportedError :: FilePath -> IOError
noReplaceUnsupportedError destination =
  mkIOError
    UnsupportedOperation
    "renameFileNoReplace"
    Nothing
    (Just destination)
    `ioeSetErrorString` "filesystem lacks atomic no-replace rename"


listDirectoryRecursively'
  :: (HasCallStack, MonadFileSystem m)
  => DirectoryTraversalMode
  -> OsPath
  -> Step ()
  -> m [(FileType, OsPath)]
listDirectoryRecursively' mode path ptnStep = do
  unfilteredEntries <- listDirectory path
  entriesWithSteps <- forM unfilteredEntries $ \entry -> do
    decoded <- decodePath entry
    let nextStep = stepApply ptnStep decoded
    return (entry, nextStep)
  let filteredEntries =
        [ (entry, step)
        | (entry, step) <- entriesWithSteps
        , null $ stepDone step
        ]
  (symlinks, entries') <-
    partitionM (isSymlink . (path </>) . fst) filteredEntries
  (dirs, files) <- partitionM (isDirectory . (path </>) . fst) entries'
  symlinks' <- forM symlinks $ \(symlink, _) -> return (Symlink, symlink)
  files' <- forM files $ \(file, _) -> do
    when (mode == RejectVanishedEntries) $ do
      let filePath = path </> file
      void (getPortableMode filePath) `catchError` \err ->
        if isDoesNotExistError err
          then throwVanishedDirectoryEntry filePath
          else throwError err
    return (File, file)
  dirs' <- forM dirs $ \(dir, step) -> do
    subentries <- listDirectoryRecursively' mode (path </> dir) step
    return $ (Directory, dir) : (fmap (dir </>) <$> subentries)
  return $ files' ++ symlinks' ++ concat dirs'


throwVanishedDirectoryEntry
  :: (MonadFileSystem m) => OsPath -> m a
throwVanishedDirectoryEntry path = do
  path' <- decodePath path
  throwError $
    mkIOError
      doesNotExistErrorType
      "listDirectoryRecursivelyStrict"
      Nothing
      (Just path')


data DirectoryTraversalMode
  = TolerateVanishedEntries
  | RejectVanishedEntries
  deriving (Eq)


handleVanishedEntry
  :: DirectoryTraversalMode -> OsPath -> IO [a] -> IO [a]
handleVanishedEntry mode path action =
  action `catchError` handle
 where
  handle err
    | not $ isDoesNotExistError err = throwError err
    | mode == TolerateVanishedEntries = return []
    | ioeGetLocation err == "listDirectoryRecursivelyStrict" = throwError err
    | otherwise = do
        path' <- decodeFS path
        throwError $
          ioeSetFileName
            (ioeSetLocation err "listDirectoryRecursivelyStrict")
            path'

#ifdef mingw32_HOST_OS
listDirectoryRecursivelyIO
  :: DirectoryTraversalMode
  -> OsPath
  -> [FilePattern]
  -> IO [(FileType, OsPath)]
listDirectoryRecursivelyIO mode path ignorePatterns = do
  resolved <- OsDirectory.canonicalizePath path
  withPinnedDirectory resolved $ go resolved (step_ ignorePatterns)
 where
  go current ptnStep = do
    unfilteredEntries <- OsDirectory.listDirectory current
    entriesWithSteps <- forM unfilteredEntries $ \entry -> do
      decoded <- decodeFS entry
      return (entry, stepApply ptnStep decoded)
    fmap concat $
      forM
        [ pair
        | pair@(_, nextStep) <- entriesWithSteps
        , null $ stepDone nextStep
        ]
        $ \(entry, nextStep) -> do
          let entryPath = current </> entry
          handleVanishedEntry mode entryPath $
            withPinnedEntry entryPath $ \fileType -> case fileType of
              Directory -> do
                descendants <- go entryPath nextStep
                return $
                  (Directory, entry)
                    : fmap (fmap (entry </>)) descendants
              entryType -> return [(entryType, entry)]


withPinnedDirectory :: OsPath -> IO a -> IO a
withPinnedDirectory path action =
  withPinnedEntry path $ \fileType ->
    if fileType == Directory
      then action
      else throwPinnedDirectoryError path "not a directory"


withPinnedEntry :: OsPath -> (FileType -> IO a) -> IO a
withPinnedEntry path action =
  withEntryHandle
    path
    ( Win32.fILE_SHARE_READ
        .|. Win32.fILE_SHARE_WRITE
        .|. Win32.fILE_SHARE_DELETE
    )
    $ \information -> do
      let attributes = information.bhfiFileAttributes
      if attributes .&. Win32.fILE_ATTRIBUTE_REPARSE_POINT /= 0
        then action Symlink
        else
          if attributes .&. Win32.fILE_ATTRIBUTE_DIRECTORY /= 0
            then
              withPinnedDirectoryEntry
                path
                (fileIdentityFromInformation information)
                (action Directory)
            else action File


withPinnedDirectoryEntry
  :: OsPath -> FileIdentity -> IO a -> IO a
withPinnedDirectoryEntry path expectedIdentity action =
  withEntryHandle
    path
    (Win32.fILE_SHARE_READ .|. Win32.fILE_SHARE_WRITE)
    $ \information -> do
      let attributes = information.bhfiFileAttributes
          supported =
            attributes .&. Win32.fILE_ATTRIBUTE_REPARSE_POINT == 0
              && attributes .&. Win32.fILE_ATTRIBUTE_DIRECTORY /= 0
          unchanged =
            fileIdentityFromInformation information == expectedIdentity
      if supported && unchanged
        then action
        else throwPinnedDirectoryError path "directory changed during traversal"


withEntryHandle
  :: OsPath
  -> Win32.DWORD
  -> (Win32.BY_HANDLE_FILE_INFORMATION -> IO a)
  -> IO a
withEntryHandle path shareMode action = do
  path' <- decodeFS path
  Exception.bracket
    ( Win32.createFile
        path'
        Win32.gENERIC_NONE
        shareMode
        Nothing
        Win32.oPEN_EXISTING
        (Win32.fILE_FLAG_BACKUP_SEMANTICS .|. fileFlagOpenReparsePoint)
        Nothing
    )
    Win32.closeHandle
    $ \handle ->
      Win32.getFileInformationByHandle handle >>= action
 where
  fileFlagOpenReparsePoint = 0x00200000


throwPinnedDirectoryError :: OsPath -> String -> IO a
throwPinnedDirectoryError path message = do
  path' <- decodeFS path
  ioError $
    mkIOError InappropriateType "listDirectoryRecursively" Nothing (Just path')
      `ioeSetErrorString` message
#else
data DirectoryStream


foreign import ccall unsafe "dojang_fdopendir"
  c_fdopendir :: CInt -> IO (Ptr DirectoryStream)


foreign import ccall unsafe "dojang_readdir_name"
  -- Returns 1 for one copied name, 0 at end of stream, or a negated errno.
  c_readdirName
    :: Ptr DirectoryStream -> CString -> CSize -> IO CInt


foreign import ccall unsafe "dojang_closedir"
  c_closedir :: Ptr DirectoryStream -> IO ()


foreign import ccall unsafe "dojang_file_type_at"
  -- Returns 1 for a directory, 2 for a symbolic link, 3 for another entry,
  -- or a negated errno.
  c_fileTypeAt :: CInt -> CString -> IO CInt


listDirectoryRecursivelyIO
  :: DirectoryTraversalMode
  -> OsPath
  -> [FilePattern]
  -> IO [(FileType, OsPath)]
listDirectoryRecursivelyIO mode path ignorePatterns = do
  resolved <- OsDirectory.canonicalizePath path
  path' <- decodeFS resolved
  withPinnedDirectoryFd Nothing path' $ \descriptor ->
    go resolved descriptor (step_ ignorePatterns)
 where
  go current descriptor ptnStep = do
    unfilteredEntries <- listDirectoryFd descriptor
    entriesWithSteps <- forM unfilteredEntries $ \entry -> do
      decoded <- decodeFS entry
      return (entry, decoded, stepApply ptnStep decoded)
    fmap concat $
      forM
        [ triple
        | triple@(_, _, nextStep) <- entriesWithSteps
        , null $ stepDone nextStep
        ]
        $ \(entry, entry', nextStep) -> do
          let entryPath = current </> entry
          handleVanishedEntry mode entryPath $ do
            entryType <- getFileTypeAt descriptor entry'
            case entryType of
              Symlink -> return [(Symlink, entry)]
              Directory ->
                withPinnedDirectoryFd
                  (Just descriptor)
                  entry'
                  $ \childDescriptor -> do
                    descendants <- go entryPath childDescriptor nextStep
                    return $
                      (Directory, entry)
                        : fmap (fmap (entry </>)) descendants
              File -> return [(File, entry)]


withPinnedDirectoryFd
  :: Maybe Fd -> FilePath -> (Fd -> IO a) -> IO a
withPinnedDirectoryFd parent path action =
  Exception.bracket
    ( Posix.openFdAt
        parent
        path
        Posix.ReadOnly
        Posix.defaultFileFlags
          { Posix.nonBlock = True
          , Posix.nofollow = True
          , Posix.cloexec = True
          , Posix.directory = True
          }
    )
    Posix.closeFd
    action


listDirectoryFd :: Fd -> IO [OsPath]
listDirectoryFd descriptor =
  Exception.bracket acquire c_closedir $ \stream -> do
    encoding <- getFileSystemEncoding
    allocaBytes maximumNameBytes $ \buffer -> go encoding stream buffer []
 where
  acquire = do
    duplicated <- Posix.dup descriptor
    stream <-
      CError.throwErrnoIfNull "fdopendir" (c_fdopendir $ fromIntegral duplicated)
        `Exception.onException` Posix.closeFd duplicated
    return stream

  go encoding stream buffer entries = do
    result <-
      c_readdirName
        stream
        buffer
        (fromIntegral maximumNameBytes)
    case compare result 0 of
      LT -> throwErrnoCode "readdir" $ negate result
      EQ -> return $ reverse entries
      GT -> do
        entry <- GHC.peekCString encoding buffer
        if entry == "." || entry == ".."
          then go encoding stream buffer entries
          else do
            encoded <- encodeFS entry
            go encoding stream buffer $ encoded : entries

  maximumNameBytes = 4096


getFileTypeAt :: Fd -> FilePath -> IO FileType
getFileTypeAt descriptor entry =
  PosixInternal.withFilePath entry $ \entryPath -> do
    result <- c_fileTypeAt (fromIntegral descriptor) entryPath
    case result of
      1 -> return Directory
      2 -> return Symlink
      3 -> return File
      value
        | value < 0 ->
            throwErrnoCode "fstatat" $ negate value
        | otherwise ->
            ioError $ userError "fstatat returned an invalid file type"


throwErrnoCode :: String -> CInt -> IO a
throwErrnoCode location err =
  Exception.throwIO $
    CError.errnoToIOError
      location
      (CError.Errno err)
      Nothing
      Nothing
#endif

#if defined(linux_HOST_OS)
atFdcwd :: CInt
atFdcwd = -100


renameNoreplace :: CUInt
renameNoreplace = 1


foreign import ccall unsafe "dojang_renameat2"
  c_renameat2
    :: CInt
    -> CString
    -> CInt
    -> CString
    -> CUInt
    -> IO CInt
#elif defined(darwin_HOST_OS)
renameExcl :: CUInt
renameExcl = 4


foreign import ccall unsafe "renamex_np"
  c_renamex_np
    :: CString
    -> CString
    -> CUInt
    -> IO CInt
#endif

#ifdef mingw32_HOST_OS
replaceFileIO :: OsPath -> OsPath -> IO ()
replaceFileIO source destination = do
  destinationExists <- doesPathExist destination
  if not destinationExists
    then OsDirectory.renameFile source destination
    else do
      permissions <- OsDirectory.getPermissions destination
      -- MoveFileEx cannot replace a destination with the read-only attribute.
      -- The source already carries the original permissions, so a successful
      -- replacement restores the attribute as part of the move.
      OsDirectory.setPermissions
        destination
        (Directory.setOwnerWritable True permissions)
      OsDirectory.renameFile source destination `catchError` \err -> do
        destinationStillExists <- doesPathExist destination
        when destinationStillExists $
          OsDirectory.setPermissions destination permissions
        sourceStillExists <- doesPathExist source
        when sourceStillExists $ do
          sourcePermissions <- OsDirectory.getPermissions source
          OsDirectory.setPermissions
            source
            (Directory.setOwnerWritable True sourcePermissions)
        throwError err


copyFilePermissionsIO :: OsPath -> OsPath -> IO ()
copyFilePermissionsIO source destination = do
  permissions <- OsDirectory.getPermissions source
  OsDirectory.setPermissions destination permissions


isRegularFileIO :: OsPath -> IO Bool
isRegularFileIO path =
  (&&) <$> doesFileExist path <*> (not <$> isSymlink path)


withRegularFileHandleIO
  :: OsPath
  -> (FileSnapshot -> IO Bool -> Handle -> IO a)
  -> IO (Maybe a)
withRegularFileHandleIO path action = do
  path' <- decodeFS path
  Exception.bracket
    (openBinaryFile path' ReadMode)
    hClose
    ( \handle -> do
        regularFile <- hIsSeekable handle
        if regularFile
          then
            Win32.withHandleToHANDLE handle $ \nativeHandle -> do
              snapshot <- getFileSnapshotFromHandle nativeHandle
              let unchanged =
                    (== snapshot) <$> getFileSnapshotFromHandle nativeHandle
              Just <$> action snapshot unchanged handle
          else return Nothing
    )


readRegularFileIO :: OsPath -> IO (Maybe ByteString)
readRegularFileIO path =
  withRegularFileHandleIO path $ \_ _ -> Data.ByteString.hGetContents


getFileIdentityIO :: OsPath -> IO (Maybe FileIdentity)
getFileIdentityIO path = do
  path' <- decodeFS path
  ( Just
      <$> Exception.bracket
        ( Win32.createFile
            path'
            Win32.gENERIC_NONE
            ( Win32.fILE_SHARE_READ
                .|. Win32.fILE_SHARE_WRITE
                .|. Win32.fILE_SHARE_DELETE
            )
            Nothing
            Win32.oPEN_EXISTING
            (Win32.fILE_FLAG_BACKUP_SEMANTICS .|. fileFlagOpenReparsePoint)
            Nothing
        )
        Win32.closeHandle
        ( \handle -> do
            information <- Win32.getFileInformationByHandle handle
            return $ fileIdentityFromInformation information
        )
    )
    `catchError` \err ->
      if isDoesNotExistError err then return Nothing else throwError err
 where
 fileFlagOpenReparsePoint = 0x00200000


getFileModeSnapshotIO :: OsPath -> IO (Maybe FileModeSnapshot)
getFileModeSnapshotIO path = do
  path' <- decodeFS path
  ( Just
      <$> Exception.bracket
        ( Win32.createFile
            path'
            Win32.gENERIC_NONE
            ( Win32.fILE_SHARE_READ
                .|. Win32.fILE_SHARE_WRITE
                .|. Win32.fILE_SHARE_DELETE
            )
            Nothing
            Win32.oPEN_EXISTING
            (Win32.fILE_FLAG_BACKUP_SEMANTICS .|. fileFlagOpenReparsePoint)
            Nothing
        )
        Win32.closeHandle
        ( \handle -> do
            information <- Win32.getFileInformationByHandle handle
            return $
              FileModeSnapshot
                (fileIdentityFromInformation information)
                PortableMode
                  { posixBits = Nothing
                  , writable =
                      information.bhfiFileAttributes
                        .&. Win32.fILE_ATTRIBUTE_READONLY
                        == 0
                  }
        )
    )
    `catchError` \err ->
      if isDoesNotExistError err then return Nothing else throwError err
 where
  fileFlagOpenReparsePoint = 0x00200000


fileIdentityFromInformation
  :: Win32.BY_HANDLE_FILE_INFORMATION -> FileIdentity
fileIdentityFromInformation information =
  FileIdentity
    (fromIntegral information.bhfiVolumeSerialNumber)
    (fromIntegral information.bhfiFileIndex)


getFileSnapshotIO :: OsPath -> IO (Maybe FileSnapshot)
getFileSnapshotIO path = do
  path' <- decodeFS path
  Exception.bracket
    ( Win32.createFile
        path'
        Win32.gENERIC_NONE
        ( Win32.fILE_SHARE_READ
            .|. Win32.fILE_SHARE_WRITE
            .|. Win32.fILE_SHARE_DELETE
        )
        Nothing
        Win32.oPEN_EXISTING
        (Win32.fILE_FLAG_BACKUP_SEMANTICS .|. fileFlagOpenReparsePoint)
        Nothing
    )
    Win32.closeHandle
    ( \handle -> do
        information <- Win32.getFileInformationByHandle handle
        let unsupportedAttributes =
              Win32.fILE_ATTRIBUTE_DIRECTORY
                .|. Win32.fILE_ATTRIBUTE_REPARSE_POINT
        if information.bhfiFileAttributes .&. unsupportedAttributes == 0
          then
            Just . fileSnapshotFromInformation information
              <$> getFileChangeTime handle
          else return Nothing
    )
    `catchError` \err ->
      if isDoesNotExistError err then return Nothing else throwError err
 where
  fileFlagOpenReparsePoint = 0x00200000


fileSnapshotFromInformation
  :: Win32.BY_HANDLE_FILE_INFORMATION -> Int64 -> FileSnapshot
fileSnapshotFromInformation information changeTime =
  FileSnapshot
    (fileIdentityFromInformation information)
    (fromIntegral information.bhfiSize)
    (fileTimeValue information.bhfiLastWriteTime)
    (Just $ toRational changeTime)
 where
  fileTimeValue (Win32Time.FILETIME value) = toRational value


getFileSnapshotFromHandle :: Win32.HANDLE -> IO FileSnapshot
getFileSnapshotFromHandle handle = do
  information <- Win32.getFileInformationByHandle handle
  fileSnapshotFromInformation information <$> getFileChangeTime handle


getFileChangeTime :: Win32.HANDLE -> IO Int64
getFileChangeTime handle =
  allocaBytes fileBasicInfoSize $ \buffer -> do
    Win32.failIfFalse_ "GetFileInformationByHandleEx" $
      c_getFileInformationByHandleEx
        handle
        fileBasicInfoClass
        buffer
        (fromIntegral fileBasicInfoSize)
    peekByteOff buffer changeTimeOffset
 where
  -- FILE_INFO_BY_HANDLE_CLASS uses 0 for FileBasicInfo.  FILE_BASIC_INFO is
  -- five naturally aligned fields: four 64-bit times followed by a DWORD.
  fileBasicInfoClass = 0
  fileBasicInfoSize = 40
  changeTimeOffset = 24


getSymbolicLinkTypeIO :: OsPath -> IO FileType
getSymbolicLinkTypeIO path = do
  path' <- decodeFS path
  attributes <- Win32.getFileAttributes path'
  return $
    if attributes .&. Win32.fILE_ATTRIBUTE_DIRECTORY /= 0
      then Directory
      else File


getPortableModeIO :: OsPath -> IO PortableMode
getPortableModeIO path = do
  permissions <- OsDirectory.getPermissions path
  return
    PortableMode
      { posixBits = Nothing
      , writable = Directory.writable permissions
      }


setPortableModeIO :: OsPath -> Word -> IO ()
setPortableModeIO path bits =
  setPortableWritableIO path $ bits .&. 0o200 /= 0


setPortableWritableIO :: OsPath -> Bool -> IO ()
setPortableWritableIO path writable' = do
  permissions <- OsDirectory.getPermissions path
  OsDirectory.setPermissions path $
    Directory.setOwnerWritable writable' permissions


createPrivateDirectoryIO :: OsPath -> IO ()
createPrivateDirectoryIO path = do
  path' <- decodeFS path
  ensurePersistentAcls path'
  withPrivateSecurityAttributes $ \attributes ->
    Win32.createDirectory path' $ Just attributes


ensurePersistentAcls :: FilePath -> IO ()
ensurePersistentAcls path = do
  absolutePath <- Directory.makeAbsolute path
  Win32String.withTString absolutePath $ \nativePath ->
    Win32String.withTStringBufferLen 32768 $ \(rootBuffer, rootLength) -> do
      Win32.failIfFalse_ "GetVolumePathNameW" $
        c_getVolumePathName
          nativePath
          rootBuffer
          (fromIntegral rootLength)
      rootPath <- Win32String.peekTString rootBuffer
      Win32String.withTString rootPath $ \nativeRoot ->
        alloca $ \flags -> do
          Win32.failIfFalse_ "GetVolumeInformationW" $
            c_getVolumeInformation
              nativeRoot
              nullPtr
              0
              nullPtr
              nullPtr
              flags
              nullPtr
              0
          capabilities <- peek flags
          unless (capabilities .&. filePersistentAcls /= 0) $
            ioError $
              userError
                "filesystem cannot enforce a private directory ACL"
 where
  filePersistentAcls = 0x00000008


withPrivateSecurityAttributes
  :: (Win32.LPSECURITY_ATTRIBUTES -> IO a) -> IO a
withPrivateSecurityAttributes action =
  Win32String.withTString "D:P(A;OICI;FA;;;OW)" $ \descriptorText ->
    alloca $ \descriptorAddress -> do
      Win32.failIfFalse_
        "ConvertStringSecurityDescriptorToSecurityDescriptorW"
        $ c_convertStringSecurityDescriptor
          descriptorText
          1
          descriptorAddress
          nullPtr
      descriptor <- peek descriptorAddress
      Exception.bracket
        (return descriptor)
        (\value -> c_localFree value >> return ())
        $ \value ->
          alloca $ \attributes -> do
            poke
              attributes
              Win32.SECURITY_ATTRIBUTES
                { Win32.nLength =
                    fromIntegral $
                      sizeOf (undefined :: Win32.SECURITY_ATTRIBUTES)
                , Win32.lpSecurityDescriptor = castPtr value
                , Win32.bInheritHandle = False
                }
            action attributes


renameDirectoryNoReplaceIO :: OsPath -> OsPath -> IO ()
renameDirectoryNoReplaceIO source destination = do
  source' <- decodeFS source
  destination' <- decodeFS destination
  Win32.moveFile source' destination'


#else
replaceFileIO :: OsPath -> OsPath -> IO ()
replaceFileIO = OsDirectory.renameFile


copyFilePermissionsIO :: OsPath -> OsPath -> IO ()
copyFilePermissionsIO source destination = do
  source' <- decodeFS source
  destination' <- decodeFS destination
  mode <- Posix.fileMode <$> Posix.getFileStatus source'
  Posix.setFileMode destination' mode


isRegularFileIO :: OsPath -> IO Bool
isRegularFileIO path = do
  path' <- decodeFS path
  (Posix.isRegularFile <$> Posix.getSymbolicLinkStatus path')
    `catchError` \err ->
      if isDoesNotExistError err then return False else throwError err


withRegularFileHandleIO
  :: OsPath
  -> (FileSnapshot -> IO Bool -> Handle -> IO a)
  -> IO (Maybe a)
withRegularFileHandleIO path action = do
  path' <- decodeFS path
  Exception.mask $ \restore -> do
    descriptor <-
      Posix.openFd
        path'
        Posix.ReadOnly
        Posix.defaultFileFlags
          { Posix.nonBlock = True
          , Posix.cloexec = True
          }
    status <-
      restore (Posix.getFdStatus descriptor)
        `Exception.onException` Posix.closeFd descriptor
    if not $ Posix.isRegularFile status
      then Posix.closeFd descriptor >> return Nothing
      else do
        handle <-
          Posix.fdToHandle descriptor
            `Exception.onException` Posix.closeFd descriptor
        let unchanged = do
              current <- Posix.getFdStatus descriptor
              return $ fileSnapshotFromStatus current == snapshot
            snapshot = fileSnapshotFromStatus status
        ( Just
            <$> restore
              ( action
                  snapshot
                  unchanged
                  handle
              )
          )
          `Exception.finally` hClose handle


readRegularFileIO :: OsPath -> IO (Maybe ByteString)
readRegularFileIO path =
  withRegularFileHandleIO path $ \_ _ -> Data.ByteString.hGetContents


fileIdentityFromStatus :: Posix.FileStatus -> FileIdentity
fileIdentityFromStatus status =
  FileIdentity
    (fromIntegral $ Posix.deviceID status)
    (fromIntegral $ Posix.fileID status)


getFileIdentityIO :: OsPath -> IO (Maybe FileIdentity)
getFileIdentityIO path = do
  path' <- decodeFS path
  ( do
      status <- Posix.getSymbolicLinkStatus path'
      return $ Just $ fileIdentityFromStatus status
    )
    `catchError` \err ->
      if isDoesNotExistError err then return Nothing else throwError err


getFileModeSnapshotIO :: OsPath -> IO (Maybe FileModeSnapshot)
getFileModeSnapshotIO path = do
  path' <- decodeFS path
  ( do
      status <- Posix.getSymbolicLinkStatus path'
      return $
        Just $
          FileModeSnapshot
            (fileIdentityFromStatus status)
            ( portableModeFromBits $
                fromIntegral $
                  Posix.fileMode status .&. 0o777
            )
    )
    `catchError` \err ->
      if isDoesNotExistError err then return Nothing else throwError err


getFileSnapshotIO :: OsPath -> IO (Maybe FileSnapshot)
getFileSnapshotIO path = do
  path' <- decodeFS path
  ( do
      status <- Posix.getSymbolicLinkStatus path'
      return $
        if Posix.isRegularFile status
          then Just $ fileSnapshotFromStatus status
          else Nothing
    )
    `catchError` \err ->
      if isDoesNotExistError err then return Nothing else throwError err


fileSnapshotFromStatus :: Posix.FileStatus -> FileSnapshot
fileSnapshotFromStatus status =
  FileSnapshot
    (fileIdentityFromStatus status)
    (fromIntegral $ Posix.fileSize status)
    (toRational $ Posix.modificationTimeHiRes status)
    (Just $ toRational $ Posix.statusChangeTimeHiRes status)


getSymbolicLinkTypeIO :: OsPath -> IO FileType
getSymbolicLinkTypeIO _ = return File


getPortableModeIO :: OsPath -> IO PortableMode
getPortableModeIO path = do
  path' <- decodeFS path
  status <- Posix.getSymbolicLinkStatus path'
  return $ portableModeFromBits $ fromIntegral $ Posix.fileMode status .&. 0o777


setPortableModeIO :: OsPath -> Word -> IO ()
setPortableModeIO path bits = do
  path' <- decodeFS path
  Posix.setFileMode path' $ fromIntegral bits


setPortableWritableIO :: OsPath -> Bool -> IO ()
setPortableWritableIO path writable' = do
  path' <- decodeFS path
  mode <- Posix.fileMode <$> Posix.getFileStatus path'
  Posix.setFileMode path' $
    if writable'
      then mode .|. 0o200
      else mode .&. complement 0o200


createPrivateDirectoryIO :: OsPath -> IO ()
createPrivateDirectoryIO path = do
  path' <- decodeFS path
  PosixDirectory.createDirectory path' 0o700
  Posix.setFileMode path' 0o700
    `Exception.onException` OsDirectory.removeDirectory path


renameDirectoryNoReplaceIO :: OsPath -> OsPath -> IO ()
renameDirectoryNoReplaceIO source destination = do
  destination' <- decodeFS destination
#if defined(linux_HOST_OS)
  source' <- decodeFS source
  PosixInternal.withFilePath source' $ \sourcePath ->
    PosixInternal.withFilePath destination' $ \destinationPath ->
      checkNoReplaceResult
        [CError.eINVAL, CError.eNOSYS, CError.eNOTSUP, CError.eOPNOTSUPP]
        destination'
        $ c_renameat2
            atFdcwd
            sourcePath
            atFdcwd
            destinationPath
            renameNoreplace
#elif defined(darwin_HOST_OS)
  source' <- decodeFS source
  PosixInternal.withFilePath source' $ \sourcePath ->
    PosixInternal.withFilePath destination' $ \destinationPath ->
      checkNoReplaceResult
        [CError.eNOTSUP, CError.eOPNOTSUPP]
        destination'
        $ c_renamex_np sourcePath destinationPath renameExcl
#else
  _ <- decodeFS source
  throwNoReplaceUnsupported destination'
#endif


checkNoReplaceResult :: [CError.Errno] -> FilePath -> IO CInt -> IO ()
checkNoReplaceResult unsupportedErrors destination action = do
  result <- action
  when (result == -1) $ do
    err <- CError.getErrno
    if err `elem` unsupportedErrors
      then throwNoReplaceUnsupported destination
      else
        Exception.throwIO $
          CError.errnoToIOError
            "renameDirectory"
            err
            Nothing
            (Just destination)


throwNoReplaceUnsupported :: FilePath -> IO a
throwNoReplaceUnsupported destination =
  Exception.throwIO $ noReplaceUnsupportedError destination
#endif


removeDirectoryRecursivelyIfIdentityIO
  :: OsPath -> FileIdentity -> IO Bool
removeDirectoryRecursivelyIfIdentityIO path expectedIdentity =
  Exception.mask $ \restore -> do
    initialIdentity <- getFileIdentityIO path
    if initialIdentity /= Just expectedIdentity
      then return False
      else do
        quarantined <- quarantineDirectory maximumQuarantineAttempts path
        case quarantined of
          Nothing -> return False
          Just quarantine -> do
            actualIdentity <- getFileIdentityIO quarantine
            if actualIdentity == Just expectedIdentity
              then restore (removeDirectoryRecursivelyIO quarantine) >> return True
              else do
                restore (restoreQuarantine quarantine)
                return False
 where
  quarantineDirectory attempts source
    | attempts < 1 =
        throwError $
          userError "could not allocate a unique cleanup quarantine path"
    | otherwise = do
        randomBytes <- getEntropy 16
        name <-
          encodeFS $
            ".dojang-cleanup-" <> encodeHex (Data.ByteString.unpack randomBytes)
        let quarantine = takeDirectory source </> name
        ( ( retryOnPermissionErrorsOnWindows 10 $
              renameDirectoryNoReplaceIO source quarantine
          )
            >> return (Just quarantine)
          )
          `catchError` \err ->
            if isDoesNotExistError err
              then return Nothing
              else
                if isAlreadyExistsError err
                  then quarantineDirectory (attempts - 1) source
                  else throwError err

  restoreQuarantine quarantine =
    ( retryOnPermissionErrorsOnWindows 10 $
        renameDirectoryNoReplaceIO quarantine path
    )
      `catchError` \err -> do
        quarantine' <- decodeFS quarantine
        throwError $
          userError $
            "cleanup preserved a concurrently replaced directory at "
              <> quarantine'
              <> ": "
              <> Exception.displayException err

  maximumQuarantineAttempts :: Int
  maximumQuarantineAttempts = 128

  encodeHex = concatMap $ \byte ->
    let encoded = showHex byte ""
    in if length encoded < 2 then '0' : encoded else encoded


removeDirectoryRecursivelyIO :: OsPath -> IO ()
removeDirectoryRecursivelyIO =
  retryOnPermissionErrorsOnWindows 10 . removeDirectoryRecursive


retryOnPermissionErrorsOnWindows :: Int -> IO () -> IO ()
retryOnPermissionErrorsOnWindows retry action
  | os /= "mingw32" = action
  | retry < 1 = action
  | otherwise =
      action `catchError` \err ->
        if isPermissionError err
          then do
            threadDelay 100
            retryOnPermissionErrorsOnWindows (retry - 1) action
          else throwError err


readRegularFileBoundedIO :: Int -> OsPath -> IO BoundedFileRead
readRegularFileBoundedIO limit path = do
  result <-
    withRegularFileHandleIO path $ \_ unchanged handle -> do
      bounded <- readHandleBounded limit handle
      case bounded of
        Nothing -> return FileSizeLimitExceeded
        Just contents -> do
          stable <- unchanged
          return $
            if stable
              then BoundedFileContents contents
              else FileChangedDuringRead
  return $ maybe NotRegularFile id result


readHandleBounded :: Int -> Handle -> IO (Maybe ByteString)
readHandleBounded limit handle = go limit []
 where
  go remaining chunks = do
    chunk <-
      Data.ByteString.hGetSome
        handle
        (max 1 $ min 32768 $ remaining + 1)
    if Data.ByteString.null chunk
      then return $ Just $ Data.ByteString.concat $ reverse chunks
      else
        let chunkLength = Data.ByteString.length chunk
        in if chunkLength > remaining
             then return Nothing
             else go (remaining - chunkLength) (chunk : chunks)


copyRegularFileIO :: OsPath -> OsPath -> IO Bool
copyRegularFileIO source destination = do
  result <-
    withRegularFileHandleIO source $ \_ _ sourceHandle -> do
      destination' <- decodeFS destination
      Exception.bracket
        (openBinaryFile destination' WriteMode)
        hClose
        (copyHandle sourceHandle)
  return $ case result of
    Nothing -> False
    Just () -> True


copyRegularFileWithSnapshotIO
  :: FileSnapshot -> OsPath -> OsPath -> IO Bool
copyRegularFileWithSnapshotIO expectedSnapshot source destination = do
  result <-
    withRegularFileHandleIO
      source
      $ \actualSnapshot sourceUnchanged sourceHandle ->
        if actualSnapshot /= expectedSnapshot
          then return False
          else Exception.mask $ \restore -> do
            destinationDirectory <- decodeFS $ takeDirectory destination
            (temporaryPath, temporaryHandle) <-
              openBinaryTempFile destinationDirectory ".dojang-copy-"
            let discardTemporary = do
                  hClose temporaryHandle `catchError` const (return ())
                  Directory.removeFile temporaryPath
                    `catchError` const (return ())
            unchanged <-
              restore
                (copyHandle sourceHandle temporaryHandle >> sourceUnchanged)
                `Exception.onException` discardTemporary
            hClose temporaryHandle
              `Exception.onException` discardTemporary
            if not unchanged
              then discardTemporary >> return False
              else do
                temporary <- encodeFS temporaryPath
                replaceFileIO temporary destination
                  `Exception.onException` discardTemporary
                return True
  return $ maybe False id result


copyHandle :: Handle -> Handle -> IO ()
copyHandle source destination = do
  chunk <- Data.ByteString.hGetSome source 32768
  unless (Data.ByteString.null chunk) $ do
    Data.ByteString.hPut destination chunk
    copyHandle source destination


validateFileLockPath :: OsPath -> IO ()
validateFileLockPath lockPath = do
  symbolicLink <-
    pathIsSymbolicLink lockPath `catchError` \err ->
      if isDoesNotExistError err then return False else throwError err
  present <- doesPathExist lockPath
  regularFile <- isRegularFileIO lockPath
  when (symbolicLink || present && not regularFile) $ do
    lockPath' <- decodeFS lockPath
    throwError $
      mkIOError InappropriateType "withFileLock" Nothing (Just lockPath')
        `ioeSetErrorString` "lock path is not a regular file"


instance MonadFileSystem IO where
  encodePath = encodeFS


  decodePath = decodeFS


  getCurrentDirectory = OsDirectory.getCurrentDirectory


  getHomeDirectory = OsDirectory.getHomeDirectory


  exists = doesPathExist


  isFile = doesFileExist


  isRegularFile = isRegularFileIO


  isDirectory = doesDirectoryExist


  isSymlink path =
    pathIsSymbolicLink path `catchError` \e ->
      if isDoesNotExistError e then return False else throwError e


  readFile src = decodePath src >>= Data.ByteString.readFile


  readRegularFile = readRegularFileIO


  readRegularFileBounded = readRegularFileBoundedIO


  copyRegularFile = copyRegularFileIO


  copyRegularFileWithSnapshot = copyRegularFileWithSnapshotIO


  writeFile dst contents = do
    dst' <- decodePath dst
    Data.ByteString.writeFile dst' contents


  createFileAtomicallyWithDefaultPermissions =
    createFileAtomicallyWithDefaultPermissionsIO


  replaceFile = replaceFileIO


  renameDirectory = renameDirectoryNoReplaceIO


  copyFileWithMetadata = OsDirectory.copyFileWithMetadata


  copyFilePermissions = copyFilePermissionsIO


  writeTemporaryFile directory template contents = do
    directory' <- decodePath directory
    (filename, handle) <- openBinaryTempFile directory' template
    ( do
        Data.ByteString.hPut handle contents
        hFlush handle
        hClose handle
        encodePath filename
      )
      `catchError` \err -> do
        hClose handle `catchError` const (return ())
        Directory.removeFile filename `catchError` const (return ())
        throwError err


  withFileLock lockPath action = do
    validateFileLockPath lockPath
    lockPath' <- decodePath lockPath
    FileLock.withFileLock lockPath' FileLock.Exclusive $ const $ do
      validateFileLockPath lockPath
      action


  canonicalizePath = OsDirectory.canonicalizePath


  readSymlinkTarget = getSymbolicLinkTarget


  getSymbolicLinkType = getSymbolicLinkTypeIO


  createDirectory = OsDirectory.createDirectory


  createPrivateDirectory = createPrivateDirectoryIO


  removeFile = OsDirectory.removeFile


  removeDirectory = OsDirectory.removeDirectory


  -- See also: https://github.com/jaspervdj/hakyll/pull/783
  removeDirectoryRecursively = removeDirectoryRecursivelyIO


  removeDirectoryRecursivelyIfIdentity =
    removeDirectoryRecursivelyIfIdentityIO


  listDirectory = OsDirectory.listDirectory


  listDirectoryRecursively =
    listDirectoryRecursivelyIO TolerateVanishedEntries


  listDirectoryRecursivelyStrict =
    listDirectoryRecursivelyIO RejectVanishedEntries


  getFileSize path = do
    isDir <- isDirectory path
    when isDir $ do
      path' <- decodePath path
      throwError $
        mkIOError InappropriateType "getFileSize" Nothing (Just path')
          `ioeSetErrorString` "it is a directory"
    OsDirectory.getFileSize path


  getFileIdentity = getFileIdentityIO


  getFileSnapshot = getFileSnapshotIO


  getFileModeSnapshot = getFileModeSnapshotIO


  copyFile = OsDirectory.copyFile


  getPortableMode = getPortableModeIO


  setPortableMode = setPortableModeIO


  setPortableWritable = setPortableWritableIO


  createSymbolicLink target link Directory =
    OsDirectory.createDirectoryLink target link
  createSymbolicLink target link _ =
    OsDirectory.createFileLink target link


type SeqNo = Int


-- | The result of a filesystem operation.
data OverlaidFile
  = -- | A file with the given contents.
    Contents ByteString
  | -- | A directory.
    Directory'
  | -- | A file that doesn't exist (i.e., it was deleted).
    Gone
  | -- | A file that was copied from the given path.
    Copied OsPath
  | -- | A symbolic link pointing at the given target, together with its
    -- intrinsic link type.
    SymlinkTo OsPath FileType
  deriving (Eq, Show)


-- | Resolves a symbolic-link target from the directory containing the link.
resolveLinkTarget :: OsPath -> OsPath -> OsPath
resolveLinkTarget link target
  | isAbsolute target = normalise target
  | otherwise = normalise $ takeDirectory link </> target


-- | Follows chains of overlaid symbolic links until a non-link overlay (or
-- no overlay) is reached, as of the current sequence number.  Throws an
-- ELOOP-style 'IOError' after too many hops so overlaid link cycles fail
-- instead of looping forever.
chaseOverlaidLinks :: String -> OsPath -> DryRunIO OsPath
chaseOverlaidLinks location path = do
  seqNo <- gets currentSequenceNumber
  chaseOverlaidLinksAt location seqNo path


-- | Like 'chaseOverlaidLinks', but observes the overlay as of the given
-- sequence number, so historical reads (e.g. through 'Copied' events) see
-- the link targets that were in effect at that time.
chaseOverlaidLinksAt :: String -> SeqNo -> OsPath -> DryRunIO OsPath
chaseOverlaidLinksAt location seqOffset = go (0 :: Int)
 where
  go :: Int -> OsPath -> DryRunIO OsPath
  go depth path
    | depth > 40 = do
        path' <- decodePath path
        throwError $
          mkIOError InvalidArgument location Nothing (Just path')
            `ioeSetErrorString` "too many levels of symbolic links"
    | otherwise = do
        oFiles <- gets overlaidFiles
        let changes =
              [ change
              | (no, change) <- maybe [] toList $ oFiles !? normalise path
              , no <= seqOffset
              ]
        case changes of
          SymlinkTo target _ : _ ->
            go (depth + 1) $ resolveLinkTarget path target
          _ -> return path


-- | Internal state of 'DryRun'.
data DryRunState = DryRunState
  { overlaidFiles :: Map OsPath (NonEmpty (SeqNo, OverlaidFile))
  -- ^ The overlaid files and their list of changes.  Each change is a pair
  -- of the global sequence number and the new event that occurred.  The latest
  -- change comes first and the oldest change comes last.
  , overlaidModes :: Map OsPath (NonEmpty (SeqNo, PortableMode))
  -- ^ The overlaid portable modes and their list of changes, newest first.
  -- A mode change is only effective while no 'Gone' change with a greater
  -- sequence number exists for the same path, since removing and recreating
  -- an entry resets its permissions.
  , nextSequenceNumber :: SeqNo
  }


currentSequenceNumber :: DryRunState -> SeqNo
currentSequenceNumber state = nextSequenceNumber state - 1


-- | A monad that can perform filesystem operations, but only in a sandbox.
-- Note that, however, it can bypass the sandboxing of the 'MonadFileSystem'
-- class by using 'liftIO'.
newtype DryRunIO a = DryRunIO {unDryRunIO :: StateT DryRunState IO a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadError IOError
    , MonadIO
    , MonadState DryRunState
    )


addChangeToFile :: OsPath -> OverlaidFile -> DryRunIO ()
addChangeToFile path change = modify' $ \state ->
  let oFiles = overlaidFiles state
      nextSeqNo = nextSequenceNumber state
      newOFiles = alter (appendChange nextSeqNo) (normalise path) oFiles
  in state
       { overlaidFiles = newOFiles
       , nextSequenceNumber = nextSeqNo + 1
       }
 where
  appendChange
    :: SeqNo
    -> Maybe (NonEmpty (SeqNo, OverlaidFile))
    -> Maybe (NonEmpty (SeqNo, OverlaidFile))
  appendChange seqNo (Just changes) = Just $ (seqNo, change) :| toList changes
  appendChange seqNo Nothing = Just $ singleton (seqNo, change)


addModeToFile :: OsPath -> PortableMode -> DryRunIO ()
addModeToFile path mode = modify' $ \state ->
  let oModes = overlaidModes state
      nextSeqNo = nextSequenceNumber state
      newOModes = alter (appendChange nextSeqNo) (normalise path) oModes
  in state{overlaidModes = newOModes, nextSequenceNumber = nextSeqNo + 1}
 where
  appendChange
    :: SeqNo
    -> Maybe (NonEmpty (SeqNo, PortableMode))
    -> Maybe (NonEmpty (SeqNo, PortableMode))
  appendChange seqNo (Just changes) = Just $ (seqNo, mode) :| toList changes
  appendChange seqNo Nothing = Just $ singleton (seqNo, mode)


-- | Observes an overlaid entry's portable mode as of the given sequence
-- number, so historical reads (e.g. through 'Copied' events) see the mode
-- that was in effect at that time rather than the source's current state.
getPortableModeAt :: SeqNo -> OsPath -> DryRunIO PortableMode
getPortableModeAt seqOffset path = do
  oFiles <- gets overlaidFiles
  oModes <- gets overlaidModes
  path' <- decodePath path
  let path'' = normalise path
  let changes =
        [ (no, change)
        | (no, change) <- maybe [] toList $ oFiles !? path''
        , no <= seqOffset
        ]
  case changes of
    (_, Gone) : _ ->
      throwError $
        mkIOError doesNotExistErrorType "getPortableMode" Nothing (Just path')
          `ioeSetErrorString` "no such file"
    _ -> do
      let goneSeq = case [no | (no, Gone) <- changes] of
            no : _ -> Just no
            [] -> Nothing
      let modeEvents =
            [ (no, mode)
            | (no, mode) <- maybe [] toList $ oModes !? path''
            , no <= seqOffset
            ]
      let activeModes =
            [m | (no, m) <- modeEvents, maybe True (no >) goneSeq]
      case activeModes of
        m : _ -> return m
        [] -> case changes of
          (no, Copied src) : _ -> getPortableModeAt no src
          (_, SymlinkTo _ _) : _ ->
            return $ PortableMode Nothing True
          (_, change) : rest -> do
            let recreated = not $ null [no | (no, Gone) <- rest]
            let fileType = case change of
                  Directory' -> Directory
                  _ -> File
            if recreated
              then return $ defaultCreatedMode fileType
              else do
                realExists <- liftIO $ doesPathExist path
                if realExists
                  then liftIO $ getPortableModeIO path
                  else return $ defaultCreatedMode fileType
          [] ->
            liftIO (getPortableModeIO path)
              `mapError` (`ioePrependLocation` "getPortableMode")


-- | The permissions a freshly created overlay entry is assumed to have.
-- This approximates the common @umask 022@ default; dry-run runs cannot
-- know the umask a real run would use without mutating the filesystem.
defaultCreatedMode :: FileType -> PortableMode
defaultCreatedMode Directory = portableModeFromBits 0o755
defaultCreatedMode _ = portableModeFromBits 0o644


readFileFromDryRunIO :: SeqNo -> OsPath -> DryRunIO ByteString
readFileFromDryRunIO seqOffset src = do
  oFiles <- gets overlaidFiles
  case oFiles !? normalise src of
    Nothing -> fallback
    Just changes ->
      let filteredChanges = filter (\(no, _) -> no <= seqOffset) changes
      in case filteredChanges of
           [] -> fallback
           (_, Contents contents) : _ -> return contents
           (seqNo, Copied src') : _ ->
             readFileFromDryRunIO seqNo src'
           (_, SymlinkTo target _) : _ -> do
             resolved <-
               chaseOverlaidLinksAt "readFile" seqOffset $
                 resolveLinkTarget src target
             readFileFromDryRunIO seqOffset resolved
           (_, Gone) : _ -> do
             src' <- decodePath src
             throwError $
               mkIOError doesNotExistErrorType "readFile" Nothing (Just src')
                 `ioeSetErrorString` "no such file"
           (_, Directory') : _ -> do
             src' <- decodePath src
             throwError $ nonDirError src'
 where
  fallback :: DryRunIO ByteString
  fallback = liftIO $ do
    isDir <- doesDirectoryExist src
    src' <- decodeFS src
    when isDir $ throwError (nonDirError src')
    Data.ByteString.readFile src'
  nonDirError :: FilePath -> IOError
  nonDirError src' =
    mkIOError InappropriateType "readFile" Nothing (Just src')
      `ioeSetErrorString` "is a directory"


readRegularFileBoundedFromDryRunIO
  :: SeqNo -> Int -> OsPath -> DryRunIO BoundedFileRead
readRegularFileBoundedFromDryRunIO seqOffset limit src = do
  oFiles <- gets overlaidFiles
  case oFiles !? normalise src of
    Nothing -> fallback
    Just changes ->
      let filteredChanges = filter (\(no, _) -> no <= seqOffset) changes
      in case filteredChanges of
           [] -> fallback
           (_, Contents contents) : _ -> return $ bounded contents
           (seqNo, Copied source) : _ ->
             readRegularFileBoundedFromDryRunIO seqNo limit source
           (_, SymlinkTo target _) : _ -> do
             resolved <-
               chaseOverlaidLinksAt "readRegularFileBounded" seqOffset $
                 resolveLinkTarget src target
             readRegularFileBoundedFromDryRunIO seqOffset limit resolved
           (_, Gone) : _ -> do
             src' <- decodePath src
             throwError $
               mkIOError
                 doesNotExistErrorType
                 "readRegularFileBounded"
                 Nothing
                 (Just src')
                 `ioeSetErrorString` "no such file"
           (_, Directory') : _ -> return NotRegularFile
 where
  fallback = liftIO $ readRegularFileBoundedIO limit src
  bounded contents
    | Data.ByteString.length contents > limit = FileSizeLimitExceeded
    | otherwise = BoundedFileContents contents


instance MonadFileSystem DryRunIO where
  encodePath = liftIO . encodeFS


  decodePath = liftIO . decodeFS


  getCurrentDirectory = liftIO OsDirectory.getCurrentDirectory


  getHomeDirectory = liftIO OsDirectory.getHomeDirectory


  exists path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) -> return False
      Just ((_, SymlinkTo target _) :| _) ->
        chaseOverlaidLinks "exists" (resolveLinkTarget path target) >>= exists
      Just (_ :| _) -> return True
      Nothing -> liftIO $ doesPathExist path


  isFile path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Contents _) :| _) -> return True
      Just ((_, Copied _) :| _) -> return True
      Just ((_, SymlinkTo target _) :| _) ->
        chaseOverlaidLinks "isFile" (resolveLinkTarget path target)
          >>= isFile
      Just (_ :| _) -> return False
      Nothing -> liftIO $ doesFileExist path


  isRegularFile path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Contents _) :| _) -> return True
      Just ((_, Copied _) :| _) -> return True
      Just (_ :| _) -> return False
      Nothing -> liftIO (isRegularFile path :: IO Bool)


  isDirectory path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Directory') :| _) -> return True
      Just ((_, SymlinkTo target _) :| _) ->
        chaseOverlaidLinks "isDirectory" (resolveLinkTarget path target)
          >>= isDirectory
      Just (_ :| _) -> return False
      Nothing -> liftIO $ doesDirectoryExist path


  isSymlink path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, SymlinkTo _ _) :| _) -> return True
      Just _ -> return False
      Nothing ->
        liftIO (pathIsSymbolicLink path)
          `catchError` \e ->
            if isDoesNotExistError e then return False else throwError e


  readFile src = do
    seqNo <- gets currentSequenceNumber
    readFileFromDryRunIO seqNo src


  readRegularFileBounded limit src = do
    seqNo <- gets currentSequenceNumber
    readRegularFileBoundedFromDryRunIO seqNo limit src


  copyRegularFileWithSnapshot expectedSnapshot source destination = do
    actualSnapshot <- getFileSnapshot source
    if actualSnapshot == Just expectedSnapshot
      then copyFile source destination >> return True
      else return False


  writeFile dst contents = do
    oFiles <- gets overlaidFiles
    let dstDir = normalise $ takeDirectory dst
    dstParentExists <- liftIO $ doesPathExist dstDir
    dstDirExists <- liftIO $ doesDirectoryExist dstDir
    dst' <- decodePath dst
    dstIsDir <- liftIO $ doesDirectoryExist dst
    case (oFiles !? dstDir, oFiles !? normalise dst) of
      (Just ((_, Gone) :| _), _) -> throwError $ noParentDirError dst'
      (Nothing, _) | not dstParentExists -> throwError $ noParentDirError dst'
      (Just ((_, Contents _) :| _), _) -> throwError $ notInsideDirError dst'
      (Just ((_, Copied _) :| _), _) -> throwError $ notInsideDirError dst'
      (Nothing, _) | not dstDirExists -> throwError $ notInsideDirError dst'
      (_, Just ((_, Directory') :| _)) -> throwError $ dirError dst'
      (_, Nothing) | dstIsDir -> throwError $ dirError dst'
      (Just ((_, SymlinkTo target _) :| _), _) -> do
        resolved <-
          chaseOverlaidLinks "writeFile" $ resolveLinkTarget dstDir target
        writeFile (resolved </> takeFileName dst) contents
      (_, Just ((_, SymlinkTo target _) :| _)) -> do
        resolved <-
          chaseOverlaidLinks "writeFile" $ resolveLinkTarget dst target
        writeFile resolved contents
      _ -> do
        addChangeToFile dst $ Contents contents
        return ()
   where
    dirError :: FilePath -> IOError
    dirError dst' =
      mkIOError InappropriateType "writeFile" Nothing (Just dst')
        `ioeSetErrorString` "is a directory"
    noParentDirError :: FilePath -> IOError
    noParentDirError dst' =
      mkIOError doesNotExistErrorType "writeFile" Nothing (Just dst')
        `ioeSetErrorString` "no parent directory"
    notInsideDirError :: FilePath -> IOError
    notInsideDirError dst' =
      mkIOError InappropriateType "writeFile" Nothing (Just dst')
        `ioeSetErrorString` "not inside a directory"


  replaceFile src dst = do
    contents <- readFile src
    writeFile dst contents
    removeFile src


  writeTemporaryFile directory template contents = do
    sequenceNumber <- gets nextSequenceNumber
    filename <- encodePath $ template <> show sequenceNumber
    let temporary = directory </> filename
    writeFile temporary contents
    return temporary


  withFileLock _ action = action


  canonicalizePath path = liftIO $ OsDirectory.canonicalizePath path


  readSymlinkTarget path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, SymlinkTo target _) :| _) -> return target
      Just ((_, Gone) :| _) -> do
        path' <- decodePath path
        throwError $
          mkIOError
            doesNotExistErrorType
            "readSymlinkTarget"
            Nothing
            (Just path')
            `ioeSetErrorString` "no such file"
      Just _ -> do
        path' <- decodePath path
        throwError $
          mkIOError InappropriateType "readSymlinkTarget" Nothing (Just path')
            `ioeSetErrorString` "not a symbolic link"
      Nothing ->
        liftIO $
          getSymbolicLinkTarget path
            `mapError` (`ioePrependLocation` "readSymlinkTarget")


  getSymbolicLinkType path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, SymlinkTo _ fileType) :| _) -> return fileType
      _ -> liftIO (getSymbolicLinkType path :: IO FileType)


  copyFile src dst = do
    oFiles <- gets overlaidFiles
    src' <- decodePath src
    srcExists <- liftIO $ doesPathExist src
    srcIsDir <- liftIO $ doesDirectoryExist src
    let dstDir = normalise $ takeDirectory dst
    dstDirExists <- liftIO $ doesPathExist dstDir
    dstDirIsDir <- liftIO $ doesDirectoryExist dstDir
    dst' <- decodePath dst
    dstIsDir <- liftIO $ doesDirectoryExist dst
    case oFiles !? normalise src of
      Just ((_, SymlinkTo target _) :| _) -> do
        resolved <-
          chaseOverlaidLinks "copyFile" $ resolveLinkTarget src target
        copyFile resolved dst
      Just ((_, Gone) :| _) -> throwError $ noSrcFileError src'
      Nothing | not srcExists -> throwError $ noSrcFileError src'
      Just ((_, Directory') :| _) -> throwError $ srcIsDirError src'
      Nothing | srcIsDir -> throwError $ srcIsDirError src'
      _ -> case (oFiles !? dstDir, oFiles !? normalise dst) of
        (Just ((_, Gone) :| _), _) -> throwError $ noParentDirError dst'
        (Nothing, _) | not dstDirExists -> throwError $ noParentDirError dst'
        (Just ((_, Contents _) :| _), _) -> throwError $ notInsideDirError dst'
        (Just ((_, Copied _) :| _), _) -> throwError $ notInsideDirError dst'
        (Nothing, _) | not dstDirIsDir -> throwError $ notInsideDirError dst'
        (_, Just ((_, Directory') :| _)) -> throwError $ dstIsDirError dst'
        (_, Nothing) | dstIsDir -> throwError $ dstIsDirError dst'
        (Just ((_, SymlinkTo target _) :| _), _) -> do
          resolved <-
            chaseOverlaidLinks "copyFile" $ resolveLinkTarget dstDir target
          copyFile src $ resolved </> takeFileName dst
        (_, Just ((_, SymlinkTo target _) :| _)) -> do
          resolved <-
            chaseOverlaidLinks "copyFile" $ resolveLinkTarget dst target
          copyFile src resolved
        _ -> do
          addChangeToFile dst $ Copied src
          return ()
   where
    noSrcFileError :: FilePath -> IOError
    noSrcFileError src' =
      mkIOError doesNotExistErrorType "copyFile" Nothing (Just src')
        `ioeSetErrorString` "source does not exist"
    srcIsDirError :: FilePath -> IOError
    srcIsDirError src' =
      mkIOError InappropriateType "copyFile" Nothing (Just src')
        `ioeSetErrorString` "source is a directory"
    noParentDirError :: FilePath -> IOError
    noParentDirError dst' =
      mkIOError doesNotExistErrorType "copyFile" Nothing (Just dst')
        `ioeSetErrorString` "no parent directory"
    notInsideDirError :: FilePath -> IOError
    notInsideDirError dst' =
      mkIOError InappropriateType "copyFile" Nothing (Just dst')
        `ioeSetErrorString` "not inside a directory"
    dstIsDirError :: FilePath -> IOError
    dstIsDirError dst' =
      mkIOError InappropriateType "copyFile" Nothing (Just dst')
        `ioeSetErrorString` "destination is a directory"


  createDirectory dst = do
    oFiles <- gets overlaidFiles
    dst' <- decodePath dst
    isFile' <- liftIO $ doesFileExist dst
    isDir <- liftIO $ doesDirectoryExist dst
    isSymlink' <-
      liftIO $
        pathIsSymbolicLink dst `catchError` \e ->
          if isDoesNotExistError e
            then return False
            else throwError $ e `ioePrependLocation` "createDirectory"
    parentExists <- liftIO $ doesPathExist parent
    parentIsDir <- liftIO $ doesDirectoryExist parent
    case (oFiles !? parent, oFiles !? normalise dst) of
      (Just ((_, Gone) :| _), _) -> throwError $ noParentDirError dst'
      (Nothing, _) | not parentExists -> throwError $ noParentDirError dst'
      (Just ((_, Contents _) :| _), _) -> throwError $ notInsideDirError dst'
      (Just ((_, Copied _) :| _), _) -> throwError $ notInsideDirError dst'
      (Nothing, _) | not parentIsDir -> throwError $ notInsideDirError dst'
      (_, Just ((_, Contents _) :| _)) -> throwError $ dstIsFileError dst'
      (_, Just ((_, Copied _) :| _)) -> throwError $ dstIsFileError dst'
      (_, Just ((_, SymlinkTo _ _) :| _)) -> throwError $ dstIsFileError dst'
      (_, Nothing) | isFile' -> throwError $ dstIsFileError dst'
      (_, Just ((_, Directory') :| _)) -> throwError $ dstIsDirError dst'
      (_, Nothing) | isDir && not isSymlink' -> throwError $ dstIsDirError dst'
      _ -> do
        addChangeToFile dst Directory'
        return ()
   where
    parent :: OsPath
    parent = normalise $ takeDirectory dst
    noParentDirError :: FilePath -> IOError
    noParentDirError dst' =
      mkIOError doesNotExistErrorType "createDirectory" Nothing (Just dst')
        `ioeSetErrorString` "no parent directory"
    notInsideDirError :: FilePath -> IOError
    notInsideDirError dst' =
      mkIOError InappropriateType "createDirectory" Nothing (Just dst')
        `ioeSetErrorString` "not inside a directory"
    dstIsFileError :: FilePath -> IOError
    dstIsFileError dst' =
      mkIOError alreadyExistsErrorType "createDirectory" Nothing (Just dst')
        `ioeSetErrorString` "destination is already a file"
    dstIsDirError :: FilePath -> IOError
    dstIsDirError dst' =
      mkIOError alreadyExistsErrorType "createDirectory" Nothing (Just dst')
        `ioeSetErrorString` "destination is already a directory"


  createPrivateDirectory path = do
    createDirectory path
    setPortableMode path 0o700


  removeFile path = do
    oFiles <- gets overlaidFiles
    path' <- decodePath path
    exists' <- liftIO $ doesPathExist path
    isSymlink' <-
      liftIO $
        pathIsSymbolicLink path
          `catchError` \e ->
            if isDoesNotExistError e
              then return False
              else throwError $ e `ioePrependLocation` "removeFile"
    isDir <- liftIO $ doesDirectoryExist path
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) -> throwError $ noFileError path'
      Nothing | not exists' -> throwError $ noFileError path'
      Just ((_, Directory') :| _) -> throwError $ dirError path'
      Nothing | isDir && not isSymlink' -> throwError $ dirError path'
      _ -> do
        addChangeToFile path Gone
        return ()
   where
    noFileError :: FilePath -> IOError
    noFileError path' =
      mkIOError doesNotExistErrorType "removeFile" Nothing (Just path')
        `ioeSetErrorString` "no such file"
    dirError :: FilePath -> IOError
    dirError path' =
      mkIOError InappropriateType "removeFile" Nothing (Just path')
        `ioeSetErrorString` "is a directory"


  removeDirectory path = do
    oFiles <- gets overlaidFiles
    path' <- decodePath path
    exists' <- liftIO $ doesPathExist path
    isDir <- liftIO $ doesDirectoryExist path
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) ->
        throwError $ noDirError path'
      Nothing
        | not exists' ->
            throwError $ noDirError path'
      Just ((_, Contents _) :| _) ->
        throwError $ nonDirError path'
      Just ((_, Copied _) :| _) ->
        throwError $ nonDirError path'
      Just ((_, SymlinkTo _ _) :| _) ->
        throwError $ nonDirError path'
      Nothing
        | not isDir ->
            throwError $ nonDirError path'
      _ -> do
        addChangeToFile path Gone
        return ()
   where
    noDirError :: FilePath -> IOError
    noDirError path' =
      mkIOError
        doesNotExistErrorType
        "removeDirectory"
        Nothing
        (Just path')
        `ioeSetErrorString` "no such directory"
    nonDirError :: FilePath -> IOError
    nonDirError path' =
      mkIOError
        InappropriateType
        "removeDirectory"
        Nothing
        (Just path')
        `ioeSetErrorString` "not a directory"


  listDirectory path = do
    oFiles <- gets overlaidFiles
    pathFP <- decodePath path
    case oFiles !? path' of
      Just ((_, Gone) :| _) ->
        throwError $ noDirError pathFP
      Just ((_, Contents _) :| _) ->
        throwError $ nonDirError pathFP
      Just ((_, Copied _) :| _) ->
        throwError $ nonDirError pathFP
      Just ((_, SymlinkTo target _) :| _) ->
        chaseOverlaidLinks "listDirectory" (resolveLinkTarget path target)
          >>= listDirectory
      Just ((_, Directory') :| _) ->
        return $ map takeFileName $ keys $ directOChildren oFiles
      Nothing -> do
        isSymlink' <-
          liftIO $
            pathIsSymbolicLink path
              `catchError` \e ->
                if isDoesNotExistError e
                  then return False
                  else throwError $ e `ioePrependLocation` "listDirectory"
        isFile' <- liftIO $ doesFileExist path
        when (isSymlink' || isFile') $ throwError (nonDirError pathFP)
        files <-
          liftIO $
            OsDirectory.listDirectory path
              `mapError` (`ioePrependLocation` "listDirectory")
        let directOChildren' = directOChildren oFiles
        let result =
              [f | f <- files, directOChildren' !? (path' </> f) /= Just Gone]
                ++ [ filename
                   | (filePath, f) <- toAscList directOChildren'
                   , f /= Gone
                   , let split = splitDirectories filePath
                   , pathDirs `isPrefixOf` split
                   , length pathDirs < length split
                   , let filename = split !! length pathDirs
                   , filename `notElem` files
                   ]
        return $ sort result
   where
    path' :: OsPath
    path' = normalise path
    pathDirs :: [OsPath]
    pathDirs = splitDirectories path'
    directOChildren
      :: Map OsPath (NonEmpty (SeqNo, OverlaidFile)) -> Map OsPath OverlaidFile
    directOChildren oFiles =
      fromList
        [ (filePath, f)
        | (filePath, (_, f) :| []) <- toAscList oFiles
        , let split = splitDirectories filePath
        , pathDirs `isPrefixOf` split
        , length pathDirs + 1 == length split
        ]
    noDirError :: FilePath -> IOError
    noDirError pathFP =
      mkIOError
        doesNotExistErrorType
        "listDirectory"
        Nothing
        (Just pathFP)
        `ioeSetErrorString` "no such directory"
    nonDirError :: FilePath -> IOError
    nonDirError pathFP =
      mkIOError
        InappropriateType
        "listDirectory"
        Nothing
        (Just pathFP)
        `ioeSetErrorString` "not a directory"


  getPortableMode path = do
    seqNo <- gets currentSequenceNumber
    getPortableModeAt seqNo path


  setPortableMode path bits = do
    exists' <- exists path
    unless exists' $ do
      path' <- decodePath path
      throwError $
        mkIOError doesNotExistErrorType "setPortableMode" Nothing (Just path')
          `ioeSetErrorString` "no such file"
    addModeToFile path $ portableModeFromBits bits


  setPortableWritable path writable' = do
    current <-
      getPortableMode path
        `mapError` (`ioePrependLocation` "setPortableWritable")
    let adjustedBits = case current.posixBits of
          Nothing -> Nothing
          Just bits
            | writable' -> Just $ bits .|. 0o200
            | otherwise -> Just $ bits .&. complement 0o200
    addModeToFile path $
      PortableMode
        { posixBits = adjustedBits
        , writable = writable'
        }


  createSymbolicLink target link fileType = do
    oFiles <- gets overlaidFiles
    link' <- decodePath link
    let linkDir = normalise $ takeDirectory link
    parentExists <- liftIO $ doesPathExist linkDir
    parentIsDir <- liftIO $ doesDirectoryExist linkDir
    realEntryExists <- liftIO $ do
      pathExists <- doesPathExist link
      linkIsSym <-
        pathIsSymbolicLink link `catchError` \e ->
          if isDoesNotExistError e then return False else throwError e
      return $ pathExists || linkIsSym
    case (oFiles !? linkDir, oFiles !? normalise link) of
      (Just ((_, Gone) :| _), _) -> throwError $ noParentDirError link'
      (Nothing, _) | not parentExists -> throwError $ noParentDirError link'
      (Just ((_, Contents _) :| _), _) -> throwError $ notInsideDirError link'
      (Just ((_, Copied _) :| _), _) -> throwError $ notInsideDirError link'
      (Nothing, _) | not parentIsDir -> throwError $ notInsideDirError link'
      (_, Just ((_, Gone) :| _)) -> create
      (_, Just _) -> throwError $ existsError link'
      (_, Nothing) | realEntryExists -> throwError $ existsError link'
      _ -> create
   where
    create :: DryRunIO ()
    create = addChangeToFile link $ SymlinkTo target fileType
    noParentDirError :: FilePath -> IOError
    noParentDirError link' =
      mkIOError doesNotExistErrorType "createSymbolicLink" Nothing (Just link')
        `ioeSetErrorString` "no parent directory"
    notInsideDirError :: FilePath -> IOError
    notInsideDirError link' =
      mkIOError InappropriateType "createSymbolicLink" Nothing (Just link')
        `ioeSetErrorString` "not inside a directory"
    existsError :: FilePath -> IOError
    existsError link' =
      mkIOError alreadyExistsErrorType "createSymbolicLink" Nothing (Just link')
        `ioeSetErrorString` "link path already exists"


  getFileSize path = do
    oFiles <- gets overlaidFiles
    path' <- decodePath path
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) ->
        throwError $ noFileError path'
      Just ((_, Directory') :| _) ->
        throwError $ nonFileError path'
      Just _ -> do
        contents <- readFile path
        return $
          fromIntegral $
            Data.ByteString.length contents
      Nothing -> do
        isDir <- isDirectory path
        if isDir
          then throwError $ nonFileError path'
          else
            liftIO (OsDirectory.getFileSize path)
              `mapError` (`ioeSetLocation` "getFileSize")
   where
    noFileError :: FilePath -> IOError
    noFileError pathFP =
      mkIOError
        doesNotExistErrorType
        "getFileSize"
        Nothing
        (Just pathFP)
        `ioeSetErrorString` "no such file"
    nonFileError :: FilePath -> IOError
    nonFileError pathFP =
      mkIOError
        InappropriateType
        "getFileSize"
        Nothing
        (Just pathFP)
        `ioeSetErrorString` "not a regular file, but a directory"


  getFileIdentity path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) -> return Nothing
      Just ((sequenceNumber, _) :| _) ->
        return $ Just $ FileIdentity (-1) $ fromIntegral sequenceNumber
      Nothing -> liftIO $ getFileIdentityIO path


  getFileSnapshot path = do
    oFiles <- gets overlaidFiles
    case oFiles !? normalise path of
      Just ((_, Gone) :| _) -> return Nothing
      Just ((_, SymlinkTo _ _) :| _) -> return Nothing
      Just ((_, Directory') :| _) -> return Nothing
      Just ((sequenceNumber, _) :| _) -> do
        identity <- getFileIdentity path
        return $
          fmap
            ( \value ->
                FileSnapshot
                  value
                  0
                  (fromIntegral sequenceNumber)
                  Nothing
            )
            identity
      Nothing -> liftIO $ getFileSnapshotIO path


  getFileModeSnapshot path = do
    identity <- getFileIdentity path
    case identity of
      Nothing -> return Nothing
      Just value ->
        Just . FileModeSnapshot value <$> getPortableMode path


-- | Performs 'DryRunIO' action in the sandbox and returns the result.
dryRunIO :: DryRunIO a -> IO a
dryRunIO = fmap fst . dryRunIO'


-- | Performs 'DryRunIO' action in the sandbox and returns the result and
-- the total number of filesystem operations that were performed.
dryRunIO' :: DryRunIO a -> IO (a, Int)
dryRunIO' action = do
  (value, state) <- runStateT (unDryRunIO action) initialState
  return (value, nextSequenceNumber state)
 where
  initialState =
    DryRunState
      { overlaidFiles = mempty
      , overlaidModes = mempty
      , nextSequenceNumber = 0
      }


-- | Performs 'DryRunIO' action in the sandbox and returns either the result
-- or an 'IOError' that occurred.
tryDryRunIO :: DryRunIO a -> IO (Either IOError a)
tryDryRunIO action = dryRunIO $ tryError action


ioePrependLocation :: IOError -> String -> IOError
ioePrependLocation e location =
  ioeSetLocation e $ case loc of
    "" -> location
    _ -> location ++ ':' : loc
 where
  loc :: String
  loc = ioeGetLocation e


mapError :: (MonadError e m) => m a -> (e -> e) -> m a
mapError action transform = catchError action (throwError . transform)
