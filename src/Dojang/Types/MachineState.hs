{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Versioned, repository-scoped state that belongs to one machine.
module Dojang.Types.MachineState
  ( MachineId
  , HookExecution (..)
  , HookExecutionKey
  , HookExecutionPolicy (..)
  , MachineState (..)
  , MigrationResult (..)
  , RepositorySelection (..)
  , StateError (..)
  , StateGenerationId
  , StateRootInputs (..)
  , canonicalizeStateRoot
  , catchStateIOErrors
  , decodeMachineState
  , defaultIntermediatePath
  , encodeMachineState
  , ensureMachineId
  , formatStateError
  , forgetMarkerPath
  , forgetRepositoryState
  , forgetRepositoryStateWith
  , isRepositoryForgetInProgress
  , hookExecutionFingerprint
  , hookExecutionKey
  , listRepositoryStates
  , machineIdText
  , manifestIdentityLockPath
  , managedTargetSnapshotRoot
  , markFirstApplied
  , markRepositoryForgetInProgress
  , migrationMarkerPath
  , parseMachineId
  , parseStateGenerationId
  , prepareRepositoryState
  , prepareRepositoryStateWithLegacyHistory
  , prepareRepositoryStateWithOwnership
  , prepareRepositoryStateWithOwnershipBeforeMigration
  , readMachineId
  , readRepositoryState
  , recordHookExecution
  , repositoryStateDirectory
  , repositoryStatePath
  , renderHookExecutionKey
  , resolveStateRoot
  , retryManagedTargetCleanup
  , sameExistingPath
  , selectRepositoryState
  , stateGenerationIdText
  , validateMachineStateStore
  , validateMigrationStateRoot
  , validateRepositoryStateGeneration
  , validateSelectedSnapshotLocation
  , updateManagedTargets
  , updateManagedTargetsWith
  , updateMachineFacts
  , withRepositoryStateGeneration
  , withStateFileLock
  ) where

import Control.Monad (filterM, forM, unless, when)
import Control.Monad.Except
  ( MonadError (catchError, throwError)
  , tryError
  )
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Char (chr, isHexDigit, ord)
import Data.List (find, isPrefixOf, isSuffixOf, nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time
  ( UTCTime
  , defaultTimeLocale
  , formatTime
  , parseTimeM
  )
import Numeric (readHex, showHex)
import System.IO.Unsafe (unsafePerformIO)
import TextShow (FromStringShow (FromStringShow), TextShow (showt))
import Prelude hiding (readFile, writeFile)

import System.OsPath
  ( OsPath
  , decodeFS
  , encodeFS
  , isAbsolute
  , makeRelative
  , normalise
  , splitDirectories
  , takeDirectory
  , (</>)
  )
import Toml (Result (..), decode, encode)
import Toml.FromValue
  ( FromValue (fromValue)
  , optKey
  , parseTableFromValue
  , reqKey
  )
import Toml.ToValue
  ( ToTable (toTable)
  , ToValue (toValue)
  , defaultTableToValue
  , table
  , (.=)
  )

import Dojang.MonadFileSystem
  ( FileType (..)
  , MonadFileSystem (..)
  , writeFileAtomically
  )
import Dojang.Types.Environment
  ( FactMap
  , OperatingSystem (..)
  , factKeyText
  , factValueText
  , isBuiltInFact
  , parseFactKey
  )
import Dojang.Types.Hook
  ( HookId
  , HookType
  , allHookTypes
  , parseHookId
  , renderHookId
  , renderHookType
  )
import Dojang.Types.ManagedTarget
  ( ManagedTarget (..)
  , SynchronizationCommand (..)
  , TargetFingerprint (..)
  , isSafeManagedRelativePath
  )
import Dojang.Types.RepositoryId
  ( RepositoryId
  , newRepositoryId
  , parseRepositoryId
  , repositoryIdText
  )


-- | The on-disk schema version understood by this release.
schemaVersion :: Integer
schemaVersion = 4


-- | A stable identifier for the local machine-state store.
newtype MachineId = MachineId RepositoryId
  deriving (Eq, Ord, Show)


-- | Parses a machine identifier from its UUID representation.
parseMachineId :: Text -> Either Text MachineId
parseMachineId = fmap MachineId . parseRepositoryId


-- | Renders a machine identifier in canonical UUID form.
machineIdText :: MachineId -> Text
machineIdText (MachineId identifier) = repositoryIdText identifier


-- | A successful stateful hook execution recorded for this machine.
data HookExecution = HookExecution
  { policy :: HookExecutionPolicy
  -- ^ Valid stateful policy and its policy-specific data.
  , succeededTime :: UTCTime
  -- ^ Time at which the hook most recently succeeded.
  }
  deriving (Eq, Show)


-- | The typed identity of a stateful hook execution.
data HookExecutionKey = HookExecutionKey HookType HookId
  deriving (Eq, Ord, Show)


-- | Builds a repository-state key from a lifecycle event and validated hook ID.
hookExecutionKey
  :: HookType
  -- ^ Lifecycle event that owns the hook.
  -> HookId
  -- ^ Validated stable hook identifier.
  -> HookExecutionKey
  -- ^ Typed execution-history key.
hookExecutionKey = HookExecutionKey


-- | Renders a typed hook execution identity for the state document.
renderHookExecutionKey
  :: HookExecutionKey
  -- ^ Typed execution-history key.
  -> Text
  -- ^ Canonical @event/id@ document key.
renderHookExecutionKey (HookExecutionKey event identifier) =
  renderHookType event <> "/" <> renderHookId identifier


-- | Stateful policy data recorded after a successful execution.
data HookExecutionPolicy
  = -- | A successful once-per-repository-and-machine execution.
    HookOnceExecution
  | -- | A successful on-change execution and its context fingerprint.
    HookOnChangeExecution Text
  deriving (Eq, Show)


-- | Returns the context fingerprint stored by an on-change execution.
hookExecutionFingerprint
  :: HookExecution
  -- ^ Successful stateful execution.
  -> Maybe Text
  -- ^ On-change fingerprint, or 'Nothing' for a once execution.
hookExecutionFingerprint execution = case execution.policy of
  HookOnceExecution -> Nothing
  HookOnChangeExecution fingerprint -> Just fingerprint


-- | An opaque identity for one creation of a repository-state record.
newtype StateGenerationId = StateGenerationId RepositoryId
  deriving (Eq, Ord, Show)


-- | Generates a fresh repository-state generation identity.
newStateGenerationId :: (MonadIO m) => m StateGenerationId
newStateGenerationId = StateGenerationId <$> newRepositoryId


-- | Parses a repository-state generation identity from canonical UUID text.
parseStateGenerationId
  :: Text
  -- ^ Canonical UUID text.
  -> Either Text StateGenerationId
  -- ^ Validated generation identity or a parse error.
parseStateGenerationId = fmap StateGenerationId . parseRepositoryId


-- | Renders a repository-state generation identity as canonical UUID text.
stateGenerationIdText
  :: StateGenerationId
  -- ^ Opaque repository-state generation identity.
  -> Text
  -- ^ Canonical lowercase UUID text.
stateGenerationIdText (StateGenerationId identifier) = repositoryIdText identifier


-- | Returns the cross-process lock that protects manifest identity creation.
manifestIdentityLockPath :: OsPath -> OsPath
manifestIdentityLockPath root = root </> path "manifest-identity.lock"


data MachineDocument = MachineDocument
  { documentMachineIdentity :: Text
  }


instance FromValue MachineDocument where
  fromValue = parseTableFromValue $ MachineDocument <$> reqKey "machine-id"


instance ToValue MachineDocument where
  toValue = defaultTableToValue


instance ToTable MachineDocument where
  toTable document = table ["machine-id" .= document.documentMachineIdentity]


data MigrationMarkerDocument = MigrationMarkerDocument
  { documentMigrationSource :: Text
  , documentMigrationDestination :: Text
  }


instance FromValue MigrationMarkerDocument where
  fromValue =
    parseTableFromValue $
      MigrationMarkerDocument
        <$> reqKey "source-path"
        <*> reqKey "destination-path"


instance ToValue MigrationMarkerDocument where
  toValue = defaultTableToValue


instance ToTable MigrationMarkerDocument where
  toTable document =
    table
      [ "source-path" .= document.documentMigrationSource
      , "destination-path" .= document.documentMigrationDestination
      ]


-- | Inputs whose values depend on the host platform and environment.
data StateRootInputs = StateRootInputs
  { homeDirectory :: OsPath
  -- ^ The user's home directory.
  , xdgDataHome :: Maybe OsPath
  -- ^ The value of @XDG_DATA_HOME@, when set.
  , localAppData :: Maybe OsPath
  -- ^ The value of @LOCALAPPDATA@, when set.
  }
  deriving (Eq, Show)


-- | Resolves Dojang's native user-data directory for a platform.
resolveStateRoot :: OperatingSystem -> StateRootInputs -> OsPath
resolveStateRoot operatingSystem inputs = normalise $ case operatingSystem of
  Linux ->
    maybe
      (inputs.homeDirectory </> path ".local" </> path "share")
      id
      (inputs.xdgDataHome >>= validAbsolutePath)
      </> path "dojang"
  MacOS ->
    inputs.homeDirectory
      </> path "Library"
      </> path "Application Support"
      </> path "dojang"
  Windows ->
    maybe
      (inputs.homeDirectory </> path "AppData" </> path "Local")
      id
      (inputs.localAppData >>= validAbsolutePath)
      </> path "dojang"
  _ ->
    inputs.homeDirectory
      </> path ".local"
      </> path "share"
      </> path "dojang"
 where
  validAbsolutePath candidate
    | isAbsolute candidate = Just candidate
    | otherwise = Nothing


-- | Resolves symbolic links and other filesystem aliases in a state root.
--
-- This must run before state directories are created so native user-data
-- directories may themselves be symbolic links without allowing symbolic
-- links inside the machine-state store.
canonicalizeStateRoot
  :: (MonadFileSystem m)
  => OsPath
  -> m (Either StateError OsPath)
canonicalizeStateRoot root =
  catchStateIOErrors $ Right <$> canonicalizePath root


-- | The persisted facts for one repository on one machine.
data MachineState = MachineState
  { schemaVersion :: Integer
  -- ^ Serialized state schema version.
  , repositoryId :: RepositoryId
  -- ^ Stable identity declared by the repository manifest.
  , machineId :: MachineId
  -- ^ Identity of the machine that owns the state.
  , generationId :: StateGenerationId
  -- ^ Opaque identity of this repository-state generation.
  , checkoutPath :: OsPath
  -- ^ Last known checkout path.
  , manifestPath :: OsPath
  -- ^ Manifest path used to verify ownership of the recorded checkout.
  , intermediatePath :: OsPath
  -- ^ Persisted full intermediate snapshot path.
  , targetSnapshotRoot :: OsPath
  -- ^ Private root for immutable managed-target baselines.
  , factsFile :: Maybe OsPath
  -- ^ Optional repository-relative or absolute shared machine-facts file.
  , declaredFacts :: FactMap
  -- ^ Repository-specific machine facts declared during enrollment.
  , createdTime :: UTCTime
  -- ^ Time at which the repository record was created.
  , updatedTime :: UTCTime
  -- ^ Time at which the repository record was last updated.
  , firstApplied :: Bool
  -- ^ Whether this repository has completed a successful apply on this machine.
  , pendingCleanupPaths :: [OsPath]
  -- ^ Published snapshot removals that still need to complete.
  , targetRecords :: Map Text ManagedTarget
  -- ^ Successfully synchronized destination entries, keyed by stable ID.
  , hookRecords :: Map Text Text
  -- ^ Opaque hook lifecycle data preserved from earlier schema versions.
  , hookExecutions :: Map HookExecutionKey HookExecution
  -- ^ Successful stateful hook executions, keyed by event and hook identity.
  }
  deriving (Eq, Show)


-- | Result of selecting a repository from all machine-state records.
data RepositorySelection
  = -- | No repository state is registered.
    NoRepositoryState
  | -- | Exactly one repository state is registered.
    SelectedRepositoryState MachineState
  | -- | More than one repository state is registered.
    AmbiguousRepositoryStates
  deriving (Eq, Show)


-- | Failures that must not be confused with absent state.
data StateError
  = -- | A state-store filesystem operation failed.
    StateIOError Text
  | -- | TOML or a typed field cannot be decoded.
    MalformedState Text
  | -- | An interrupted migration marker cannot be decoded.
    MalformedMigrationMarker Text
  | -- | The record uses a schema version this release cannot read.
    UnsupportedSchemaVersion Integer
  | -- | The expected and stored repository identities differ.
    RepositoryIdentityMismatch RepositoryId RepositoryId
  | -- | The expected and stored machine identities differ.
    MachineProvenanceMismatch MachineId MachineId
  | -- | A lifecycle update cannot find its repository record.
    MissingRepositoryState RepositoryId
  | -- | A lifecycle update belongs to a replaced repository record.
    RepositoryStateGenerationMismatch
      RepositoryId
      StateGenerationId
      StateGenerationId
  | -- | Only forget may proceed after repository cleanup was approved.
    RepositoryForgetInProgress RepositoryId
  | -- | Two existing checkouts declare one repository identity.
    DuplicateRepositoryIdentity RepositoryId OsPath OsPath
  | -- | A recorded checkout no longer declares its repository identity.
    StaleRepositoryCheckout RepositoryId OsPath
  | -- | The old and proposed intermediate snapshots differ.
    ConflictingSnapshots OsPath OsPath
  | -- | A legacy snapshot contains a symbolic link that cannot be copied.
    UnsupportedSnapshotSymlink OsPath
  | -- | A snapshot entry is not a regular file or directory.
    UnsupportedSnapshotEntry OsPath
  | -- | A configured snapshot path exists but is not a directory.
    InvalidSnapshotLocation OsPath
  | -- | A snapshot destination is nested in the legacy source tree.
    SnapshotInsideLegacy OsPath OsPath
  | -- | A snapshot exists without state or a legacy source that owns it.
    UnownedSnapshot OsPath
  | -- | A state record points to a snapshot that is no longer present.
    MissingSnapshot OsPath
  | -- | Snapshot paths overlap and cannot be copied safely.
    OverlappingSnapshots OsPath OsPath
  | -- | The machine-state root overlaps the legacy snapshot.
    OverlappingStateRoot OsPath OsPath
  | -- | A snapshot contains a protected checkout or machine-state path.
    ProtectedSnapshotLocation OsPath OsPath
  | -- | A retry requested a destination other than the journaled destination.
    MigrationDestinationMismatch OsPath OsPath
  | -- | A journaled migration names an unexpected source snapshot.
    MigrationSourceMismatch OsPath OsPath
  | -- | Verified external state was saved, but the old copy could not be removed.
    LegacyCleanupFailed OsPath OsPath Text
  deriving (Eq, Show)


-- | Work performed while preparing a repository record.
data MigrationResult
  = -- | A new record was created without a legacy snapshot.
    CreatedRepositoryState
  | -- | The existing record and snapshot location were reused.
    ReusedRepositoryState
  | -- | A missing old checkout was replaced with its new location.
    MovedRepositoryCheckout
  | -- | A worktree-local snapshot was copied and verified.
    MigratedLegacySnapshot
  | -- | A marked partial copy was discarded and copied again.
    RecoveredInterruptedMigration
  | -- | A command-line intermediate path replaced the stored path.
    PersistedIntermediateOverride
  deriving (Eq, Show)


-- | A validated action for preparing a repository without an existing record.
data NewStatePlan
  = RejectNewState StateError
  | PersistNewState MigrationResult
  | MigrateLegacySnapshot MigrationResult
  | RecoverLegacySnapshot MigrationResult
  deriving (Eq, Show)


-- | Formats a state failure as an actionable diagnostic.
formatStateError :: StateError -> Text
formatStateError (StateIOError details) =
  "The machine-state store could not be read or updated.  Check its path and "
    <> "permissions, then retry.\n"
    <> details
formatStateError (MalformedState details) =
  "The machine-state file is malformed.  Repair or remove the affected state "
    <> "record, then run `dojang migrate` again.\n"
    <> details
formatStateError (MalformedMigrationMarker details) =
  "The interrupted-migration marker is malformed.  Preserve the legacy and "
    <> "external snapshots, repair the marker, and retry `dojang migrate`.\n"
    <> details
formatStateError (UnsupportedSchemaVersion version) =
  "The machine-state schema version "
    <> Text.pack (show version)
    <> " is not supported by this Dojang version.  Upgrade Dojang or restore "
    <> "a compatible state backup."
formatStateError (RepositoryIdentityMismatch expected actual) =
  "The state record belongs to repository "
    <> repositoryIdText actual
    <> ", but the manifest declares "
    <> repositoryIdText expected
    <> ".  Do not reuse state between repositories."
formatStateError (MachineProvenanceMismatch expected actual) =
  "The snapshot belongs to machine "
    <> machineIdText actual
    <> ", not this machine ("
    <> machineIdText expected
    <> ").  Restore this machine's state or run migration from a trusted local mirror."
formatStateError (MissingRepositoryState identifier) =
  "The machine-state record for repository "
    <> repositoryIdText identifier
    <> " disappeared before its lifecycle update.  Run the command again after "
    <> "restoring the repository state."
formatStateError (RepositoryStateGenerationMismatch identifier expected actual) =
  "The machine-state record for repository "
    <> repositoryIdText identifier
    <> " was replaced while a lifecycle action was running (expected generation "
    <> stateGenerationIdText expected
    <> ", found "
    <> stateGenerationIdText actual
    <> ").  Run the command again with the current repository state."
formatStateError (RepositoryForgetInProgress identifier) =
  "Repository forget is still in progress for "
    <> repositoryIdText identifier
    <> ".  Run `dojang forget` again before using another stateful command."
formatStateError (DuplicateRepositoryIdentity identifier oldPath newPath) =
  "Repository identity "
    <> repositoryIdText identifier
    <> " is already used by checkout "
    <> osPathText oldPath
    <> ", so checkout "
    <> osPathText newPath
    <> " cannot share its state.  Give the duplicate repository a new identity."
formatStateError (StaleRepositoryCheckout identifier checkoutPath) =
  "The recorded checkout at "
    <> osPathText checkoutPath
    <> " is missing or no longer declares repository identity "
    <> repositoryIdText identifier
    <> ".  Locate the repository and select it explicitly with "
    <> "`-r`/`--repository-dir`."
formatStateError (ConflictingSnapshots oldPath newPath) =
  "Both "
    <> osPathText oldPath
    <> " and "
    <> osPathText newPath
    <> " contain different intermediate snapshots.  Preserve both copies, "
    <> "choose the correct common ancestor, and retry `dojang migrate`."
formatStateError (UnsupportedSnapshotSymlink symlinkPath) =
  "Cannot migrate symbolic link "
    <> osPathText symlinkPath
    <> " because intermediate snapshots support regular files and directories only."
formatStateError (UnsupportedSnapshotEntry entryPath) =
  "Cannot migrate snapshot entry "
    <> osPathText entryPath
    <> " because it is not a regular file or directory.  Preserve the snapshot, "
    <> "remove the unsupported entry, and retry."
formatStateError (InvalidSnapshotLocation snapshotPath) =
  "The intermediate snapshot path "
    <> osPathText snapshotPath
    <> " exists but is not a directory.  Move that entry aside and retry."
formatStateError (SnapshotInsideLegacy legacyPath snapshotPath) =
  "The intermediate snapshot path "
    <> osPathText snapshotPath
    <> " is inside the legacy snapshot at "
    <> osPathText legacyPath
    <> ".  Choose a path outside `.dojang` and retry."
formatStateError (UnownedSnapshot snapshotPath) =
  "The intermediate snapshot at "
    <> osPathText snapshotPath
    <> " has no state record or legacy snapshot that proves its ownership.  "
    <> "Preserve it, move it aside, and retry."
formatStateError (MissingSnapshot snapshotPath) =
  "The machine-state record points to missing intermediate snapshot "
    <> osPathText snapshotPath
    <> ".  Restore the snapshot from a trusted copy before continuing."
formatStateError (OverlappingSnapshots oldPath newPath) =
  "The intermediate snapshot paths "
    <> osPathText oldPath
    <> " and "
    <> osPathText newPath
    <> " overlap.  Choose a path outside the current snapshot and retry."
formatStateError (OverlappingStateRoot stateRoot legacyPath) =
  "The machine-state root "
    <> osPathText stateRoot
    <> " overlaps the legacy snapshot at "
    <> osPathText legacyPath
    <> ".  Choose a native data directory outside `.dojang` and retry."
formatStateError (ProtectedSnapshotLocation snapshotPath protectedPath) =
  "The intermediate snapshot path "
    <> osPathText snapshotPath
    <> " contains the protected path "
    <> osPathText protectedPath
    <> ".  Choose a dedicated snapshot directory and retry."
formatStateError (MigrationDestinationMismatch recorded requested) =
  "The interrupted migration targets "
    <> osPathText recorded
    <> ", but this retry requested "
    <> osPathText requested
    <> ".  Retry with the original intermediate path so unrelated data is not removed."
formatStateError (MigrationSourceMismatch recorded expected) =
  "The interrupted migration records source "
    <> osPathText recorded
    <> ", but the current repository state expects "
    <> osPathText expected
    <> ".  Preserve both paths and repair the migration marker before retrying."
formatStateError (LegacyCleanupFailed oldPath newPath details) =
  "The external snapshot was verified at "
    <> osPathText newPath
    <> ", but the old recovery copy at "
    <> osPathText oldPath
    <> " could not be removed.  Keep it until you have checked the external "
    <> "copy, then remove it manually.\n"
    <> details


stateIOError :: IOError -> StateError
stateIOError = StateIOError . showt . FromStringShow


-- | Converts filesystem failures into the machine-state error channel.
--
-- State operations returning an explicit 'StateError' use this at their
-- public boundary so callers never need a second exception path for ordinary
-- state-store I/O failures.
catchStateIOErrors
  :: (MonadFileSystem m)
  => m (Either StateError a)
  -> m (Either StateError a)
catchStateIOErrors action =
  action `catchError` (return . Left . stateIOError)


-- | Acquires a machine-state lock without changing the action's I/O errors.
--
-- A failure to acquire the lock is returned as a 'StateError'.  Once the lock
-- is held, any 'IOError' raised by the supplied action remains in the monad's
-- original error channel.
withStateFileLock
  :: (MonadFileSystem m)
  => OsPath
  -> m a
  -> m (Either StateError a)
withStateFileLock lockPath action = do
  acquired <- catchStateIOErrors $
    withFileLock lockPath $ do
      result <- tryError action
      return $ Right result
  case acquired of
    Left err -> return $ Left err
    Right (Left err) -> throwError err
    Right (Right value) -> return $ Right value


data StateDocument = StateDocument
  { documentSchemaVersion :: Integer
  , documentRepositoryId :: Text
  , documentMachineId :: Text
  , documentGenerationId :: Text
  , documentCheckoutPath :: Text
  , documentManifestPath :: Text
  , documentIntermediatePath :: Text
  , documentTargetSnapshotRoot :: Text
  , documentFactsFile :: Maybe Text
  , documentFacts :: Map Text Text
  , documentCreatedTime :: Text
  , documentUpdatedTime :: Text
  , documentLifecycle :: LifecycleDocument
  , documentTargets :: Map Text TargetDocument
  , documentHooks :: Map Text Text
  , documentHookExecutions :: Map Text HookExecutionDocument
  }


-- | Schema version 3, before repository machine facts were introduced.
data LegacyV3StateDocument = LegacyV3StateDocument
  { v3DocumentSchemaVersion :: Integer
  , v3DocumentRepositoryId :: Text
  , v3DocumentMachineId :: Text
  , v3DocumentGenerationId :: Text
  , v3DocumentCheckoutPath :: Text
  , v3DocumentManifestPath :: Text
  , v3DocumentIntermediatePath :: Text
  , v3DocumentTargetSnapshotRoot :: Text
  , v3DocumentCreatedTime :: Text
  , v3DocumentUpdatedTime :: Text
  , v3DocumentLifecycle :: LifecycleDocument
  , v3DocumentTargets :: Map Text TargetDocument
  , v3DocumentHooks :: Map Text Text
  , v3DocumentHookExecutions :: Map Text HookExecutionDocument
  }


-- | Schema version 2, before typed hook execution records were introduced.
data LegacyV2StateDocument = LegacyV2StateDocument
  { v2DocumentSchemaVersion :: Integer
  , v2DocumentRepositoryId :: Text
  , v2DocumentMachineId :: Text
  , v2DocumentCheckoutPath :: Text
  , v2DocumentManifestPath :: Text
  , v2DocumentIntermediatePath :: Text
  , v2DocumentTargetSnapshotRoot :: Text
  , v2DocumentCreatedTime :: Text
  , v2DocumentUpdatedTime :: Text
  , v2DocumentLifecycle :: LifecycleDocument
  , v2DocumentTargets :: Map Text TargetDocument
  , v2DocumentHooks :: Map Text Text
  }


-- | Schema version 1, before managed-target records were introduced.
data LegacyStateDocument = LegacyStateDocument
  { legacyDocumentSchemaVersion :: Integer
  , legacyDocumentRepositoryId :: Text
  , legacyDocumentMachineId :: Text
  , legacyDocumentCheckoutPath :: Text
  , legacyDocumentManifestPath :: Text
  , legacyDocumentIntermediatePath :: Text
  , legacyDocumentCreatedTime :: Text
  , legacyDocumentUpdatedTime :: Text
  , legacyDocumentLifecycle :: LifecycleDocument
  , legacyDocumentTargets :: Map Text Text
  , legacyDocumentHooks :: Map Text Text
  }


data TargetDocument = TargetDocument
  { targetDocumentId :: Text
  , targetDocumentRouteName :: Text
  , targetDocumentSourcePath :: Text
  , targetDocumentRouteType :: Text
  , targetDocumentDestinationPath :: Text
  , targetDocumentSnapshotPath :: Text
  , targetDocumentRouteDefinition :: Text
  , targetDocumentRouteProvenance :: Map Text Text
  , targetDocumentFingerprintKind :: Text
  , targetDocumentFingerprintSize :: Integer
  , targetDocumentFingerprintDigest :: Text
  , targetDocumentUpdatedBy :: Text
  , targetDocumentUpdatedTime :: Text
  }


data HookExecutionDocument = HookExecutionDocument
  { hookExecutionDocumentEvent :: Text
  , hookExecutionDocumentId :: Text
  , hookExecutionDocumentPolicy :: Text
  , hookExecutionDocumentFingerprint :: Maybe Text
  , hookExecutionDocumentSucceededTime :: Text
  }


instance FromValue HookExecutionDocument where
  fromValue =
    parseTableFromValue $
      HookExecutionDocument
        <$> reqKey "event"
        <*> reqKey "id"
        <*> reqKey "policy"
        <*> optKey "fingerprint"
        <*> reqKey "succeeded-at"


instance ToValue HookExecutionDocument where
  toValue = defaultTableToValue


instance ToTable HookExecutionDocument where
  toTable execution =
    table $
      [ "event" .= execution.hookExecutionDocumentEvent
      , "id" .= execution.hookExecutionDocumentId
      , "policy" .= execution.hookExecutionDocumentPolicy
      , "succeeded-at" .= execution.hookExecutionDocumentSucceededTime
      ]
        ++ maybe
          []
          (\value -> ["fingerprint" .= value])
          execution.hookExecutionDocumentFingerprint


instance FromValue TargetDocument where
  fromValue =
    parseTableFromValue $
      TargetDocument
        <$> reqKey "id"
        <*> reqKey "route-name"
        <*> reqKey "source-path"
        <*> reqKey "route-type"
        <*> reqKey "destination-path"
        <*> reqKey "snapshot-path"
        <*> reqKey "route-definition"
        <*> reqKey "route-provenance"
        <*> reqKey "fingerprint-kind"
        <*> reqKey "fingerprint-size"
        <*> reqKey "fingerprint-digest"
        <*> reqKey "updated-by"
        <*> reqKey "updated-at"


instance ToValue TargetDocument where
  toValue = defaultTableToValue


instance ToTable TargetDocument where
  toTable target =
    table
      [ "id" .= target.targetDocumentId
      , "route-name" .= target.targetDocumentRouteName
      , "source-path" .= target.targetDocumentSourcePath
      , "route-type" .= target.targetDocumentRouteType
      , "destination-path" .= target.targetDocumentDestinationPath
      , "snapshot-path" .= target.targetDocumentSnapshotPath
      , "route-definition" .= target.targetDocumentRouteDefinition
      , "route-provenance" .= target.targetDocumentRouteProvenance
      , "fingerprint-kind" .= target.targetDocumentFingerprintKind
      , "fingerprint-size" .= target.targetDocumentFingerprintSize
      , "fingerprint-digest" .= target.targetDocumentFingerprintDigest
      , "updated-by" .= target.targetDocumentUpdatedBy
      , "updated-at" .= target.targetDocumentUpdatedTime
      ]


newtype SchemaDocument = SchemaDocument
  { schemaDocumentVersion :: Integer
  }


instance FromValue SchemaDocument where
  fromValue = parseTableFromValue $ SchemaDocument <$> reqKey "schema-version"


data LifecycleDocument = LifecycleDocument
  { lifecycleFirstApplied :: Bool
  , lifecyclePendingCleanup :: [Text]
  }


instance FromValue LifecycleDocument where
  fromValue =
    parseTableFromValue $
      LifecycleDocument
        <$> reqKey "first-applied"
        <*> (fromMaybe [] <$> optKey "pending-cleanup")


instance ToValue LifecycleDocument where
  toValue = defaultTableToValue


instance ToTable LifecycleDocument where
  toTable lifecycle =
    table
      [ "first-applied" .= lifecycle.lifecycleFirstApplied
      , "pending-cleanup" .= lifecycle.lifecyclePendingCleanup
      ]


instance FromValue StateDocument where
  fromValue =
    parseTableFromValue $
      StateDocument
        <$> reqKey "schema-version"
        <*> reqKey "repository-id"
        <*> reqKey "machine-id"
        <*> reqKey "generation-id"
        <*> reqKey "checkout-path"
        <*> reqKey "manifest-path"
        <*> reqKey "intermediate-path"
        <*> reqKey "target-snapshot-root"
        <*> optKey "facts-file"
        <*> (fromMaybe Map.empty <$> optKey "facts")
        <*> reqKey "created-at"
        <*> reqKey "updated-at"
        <*> reqKey "lifecycle"
        <*> reqKey "targets"
        <*> reqKey "hooks"
        <*> reqKey "hook-executions"


instance FromValue LegacyV3StateDocument where
  fromValue =
    parseTableFromValue $
      LegacyV3StateDocument
        <$> reqKey "schema-version"
        <*> reqKey "repository-id"
        <*> reqKey "machine-id"
        <*> reqKey "generation-id"
        <*> reqKey "checkout-path"
        <*> reqKey "manifest-path"
        <*> reqKey "intermediate-path"
        <*> reqKey "target-snapshot-root"
        <*> reqKey "created-at"
        <*> reqKey "updated-at"
        <*> reqKey "lifecycle"
        <*> reqKey "targets"
        <*> reqKey "hooks"
        <*> reqKey "hook-executions"


instance FromValue LegacyV2StateDocument where
  fromValue =
    parseTableFromValue $
      LegacyV2StateDocument
        <$> reqKey "schema-version"
        <*> reqKey "repository-id"
        <*> reqKey "machine-id"
        <*> reqKey "checkout-path"
        <*> reqKey "manifest-path"
        <*> reqKey "intermediate-path"
        <*> reqKey "target-snapshot-root"
        <*> reqKey "created-at"
        <*> reqKey "updated-at"
        <*> reqKey "lifecycle"
        <*> reqKey "targets"
        <*> reqKey "hooks"


instance FromValue LegacyStateDocument where
  fromValue =
    parseTableFromValue $
      LegacyStateDocument
        <$> reqKey "schema-version"
        <*> reqKey "repository-id"
        <*> reqKey "machine-id"
        <*> reqKey "checkout-path"
        <*> reqKey "manifest-path"
        <*> reqKey "intermediate-path"
        <*> reqKey "created-at"
        <*> reqKey "updated-at"
        <*> reqKey "lifecycle"
        <*> reqKey "targets"
        <*> reqKey "hooks"


instance ToValue StateDocument where
  toValue = defaultTableToValue


instance ToTable StateDocument where
  toTable document =
    table $
      [ "schema-version" .= document.documentSchemaVersion
      , "repository-id" .= document.documentRepositoryId
      , "machine-id" .= document.documentMachineId
      , "generation-id" .= document.documentGenerationId
      , "checkout-path" .= document.documentCheckoutPath
      , "manifest-path" .= document.documentManifestPath
      , "intermediate-path" .= document.documentIntermediatePath
      , "target-snapshot-root" .= document.documentTargetSnapshotRoot
      ]
        ++ maybe
          []
          (\factsFile -> ["facts-file" .= factsFile])
          document.documentFactsFile
        ++ [ "facts" .= document.documentFacts
           , "created-at" .= document.documentCreatedTime
           , "updated-at" .= document.documentUpdatedTime
           , "lifecycle" .= document.documentLifecycle
           , "targets" .= document.documentTargets
           , "hooks" .= document.documentHooks
           , "hook-executions" .= document.documentHookExecutions
           ]


-- | Encodes a repository record to versioned TOML.
encodeMachineState :: MachineState -> Text
encodeMachineState state = showt $ FromStringShow $ encode document
 where
  document =
    StateDocument
      state.schemaVersion
      (repositoryIdText state.repositoryId)
      (machineIdText state.machineId)
      (stateGenerationIdText state.generationId)
      (osPathText state.checkoutPath)
      (osPathText state.manifestPath)
      (osPathText state.intermediatePath)
      (osPathText state.targetSnapshotRoot)
      (osPathText <$> state.factsFile)
      ( Map.fromList
          [ (factKeyText key, factValueText value)
          | (key, value) <- Map.toList state.declaredFacts
          ]
      )
      (timeText state.createdTime)
      (timeText state.updatedTime)
      ( LifecycleDocument
          state.firstApplied
          (osPathText <$> state.pendingCleanupPaths)
      )
      (targetToDocument <$> state.targetRecords)
      state.hookRecords
      ( Map.fromList
          [ (renderHookExecutionKey key, hookExecutionToDocument key execution)
          | (key, execution) <- Map.toList state.hookExecutions
          ]
      )


-- | Decodes and validates a repository record for the expected owners.
decodeMachineState
  :: RepositoryId
  -> MachineId
  -> Text
  -> Either StateError MachineState
decodeMachineState = decodeMachineStateWithTargetRoot Nothing


decodeMachineStateWithTargetRoot
  :: Maybe OsPath
  -> RepositoryId
  -> MachineId
  -> Text
  -> Either StateError MachineState
decodeMachineStateWithTargetRoot expectedTargetRoot expectedRepository expectedMachine source = do
  versionDocument <- case decodedVersion of
    Success _ value -> Right value
    Failure errors -> Left $ MalformedState $ Text.intercalate "\n" $ Text.pack <$> errors
  document <- case versionDocument.schemaDocumentVersion of
    1 -> upgradeLegacyDocument expectedTargetRoot source
    2 -> upgradeV2Document source
    3 -> upgradeV3Document source
    version
      | version == schemaVersion -> decodeCurrentDocument source
      | otherwise -> Left $ UnsupportedSchemaVersion version
  decodeDocument document
 where
  decodedVersion :: Result String SchemaDocument
  decodedVersion = decode $ Text.unpack source

  decodeDocument :: StateDocument -> Either StateError MachineState
  decodeDocument document = do
    actualRepository <-
      mapLeft MalformedState $ parseRepositoryId document.documentRepositoryId
    actualMachine <-
      mapLeft MalformedState $ parseMachineId document.documentMachineId
    generation <-
      mapLeft MalformedState $
        parseStateGenerationId document.documentGenerationId
    whenEither (actualRepository /= expectedRepository) $
      RepositoryIdentityMismatch expectedRepository actualRepository
    whenEither (actualMachine /= expectedMachine) $
      MachineProvenanceMismatch expectedMachine actualMachine
    checkout <- mapLeft MalformedState $ textOsPath document.documentCheckoutPath
    manifest <- mapLeft MalformedState $ textOsPath document.documentManifestPath
    intermediate <-
      mapLeft MalformedState $ textOsPath document.documentIntermediatePath
    targetSnapshots <-
      mapLeft MalformedState $ textOsPath document.documentTargetSnapshotRoot
    factsFile' <-
      traverse
        (mapLeft MalformedState . textOsPath)
        document.documentFactsFile
    facts <- factsFromDocument document.documentFacts
    validateMachineFacts factsFile' facts
    whenEither (not $ isAbsolute checkout) $
      MalformedState "The checkout-path field must be absolute."
    whenEither (not $ isAbsolute manifest) $
      MalformedState "The manifest-path field must be absolute."
    whenEither (not $ isAbsolute intermediate) $
      MalformedState "The intermediate-path field must be absolute."
    whenEither (not $ isAbsolute targetSnapshots) $
      MalformedState "The target-snapshot-root field must be absolute."
    created <- parseTime "created-at" document.documentCreatedTime
    updated <- parseTime "updated-at" document.documentUpdatedTime
    targets <- traverseWithKeyEither targetFromDocument document.documentTargets
    hookExecutionPairs <-
      traverseWithKeyEither hookExecutionFromDocument document.documentHookExecutions
    let hookExecutions = Map.fromList $ Map.elems hookExecutionPairs
    pendingCleanup <-
      traverse
        (mapLeft MalformedState . textOsPath)
        document.documentLifecycle.lifecyclePendingCleanup
    whenEither (any (not . isAbsolute) pendingCleanup) $
      MalformedState "Every pending-cleanup path must be absolute."
    whenEither
      ( any (not . isProperDescendant targetSnapshots . (.snapshotPath)) $
          Map.elems targets
      )
      ( MalformedState
          "Every managed target snapshot-path must be inside target-snapshot-root."
      )
    whenEither
      (any (not . isCleanupPathInside intermediate targetSnapshots) pendingCleanup)
      ( MalformedState
          "Every pending-cleanup path must be inside a managed snapshot root."
      )
    return $
      MachineState
        schemaVersion
        actualRepository
        actualMachine
        generation
        checkout
        manifest
        intermediate
        targetSnapshots
        factsFile'
        facts
        created
        updated
        document.documentLifecycle.lifecycleFirstApplied
        pendingCleanup
        targets
        document.documentHooks
        hookExecutions


decodeCurrentDocument :: Text -> Either StateError StateDocument
decodeCurrentDocument source = case decode $ Text.unpack source of
  Success _ value -> Right value
  Failure errors -> Left $ MalformedState $ Text.intercalate "\n" $ Text.pack <$> errors


factsFromDocument :: Map Text Text -> Either StateError FactMap
factsFromDocument = fmap Map.fromList . traverse parseFact . Map.toList
 where
  parseFact (keyText, value) = do
    key <- mapLeft MalformedState $ parseFactKey keyText
    return (key, fromString $ Text.unpack value)


validateMachineFacts :: Maybe OsPath -> FactMap -> Either StateError ()
validateMachineFacts factsFile' facts = do
  whenEither
    ( maybe
        False
        ( \candidate ->
            not (isAbsolute candidate)
              && not (isSafeManagedRelativePath candidate)
        )
        factsFile'
    )
    (MalformedState "The facts-file field must be absolute or a safe relative path.")
  whenEither (any isBuiltInFact $ Map.keys facts) $
    MalformedState "The facts table must not override a built-in machine fact."


upgradeV3Document :: Text -> Either StateError StateDocument
upgradeV3Document source = case decoded of
  Success _ document -> do
    whenEither (document.v3DocumentSchemaVersion /= 3) $
      UnsupportedSchemaVersion document.v3DocumentSchemaVersion
    Right $
      StateDocument
        schemaVersion
        document.v3DocumentRepositoryId
        document.v3DocumentMachineId
        document.v3DocumentGenerationId
        document.v3DocumentCheckoutPath
        document.v3DocumentManifestPath
        document.v3DocumentIntermediatePath
        document.v3DocumentTargetSnapshotRoot
        Nothing
        Map.empty
        document.v3DocumentCreatedTime
        document.v3DocumentUpdatedTime
        document.v3DocumentLifecycle
        document.v3DocumentTargets
        document.v3DocumentHooks
        document.v3DocumentHookExecutions
  Failure errors ->
    Left $ MalformedState $ Text.intercalate "\n" $ Text.pack <$> errors
 where
  decoded :: Result String LegacyV3StateDocument
  decoded = decode $ Text.unpack source


upgradeV2Document :: Text -> Either StateError StateDocument
upgradeV2Document source = case decoded of
  Success _ document -> do
    whenEither (document.v2DocumentSchemaVersion /= 2) $
      UnsupportedSchemaVersion document.v2DocumentSchemaVersion
    Right $
      StateDocument
        schemaVersion
        document.v2DocumentRepositoryId
        document.v2DocumentMachineId
        document.v2DocumentRepositoryId
        document.v2DocumentCheckoutPath
        document.v2DocumentManifestPath
        document.v2DocumentIntermediatePath
        document.v2DocumentTargetSnapshotRoot
        Nothing
        Map.empty
        document.v2DocumentCreatedTime
        document.v2DocumentUpdatedTime
        document.v2DocumentLifecycle
        document.v2DocumentTargets
        document.v2DocumentHooks
        Map.empty
  Failure errors ->
    Left $ MalformedState $ Text.intercalate "\n" $ Text.pack <$> errors
 where
  decoded :: Result String LegacyV2StateDocument
  decoded = decode $ Text.unpack source


upgradeLegacyDocument
  :: Maybe OsPath
  -> Text
  -> Either StateError StateDocument
upgradeLegacyDocument expectedTargetRoot source = do
  legacy <- case decoded of
    Success _ value -> Right value
    Failure errors -> Left $ MalformedState $ Text.intercalate "\n" $ Text.pack <$> errors
  whenEither (legacy.legacyDocumentSchemaVersion /= 1) $
    UnsupportedSchemaVersion legacy.legacyDocumentSchemaVersion
  whenEither (not $ Map.null legacy.legacyDocumentTargets) $
    MalformedState "Schema-v1 target data cannot be upgraded safely."
  intermediate <-
    mapLeft MalformedState $ textOsPath legacy.legacyDocumentIntermediatePath
  let targetRoot =
        case expectedTargetRoot of
          Just root -> root
          Nothing -> takeDirectory intermediate </> path "targets"
  return $
    StateDocument
      schemaVersion
      legacy.legacyDocumentRepositoryId
      legacy.legacyDocumentMachineId
      legacy.legacyDocumentRepositoryId
      legacy.legacyDocumentCheckoutPath
      legacy.legacyDocumentManifestPath
      legacy.legacyDocumentIntermediatePath
      (osPathText targetRoot)
      Nothing
      Map.empty
      legacy.legacyDocumentCreatedTime
      legacy.legacyDocumentUpdatedTime
      legacy.legacyDocumentLifecycle
      Map.empty
      legacy.legacyDocumentHooks
      Map.empty
 where
  decoded :: Result String LegacyStateDocument
  decoded = decode $ Text.unpack source


targetToDocument :: ManagedTarget -> TargetDocument
targetToDocument target =
  TargetDocument
    target.targetId
    (osPathText target.routeName)
    (osPathText target.sourcePath)
    ( case target.routeType of
        File -> "file"
        Directory -> "directory"
        Symlink -> "symlink"
    )
    (osPathText target.destinationPath)
    (osPathText target.snapshotPath)
    target.routeDefinition
    target.routeProvenance
    fingerprintKind
    fingerprintSize
    fingerprintDigest
    (case target.updatedBy of Applied -> "apply"; Reflected -> "reflect")
    (timeText target.updatedTime)
 where
  (fingerprintKind, fingerprintSize, fingerprintDigest) =
    case target.fingerprint of
      FileFingerprint size digest -> ("file", size, digest)
      DirectoryFingerprint -> ("directory", 0, "")


hookExecutionToDocument
  :: HookExecutionKey -> HookExecution -> HookExecutionDocument
hookExecutionToDocument (HookExecutionKey event identifier) execution =
  HookExecutionDocument
    (renderHookType event)
    (renderHookId identifier)
    policy
    fingerprint
    (timeText execution.succeededTime)
 where
  (policy, fingerprint) = case execution.policy of
    HookOnceExecution -> ("once", Nothing)
    HookOnChangeExecution value -> ("on-change", Just value)


hookExecutionFromDocument
  :: Text
  -> HookExecutionDocument
  -> Either StateError (HookExecutionKey, HookExecution)
hookExecutionFromDocument key execution = do
  event <-
    maybe
      (Left $ MalformedState "A hook execution has an unknown lifecycle event.")
      Right
      ( find
          ((== execution.hookExecutionDocumentEvent) . renderHookType)
          allHookTypes
      )
  identifier <-
    mapLeft MalformedState $ parseHookId execution.hookExecutionDocumentId
  whenEither
    (renderHookId identifier /= execution.hookExecutionDocumentId)
    (MalformedState "A hook execution has a noncanonical hook id.")
  let typedKey = HookExecutionKey event identifier
  whenEither (key /= renderHookExecutionKey typedKey) $
    MalformedState
      "A hook execution table key does not match its event and id fields."
  policy <- case execution.hookExecutionDocumentPolicy of
    "once" ->
      HookOnceExecution
        <$ whenEither
          (execution.hookExecutionDocumentFingerprint /= Nothing)
          (MalformedState "A once hook execution must not have a fingerprint.")
    "on-change" ->
      maybe
        (Left $ MalformedState "An on-change hook execution must have a fingerprint.")
        (Right . HookOnChangeExecution)
        execution.hookExecutionDocumentFingerprint
    _ -> Left $ MalformedState "A hook execution has an unknown stateful policy."
  succeeded <-
    parseTime
      "hook-executions.succeeded-at"
      execution.hookExecutionDocumentSucceededTime
  return (typedKey, HookExecution policy succeeded)


targetFromDocument :: Text -> TargetDocument -> Either StateError ManagedTarget
targetFromDocument key target = do
  whenEither (key /= target.targetDocumentId) $
    MalformedState "A target table key does not match its id field."
  route <- mapLeft MalformedState $ textOsPath target.targetDocumentRouteName
  source <- mapLeft MalformedState $ textOsPath target.targetDocumentSourcePath
  destination <-
    mapLeft MalformedState $ textOsPath target.targetDocumentDestinationPath
  snapshot <-
    mapLeft MalformedState $ textOsPath target.targetDocumentSnapshotPath
  whenEither (isAbsolute route) $
    MalformedState "A managed target route-name must be relative."
  whenEither (isAbsolute source) $
    MalformedState "A managed target source-path must be relative."
  whenEither (not $ isSafeManagedRelativePath source) $
    MalformedState "A managed target source-path must not contain parent traversal."
  routeType <- case target.targetDocumentRouteType of
    "file" -> Right File
    "directory" -> Right Directory
    "symlink" -> Right Symlink
    other -> Left $ MalformedState $ "Unknown target route type: " <> other
  whenEither (not $ isAbsolute destination) $
    MalformedState "A managed target destination-path must be absolute."
  whenEither (not $ isAbsolute snapshot) $
    MalformedState "A managed target snapshot-path must be absolute."
  fingerprint <- case target.targetDocumentFingerprintKind of
    "file" ->
      Right $
        FileFingerprint
          target.targetDocumentFingerprintSize
          target.targetDocumentFingerprintDigest
    "directory" -> Right DirectoryFingerprint
    other -> Left $ MalformedState $ "Unknown target fingerprint kind: " <> other
  command <- case target.targetDocumentUpdatedBy of
    "apply" -> Right Applied
    "reflect" -> Right Reflected
    other -> Left $ MalformedState $ "Unknown target update command: " <> other
  updated <- parseTime "targets.updated-at" target.targetDocumentUpdatedTime
  return $
    ManagedTarget
      key
      route
      source
      routeType
      destination
      snapshot
      target.targetDocumentRouteDefinition
      target.targetDocumentRouteProvenance
      fingerprint
      command
      updated


traverseWithKeyEither
  :: (Ord key)
  => (key -> value -> Either error result)
  -> Map key value
  -> Either error (Map key result)
traverseWithKeyEither f =
  fmap Map.fromList
    . traverse (\(key, value) -> fmap (\result -> (key, result)) $ f key value)
    . Map.toList


encodeMigrationMarker :: OsPath -> OsPath -> ByteString
encodeMigrationMarker source destination =
  encodeUtf8 $
    showt $
      FromStringShow $
        encode $
          MigrationMarkerDocument
            (osPathText source)
            (osPathText destination)


decodeMigrationMarker :: ByteString -> Either StateError (OsPath, OsPath)
decodeMigrationMarker contents = do
  source <-
    mapLeft
      ( MalformedMigrationMarker
          . ("The marker is not valid UTF-8: " <>)
          . Text.pack
          . show
      )
      (decodeUtf8' contents)
  document <- case decoded source of
    Failure errors ->
      Left $
        MalformedMigrationMarker $
          Text.intercalate "\n" $
            Text.pack <$> errors
    Success _ value -> Right value
  markerSource <-
    mapLeft MalformedMigrationMarker $
      textOsPath document.documentMigrationSource
  markerDestination <-
    mapLeft MalformedMigrationMarker $
      textOsPath document.documentMigrationDestination
  return (markerSource, markerDestination)
 where
  decoded :: Text -> Result String MigrationMarkerDocument
  decoded = decode . Text.unpack


-- | Directory that contains a repository's state and snapshots.
repositoryStateDirectory :: OsPath -> RepositoryId -> OsPath
repositoryStateDirectory root identifier =
  normalise $
    root </> path "repositories" </> textPath (repositoryIdText identifier)


-- | TOML state record path for a repository.
repositoryStatePath :: OsPath -> RepositoryId -> OsPath
repositoryStatePath root identifier =
  repositoryStateDirectory root identifier </> path "state.toml"


repositoryStateLockPath :: OsPath -> RepositoryId -> OsPath
repositoryStateLockPath root identifier =
  repositoryStateDirectory root identifier </> path "state.lock"


-- | Default full intermediate snapshot path for a repository.
defaultIntermediatePath :: OsPath -> RepositoryId -> OsPath
defaultIntermediatePath root identifier =
  normalise $
    repositoryStateDirectory root identifier
      </> path "snapshots"
      </> path "current"


-- | Private root containing immutable managed-target baselines.
managedTargetSnapshotRoot :: OsPath -> RepositoryId -> OsPath
managedTargetSnapshotRoot root identifier =
  normalise $
    repositoryStateDirectory root identifier
      </> path "snapshots"
      </> path "targets"


-- | Marker used to make an interrupted legacy migration retryable.
migrationMarkerPath :: OsPath -> RepositoryId -> OsPath
migrationMarkerPath root identifier =
  repositoryStateDirectory root identifier </> path "migration-in-progress"


-- | Marker that preserves approval across an interrupted repository forget.
forgetMarkerPath :: OsPath -> RepositoryId -> OsPath
forgetMarkerPath root identifier =
  repositoryStateDirectory root identifier </> path "forget-in-progress"


-- | Checks whether repository cleanup was approved before an interruption.
--
-- Callers that use this result to skip lifecycle validation must hold the
-- repository state lock until cleanup completes.
isRepositoryForgetInProgress
  :: (MonadFileSystem m) => OsPath -> RepositoryId -> m (Either StateError Bool)
isRepositoryForgetInProgress root identifier =
  catchStateIOErrors $
    validateStateDocument
      "repository forget marker"
      (forgetMarkerPath root identifier)


-- | Atomically records approval for destructive repository-state cleanup.
--
-- Callers must validate the current state while holding the repository state
-- lock before creating this marker.
markRepositoryForgetInProgress
  :: (MonadFileSystem m) => OsPath -> RepositoryId -> m (Either StateError ())
markRepositoryForgetInProgress root identifier = catchStateIOErrors $ do
  let marker = forgetMarkerPath root identifier
  validated <- validateStateDocument "repository forget marker" marker
  case validated of
    Left err -> return $ Left err
    Right True -> return $ Right ()
    Right False -> do
      writeFileAtomically marker "forget-in-progress.tmp" "approved"
      return $ Right ()


-- | Reads a repository state record, distinguishing absence from invalid state.
readRepositoryState
  :: (MonadFileSystem m)
  => OsPath
  -> RepositoryId
  -> MachineId
  -> m (Either StateError (Maybe MachineState))
readRepositoryState root repositoryId' machineId' = catchStateIOErrors $ do
  let targetRoot = managedTargetSnapshotRoot root repositoryId'
  targetSymlink <- findPrivateStateSymlink root repositoryId' targetRoot
  case targetSymlink of
    Just symlink -> return $ Left $ UnsupportedSnapshotSymlink symlink
    Nothing -> do
      let statePath = repositoryStatePath root repositoryId'
      validated <- validateStateDocument "repository state entry" statePath
      case validated of
        Left err -> return $ Left err
        Right False -> return $ Right Nothing
        Right True -> do
          contents <- readFile statePath
          return $ do
            source <- decodeStateUtf8 contents
            state <-
              decodeMachineStateWithTargetRoot
                (Just targetRoot)
                repositoryId'
                machineId'
                source
            whenEither
              (normalise state.targetSnapshotRoot /= targetRoot)
              ( MalformedState
                  "The target-snapshot-root field does not belong to this repository."
              )
            return $ Just state


findPrivateStateSymlink
  :: (MonadFileSystem m)
  => OsPath
  -> RepositoryId
  -> OsPath
  -> m (Maybe OsPath)
findPrivateStateSymlink root repositoryId' candidate = do
  parentComponent <- encodePath ".."
  let stateDirectory = repositoryStateDirectory root repositoryId'
  let relative = makeRelative stateDirectory candidate
  let components = splitDirectories relative
  let insidePrivateState =
        not (isAbsolute relative)
          && case components of
            [] -> True
            first : _ -> first /= parentComponent
  if not insidePrivateState
    then return Nothing
    else firstSymlink $ scanl (</>) stateDirectory components
 where
  firstSymlink [] = return Nothing
  firstSymlink (path' : rest) = do
    symlink <- isSymlink path'
    if symlink then return (Just path') else firstSymlink rest


-- | Validates a persisted intermediate snapshot before destructive use.
--
-- The snapshot must not be a symbolic link, contain a protected checkout or
-- machine-state path, or pass through any symbolic-link ancestor.
validateSelectedSnapshotLocation
  :: (MonadFileSystem m)
  => OsPath
  -> RepositoryId
  -> [OsPath]
  -> OsPath
  -> m (Either StateError ())
validateSelectedSnapshotLocation root repositoryId' protectedCheckouts snapshotPath =
  catchStateIOErrors $ do
    symlink <- findUnsupportedSnapshotSymlink root repositoryId' snapshotPath
    case symlink of
      Just symlinkPath ->
        return $ Left $ UnsupportedSnapshotSymlink symlinkPath
      Nothing ->
        validateProtectedSnapshotLocation
          root
          repositoryId'
          protectedCheckouts
          snapshotPath


findUnsupportedSnapshotSymlink
  :: (MonadFileSystem m)
  => OsPath
  -> RepositoryId
  -> OsPath
  -> m (Maybe OsPath)
findUnsupportedSnapshotSymlink _ _ = findPathSymlink


findDirectOrPrivateSnapshotSymlink
  :: (MonadFileSystem m)
  => OsPath
  -> RepositoryId
  -> OsPath
  -> m (Maybe OsPath)
findDirectOrPrivateSnapshotSymlink root repositoryId' snapshotPath = do
  directSymlink <- isSymlink snapshotPath
  if directSymlink
    then return $ Just snapshotPath
    else findPrivateStateSymlink root repositoryId' snapshotPath


findPathSymlink :: (MonadFileSystem m) => OsPath -> m (Maybe OsPath)
findPathSymlink candidate =
  firstSnapshotSymlink (pathRoot candidate') candidate'
 where
  candidate' = normalise candidate


pathRoot :: OsPath -> OsPath
pathRoot candidate
  | parent == candidate' = candidate'
  | otherwise = pathRoot parent
 where
  candidate' = normalise candidate
  parent = normalise $ takeDirectory candidate'


validateProtectedSnapshotLocation
  :: (MonadFileSystem m)
  => OsPath
  -> RepositoryId
  -> [OsPath]
  -> OsPath
  -> m (Either StateError ())
validateProtectedSnapshotLocation
  root
  repositoryId'
  protectedCheckouts
  snapshotPath = do
    snapshotsName <- encodePath "snapshots"
    resolvedSnapshot <- canonicalizePath snapshotPath
    resolvedCheckouts <- mapM canonicalizePath protectedCheckouts
    resolvedRoot <- canonicalizePath root
    resolvedStateDirectory <-
      canonicalizePath $ repositoryStateDirectory root repositoryId'
    resolvedSnapshotStore <-
      canonicalizePath $ resolvedStateDirectory </> snapshotsName
    let targetSnapshotRoot = managedTargetSnapshotRoot root repositoryId'
    resolvedTargetSnapshotRoot <- canonicalizePath targetSnapshotRoot
    let contains protected =
          resolvedSnapshot == protected
            || isProperDescendant resolvedSnapshot protected
    let inside directory =
          resolvedSnapshot == directory
            || isProperDescendant directory resolvedSnapshot
    return $ case find (contains . snd) $ zip protectedCheckouts resolvedCheckouts of
      Just (protectedCheckout, _) ->
        Left $ ProtectedSnapshotLocation snapshotPath protectedCheckout
      Nothing ->
        if resolvedSnapshot == resolvedTargetSnapshotRoot
          || pathsOverlap resolvedSnapshot resolvedTargetSnapshotRoot
          then Left $ ProtectedSnapshotLocation snapshotPath targetSnapshotRoot
          else
            if contains resolvedRoot
              then Left $ ProtectedSnapshotLocation snapshotPath root
              else
                if inside resolvedRoot && not (inside resolvedSnapshotStore)
                  then
                    Left $
                      ProtectedSnapshotLocation
                        snapshotPath
                        (repositoryStateDirectory root repositoryId')
                  else Right ()


-- | Reads or creates the identity of the local machine-state store.
ensureMachineId
  :: (MonadFileSystem m, MonadIO m)
  => OsPath
  -> m (Either StateError MachineId)
ensureMachineId root = catchStateIOErrors $ do
  createDirectories root
  withFileLock (root </> path "machine.lock") $ do
    loaded <- readMachineId root
    case loaded of
      Left err -> return $ Left err
      Right (Just identifier) -> return $ Right identifier
      Right Nothing -> do
        hasRepositoryData <- repositoryStoreHasData root
        if hasRepositoryData
          then
            return $
              Left $
                MalformedState
                  "The machine identity is missing while repository state remains."
          else do
            identifier <- MachineId <$> newRepositoryId
            let document = MachineDocument $ machineIdText identifier
            writeFileAtomically
              (root </> path "machine.toml")
              "machine.toml.tmp"
              (encodeUtf8 $ showt $ FromStringShow $ encode document)
            return $ Right identifier


-- | Reads the machine identity without creating a state store.
readMachineId
  :: (MonadFileSystem m)
  => OsPath
  -> m (Either StateError (Maybe MachineId))
readMachineId root = catchStateIOErrors $ do
  validatedRoot <- validateStateRoot root
  case validatedRoot of
    Left err -> return $ Left err
    Right () -> do
      let filename = root </> path "machine.toml"
      validated <- validateStateDocument "machine identity" filename
      case validated of
        Left err -> return $ Left err
        Right False -> do
          hasRepositoryData <- repositoryStoreHasData root
          return $
            if hasRepositoryData
              then
                Left $
                  MalformedState
                    "The machine identity is missing while repository state remains."
              else Right Nothing
        Right True -> do
          contents <- readFile filename
          return $ do
            source <- decodeStateUtf8 contents
            let decoded :: Result String MachineDocument
                decoded = decode $ Text.unpack source
            case decoded of
              Failure errors ->
                Left $
                  MalformedState $
                    Text.intercalate "\n" $
                      Text.pack <$> errors
              Success _ document ->
                Just
                  <$> mapLeft
                    MalformedState
                    (parseMachineId document.documentMachineIdentity)


validateStateRoot
  :: (MonadFileSystem m)
  => OsPath
  -> m (Either StateError ())
validateStateRoot root = do
  symlink <- isSymlink root
  present <- exists root
  directory <- isDirectory root
  return $
    if symlink || present && not directory
      then
        Left $
          MalformedState $
            "Machine-state root "
              <> osPathText root
              <> " is not a regular directory."
      else Right ()


validateStateDocument
  :: (MonadFileSystem m)
  => Text
  -> OsPath
  -> m (Either StateError Bool)
validateStateDocument description filename = do
  symlink <- isSymlink filename
  present <- exists filename
  if not present && not symlink
    then return $ Right False
    else do
      regularFile <- isRegularFile filename
      return $
        if symlink || not regularFile
          then
            Left $
              MalformedState $
                "The "
                  <> description
                  <> " "
                  <> osPathText filename
                  <> " is not a regular file."
          else Right True


repositoryStoreHasData :: (MonadFileSystem m) => OsPath -> m Bool
repositoryStoreHasData root = do
  let repositories = root </> path "repositories"
  symlink <- isSymlink repositories
  present <- exists repositories
  directory <- isDirectory repositories
  if symlink || present && not directory
    then return True
    else
      if not directory
        then return False
        else not . null <$> listDirectory repositories


-- | Validates the machine-state store before a command can modify a checkout.
validateMachineStateStore
  :: (MonadFileSystem m)
  => OsPath
  -> m (Either StateError ())
validateMachineStateStore root = catchStateIOErrors $ do
  validated <- validateRepositoryStoreRoot root
  case validated of
    Left err -> return $ Left err
    Right () -> fmap (const ()) <$> readMachineId root


-- | Validates that migration state cannot be removed with the legacy snapshot.
validateMigrationStateRoot
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> m (Either StateError ())
validateMigrationStateRoot checkout stateRoot = catchStateIOErrors $ do
  legacyName <- encodePath ".dojang"
  let legacy = normalise checkout </> legacyName
  let stateRoot' = normalise stateRoot
  resolvedLegacy <- canonicalizePath legacy
  resolvedStateRoot <- canonicalizePath stateRoot'
  return $
    if resolvedLegacy == resolvedStateRoot
      || pathsOverlap resolvedLegacy resolvedStateRoot
      then Left $ OverlappingStateRoot stateRoot' legacy
      else Right ()


validateRepositoryStoreRoot
  :: (MonadFileSystem m)
  => OsPath
  -> m (Either StateError ())
validateRepositoryStoreRoot root = do
  validatedRoot <- validateStateRoot root
  case validatedRoot of
    Left err -> return $ Left err
    Right () -> do
      let repositories = root </> path "repositories"
      symlink <- isSymlink repositories
      present <- exists repositories
      directory <- isDirectory repositories
      if symlink || present && not directory
        then do
          repositoriesPath <- Text.pack <$> decodePath repositories
          return $
            Left $
              MalformedState $
                "Repository state store "
                  <> repositoriesPath
                  <> " is not a regular directory."
        else return $ Right ()


validateRepositoryStateDirectory
  :: (MonadFileSystem m)
  => OsPath
  -> RepositoryId
  -> m (Either StateError ())
validateRepositoryStateDirectory root repositoryId' = do
  let repositoryDirectory = repositoryStateDirectory root repositoryId'
  symlink <- isSymlink repositoryDirectory
  present <- exists repositoryDirectory
  directory <- isDirectory repositoryDirectory
  if symlink || present && not directory
    then do
      repositoryPath <- Text.pack <$> decodePath repositoryDirectory
      return $
        Left $
          MalformedState $
            "Repository state directory "
              <> repositoryPath
              <> " is not a regular directory."
    else return $ Right ()


-- | Lists every valid repository record owned by the current machine.
listRepositoryStates
  :: (MonadFileSystem m)
  => OsPath
  -> MachineId
  -> m (Either StateError [MachineState])
listRepositoryStates root machineId' = catchStateIOErrors $ do
  let repositories = root </> path "repositories"
  validated <- validateRepositoryStoreRoot root
  present <- exists repositories
  case validated of
    Left err -> return $ Left err
    Right () -> do
      if not present
        then return $ Right []
        else do
          entries <- listDirectory repositories
          loadEntries repositories entries []
 where
  loadEntries _ [] states = return $ Right $ reverse states
  loadEntries repositories (entry : rest) states = do
    entryText <- Text.pack <$> decodePath entry
    case parseRepositoryId entryText of
      Left _ -> do
        let repositoryDirectory = repositories </> entry
        symlink <- isSymlink repositoryDirectory
        directory <- isDirectory repositoryDirectory
        if symlink || not directory
          then malformedRepositoryEntry repositoryDirectory
          else do
            loaded <-
              withFileLock (repositoryDirectory </> path "state.lock") $ do
                contents <- listDirectory repositoryDirectory
                let orphaned = filter (/= path "state.lock") contents
                if null orphaned
                  then return $ Right ()
                  else malformedRepositoryEntry repositoryDirectory
            case loaded of
              Left err -> return $ Left err
              Right () -> loadEntries repositories rest states
      Right repositoryId'
        | entryText /= repositoryIdText repositoryId' ->
            noncanonicalRepositoryEntry $ repositories </> entry
      Right repositoryId' -> do
        let repositoryDirectory = repositories </> entry
        let stateFile = repositoryDirectory </> path "state.toml"
        validatedDirectory <- validateRepositoryStateDirectory root repositoryId'
        case validatedDirectory of
          Left err -> return $ Left err
          Right () -> do
            loaded <-
              withFileLock (repositoryStateLockPath root repositoryId') $ do
                validated <- validateStateDocument "repository state entry" stateFile
                case validated of
                  Left err -> return $ Left err
                  Right False -> do
                    contents <- listDirectory repositoryDirectory
                    let unlocked = filter (/= path "state.lock") contents
                    forgetting <-
                      isRepositoryForgetInProgress root repositoryId'
                    case forgetting of
                      Left err -> return $ Left err
                      Right inProgress -> do
                        when inProgress $
                          removeFile $
                            forgetMarkerPath root repositoryId'
                        orphaned <-
                          filterM
                            ( fmap not
                                . isPersistentHookLock repositoryDirectory
                            )
                            (filter (/= path "forget-in-progress") unlocked)
                        retryable <-
                          isRetryableInitialSnapshot root repositoryId' orphaned
                        if null orphaned || retryable
                          then return $ Right Nothing
                          else do
                            repositoryPath <-
                              Text.pack <$> decodePath repositoryDirectory
                            return $
                              Left $
                                MalformedState $
                                  "Repository state directory "
                                    <> repositoryPath
                                    <> " contains data but has no state.toml record."
                  Right True -> do
                    readRepositoryState root repositoryId' machineId'
            case loaded of
              Left err -> return $ Left err
              Right Nothing -> loadEntries repositories rest states
              Right (Just state) -> loadEntries repositories rest (state : states)

  malformedRepositoryEntry repositoryDirectory = do
    repositoryPath <- Text.pack <$> decodePath repositoryDirectory
    return $
      Left $
        MalformedState $
          "Repository state entry "
            <> repositoryPath
            <> " does not have a valid repository ID."

  noncanonicalRepositoryEntry repositoryDirectory = do
    repositoryPath <- Text.pack <$> decodePath repositoryDirectory
    return $
      Left $
        MalformedState $
          "Repository state entry "
            <> repositoryPath
            <> " does not use the canonical lowercase repository ID."

  isPersistentHookLock repositoryDirectory entry = do
    name <- decodePath entry
    regular <- isRegularFile $ repositoryDirectory </> entry
    let digest = take 64 $ drop 5 name
    return $
      regular
        && length name == 74
        && "hook-" `isPrefixOf` name
        && ".lock" `isSuffixOf` name
        && all isHexDigit digest


isRetryableInitialSnapshot
  :: (MonadFileSystem m) => OsPath -> RepositoryId -> [OsPath] -> m Bool
isRetryableInitialSnapshot root repositoryId' orphaned = do
  let snapshotsName = path "snapshots"
  let currentName = path "current"
  let snapshots =
        repositoryStateDirectory root repositoryId' </> snapshotsName
  let current = snapshots </> currentName
  if sort orphaned /= [snapshotsName]
    then return False
    else do
      snapshotsSymlink <- isSymlink snapshots
      snapshotsDirectory <- isDirectory snapshots
      if snapshotsSymlink || not snapshotsDirectory
        then return False
        else do
          currentSymlink <- isSymlink current
          currentDirectory <- isDirectory current
          if currentSymlink || not currentDirectory
            then return False
            else do
              snapshotChildren <- sort <$> listDirectory snapshots
              snapshot <- snapshotEntries current
              return $ snapshotChildren == [currentName] && snapshot == Right []


-- | Classifies all repository records without discarding unavailable checkouts.
selectRepositoryState :: [MachineState] -> RepositorySelection
selectRepositoryState [] = NoRepositoryState
selectRepositoryState [state] = SelectedRepositoryState state
selectRepositoryState _ = AmbiguousRepositoryStates


-- | Prepares a repository record and safely migrates a legacy local snapshot.
--
-- An explicit intermediate path is persisted.  Relative paths are resolved
-- against the checkout before they are stored.
prepareRepositoryState
  :: (MonadFileSystem m, MonadIO m)
  => OsPath
  -> RepositoryId
  -> MachineId
  -> OsPath
  -> Maybe OsPath
  -> UTCTime
  -> m (Either StateError (MachineState, MigrationResult))
prepareRepositoryState root repositoryId' machineId' checkout explicit =
  prepareRepositoryStateWithLegacyHistory
    root
    repositoryId'
    machineId'
    checkout
    explicit
    False


-- | Prepares state while preserving known legacy first-apply history.
prepareRepositoryStateWithLegacyHistory
  :: (MonadFileSystem m, MonadIO m)
  => OsPath
  -> RepositoryId
  -> MachineId
  -> OsPath
  -> Maybe OsPath
  -> Bool
  -- ^ Whether the legacy registry proves a successful apply for this checkout.
  -> UTCTime
  -> m (Either StateError (MachineState, MigrationResult))
prepareRepositoryStateWithLegacyHistory
  root
  repositoryId'
  machineId'
  checkout
  explicit
  legacyFirstApplied
  now = do
    defaultManifestName <- encodePath "dojang.toml"
    prepareRepositoryStateWithOwnership
      root
      repositoryId'
      machineId'
      checkout
      (normalise $ checkout </> defaultManifestName)
      explicit
      (\_ _ -> return False)
      legacyFirstApplied
      now


-- | Prepares state while verifying whether an old checkout still owns the
-- repository identity.
--
-- The ownership check runs while the repository-state lock is held.  It should
-- return 'True' only when the supplied checkout has a readable manifest that
-- declares this repository identity.
prepareRepositoryStateWithOwnership
  :: (MonadFileSystem m, MonadIO m)
  => OsPath
  -> RepositoryId
  -> MachineId
  -> OsPath
  -> OsPath
  -- ^ Absolute path to the manifest used by the current checkout.
  -> Maybe OsPath
  -> (OsPath -> OsPath -> m Bool)
  -- ^ Verifies the previous checkout using its recorded manifest path.
  -> Bool
  -- ^ Whether the legacy registry proves a successful apply for this checkout.
  -> UTCTime
  -> m (Either StateError (MachineState, MigrationResult))
prepareRepositoryStateWithOwnership
  root
  repositoryId'
  machineId'
  checkout
  manifest
  explicit
  checkoutOwnsIdentity
  legacyFirstApplied
  now =
    prepareRepositoryStateWithOwnershipBeforeMigration
      root
      repositoryId'
      machineId'
      checkout
      manifest
      explicit
      checkoutOwnsIdentity
      legacyFirstApplied
      now
      (return ())


-- | Prepares state after running an action once a new snapshot layout has
-- passed validation but before migration changes it.
--
-- This lets callers publish metadata needed to retry an interrupted migration
-- without publishing it for a snapshot layout that is already known to be
-- invalid.  The action is not run when an existing repository state is reused.
prepareRepositoryStateWithOwnershipBeforeMigration
  :: (MonadFileSystem m, MonadIO m)
  => OsPath
  -> RepositoryId
  -> MachineId
  -> OsPath
  -> OsPath
  -> Maybe OsPath
  -> (OsPath -> OsPath -> m Bool)
  -> Bool
  -> UTCTime
  -> m ()
  -- ^ Action to run after validating a new migration.
  -> m (Either StateError (MachineState, MigrationResult))
prepareRepositoryStateWithOwnershipBeforeMigration
  root
  repositoryId'
  machineId'
  checkout
  manifest
  explicit
  checkoutOwnsIdentity
  legacyFirstApplied
  now
  beforeMigration = do
    resolvedExplicit <- resolveExplicitSnapshot checkout explicit
    case resolvedExplicit of
      Left err -> return $ Left err
      Right explicit' -> do
        validatedStore <- validateRepositoryStoreRoot root
        case validatedStore of
          Left err -> return $ Left err
          Right () -> do
            validatedDirectory <-
              validateRepositoryStateDirectory root repositoryId'
            case validatedDirectory of
              Left err -> return $ Left err
              Right () -> do
                createDirectories $ repositoryStateDirectory root repositoryId'
                withFileLock (repositoryStateLockPath root repositoryId') $
                  prepareRepositoryStateUnlocked
                    root
                    repositoryId'
                    machineId'
                    checkout
                    manifest
                    explicit'
                    checkoutOwnsIdentity
                    legacyFirstApplied
                    now
                    beforeMigration


resolveExplicitSnapshot
  :: (MonadFileSystem m)
  => OsPath
  -> Maybe OsPath
  -> m (Either StateError (Maybe OsPath))
resolveExplicitSnapshot _ Nothing = return $ Right Nothing
resolveExplicitSnapshot checkout (Just explicit) = catchStateIOErrors $ do
  let candidate =
        normalise $
          if isAbsolute explicit then explicit else checkout </> explicit
  directSymlink <- isSymlink candidate
  if directSymlink
    then return $ Left $ UnsupportedSnapshotSymlink candidate
    else Right . Just <$> canonicalizePath candidate


prepareRepositoryStateUnlocked
  :: (MonadFileSystem m, MonadIO m)
  => OsPath
  -> RepositoryId
  -> MachineId
  -> OsPath
  -> OsPath
  -> Maybe OsPath
  -> (OsPath -> OsPath -> m Bool)
  -> Bool
  -> UTCTime
  -> m ()
  -> m (Either StateError (MachineState, MigrationResult))
prepareRepositoryStateUnlocked
  root
  repositoryId'
  machineId'
  checkout
  manifest
  explicit
  checkoutOwnsIdentity
  legacyFirstApplied
  now
  beforeMigration = do
    forgetting <- isRepositoryForgetInProgress root repositoryId'
    case forgetting of
      Left err -> return $ Left err
      Right True -> return $ Left $ RepositoryForgetInProgress repositoryId'
      Right False -> do
        loaded <- readRepositoryState root repositoryId' machineId'
        case loaded of
          Left err -> return $ Left err
          Right (Just state) -> prepareExisting state
          Right Nothing -> prepareNew
   where
    checkout' = normalise checkout
    manifest' = normalise manifest
    legacy = checkout' </> path ".dojang"
    defaultPath = defaultIntermediatePath root repositoryId'
    requested = normalise $ case explicit of
      Nothing -> defaultPath
      Just value
        | isAbsolute value -> value
        | otherwise -> checkout' </> value
    marker = migrationMarkerPath root repositoryId'

    doesMigrationMarkerExist = do
      present <- exists marker
      symlink <- isSymlink marker
      return $ present || symlink

    prepareExisting state = do
      oldCheckoutExists <- exists state.checkoutPath
      sameCheckout <-
        if oldCheckoutExists
          then sameExistingPath state.checkoutPath checkout'
          else return False
      oldCheckoutOwnsIdentity <-
        if state.checkoutPath /= checkout' && oldCheckoutExists && not sameCheckout
          then checkoutOwnsIdentity state.checkoutPath state.manifestPath
          else return False
      if oldCheckoutOwnsIdentity
        then
          return $
            Left $
              DuplicateRepositoryIdentity
                repositoryId'
                state.checkoutPath
                checkout'
        else do
          let desired = maybe state.intermediatePath (const requested) explicit
          let protectedCheckouts = [state.checkoutPath, checkout']
          currentValidation <-
            validateSelectedSnapshotLocation
              root
              repositoryId'
              protectedCheckouts
              state.intermediatePath
          case currentValidation of
            Left err -> return $ Left err
            Right () -> do
              current <- retryManagedTargetCleanupUnlocked root state
              desiredValidation <-
                validateSelectedSnapshotLocation
                  root
                  repositoryId'
                  protectedCheckouts
                  desired
              case desiredValidation of
                Left err -> return $ Left err
                Right () -> do
                  recovered <- recoverMigration current.intermediatePath desired
                  case recovered of
                    Left err -> return $ Left err
                    Right () -> do
                      migration <- migrateIfNeeded current.intermediatePath desired
                      case migration of
                        Left err -> return $ Left err
                        Right migrationResult -> do
                          let moved = not sameCheckout
                          let relativeManifest = makeRelative checkout' manifest'
                          let manifestInsideCheckout =
                                not (isAbsolute relativeManifest)
                                  && case splitDirectories relativeManifest of
                                    [] -> True
                                    first : _ -> first /= path ".."
                          let storedManifest =
                                if sameCheckout && manifestInsideCheckout
                                  then
                                    normalise $
                                      current.checkoutPath </> relativeManifest
                                  else manifest'
                          let storedIntermediate =
                                if migrationResult == ReusedRepositoryState
                                  then current.intermediatePath
                                  else desired
                          let updated =
                                current
                                  { checkoutPath =
                                      if sameCheckout
                                        then current.checkoutPath
                                        else checkout'
                                  , manifestPath = storedManifest
                                  , intermediatePath = storedIntermediate
                                  , updatedTime = now
                                  }
                          writeState root updated
                          cleanup <- cleanupSnapshot current.intermediatePath desired
                          case cleanup of
                            Left err -> return $ Left err
                            Right () -> do
                              markerExists <- doesMigrationMarkerExist
                              when markerExists $ removeFile marker
                              let finalResult
                                    | migrationResult
                                        == PersistedIntermediateOverride =
                                        migrationResult
                                    | moved = MovedRepositoryCheckout
                                    | otherwise = ReusedRepositoryState
                              return $
                                Right
                                  ( updated
                                  , finalResult
                                  )

    prepareNew = do
      legacySymlink <-
        findDirectOrPrivateSnapshotSymlink root repositoryId' legacy
      requestedSymlink <-
        findUnsupportedSnapshotSymlink root repositoryId' requested
      case legacySymlink of
        Just symlink -> return $ Left $ UnsupportedSnapshotSymlink symlink
        Nothing ->
          case requestedSymlink of
            Just symlink -> return $ Left $ UnsupportedSnapshotSymlink symlink
            Nothing -> do
              protected <-
                validateProtectedSnapshotLocation
                  root
                  repositoryId'
                  [checkout']
                  requested
              case protected of
                Left err -> return $ Left err
                Right () -> do
                  resolvedLegacy <- canonicalizePath legacy
                  resolvedRequested <- canonicalizePath requested
                  if isProperDescendant resolvedLegacy resolvedRequested
                    then return $ Left $ SnapshotInsideLegacy legacy requested
                    else do
                      plan <- planNewPaths resolvedLegacy resolvedRequested
                      case validateNewPaths plan of
                        Left err -> return $ Left err
                        Right () -> beforeMigration >> prepareNewPaths plan

    planNewPaths resolvedLegacy resolvedRequested = do
      markerExists <- doesMigrationMarkerExist
      legacyPresent <- exists legacy
      legacyExists <- isDirectory legacy
      destinationPresent <- exists requested
      destinationExists <- isDirectory requested
      let sameDestination = resolvedLegacy == resolvedRequested
      if legacyPresent && not legacyExists
        then return $ RejectNewState $ InvalidSnapshotLocation legacy
        else
          if destinationPresent && not destinationExists
            then return $ RejectNewState $ InvalidSnapshotLocation requested
            else
              if markerExists
                then do
                  validated <- validateMigrationMarker legacy requested
                  case validated of
                    Left err -> return $ RejectNewState err
                    Right () ->
                      if not legacyExists
                        then return $ RejectNewState $ MissingSnapshot legacy
                        else
                          if sameDestination
                            then
                              planValidatedSnapshot
                                legacy
                                (PersistNewState RecoveredInterruptedMigration)
                            else
                              planRecoverableCopy
                                legacy
                                requested
                                ( RecoverLegacySnapshot
                                    RecoveredInterruptedMigration
                                )
                else
                  if legacyExists && not sameDestination
                    then
                      if destinationExists
                        then do
                          comparison <- treesEqual legacy requested
                          case comparison of
                            Left err -> return $ RejectNewState err
                            Right True ->
                              return $
                                PersistNewState MigratedLegacySnapshot
                            Right False ->
                              return $
                                RejectNewState $
                                  ConflictingSnapshots legacy requested
                        else
                          planValidatedSnapshot
                            legacy
                            (MigrateLegacySnapshot MigratedLegacySnapshot)
                    else
                      if destinationExists && not legacyExists
                        then do
                          snapshot <- snapshotEntries requested
                          return $ case snapshot of
                            Left err -> RejectNewState err
                            Right entries -> case explicit of
                              Just _ ->
                                PersistNewState MigratedLegacySnapshot
                              Nothing
                                | requested == defaultPath && null entries ->
                                    PersistNewState CreatedRepositoryState
                                | otherwise ->
                                    RejectNewState $ UnownedSnapshot requested
                        else
                          if legacyExists
                            then
                              planValidatedSnapshot
                                legacy
                                (PersistNewState CreatedRepositoryState)
                            else return $ PersistNewState CreatedRepositoryState

    planValidatedSnapshot snapshotPath plan = do
      validated <- validateSnapshot snapshotPath
      return $ either RejectNewState (const plan) validated

    planRecoverableCopy sourcePath destinationPath plan = do
      validated <- validateRecoverableCopy sourcePath destinationPath
      return $ either RejectNewState (const plan) validated

    validateNewPaths (RejectNewState err) = Left err
    validateNewPaths _ = Right ()

    prepareNewPaths (RejectNewState err) = return $ Left err
    prepareNewPaths (PersistNewState result) = persistAndCleanup result
    prepareNewPaths (MigrateLegacySnapshot result) = migrateLegacy result
    prepareNewPaths (RecoverLegacySnapshot result) = do
      recovered <- recoverCopyDestination legacy requested
      case recovered of
        Left err -> return $ Left err
        Right () -> migrateLegacy result

    validateSnapshot snapshotPath =
      fmap (const ()) <$> snapshotEntries snapshotPath

    validateRecoverableCopy sourcePath destinationPath = do
      sourceSnapshot <- snapshotEntries sourcePath
      case sourceSnapshot of
        Left err -> return $ Left err
        Right _ -> do
          destinationPresent <- exists destinationPath
          if not destinationPresent
            then return $ Right ()
            else do
              compatible <- partialSnapshotMatches sourcePath destinationPath
              return $ case compatible of
                Left err -> Left err
                Right True -> Right ()
                Right False ->
                  Left $ ConflictingSnapshots sourcePath destinationPath

    migrateLegacy result = do
      createDirectories $ repositoryStateDirectory root repositoryId'
      markerExists <- doesMigrationMarkerExist
      unless markerExists $
        writeMigrationMarker marker legacy requested
      copied <- copyTree legacy requested
      case copied of
        Left err -> return $ Left err
        Right () -> do
          comparison <- treesEqual legacy requested
          case comparison of
            Left err -> return $ Left err
            Right False -> return $ Left $ ConflictingSnapshots legacy requested
            Right True -> persistAndCleanup result

    persistAndCleanup result = do
      generation <- newStateGenerationId
      requestedExists <- isDirectory requested
      unless requestedExists $ createDirectories requested
      snapshot <- snapshotEntries requested
      case snapshot of
        Left err -> return $ Left err
        Right _ -> do
          let state =
                MachineState
                  schemaVersion
                  repositoryId'
                  machineId'
                  generation
                  checkout'
                  manifest'
                  requested
                  (managedTargetSnapshotRoot root repositoryId')
                  Nothing
                  Map.empty
                  now
                  now
                  legacyFirstApplied
                  []
                  Map.empty
                  Map.empty
                  Map.empty
          legacyExists <- isDirectory legacy
          when (legacyExists && requested /= legacy) $ do
            markerExists <- doesMigrationMarkerExist
            unless markerExists $
              writeMigrationMarker marker legacy requested
          comparison <-
            if result == MigratedLegacySnapshot
              && legacyExists
              && requested /= legacy
              then treesEqual legacy requested
              else return $ Right True
          case comparison of
            Left err -> return $ Left err
            Right False ->
              return $ Left $ ConflictingSnapshots legacy requested
            Right True -> do
              writeState root state
              cleanup <- cleanupSnapshot legacy requested
              case cleanup of
                Left err -> return $ Left err
                Right () -> do
                  markerExists <- doesMigrationMarkerExist
                  when markerExists $ removeFile marker
                  return $ Right (state, result)

    recoverMigration currentPath desiredPath = do
      markerExists <- doesMigrationMarkerExist
      if not markerExists
        then return $ Right ()
        else do
          loadedMarker <- readMigrationMarker
          case loadedMarker of
            Left err -> return $ Left err
            Right (sourcePath, destinationPath) -> do
              resolvedSource <- canonicalizePath sourcePath
              resolvedDestination <- canonicalizePath destinationPath
              resolvedCurrent <- canonicalizePath currentPath
              resolvedDesired <- canonicalizePath desiredPath
              if resolvedDestination /= resolvedDesired
                then
                  return $
                    Left $
                      MigrationDestinationMismatch destinationPath desiredPath
                else
                  if resolvedSource == resolvedDestination
                    then do
                      snapshot <- snapshotEntries currentPath
                      case snapshot of
                        Left err -> return $ Left err
                        Right _ -> removeFile marker >> return (Right ())
                    else
                      if resolvedCurrent == resolvedSource
                        then recoverCopyDestination sourcePath destinationPath
                        else
                          if resolvedCurrent == resolvedDestination
                            then finishRecoveredMigration sourcePath destinationPath
                            else
                              return $
                                Left $
                                  MigrationSourceMismatch sourcePath currentPath

    recoverCopyDestination sourcePath destinationPath = do
      sourceSnapshot <- snapshotEntries sourcePath
      case sourceSnapshot of
        Left err -> return $ Left err
        Right _ -> do
          destinationIsSymlink <- isSymlink destinationPath
          destinationPresent <- exists destinationPath
          destinationExists <- isDirectory destinationPath
          if destinationIsSymlink
            then return $ Left $ UnsupportedSnapshotSymlink destinationPath
            else
              if destinationPresent && not destinationExists
                then return $ Left $ InvalidSnapshotLocation destinationPath
                else do
                  if not destinationExists
                    then return $ Right ()
                    else do
                      compatible <- partialSnapshotMatches sourcePath destinationPath
                      case compatible of
                        Left err -> return $ Left err
                        Right False ->
                          return $
                            Left $
                              ConflictingSnapshots sourcePath destinationPath
                        Right True -> do
                          removeDirectoryRecursively destinationPath
                          return $ Right ()

    finishRecoveredMigration sourcePath destinationPath = do
      destinationSnapshot <- snapshotEntries destinationPath
      case destinationSnapshot of
        Left err -> return $ Left err
        Right _ -> do
          sourcePresent <- exists sourcePath
          if not sourcePresent
            then removeFile marker >> return (Right ())
            else do
              -- Cleanup can leave the old source as a subset of the complete
              -- destination, so compare in the interrupted-copy direction.
              comparison <- partialSnapshotMatches destinationPath sourcePath
              case comparison of
                Left err -> return $ Left err
                Right False ->
                  return $
                    Left $
                      ConflictingSnapshots sourcePath destinationPath
                Right True -> do
                  cleanup <- cleanupSnapshot sourcePath destinationPath
                  case cleanup of
                    Left err -> return $ Left err
                    Right () -> removeFile marker >> return (Right ())

    readMigrationMarker = do
      symlink <- isSymlink marker
      regularFile <- isRegularFile marker
      if symlink || not regularFile
        then
          return $
            Left $
              MalformedMigrationMarker $
                "Migration marker "
                  <> osPathText marker
                  <> " is not a regular file."
        else do
          contents <- readFile marker
          return $ decodeMigrationMarker contents

    validateMigrationMarker expectedSource candidate = do
      loadedMarker <- readMigrationMarker
      case loadedMarker of
        Left err -> return $ Left err
        Right (recordedSource, recordedDestination) -> do
          resolvedSource <- canonicalizePath recordedSource
          resolvedExpectedSource <- canonicalizePath expectedSource
          resolvedRecorded <- canonicalizePath recordedDestination
          resolvedCandidate <- canonicalizePath candidate
          if resolvedSource /= resolvedExpectedSource
            then
              return $
                Left $
                  MigrationSourceMismatch recordedSource expectedSource
            else
              if resolvedRecorded /= resolvedCandidate
                then
                  return $
                    Left $
                      MigrationDestinationMismatch recordedDestination candidate
                else
                  if resolvedSource /= resolvedRecorded
                    && pathsOverlap resolvedSource resolvedRecorded
                    then
                      return $
                        Left $
                          OverlappingSnapshots
                            recordedSource
                            recordedDestination
                    else return $ Right ()

    cleanupSnapshot oldPath newPath = do
      oldExists <- isDirectory oldPath
      sameSnapshot <-
        if oldExists
          then sameExistingPath oldPath newPath
          else return False
      if not oldExists || sameSnapshot
        then return $ Right ()
        else
          (removeDirectoryRecursively oldPath >> return (Right ()))
            `catchError` \err ->
              return $
                Left $
                  LegacyCleanupFailed oldPath newPath $
                    Text.pack $
                      show err

    migrateIfNeeded oldPath newPath = do
      resolvedOld <- canonicalizePath oldPath
      resolvedNew <- canonicalizePath newPath
      if resolvedOld == resolvedNew
        then do
          snapshot <- snapshotEntries oldPath
          return $ ReusedRepositoryState <$ snapshot
        else
          if pathsOverlap resolvedOld resolvedNew
            then return $ Left $ OverlappingSnapshots oldPath newPath
            else do
              oldSymlink <- isSymlink oldPath
              newSymlink <- isSymlink newPath
              oldPresent <- exists oldPath
              oldExists <- isDirectory oldPath
              newPresent <- exists newPath
              newExists <- isDirectory newPath
              if oldSymlink
                then return $ Left $ UnsupportedSnapshotSymlink oldPath
                else
                  if newSymlink
                    then return $ Left $ UnsupportedSnapshotSymlink newPath
                    else
                      if not oldPresent
                        then return $ Left $ MissingSnapshot oldPath
                        else
                          if not oldExists
                            then return $ Left $ InvalidSnapshotLocation oldPath
                            else
                              if newPresent && not newExists
                                then return $ Left $ InvalidSnapshotLocation newPath
                                else case (oldExists, newExists) of
                                  (False, _) -> return $ Left $ MissingSnapshot oldPath
                                  (True, False) -> do
                                    markerExists <- doesMigrationMarkerExist
                                    unless markerExists $
                                      writeMigrationMarker marker oldPath newPath
                                    copied <- copyTree oldPath newPath
                                    case copied of
                                      Left err -> return $ Left err
                                      Right () -> do
                                        comparison <- treesEqual oldPath newPath
                                        return $ case comparison of
                                          Left err -> Left err
                                          Right True ->
                                            Right PersistedIntermediateOverride
                                          Right False ->
                                            Left $ ConflictingSnapshots oldPath newPath
                                  (True, True) -> do
                                    comparison <- treesEqual oldPath newPath
                                    case comparison of
                                      Left err -> return $ Left err
                                      Right True -> do
                                        markerExists <- doesMigrationMarkerExist
                                        unless markerExists $
                                          writeMigrationMarker marker oldPath newPath
                                        return $ Right PersistedIntermediateOverride
                                      Right False ->
                                        return $
                                          Left $
                                            ConflictingSnapshots oldPath newPath


-- | Persists completion of the first successful apply for a repository.
--
-- Reloads the repository record while holding its lock so a concurrent snapshot
-- migration cannot be overwritten by stale state.  Returns an error when the
-- current record is missing or invalid.
markFirstApplied
  :: (MonadFileSystem m)
  => OsPath
  -> UTCTime
  -> MachineState
  -> m (Either StateError MachineState)
markFirstApplied root now state = catchStateIOErrors $ do
  createDirectories $ repositoryStateDirectory root state.repositoryId
  withFileLock (repositoryStateLockPath root state.repositoryId) $ do
    forgetting <- isRepositoryForgetInProgress root state.repositoryId
    case forgetting of
      Left err -> return $ Left err
      Right True ->
        return $ Left $ RepositoryForgetInProgress state.repositoryId
      Right False -> do
        loaded <- readRepositoryState root state.repositoryId state.machineId
        case loaded of
          Left err -> return $ Left err
          Right Nothing -> return $ Left $ MissingRepositoryState state.repositoryId
          Right (Just current) -> do
            let updated = current{firstApplied = True, updatedTime = now}
            writeState root updated
            return $ Right updated


-- | Atomically applies machine-fact source changes to a repository record.
--
-- The fact map contains only additions or replacements requested by the
-- caller.  The optional path replaces the associated facts file when present;
-- 'Nothing' preserves the association reloaded under the repository lock.
updateMachineFacts
  :: (MonadFileSystem m)
  => OsPath
  -- ^ Platform-native machine-state root.
  -> UTCTime
  -- ^ Time to store as the record's last update.
  -> MachineState
  -- ^ Repository-state generation observed before enrollment began.
  -> Maybe OsPath
  -- ^ Facts-file association to replace, or 'Nothing' to preserve it.
  -> FactMap
  -- ^ Declared fact additions and replacements to merge into current state.
  -> m (Either StateError MachineState)
  -- ^ Updated state, or a validation or persistence error.
updateMachineFacts root now state requestedFactsFile declaredFactUpdates =
  case validateMachineFacts requestedFactsFile declaredFactUpdates of
    Left err -> return $ Left err
    Right () -> catchStateIOErrors $ do
      createDirectories $ repositoryStateDirectory root state.repositoryId
      withFileLock (repositoryStateLockPath root state.repositoryId) $ do
        forgetting <- isRepositoryForgetInProgress root state.repositoryId
        case forgetting of
          Left err -> return $ Left err
          Right True ->
            return $ Left $ RepositoryForgetInProgress state.repositoryId
          Right False -> do
            loaded <- readRepositoryState root state.repositoryId state.machineId
            case loaded of
              Left err -> return $ Left err
              Right Nothing -> return $ Left $ MissingRepositoryState state.repositoryId
              Right (Just current) ->
                case validateRepositoryStateGeneration state current of
                  Left err -> return $ Left err
                  Right () -> do
                    let updated =
                          current
                            { factsFile =
                                maybe current.factsFile Just requestedFactsFile
                            , declaredFacts =
                                Map.union declaredFactUpdates current.declaredFacts
                            , updatedTime = now
                            }
                    writeState root updated
                    return $ Right updated


-- | Records one successful stateful hook execution atomically.
--
-- The typed key and execution data cannot contain invalid policy combinations.
-- The repository record is reloaded while holding its lock so hook history
-- cannot overwrite concurrent lifecycle or managed-target updates.
recordHookExecution
  :: (MonadFileSystem m)
  => OsPath
  -- ^ Platform-native machine-state root.
  -> UTCTime
  -- ^ Time to store as the record's last update.
  -> MachineState
  -- ^ Repository-state generation captured by the hook command.
  -> HookExecutionKey
  -- ^ Typed event and hook identity.
  -> HookExecution
  -- ^ Successful execution data to persist.
  -> m (Either StateError MachineState)
  -- ^ Updated state, or a validation or persistence error.
recordHookExecution root now state key execution =
  catchStateIOErrors $ do
    createDirectories $ repositoryStateDirectory root state.repositoryId
    withFileLock (repositoryStateLockPath root state.repositoryId) $ do
      forgetting <- isRepositoryForgetInProgress root state.repositoryId
      case forgetting of
        Left err -> return $ Left err
        Right True ->
          return $ Left $ RepositoryForgetInProgress state.repositoryId
        Right False -> do
          loaded <- readRepositoryState root state.repositoryId state.machineId
          case loaded of
            Left err -> return $ Left err
            Right Nothing ->
              return $ Left $ MissingRepositoryState state.repositoryId
            Right (Just current) ->
              case validateRepositoryStateGeneration state current of
                Left err -> return $ Left err
                Right () -> do
                  let updated =
                        current
                          { hookExecutions =
                              Map.insert
                                key
                                execution
                                current.hookExecutions
                          , updatedTime = now
                          }
                  writeState root updated
                  return $ Right updated


-- | Validates that two records belong to the same repository-state generation.
--
-- The first record is the generation captured by the caller; the second is
-- the current record reloaded from storage.
validateRepositoryStateGeneration
  :: MachineState
  -- ^ Repository-state generation captured by the caller.
  -> MachineState
  -- ^ Current repository-state record.
  -> Either StateError ()
  -- ^ Success when the generation IDs match, or a mismatch error.
validateRepositoryStateGeneration expected actual
  | actual.generationId == expected.generationId = Right ()
  | otherwise =
      Left $
        RepositoryStateGenerationMismatch
          expected.repositoryId
          expected.generationId
          actual.generationId


-- | Runs an action only while the captured state generation is still current.
--
-- The generation is checked under the repository lock, and the supplied action
-- runs before that lock is released.  This gives callers a linearization point
-- for effects that must not start after a concurrent repository forget.
withRepositoryStateGeneration
  :: (MonadFileSystem m)
  => OsPath
  -- ^ Platform-native machine-state root.
  -> MachineState
  -- ^ Repository-state generation captured by the caller.
  -> m a
  -- ^ Effect to start while the repository lock remains held.
  -> m (Either StateError a)
  -- ^ Effect result, or an error when the captured generation is stale.
withRepositoryStateGeneration root expected action = catchStateIOErrors $ do
  createDirectories $ repositoryStateDirectory root expected.repositoryId
  withFileLock (repositoryStateLockPath root expected.repositoryId) $ do
    forgetting <- isRepositoryForgetInProgress root expected.repositoryId
    case forgetting of
      Left err -> return $ Left err
      Right True ->
        return $ Left $ RepositoryForgetInProgress expected.repositoryId
      Right False -> do
        loaded <-
          readRepositoryState root expected.repositoryId expected.machineId
        case loaded of
          Left err -> return $ Left err
          Right Nothing ->
            return $ Left $ MissingRepositoryState expected.repositoryId
          Right (Just current) ->
            case validateRepositoryStateGeneration expected current of
              Left err -> return $ Left err
              Right () -> Right <$> action


-- | Atomically replaces managed-target records after successful syncs.
--
-- The current record is reloaded under its repository lock so concurrent
-- lifecycle updates and snapshot migrations are not overwritten.
updateManagedTargets
  :: (MonadFileSystem m)
  => OsPath
  -> UTCTime
  -> MachineState
  -> (Map Text ManagedTarget -> Map Text ManagedTarget)
  -> m (Either StateError MachineState)
updateManagedTargets root now state update = do
  result <-
    updateManagedTargetsWith
      root
      now
      state
      (\records -> return (update records, ()))
      (\_ _ -> [])
      (\_ _ -> return ())
      (\_ _ -> return ())
  return $ fst <$> result


-- | Changes managed-target records and journals locked post-write cleanup.
--
-- The current record is reloaded before the update callback.  Any cleanup left
-- by an earlier update is retried first.  New cleanup paths are published with
-- the changed records, removed while the repository lock remains held, and
-- cleared by a second atomic state write.  If cleanup or that final write
-- fails, the published journal makes the work retryable.  If initial
-- publication fails, the rollback callback removes resources created by the
-- update callback.
updateManagedTargetsWith
  :: (MonadFileSystem m)
  => OsPath
  -> UTCTime
  -> MachineState
  -> (Map Text ManagedTarget -> m (Map Text ManagedTarget, result))
  -> (MachineState -> result -> [OsPath])
  -> (MachineState -> result -> m ())
  -> (MachineState -> result -> m ())
  -> m (Either StateError (MachineState, result))
updateManagedTargetsWith root now state update cleanupPaths afterPublish rollback =
  catchStateIOErrors $ do
    createDirectories $ repositoryStateDirectory root state.repositoryId
    withFileLock (repositoryStateLockPath root state.repositoryId) $ do
      forgetting <- isRepositoryForgetInProgress root state.repositoryId
      case forgetting of
        Left err -> return $ Left err
        Right True ->
          return $ Left $ RepositoryForgetInProgress state.repositoryId
        Right False -> do
          loaded <- readRepositoryState root state.repositoryId state.machineId
          case loaded of
            Left err -> return $ Left err
            Right Nothing -> return $ Left $ MissingRepositoryState state.repositoryId
            Right (Just current) -> do
              current' <- retryManagedTargetCleanupUnlocked root current
              (records, result) <- update current'.targetRecords
              let updatedWithoutCleanup =
                    current'
                      { targetRecords = records
                      , updatedTime = now
                      }
              let pending = nub $ cleanupPaths updatedWithoutCleanup result
              if any (not . validCleanupPath updatedWithoutCleanup) pending
                then do
                  rollback current' result
                  return $
                    Left $
                      MalformedState
                        "A managed-target update tried to clean outside its snapshot roots."
                else do
                  let published =
                        updatedWithoutCleanup{pendingCleanupPaths = pending}
                  writeState root published `catchError` \err -> do
                    rollback current' result
                    throwError err
                  afterPublish published result
                  cleanupManagedTargetPaths published pending
                  let completed = published{pendingCleanupPaths = []}
                  unless (null pending) $ writeState root completed
                  return $ Right (completed, result)


-- | Retries cleanup recorded by a previously published managed-target update.
--
-- Cleanup and journal removal run under the repository lock.  Missing paths
-- are accepted so a partially completed prior attempt remains idempotent.
retryManagedTargetCleanup
  :: (MonadFileSystem m)
  => OsPath
  -> MachineState
  -> m (Either StateError MachineState)
retryManagedTargetCleanup root state = catchStateIOErrors $ do
  createDirectories $ repositoryStateDirectory root state.repositoryId
  withFileLock (repositoryStateLockPath root state.repositoryId) $ do
    loaded <- readRepositoryState root state.repositoryId state.machineId
    case loaded of
      Left err -> return $ Left err
      Right Nothing -> return $ Left $ MissingRepositoryState state.repositoryId
      Right (Just current) -> Right <$> retryManagedTargetCleanupUnlocked root current


retryManagedTargetCleanupUnlocked
  :: (MonadFileSystem m) => OsPath -> MachineState -> m MachineState
retryManagedTargetCleanupUnlocked root state
  | null state.pendingCleanupPaths = return state
  | otherwise = do
      cleanupManagedTargetPaths state state.pendingCleanupPaths
      let completed = state{pendingCleanupPaths = []}
      writeState root completed
      return completed


cleanupManagedTargetPaths
  :: (MonadFileSystem m) => MachineState -> [OsPath] -> m ()
cleanupManagedTargetPaths state = mapM_ $ cleanupManagedTargetPath state


cleanupManagedTargetPath
  :: (MonadFileSystem m) => MachineState -> OsPath -> m ()
cleanupManagedTargetPath state candidate =
  case cleanupRoot state candidate' of
    Nothing -> refuse "outside its snapshot roots"
    Just root -> do
      symbolicLink <- findPathSymlink candidate'
      case symbolicLink of
        Just _ -> refuse "because it has a symbolic-link path component"
        Nothing -> return ()
      present <- exists candidate'
      directory <- isDirectory candidate'
      file <- isFile candidate'
      if directory
        then removeDirectoryRecursively candidate'
        else
          if file
            then removeFile candidate'
            else when present $ refuse "because it is not a regular entry"
      pruneEmptyAncestors root $ normalise $ takeDirectory candidate'
 where
  candidate' = normalise candidate
  refuse reason = do
    rendered <- decodePath candidate'
    throwError $
      userError $
        "Refusing to clean managed snapshot " <> rendered <> " " <> reason <> "."


firstSnapshotSymlink
  :: (MonadFileSystem m) => OsPath -> OsPath -> m (Maybe OsPath)
firstSnapshotSymlink root candidate = firstSymlink paths
 where
  relative = makeRelative root candidate
  paths = scanl (</>) root $ splitDirectories relative

  firstSymlink [] = return Nothing
  firstSymlink (path' : rest) = do
    symbolicLink <- isSymlink path'
    if symbolicLink then return $ Just path' else firstSymlink rest


pruneEmptyAncestors
  :: (MonadFileSystem m) => OsPath -> OsPath -> m ()
pruneEmptyAncestors root directory
  | directory == root' = return ()
  | not $ isProperDescendant root' directory = return ()
  | otherwise = do
      symbolicLink <- isSymlink directory
      when symbolicLink $ do
        rendered <- decodePath directory
        throwError $
          userError $
            "Refusing to prune symbolic-link snapshot directory "
              <> rendered
              <> "."
      present <- isDirectory directory
      when present $ do
        entries <- listDirectory directory
        when (null entries) $ do
          removeDirectory directory
          pruneEmptyAncestors root' $ normalise $ takeDirectory directory
 where
  root' = normalise root


cleanupRoot :: MachineState -> OsPath -> Maybe OsPath
cleanupRoot state candidate
  | not $ isAbsolute candidate = Nothing
  | isProperDescendant state.targetSnapshotRoot candidate =
      Just $ normalise state.targetSnapshotRoot
  | isProperDescendant state.intermediatePath candidate =
      Just $ normalise state.intermediatePath
  | otherwise = Nothing


validCleanupPath :: MachineState -> OsPath -> Bool
validCleanupPath state = maybe False (const True) . cleanupRoot state


-- | Removes one repository record while preserving the machine identity.
--
-- The repository directory and its lock may remain; state enumeration ignores
-- directories that contain no @state.toml@ file.
forgetRepositoryState
  :: (MonadFileSystem m)
  => OsPath
  -> MachineState
  -> m (Either StateError ())
forgetRepositoryState root state = do
  result <-
    forgetRepositoryStateWith
      root
      state.repositoryId
      state.machineId
      (const $ return ())
  return $ () <$ result


-- | Reloads and forgets one repository while holding its state lock.
--
-- The callback can validate the freshly loaded record and remove its private
-- snapshots.  The state document is deleted only after that callback succeeds,
-- and the lock remains held throughout both operations.
forgetRepositoryStateWith
  :: (MonadFileSystem m)
  => OsPath
  -> RepositoryId
  -> MachineId
  -> (MachineState -> m result)
  -> m (Either StateError (Maybe result))
forgetRepositoryStateWith root repositoryId' machineId' beforeForget =
  catchStateIOErrors $ do
    createDirectories $ repositoryStateDirectory root repositoryId'
    withFileLock (repositoryStateLockPath root repositoryId') $ do
      loaded <- readRepositoryState root repositoryId' machineId'
      case loaded of
        Left err -> return $ Left err
        Right Nothing -> do
          forgetting <- isRepositoryForgetInProgress root repositoryId'
          case forgetting of
            Left err -> return $ Left err
            Right inProgress -> do
              when inProgress $
                removeFile $
                  forgetMarkerPath root repositoryId'
              return $ Right Nothing
        Right (Just current) -> do
          result <- beforeForget current
          let migrationMarker = migrationMarkerPath root repositoryId'
          migrationMarkerPresent <- exists migrationMarker
          migrationMarkerSymbolicLink <- isSymlink migrationMarker
          when (migrationMarkerPresent || migrationMarkerSymbolicLink) $
            removeFile migrationMarker
          forgetting <- isRepositoryForgetInProgress root repositoryId'
          case forgetting of
            Left err -> return $ Left err
            Right inProgress -> do
              let statePath = repositoryStatePath root repositoryId'
              present <- exists statePath
              symbolicLink <- isSymlink statePath
              when (present || symbolicLink) $ removeFile statePath
              when inProgress $
                removeFile $
                  forgetMarkerPath root repositoryId'
              return $ Right $ Just result


writeState :: (MonadFileSystem m) => OsPath -> MachineState -> m ()
writeState root state = do
  let directory = repositoryStateDirectory root state.repositoryId
  let destination = repositoryStatePath root state.repositoryId
  createDirectories directory
  writeFileAtomically
    destination
    "state.toml.tmp"
    (encodeUtf8 $ encodeMachineState state)


writeMigrationMarker
  :: (MonadFileSystem m) => OsPath -> OsPath -> OsPath -> m ()
writeMigrationMarker marker source destination =
  writeFileAtomically
    marker
    "migration-in-progress.tmp"
    (encodeMigrationMarker source destination)


isProperDescendant :: OsPath -> OsPath -> Bool
isProperDescendant parent candidate =
  candidate' /= parent'
    && not (isAbsolute relative)
    && not (null components)
    && path ".." `notElem` components
 where
  parent' = normalise parent
  candidate' = normalise candidate
  relative = makeRelative parent' candidate'
  components = splitDirectories relative


isCleanupPathInside :: OsPath -> OsPath -> OsPath -> Bool
isCleanupPathInside intermediate targetSnapshots candidate =
  isAbsolute candidate
    && ( isProperDescendant intermediate candidate
           || isProperDescendant targetSnapshots candidate
       )


pathsOverlap :: OsPath -> OsPath -> Bool
pathsOverlap left right =
  isProperDescendant left right || isProperDescendant right left


-- | Tests whether two existing paths resolve to the same filesystem entry.
sameExistingPath :: (MonadFileSystem m) => OsPath -> OsPath -> m Bool
sameExistingPath left right
  | left == right = return True
  | otherwise =
      (==) <$> canonicalizePath left <*> canonicalizePath right


copyTree :: (MonadFileSystem m) => OsPath -> OsPath -> m (Either StateError ())
copyTree source destination = do
  snapshot <- snapshotEntries source
  case snapshot of
    Left err -> return $ Left err
    Right entries -> do
      createDirectories destination
      copyEntries entries
 where
  copyEntries [] = return $ Right ()
  copyEntries (entry@(fileType, relative) : rest) = do
    validated <- validateSnapshotEntry source entry
    case validated of
      Left err -> return $ Left err
      Right () -> case fileType of
        Directory -> do
          createDirectories $ destination </> relative
          copyEntries rest
        File -> do
          copyFile (source </> relative) (destination </> relative)
          copyEntries rest
        Symlink ->
          return $ Left $ UnsupportedSnapshotSymlink (source </> relative)


snapshotEntries
  :: (MonadFileSystem m)
  => OsPath
  -> m (Either StateError [(FileType, OsPath)])
snapshotEntries root = do
  rootIsSymlink <- isSymlink root
  present <- exists root
  directory <- isDirectory root
  if rootIsSymlink
    then return $ Left $ UnsupportedSnapshotSymlink root
    else
      if not present
        then return $ Left $ MissingSnapshot root
        else
          if not directory
            then return $ Left $ InvalidSnapshotLocation root
            else do
              entries <- listDirectoryRecursively root []
              validated <- validateSnapshotEntries root entries
              return $ entries <$ validated


validateSnapshotEntries
  :: (MonadFileSystem m)
  => OsPath
  -> [(FileType, OsPath)]
  -> m (Either StateError ())
validateSnapshotEntries _ [] = return $ Right ()
validateSnapshotEntries root (entry : rest) = do
  validated <- validateSnapshotEntry root entry
  case validated of
    Left err -> return $ Left err
    Right () -> validateSnapshotEntries root rest


validateSnapshotEntry
  :: (MonadFileSystem m)
  => OsPath
  -> (FileType, OsPath)
  -> m (Either StateError ())
validateSnapshotEntry root (fileType, relative) = do
  let entry = root </> relative
  symlink <- isSymlink entry
  if symlink || fileType == Symlink
    then return $ Left $ UnsupportedSnapshotSymlink entry
    else case fileType of
      Directory -> do
        directory <- isDirectory entry
        return $
          if directory
            then Right ()
            else Left $ UnsupportedSnapshotEntry entry
      File -> do
        regularFile <- isRegularFile entry
        return $
          if regularFile
            then Right ()
            else Left $ UnsupportedSnapshotEntry entry
      Symlink -> return $ Left $ UnsupportedSnapshotSymlink entry


treesEqual
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> m (Either StateError Bool)
treesEqual left right = do
  leftResult <- snapshotEntries left
  rightResult <- snapshotEntries right
  case (leftResult, rightResult) of
    (Left err, _) -> return $ Left err
    (_, Left err) -> return $ Left err
    (Right leftEntries', Right rightEntries') -> do
      let leftEntries = sort leftEntries'
      let rightEntries = sort rightEntries'
      if leftEntries /= rightEntries
        then return $ Right False
        else Right <$> snapshotEntriesMatch left right leftEntries


partialSnapshotMatches
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> m (Either StateError Bool)
partialSnapshotMatches source destination = do
  sourceResult <- snapshotEntries source
  destinationResult <- snapshotEntries destination
  case (sourceResult, destinationResult) of
    (Left err, _) -> return $ Left err
    (_, Left err) -> return $ Left err
    (Right sourceEntries, Right destinationEntries) ->
      if any (`notElem` sourceEntries) destinationEntries
        then return $ Right False
        else Right <$> snapshotEntriesMatch source destination destinationEntries


snapshotEntriesMatch
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> [(FileType, OsPath)]
  -> m Bool
snapshotEntriesMatch leftRoot rightRoot entries =
  and <$> forM entries sameEntry
 where
  sameEntry (fileType, relative) = case fileType of
    Directory -> return True
    File ->
      (==)
        <$> readFile (leftRoot </> relative)
        <*> readFile (rightRoot </> relative)
    Symlink -> return False


timeFormat :: String
timeFormat = "%Y-%m-%dT%H:%M:%S%QZ"


timeText :: UTCTime -> Text
timeText = Text.pack . formatTime defaultTimeLocale timeFormat


parseTime :: Text -> Text -> Either StateError UTCTime
parseTime field value =
  maybe
    (Left $ MalformedState $ "Invalid " <> field <> ".")
    Right
    (parseTimeM True defaultTimeLocale timeFormat $ Text.unpack value)


osPathText :: OsPath -> Text
osPathText value =
  if any isSurrogate decoded || pathEncodingPrefixString `isPrefixOf` decoded
    then pathEncodingPrefix <> Text.pack (concatMap encodeCodePoint decoded)
    else Text.pack decoded
 where
  decoded = unsafePerformIO $ decodeFS value


textOsPath :: Text -> Either Text OsPath
textOsPath value = do
  decoded <- case Text.stripPrefix pathEncodingPrefix value of
    Nothing -> Right $ Text.unpack value
    Just encoded -> decodeCodePoints $ Text.unpack encoded
  Right $ unsafePerformIO $ encodeFS decoded


pathEncodingPrefix :: Text
pathEncodingPrefix = Text.pack pathEncodingPrefixString


pathEncodingPrefixString :: String
pathEncodingPrefixString = "dojang-os-path-v1:"


isSurrogate :: Char -> Bool
isSurrogate character = codePoint >= 0xd800 && codePoint <= 0xdfff
 where
  codePoint = ord character


encodeCodePoint :: Char -> String
encodeCodePoint character = replicate (6 - length digits) '0' <> digits
 where
  digits = showHex (ord character) ""


decodeCodePoints :: String -> Either Text String
decodeCodePoints [] = Right []
decodeCodePoints encoded
  | length encoded < 6 = Left "Invalid encoded filesystem path."
  | otherwise = case readHex chunk of
      [(codePoint, "")]
        | codePoint <= 0x10ffff ->
            (chr codePoint :) <$> decodeCodePoints rest
      _ -> Left "Invalid encoded filesystem path."
 where
  (chunk, rest) = splitAt 6 encoded


textPath :: Text -> OsPath
textPath = unsafePerformIO . encodeFS . Text.unpack


path :: FilePath -> OsPath
path = unsafePerformIO . encodeFS


whenEither :: Bool -> e -> Either e ()
whenEither condition err = if condition then Left err else Right ()


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right


decodeStateUtf8 :: ByteString -> Either StateError Text
decodeStateUtf8 contents =
  mapLeft
    ( MalformedState
        . ("The state record is not valid UTF-8: " <>)
        . Text.pack
        . show
    )
    (decodeUtf8' contents)
