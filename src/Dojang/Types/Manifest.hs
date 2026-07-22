{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Manifest
  ( IgnoreMap
  , Manifest
    ( Manifest
    , ManifestWithCodecBackends
    , codecBackends
    , fileRoutes
    , hooks
    , ignorePatterns
    , monikers
    , repositoryId
    , variables
    )
  , manifest
  , manifestWithCodecBackends
  , manifestWithVariables
  ) where

import Data.Map.Strict (Map, fromList, toAscList, toList)
import System.FilePattern (FilePattern)
import System.OsPath (OsPath, normalise)

import Dojang.MonadFileSystem (FileType (..))
import Dojang.Types.CodecBackend (CodecBackendMap)
import Dojang.Types.FilePathExpression (FilePathExpression)
import Dojang.Types.FileRoute (FileRoute, fileRoute)
import Dojang.Types.FileRouteMap (FileRouteMap)
import Dojang.Types.Hook (HookMap)
import Dojang.Types.ManifestVariable (ManifestVariableMap)
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (MonikerName)
import Dojang.Types.RepositoryId (RepositoryId)


-- | A map of directory routes to file patterns that should be ignored.
type IgnoreMap = Map OsPath [FilePattern]


-- | A manifest of the directory routes and the definitions of monikers that
-- are used to resolve them.
data Manifest = ManifestWithCodecBackends
  { repositoryId :: Maybe RepositoryId
  -- ^ Stable repository identity, omitted by legacy manifests.
  , monikers :: MonikerMap
  -- ^ Named environment predicates used by routes and variables.
  , variables :: ManifestVariableMap
  -- ^ Declarative values available to file-path expressions.
  , fileRoutes :: FileRouteMap
  -- ^ File and directory routes selected by environment predicates.
  , ignorePatterns :: IgnoreMap
  -- ^ Ignore patterns keyed by directory route.
  , codecBackends :: CodecBackendMap
  -- ^ Reusable external backends for sensitive route codecs.
  , hooks :: HookMap
  -- ^ Hooks run before and after supported commands.
  }
  deriving (Eq, Show)


-- | Constructs a manifest without external codec backends.
--
-- Use 'manifestWithCodecBackends' when a backend registry is required.
pattern Manifest
  :: Maybe RepositoryId
  -> MonikerMap
  -> ManifestVariableMap
  -> FileRouteMap
  -> IgnoreMap
  -> HookMap
  -> Manifest
pattern Manifest repositoryId monikers variables fileRoutes ignorePatterns hooks <-
  ManifestWithCodecBackends
    repositoryId
    monikers
    variables
    fileRoutes
    ignorePatterns
    _
    hooks
 where
  Manifest repositoryId monikers variables fileRoutes ignorePatterns hooks =
    ManifestWithCodecBackends
      repositoryId
      monikers
      variables
      fileRoutes
      ignorePatterns
      mempty
      hooks


{-# COMPLETE Manifest #-}


-- | Constructs a manifest with an explicit codec backend registry.
manifestWithCodecBackends
  :: Maybe RepositoryId
  -> MonikerMap
  -> ManifestVariableMap
  -> FileRouteMap
  -> IgnoreMap
  -> CodecBackendMap
  -> HookMap
  -> Manifest
manifestWithCodecBackends = ManifestWithCodecBackends


-- | Makes a 'Manifest' from definitions of monikers and directory routes.
manifest
  :: MonikerMap
  -- ^ The monikers that are used to resolve the directory routes.
  -> Map OsPath [(MonikerName, Maybe FilePathExpression)]
  -- ^ The file routes that are resolved by the monikers.  The keys are
  -- the file names, and the values are the pairs of 'MonikerName's and
  -- 'FilePathExpression's that make up each 'FileRoute'.
  -- If the same 'MonikerName' is used more than once, then the latest one
  -- will be used.  If the same file name appears in the directory routes
  -- (the next parameter), then the directory route will take precedence.
  -> Map OsPath [(MonikerName, Maybe FilePathExpression)]
  -- ^ The directory routes that are resolved by the monikers.  The keys are
  -- the names of the directories, and the values are the pairs of
  -- 'MonikerName's and 'FilePathExpression's that make up each 'FileRoute'.
  -- If the same 'MonikerName' is used more than once, then the latest one
  -- will be used.  If the same file name appears in the file routes
  -- (the previous parameter), then the directory route will take precedence.
  -> IgnoreMap
  -- ^ The file patterns that should be ignored for each directory route.
  -> HookMap
  -- ^ The hooks to run before and after applying.
  -> Manifest
  -- ^ The made 'Manifest'.
manifest monikers' = manifestWithVariables monikers' mempty


-- | Makes a 'Manifest' with declarative variables.
manifestWithVariables
  :: MonikerMap
  -- ^ The monikers that are used to resolve routes and variable branches.
  -> ManifestVariableMap
  -- ^ Declarative values available to file-path expressions.
  -> Map OsPath [(MonikerName, Maybe FilePathExpression)]
  -- ^ File routes resolved by the monikers.
  -> Map OsPath [(MonikerName, Maybe FilePathExpression)]
  -- ^ Directory routes resolved by the monikers.
  -> IgnoreMap
  -- ^ Ignore patterns keyed by directory route.
  -> HookMap
  -- ^ Hooks to run around commands.
  -> Manifest
  -- ^ The made 'Manifest'.
manifestWithVariables
  monikers'
  variables'
  fileRoutes'
  dirRoutes'
  ignorePatterns'
  hooks' =
    Manifest
      Nothing
      monikers'
      variables'
      (fromList $ files ++ dirs)
      ignores
      hooks'
   where
    files :: [(OsPath, FileRoute)]
    files = do
      (filename, monikerPairs) <- toList fileRoutes'
      pure (normalise filename, fileRoute monikers' monikerPairs File)
    dirs :: [(OsPath, FileRoute)]
    dirs = do
      (dirName, monikerPairs) <- toList dirRoutes'
      pure (normalise dirName, fileRoute monikers' monikerPairs Directory)
    ignores :: IgnoreMap
    ignores =
      fromList
        [ (normalise p, patterns)
        | (p, patterns) <- toAscList ignorePatterns'
        ]
