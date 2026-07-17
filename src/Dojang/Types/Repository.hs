{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Repository
  ( Repository (..)
  , RouteMapWarning (..)
  , RouteResult (..)
  , findOverlappingRouteResults
  , overlaps
  , routePaths
  , routePathsWithVariables
  ) where

import Control.Monad (forM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (toLazyByteString, word32BE)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive (original)
import Data.Char (ord)
import Data.Function ((&))
import Data.List (isPrefixOf, nub)
import Data.List.NonEmpty
  ( NonEmpty ((:|))
  , append
  , nub
  , singleton
  , sort
  , toList
  )

import Data.Map.Strict (Map, findWithDefault, fromListWith, toList)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric (showHex)
import System.FilePattern (FilePattern, matchMany)
import System.OsPath
  ( OsPath
  , OsString
  , makeRelative
  , normalise
  , splitDirectories
  , toChar
  , unpack
  , (</>)
  )

import Dojang.MonadFileSystem (FileType, MonadFileSystem (..))
import Dojang.Types.Environment
  ( Environment (..)
  , Kernel (..)
  , factKeyText
  , factValueText
  )
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning)
import Dojang.Types.FilePathExpression
  ( EnvironmentVariable
  , FilePathExpression
  , environmentVariables
  , toPathText
  )
import Dojang.Types.FilePathExpression.Expansion
  ( ExpansionWarning
  , VariableGetter
  , VariableLookup (..)
  )
import Dojang.Types.FileRoute
  ( FileRoute (..)
  , RouteWarning
  , dispatch
  , routePathWithVariables
  )
import Dojang.Types.FileRoute qualified as FileRoute (RouteWarning (..))
import Dojang.Types.Manifest (Manifest (..))


-- | A repository, which is a directory containing a manifest file and dotfiles.
data Repository = Repository
  { sourcePath :: OsPath
  -- ^ The path to the repository.
  , intermediatePath :: OsPath
  -- ^ The path to the intermediate directory, which is managed by Dojang and
  -- contains the post-processed files.
  , manifest :: Manifest
  -- ^ The manifest of the repository.
  }


-- | The expanded dispatch paths of a 'FileRoute'.
data RouteResult = RouteResult
  { sourcePath :: OsPath
  -- ^ The source path.  It is either absolute or relative to the current
  -- working directory.
  , routeName :: OsPath
  -- ^ The source path relative from the repository root, i.e., the route name.
  , destinationPath :: OsPath
  -- ^ The destination path.   It is either absolute or relative to
  -- the current working directory.
  , fileType :: FileType
  , routeDefinition :: Text
  -- ^ Canonical selected destination expression.
  , routeProvenance :: Map Text Text
  -- ^ Environment facts and privacy-preserving input fingerprints.
  }
  deriving (Eq, Show)


-- | A warning that can occur during routing paths.
data RouteMapWarning
  = -- | A warning that can occur during environment predicate evaluation.
    EnvironmentPredicateWarning EvaluationWarning
  | -- | A warning that can occur during file path expression expansion.
    FilePathExpressionWarning ExpansionWarning
  | -- | A warning that two or more routes overlap in their destination paths
    -- (e.g., @/foo/bar@ and @/foo/bar/baz@).
    OverlapDestinationPathsWarning
      OsPath
      -- ^ The source path of the route that have overlapping destination paths.
      OsPath
      -- ^ The destination path that overlaps.
      (NonEmpty (OsPath, OsPath))
      -- ^ The pairs of corresponding source and destination paths that overlap.
  deriving (Eq, Show)


-- | Route the paths in the repository.  This will return a list of expanded
-- paths, along with any warnings that were generated.  Null routes will be
-- ignored.
routePaths
  :: forall m
   . (MonadFileSystem m)
  => Repository
  -- ^ The repository.
  -> Environment
  -- ^ The environment to use when evaluating the environment predicates.
  -> (EnvironmentVariable -> m (Maybe OsString))
  -- ^ The function to look up environment variables.
  -> m ([RouteResult], [RouteMapWarning])
  -- ^ The expanded paths, along with any warnings that were generated.
routePaths repo env lookupEnvVar = do
  routePathsWithVariables repo env legacyGetter
 where
  legacyGetter variable = do
    value <- lookupEnvVar variable
    rendered <- case value of
      Nothing -> return "unset"
      Just value' | value' == mempty -> return "empty"
      Just value' -> return $ "sha256:" <> sha256OsString value'
    return $
      VariableLookup value [] $
        Map.singleton ("env." <> variable) rendered


-- | Routes paths through a warning- and provenance-aware variable getter.
routePathsWithVariables
  :: forall m
   . (MonadFileSystem m)
  => Repository
  -> Environment
  -> VariableGetter m
  -> m ([RouteResult], [RouteMapWarning])
routePathsWithVariables repo env lookupVariable = do
  paths <- forM fileRoutes $ \(src, route) -> do
    (dstPath, warnings, _) <-
      routePathWithVariables route env lookupVariable
    let selected = case fst $ dispatch env route of
          Just expression : _ -> Just expression
          _ -> Nothing
    provenance <- makeProvenance selected
    let definition = maybe "" toPathText selected
    return (src, dstPath, route.fileType, definition, provenance, warnings)
  let paths' =
        [ RouteResult (repo.sourcePath </> src) src dst' ft definition provenance
        | (src, Just dst', ft, definition, provenance, _) <- paths
        ]
  overlappingRoutes <-
    forM (Data.Map.Strict.toList $ findOverlappingRouteResults paths') $
      \((name, dst), overlaps') -> do
        let ignorePatterns =
              findWithDefault [] name repo.manifest.ignorePatterns
        filtered <- filterIgnoredPathsFromOverlaps dst ignorePatterns overlaps'
        return ((name, dst), filtered)
  let warnings =
        [translateWarning w | (_, _, _, _, _, ws) <- paths, w <- ws]
          ++ [ OverlapDestinationPathsWarning name dst (o :| overlaps')
             | ((name, dst), o : overlaps') <- overlappingRoutes
             ]
  return (paths', Data.List.nub warnings)
 where
  fileRoutes :: [(OsPath, FileRoute)]
  fileRoutes = Data.Map.Strict.toList repo.manifest.fileRoutes
  makeProvenance :: Maybe FilePathExpression -> m (Map Text Text)
  makeProvenance selected = do
    inputs <- case selected of
      Nothing -> return []
      Just expression ->
        forM (Set.toList $ environmentVariables expression) $ \variable -> do
          found <- lookupVariable variable
          return $ Map.toList found.provenance
    return $
      Map.fromList $
        [ ("operating-system", original env.operatingSystem.identifier)
        , ("architecture", original env.architecture.identifier)
        , ("kernel-name", original env.kernel.name)
        , ("kernel-release", original env.kernel.release)
        ]
          ++ concat inputs
          ++ [ ("fact." <> factKeyText key, factValueText value)
             | (key, value) <- Map.toAscList env.additionalFacts
             ]
  filterIgnoredPathsFromOverlaps
    :: OsPath
    -> [FilePattern]
    -> NonEmpty (OsPath, OsPath)
    -> m [(OsPath, OsPath)]
  filterIgnoredPathsFromOverlaps base patterns paths = do
    paths' <- forM pathList $ \pair@(_, dst') -> do
      dstFP <- decodePath $ makeRelative base' $ normalise dst'
      return (pair, dstFP)
    let ignored = [pair | (_, pair, _) <- matchMany patterns' paths']
    return [pair | pair <- pathList, pair `notElem` ignored]
   where
    base' :: OsPath
    base' = normalise base
    pathList :: [(OsPath, OsPath)]
    pathList = Data.List.NonEmpty.toList paths
    patterns' :: [((), FilePattern)]
    patterns' =
      [((), pattern) | pattern <- patterns]
        ++ [((), pattern ++ "/**/*") | pattern <- patterns]


sha256OsString :: OsString -> Text
sha256OsString =
  digestHex
    . SHA256.hash
    . LazyByteString.toStrict
    . toLazyByteString
    . foldMap (word32BE . fromIntegral . ord . toChar)
    . unpack


digestHex :: ByteString.ByteString -> Text
digestHex = Text.pack . concatMap byteHex . ByteString.unpack
 where
  byteHex byte = case showHex byte "" of
    [digit] -> ['0', digit]
    digits -> digits


translateWarning :: RouteWarning -> RouteMapWarning
translateWarning (FileRoute.EnvironmentPredicateWarning w) =
  EnvironmentPredicateWarning w
translateWarning (FileRoute.FilePathExpressionWarning w) =
  FilePathExpressionWarning w


findOverlappingRouteResults
  :: [RouteResult] -> Map (OsPath, OsPath) (NonEmpty (OsPath, OsPath))
findOverlappingRouteResults routes =
  fromListWith append found & fmap (Data.List.NonEmpty.nub . sort)
 where
  pairs :: [(OsPath, OsPath)]
  pairs =
    [ (name, dst)
    | RouteResult{routeName = name, destinationPath = dst} <- routes
    ]
  found :: [((OsPath, OsPath), NonEmpty (OsPath, OsPath))]
  found =
    [ ((name, dst), singleton (name', dst'))
    | (name, dst) <- pairs
    , (name', dst') <- pairs
    , name /= name'
    , dst `overlaps` dst'
    ]


-- | Returns whether the two paths overlap.  The former path is assumed to be
-- shorter than or equal to the latter path.
overlaps :: OsPath -> OsPath -> Bool
overlaps a b =
  (a' == b') || aDirs `isPrefixOf` bDirs
 where
  a' :: OsPath
  a' = normalise a
  b' :: OsPath
  b' = normalise b
  aDirs :: [OsPath]
  aDirs = splitDirectories a'
  bDirs :: [OsPath]
  bDirs = splitDirectories b'
