{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Syntax.Manifest.Writer
  ( WriteError (..)
  , writeManifest
  , writeManifestFile
  ) where

import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (Bifunctor (second))
import Data.List (group, partition, sort)
import Data.List.NonEmpty (NonEmpty ((:|)), length, toList)
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.IsList (IsList (fromList))
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (all, any, writeFile)

import Data.CaseInsensitive (CI (original))
import Data.HashMap.Strict (toList)
import Data.Map.Strict (toList)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import System.OsPath (OsPath, decodeFS)
import TextShow (FromStringShow (FromStringShow), TextShow (showt))
import Toml.Pretty (prettyTomlOrdered)
import Toml.ToValue (ToTable (toTable))

import qualified Data.Map.Strict
import Dojang.MonadFileSystem
  ( FileType (Directory)
  , MonadFileSystem (writeFile)
  )
import Dojang.Syntax.EnvironmentPredicate.Writer (writeEnvironmentPredicate)
import Dojang.Syntax.Manifest.Internal
  ( EnvironmentPredicate' (..)
  , FileRoute'
  , FileRouteMap'
  , FlatOrNonEmptyStrings (..)
  , Hooks' (..)
  , IgnoreMap'
  , Manifest' (Manifest')
  , MonikerMap'
  , always
  )
import qualified Dojang.Syntax.Manifest.Internal as Internal
import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (..)
  , normalizePredicate
  )
import Dojang.Types.FilePathExpression (FilePathExpression, toPathText)
import Dojang.Types.FileRoute (FileRoute (..))
import Dojang.Types.FileRouteMap (FileRouteMap)
import Dojang.Types.Hook (Hook (..), HookMap, HookType (..))
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (MonikerName)


schema :: Text
schema = "https://schema.dojang.dev/2023-11/manifest.schema.json"


-- | An error that can occur while encoding a 'Manifest'.
data WriteError
  = -- | A route contains an environment predicate that cannot be represented
    -- by any moniker in the manifest.
    UnrepresentableRoutePredicate
      OsPath
      -- ^ The source path of the route.
      EnvironmentPredicate
      -- ^ The predicate that cannot be represented.
  | -- | Multiple route predicates resolve to the same moniker and therefore
    -- cannot be represented as distinct entries in the manifest.
    DuplicateRouteMoniker
      OsPath
      -- ^ The source path of the route.
      MonikerName
      -- ^ The moniker shared by multiple route predicates.
  deriving (Eq)


instance Show WriteError where
  show (UnrepresentableRoutePredicate path predicate) =
    "Route predicate "
      <> show predicate
      <> " for path "
      <> show (decodePath path)
      <> " cannot be represented by any moniker."
  show (DuplicateRouteMoniker path moniker) =
    "Multiple route predicates for path "
      <> show (decodePath path)
      <> " resolve to moniker "
      <> show moniker
      <> "."


-- | Encodes a 'Manifest' into a TOML document.
writeManifest
  :: Manifest
  -- ^ The 'Manifest' to encode.
  -> Either WriteError Text
  -- ^ The encoded TOML document, or an error if the manifest contains route
  -- predicates that the TOML format cannot represent without data loss.
writeManifest manifest = do
  manifest' <- mapManifest' manifest
  let tbl = toTable manifest'
  pure $
    "#:schema "
      <> schema
      <> "\n\n"
      <> (showt $ FromStringShow $ prettyTomlOrdered order tbl)
 where
  order :: [String] -> String -> Either Int String
  order [] field = case field of
    "dirs" -> Left 1
    "files" -> Left 2
    "ignores" -> Left 3
    "monikers" -> Left 4
    _ -> Right field
  order _ field = Right field


-- | Writes a 'Manifest' file to the given path.  Throws an 'IOError' if the
-- manifest cannot be encoded or an error occurs while writing the file.
writeManifestFile
  :: (MonadFileSystem m)
  => Manifest
  -- ^ The 'Manifest' to write.
  -> OsPath
  -- ^ The path to write the 'Manifest' to.
  -> m ()
writeManifestFile manifest filePath =
  case writeManifest manifest of
    Left err -> throwError $ userError $ show err
    Right toml -> writeFile filePath $ encodeUtf8 toml


mapManifest' :: Manifest -> Either WriteError Manifest'
mapManifest' manifest = do
  (dirs, files) <- mapFiles manifest.fileRoutes manifest.monikers
  pure $ Manifest' monikers' dirs files ignores hooks'
 where
  monikers' :: MonikerMap'
  monikers' = mapMonikers' manifest.monikers
  ignores :: IgnoreMap'
  ignores =
    fromList
      [ (decodePath path, pattern)
      | (path, pattern) <- Data.Map.Strict.toList manifest.ignorePatterns
      ]
  hooks' :: Maybe Hooks'
  hooks' =
    if Data.Map.Strict.null manifest.hooks
      then Nothing
      else Just $ mapHooks' manifest.hooks


mapFiles
  :: FileRouteMap
  -> MonikerMap
  -> Either WriteError (FileRouteMap', FileRouteMap')
mapFiles fileRouteMap monikers = do
  dirs' <- traverse mapRoute dirs
  files' <- traverse mapRoute files
  pure (fromList dirs', fromList files')
 where
  mapRoute :: (OsPath, FileRoute) -> Either WriteError (FilePath, FileRoute')
  mapRoute (path, route) = do
    route' <- mapFileRoute' path route monikers
    pure (decodePath path, route')
  dirs :: [(OsPath, FileRoute)]
  files :: [(OsPath, FileRoute)]
  (dirs, files) =
    partition
      ((== Directory) . fileType . snd)
      $ Data.Map.Strict.toList fileRouteMap


decodePath :: OsPath -> FilePath
decodePath = unsafePerformIO . decodeFS


mapFileRoute'
  :: OsPath -> FileRoute -> MonikerMap -> Either WriteError FileRoute'
mapFileRoute' routePath fileRoute monikers = do
  mappedPredicates <- traverse mapPredicate fileRoute.predicates
  case duplicateMoniker mappedPredicates of
    Nothing -> Right $ fromList mappedPredicates
    Just moniker -> Left $ DuplicateRouteMoniker routePath moniker
 where
  duplicateMoniker :: [(MonikerName, Text)] -> Maybe MonikerName
  duplicateMoniker entries =
    listToMaybe
      [ moniker
      | moniker : _ : _ <- group $ sort $ fst <$> entries
      ]
  mapPredicate
    :: (EnvironmentPredicate, Maybe FilePathExpression)
    -> Either WriteError (MonikerName, Text)
  mapPredicate (predicate, filePath) =
    case normalizePredicate predicate of
      Moniker name -> Right (name, path)
      normalizedPredicate -> case lookBack monikers normalizedPredicate of
        Just name -> Right (name, path)
        Nothing ->
          Left $ UnrepresentableRoutePredicate routePath normalizedPredicate
   where
    path = maybe "" toPathText filePath


lookBack :: MonikerMap -> EnvironmentPredicate -> Maybe MonikerName
lookBack monikers predicate =
  listToMaybe $
    sort
      [ n
      | (n, p) <- Data.HashMap.Strict.toList monikers
      , normalizePredicate p == normalizedPred
      ]
 where
  normalizedPred :: EnvironmentPredicate
  normalizedPred = normalizePredicate predicate


mapMonikers' :: MonikerMap -> MonikerMap'
mapMonikers' monikers =
  fromList $
    second (mapEnvironmentPredicate' . normalizePredicate)
      <$> Data.HashMap.Strict.toList monikers


mapEnvironmentPredicate' :: EnvironmentPredicate -> EnvironmentPredicate'
mapEnvironmentPredicate' Always = always
mapEnvironmentPredicate' (Moniker n) = always{all = Just $ fromList [n]}
mapEnvironmentPredicate' (OperatingSystem os) =
  always{os = Just $ Flat $ unpack $ original os.identifier}
mapEnvironmentPredicate' (Architecture arch) =
  always{arch = Just $ Flat $ unpack $ original arch.identifier}
mapEnvironmentPredicate' (KernelName kernel) =
  always{kernel = Just $ Flat $ unpack $ original kernel}
mapEnvironmentPredicate' (KernelRelease kernelRelease) =
  always{kernelRelease = Just $ Flat $ unpack $ original kernelRelease}
mapEnvironmentPredicate' (And predicates') =
  fromMaybe
    always{when = Just $ writeEnvironmentPredicate $ And predicates'}
    (xor' $ mapEnvironmentPredicate' <$> predicates')
 where
  xor' :: NonEmpty EnvironmentPredicate' -> Maybe EnvironmentPredicate'
  xor' (predicate :| []) = Just predicate
  xor' (a :| predicates'') =
    let r : est = predicates''
    in case xor' (r :| est) of
         Nothing -> Nothing
         Just b -> do
           os' <- xorMaybe a.os b.os
           arch' <- xorMaybe a.arch b.arch
           kernel' <- xorMaybe a.kernel b.kernel
           kernelRelease' <- xorMaybe a.kernelRelease b.kernelRelease
           all' <- xorMaybe a.all b.all
           any' <- xorMaybe a.any b.any
           when' <- xorMaybe a.when b.when
           return
             EnvironmentPredicate'
               { os = os'
               , arch = arch'
               , kernel = kernel'
               , kernelRelease = kernelRelease'
               , all = all'
               , any = any'
               , when = when'
               }
  xorMaybe :: Maybe a -> Maybe a -> Maybe (Maybe a)
  xorMaybe (Just _) (Just _) = Nothing
  xorMaybe (Just a) Nothing = Just $ Just a
  xorMaybe Nothing (Just b) = Just $ Just b
  xorMaybe Nothing Nothing = Just Nothing
mapEnvironmentPredicate' (Or predicates')
  | Prelude.length oses == length' =
      always{os = Just $ NonEmpty $ toNonEmpty oses}
  | Prelude.length arches == length' =
      always{arch = Just $ NonEmpty $ toNonEmpty arches}
  | Prelude.length kernels == length' =
      always{kernel = Just $ NonEmpty $ toNonEmpty kernels}
  | Prelude.length kernelReleases == length' =
      always{kernelRelease = Just $ NonEmpty $ toNonEmpty kernelReleases}
  | Prelude.length monikers' == length' =
      always{any = Just $ toNonEmpty monikers'}
  | otherwise =
      always{when = Just $ writeEnvironmentPredicate $ Or predicates'}
 where
  length' :: Int
  length' = Data.List.NonEmpty.length predicates'
  oses :: [String]
  oses =
    [ unpack $ original os'.identifier
    | OperatingSystem os' <- Data.List.NonEmpty.toList predicates'
    ]
  arches :: [String]
  arches =
    [ unpack $ original arch'.identifier
    | Architecture arch' <- Data.List.NonEmpty.toList predicates'
    ]
  kernels :: [String]
  kernels =
    [ unpack $ original kernel'
    | KernelName kernel' <- Data.List.NonEmpty.toList predicates'
    ]
  kernelReleases :: [String]
  kernelReleases =
    [ unpack $ original kernelRelease'
    | KernelRelease kernelRelease' <- Data.List.NonEmpty.toList predicates'
    ]
  monikers' :: [MonikerName]
  monikers' = [n | Moniker n <- Data.List.NonEmpty.toList predicates']
  toNonEmpty :: [a] -> NonEmpty a
  toNonEmpty (x : xs) = x :| xs
  toNonEmpty [] = error "toNonEmpty: empty list"
mapEnvironmentPredicate' pred' =
  always{when = Just $ writeEnvironmentPredicate pred'}


mapHooks' :: HookMap -> Hooks'
mapHooks' hookMap =
  Hooks'
    { preApply = mapHookList PreApply
    , preFirstApply = mapHookList PreFirstApply
    , postFirstApply = mapHookList PostFirstApply
    , postApply = mapHookList PostApply
    }
 where
  mapHookList :: HookType -> Maybe [Internal.Hook']
  mapHookList hookType =
    case Data.Map.Strict.lookup hookType hookMap of
      Nothing -> Nothing
      Just [] -> Nothing
      Just hooks' -> Just $ mapHook <$> hooks'
  mapHook :: Hook -> Internal.Hook'
  mapHook hook =
    Internal.Hook'
      { Internal.command = decodePath' hook.command
      , Internal.args = if null hook.args then Nothing else Just hook.args
      , Internal.moniker = Nothing -- moniker is already resolved into condition
      , Internal.condition =
          if hook.condition == Always
            then Nothing
            else Just $ writeEnvironmentPredicate hook.condition
      , Internal.workingDirectory = decodePath <$> hook.workingDirectory
      , Internal.ignoreFailure = if hook.ignoreFailure then Just True else Nothing
      }
  decodePath' :: OsPath -> Text
  decodePath' = unsafePerformIO . fmap pack . decodeFS
