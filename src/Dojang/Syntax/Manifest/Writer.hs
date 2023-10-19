{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Syntax.Manifest.Writer
  ( writeManifest
  , writeManifestFile
  ) where

import Data.Bifunctor (Bifunctor (bimap, second))
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty ((:|)), length, toList)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import GHC.IsList (IsList (fromList))
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (all, any, writeFile)

import Data.CaseInsensitive (CI (original))
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import System.OsPath (OsPath, decodeFS)
import TextShow (FromStringShow (FromStringShow), TextShow (showt))
import Toml (encode)

import Dojang.MonadFileSystem (MonadFileSystem (writeFile))
import Dojang.Syntax.EnvironmentPredicate.Writer (writeEnvironmentPredicate)
import Dojang.Syntax.Manifest.Internal
  ( EnvironmentPredicate' (..)
  , FileRoute'
  , FileRouteMap'
  , FlatOrNonEmptyStrings (..)
  , Manifest' (Manifest')
  , MonikerMap'
  , always
  )
import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (..)
  , normalizePredicate
  )
import Dojang.Types.FilePathExpression (toPathText)
import Dojang.Types.FileRoute (FileRoute (..), FileType (Directory))
import Dojang.Types.FileRouteMap (FileRouteMap, toList)
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.MonikerMap (MonikerMap, toList)
import Dojang.Types.MonikerName (MonikerName)


-- | Encodes a 'Manifest' into a TOML document.
writeManifest :: Manifest -> Text
writeManifest manifest =
  showt $ FromStringShow $ encode manifest'
 where
  manifest' :: Manifest'
  manifest' = mapManifest' manifest


-- | Writes a 'Manifest' file to the given path.
writeManifestFile
  :: (MonadFileSystem m)
  => Manifest
  -- ^ The 'Manifest' to write.
  -> OsPath
  -- ^ The path to write the 'Manifest' to.
  -> m (Either IOError ())
  -- ^ The error, if any, that occurred while writing the 'Manifest'.
writeManifestFile manifest filePath =
  writeFile filePath $ encodeUtf8 $ writeManifest manifest


mapManifest' :: Manifest -> Manifest'
mapManifest' manifest =
  Manifest' monikers' dirs files
 where
  dirs :: FileRouteMap'
  files :: FileRouteMap'
  (dirs, files) = mapFiles manifest.fileRoutes manifest.monikers
  monikers' :: MonikerMap'
  monikers' = mapMonikers' manifest.monikers


mapFiles :: FileRouteMap -> MonikerMap -> (FileRouteMap', FileRouteMap')
mapFiles fileRouteMap monikers =
  ( fromList (bimap decodePath (`mapFileRoute'` monikers) <$> dirs)
  , fromList (bimap decodePath (`mapFileRoute'` monikers) <$> files)
  )
 where
  dirs :: [(OsPath, FileRoute)]
  files :: [(OsPath, FileRoute)]
  (dirs, files) =
    partition
      ((== Directory) . fileType . snd)
      $ Dojang.Types.FileRouteMap.toList fileRouteMap
  decodePath :: OsPath -> FilePath
  decodePath = unsafePerformIO . decodeFS


mapFileRoute' :: FileRoute -> MonikerMap -> FileRoute'
mapFileRoute' fileRoute monikers =
  (fromList . catMaybes)
    [ case (normalizePredicate pred', maybe "" toPathText filePath) of
      (Moniker n, path) -> Just (n, path)
      (pred'', path) -> case lookBack monikers pred'' of
        Just n -> Just (n, path)
        Nothing -> Nothing
    | (pred', filePath) <- fileRoute.predicates
    ]


lookBack :: MonikerMap -> EnvironmentPredicate -> Maybe MonikerName
lookBack monikers predicate =
  listToMaybe
    [ n
    | (n, p) <- Dojang.Types.MonikerMap.toList monikers
    , normalizePredicate p == normalizedPred
    ]
 where
  normalizedPred :: EnvironmentPredicate
  normalizedPred = normalizePredicate predicate


mapMonikers' :: MonikerMap -> MonikerMap'
mapMonikers' monikers =
  fromList
    $ second (mapEnvironmentPredicate' . normalizePredicate)
    <$> Dojang.Types.MonikerMap.toList monikers


mapEnvironmentPredicate' :: EnvironmentPredicate -> EnvironmentPredicate'
mapEnvironmentPredicate' Always = always
mapEnvironmentPredicate' (Moniker n) = always{all = Just $ fromList [n]}
mapEnvironmentPredicate' (OperatingSystem os) =
  always{os = Just $ Flat $ unpack $ original os.identifier}
mapEnvironmentPredicate' (Architecture arch) =
  always{arch = Just $ Flat $ unpack $ original arch.identifier}
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
          all' <- xorMaybe a.all b.all
          any' <- xorMaybe a.any b.any
          when' <- xorMaybe a.when b.when
          return
            EnvironmentPredicate'
              { os = os'
              , arch = arch'
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
  monikers' :: [MonikerName]
  monikers' = [n | Moniker n <- Data.List.NonEmpty.toList predicates']
  toNonEmpty :: [a] -> NonEmpty a
  toNonEmpty l = head l :| tail l
mapEnvironmentPredicate' pred' =
  always{when = Just $ writeEnvironmentPredicate pred'}
