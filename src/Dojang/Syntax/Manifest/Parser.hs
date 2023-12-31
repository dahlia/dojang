{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Syntax.Manifest.Parser
  ( Error (..)
  , formatErrors
  , readManifest
  , readManifestFile
  ) where

import Data.Bifunctor (Bifunctor (first))
import Data.Either (lefts)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty (toList)
import Data.String (IsString (fromString))
import Data.Void (Void)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (readFile)

import Data.HashMap.Strict as HashMap (fromList)
import Data.Map.Strict as Map (Map, fromList, toList)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import System.FilePattern (FilePattern)
import System.OsPath (OsPath, encodeFS)
import Toml (Result (..), decode)

import Dojang.MonadFileSystem (FileType (..), MonadFileSystem (readFile))
import Dojang.Syntax.EnvironmentPredicate.Parser
  ( ParseErrorBundle
  , errorBundlePretty
  , parseEnvironmentPredicate
  )
import Dojang.Syntax.FilePathExpression.Parser
  ( errorBundlePretty
  , parseFilePathExpression
  )
import Dojang.Syntax.Manifest.Internal
  ( EnvironmentPredicate' (..)
  , FileRoute'
  , FileRouteMap'
  , FlatOrNonEmptyStrings (..)
  , Manifest' (..)
  , MonikerMap'
  )
import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (..)
  , normalizePredicate
  )
import Dojang.Types.FilePathExpression (FilePathExpression)
import Dojang.Types.FileRoute (FileRoute, fileRoute)
import Dojang.Types.FileRouteMap (FileRouteMap)
import Dojang.Types.Manifest (Manifest (Manifest))
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (MonikerName)


-- | An error made during parsing.
data Error
  = -- | TOML parsing errors.
    TomlErrors (NonEmpty String)
  | -- | An error made during parsing an 'EnvironmentPredicate'.
    EnvironmentPredicateError (ParseErrorBundle Text Void)
  | -- | An error made during parsing a 'FilePathExpression'.
    FilePathExpressionError (ParseErrorBundle Text Void)


-- | A warning message made during parsing.
type TomlWarning = Text


-- | Decodes a 'Manifest' from a 'Text'.
readManifest
  :: Text
  -- ^ A TOML document text to parse.
  -> Either Error (Manifest, [TomlWarning])
  -- ^ A decoded 'Manifest' with warnings, or a list of errors.
readManifest toml = case decode $ unpack toml of
  Success warnings manifest' ->
    case mapManifest manifest' of
      Left e -> Left e
      Right m -> Right (m, pack <$> warnings)
  Failure (e : es) -> Left $ TomlErrors $ e :| es
  Failure [] -> Left $ TomlErrors $ "unknown error" :| []


-- | Reads a 'Manifest' file from the given path.  It assumes that the file
-- is encoded in UTF-8.  Throws an 'IOError' if the file cannot be read.
readManifestFile
  :: (MonadFileSystem m)
  => OsPath
  -- ^ A path to the manifest file.
  -> m (Either Error (Manifest, [TomlWarning]))
  -- ^ A decoded manifest with warnings, or a list of errors.
readManifestFile filePath = do
  content <- readFile filePath
  return $ readManifest $ decodeUtf8Lenient content


-- | Format error messages.
formatErrors :: Error -> [Text]
formatErrors (TomlErrors es) = Data.List.NonEmpty.toList $ fmap pack es
formatErrors (EnvironmentPredicateError e) =
  [Dojang.Syntax.EnvironmentPredicate.Parser.errorBundlePretty e]
formatErrors (FilePathExpressionError e) =
  [Dojang.Syntax.FilePathExpression.Parser.errorBundlePretty e]


mapManifest :: Manifest' -> Either Error Manifest
mapManifest manifest' =
  case monikersResult of
    Left e -> Left e
    Right monikers' ->
      case mapFileRouteMap monikers' manifest'.dirs manifest'.files of
        Left e -> Left e
        Right fileRoutes' -> Right $ Manifest monikers' fileRoutes' ignores'
 where
  monikersResult :: Either Error MonikerMap
  monikersResult = mapMonikerMap manifest'.monikers
  ignores' :: Map OsPath [FilePattern]
  ignores' =
    Map.fromList
      [ (encodePath path, pattern)
      | (path, pattern) <- Map.toList manifest'.ignores
      ]


mapMonikerMap :: MonikerMap' -> Either Error MonikerMap
mapMonikerMap m =
  case errors of
    e : _ -> Left e
    _ ->
      Right
        $ HashMap.fromList
          [(name, pred') | (name, Right pred') <- results]
 where
  results :: [(MonikerName, Either Error EnvironmentPredicate)]
  results =
    [ (name, mapEnvironmentPredicate' pred')
    | (name, pred') <- toList m
    ]
  errors :: [Error]
  errors = lefts [r | (_, r) <- results]


mapEnvironmentPredicate'
  :: EnvironmentPredicate'
  -> Either Error EnvironmentPredicate
mapEnvironmentPredicate' envPred' =
  case when of
    Right pred' ->
      Right
        $ normalizePredicate
        $ And [os, arch, kernel, kernelRelease, all', any', pred']
    Left err -> Left err
 where
  os :: EnvironmentPredicate
  os =
    maybe
      Always
      (Or . fmap OperatingSystem . mapFlatOrNonEmptyStrings)
      envPred'.os
  arch :: EnvironmentPredicate
  arch =
    maybe
      Always
      (Or . fmap Architecture . mapFlatOrNonEmptyStrings)
      envPred'.arch
  kernel :: EnvironmentPredicate
  kernel =
    maybe
      Always
      (Or . fmap KernelName . mapFlatOrNonEmptyStrings)
      envPred'.kernel
  kernelRelease :: EnvironmentPredicate
  kernelRelease =
    maybe
      Always
      (Or . fmap KernelRelease . mapFlatOrNonEmptyStrings)
      envPred'.kernelRelease
  all' :: EnvironmentPredicate
  all' = maybe Always (And . fmap Moniker) envPred'.all
  any' :: EnvironmentPredicate
  any' = maybe Always (Or . fmap Moniker) envPred'.any
  when :: Either Error EnvironmentPredicate
  when =
    maybe
      (Right Always)
      (first EnvironmentPredicateError . parseEnvironmentPredicate "")
      envPred'.when


mapFlatOrNonEmptyStrings :: (IsString a) => FlatOrNonEmptyStrings -> NonEmpty a
mapFlatOrNonEmptyStrings (Flat a) = [fromString a]
mapFlatOrNonEmptyStrings (NonEmpty as) = fromString <$> as


mapFileRouteMap
  :: MonikerMap -> FileRouteMap' -> FileRouteMap' -> Either Error FileRouteMap
mapFileRouteMap monikerMap dirs files =
  case errors of
    e : _ -> Left e
    _ -> Right $ Map.fromList [(name, route) | (name, Right route) <- results]
 where
  results :: [(OsPath, Either Error FileRoute)]
  results =
    [ (encodePath name, mapFileRoute monikerMap route Directory)
    | (name, route) <- toList dirs
    ]
      ++ [ (encodePath name, mapFileRoute monikerMap route File)
         | (name, route) <- toList files
         ]
  errors :: [Error]
  errors = lefts [r | (_, r) <- results]


encodePath :: String -> OsPath
encodePath = unsafePerformIO . encodeFS


mapFileRoute
  :: MonikerMap
  -> FileRoute'
  -> FileType
  -> Either Error FileRoute
mapFileRoute monikerMap fileRoute' fileType =
  case errors of
    e : _ -> Left $ FilePathExpressionError e
    _ ->
      Right
        $ fileRoute
          monikerMap
          ( [(name, Just expr') | (name, Just (Right expr')) <- results]
              ++ [(name, Nothing) | (name, Nothing) <- results]
          )
          fileType
 where
  results
    :: [ ( MonikerName
         , Maybe (Either (ParseErrorBundle Text Void) FilePathExpression)
         )
       ]
  results =
    [ ( name
      , if expr == "" then Nothing else Just $ parseFilePathExpression "" expr
      )
    | (name, expr) <- toList fileRoute'
    ]
  errors :: [ParseErrorBundle Text Void]
  errors = lefts [r | (_, Just r) <- results]
