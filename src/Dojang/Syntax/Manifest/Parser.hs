{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Syntax.Manifest.Parser
  ( DetailedRouteError (..)
  , Error (..)
  , formatErrors
  , readManifest
  , readManifestFile
  ) where

import Data.Bifunctor (Bifunctor (first))
import Data.Either (lefts)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty (toList)
import Data.String (IsString (fromString))
import Data.Void (Void)
import qualified System.FilePath.Windows as Windows
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (readFile)

import Data.CaseInsensitive (CI (original))
import Data.HashMap.Strict as HashMap (fromList)
import Data.Map.Strict as Map
  ( Map
  , elems
  , empty
  , fromList
  , fromListWith
  , toList
  )
import Data.Text (Text, intercalate, pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import System.FilePattern (FilePattern)
import System.OsPath
  ( OsPath
  , decodeFS
  , encodeFS
  , isAbsolute
  , normalise
  , splitDirectories
  )
import Toml (Result (..), decode)

import qualified Data.HashMap.Strict as HashMap
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
  , FileRoute' (..)
  , FileRouteBranch' (..)
  , FileRouteMap'
  , FlatOrNonEmptyStrings (..)
  , Manifest' (..)
  , MonikerMap'
  )
import qualified Dojang.Syntax.Manifest.Internal as Internal
import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (..)
  , normalizePredicate
  )
import Dojang.Types.FilePathExpression
  ( FilePathExpression (BareComponent)
  )
import Dojang.Types.FileRoute
  ( FileRoute
  , fileRoute
  , fileRoutePreservingOrder
  )
import Dojang.Types.FileRouteMap (FileRouteMap)
import Dojang.Types.Hook (Hook (..), HookMap, HookType (..))
import Dojang.Types.Manifest (Manifest (Manifest))
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (MonikerName)
import Dojang.Types.RepositoryId (parseRepositoryId)


-- | An error made during parsing.
data Error
  = -- | The manifest is not valid UTF-8.
    InvalidUtf8 Text
  | -- | TOML parsing errors.
    TomlErrors (NonEmpty String)
  | -- | An error made during parsing an 'EnvironmentPredicate'.
    EnvironmentPredicateError (ParseErrorBundle Text Void)
  | -- | An error made during parsing a 'FilePathExpression'.
    FilePathExpressionError (ParseErrorBundle Text Void)
  | -- | An invalid branch in a detailed file route.
    FileRouteBranchError FileType OsPath Int DetailedRouteError
  | -- | A route name can escape the repository root.
    InvalidRoutePathError FileType OsPath
  | -- | Distinct route names normalize to one lifecycle identity.
    DuplicateNormalizedRoutePathError OsPath OsPath
  | -- | The repository identity is not a UUID.
    RepositoryIdError Text


-- | An error in a branch of a detailed file route.
data DetailedRouteError
  = -- | The branch has neither a @moniker@ nor a @when@ field.
    MissingRouteCondition
  | -- | The branch has both a @moniker@ and a @when@ field.
    ConflictingRouteConditions
  | -- | The @moniker@ field refers to an undefined moniker.
    UnknownRouteMoniker MonikerName
  | -- | The branch contains fields other than @moniker@, @when@, and @path@.
    UnexpectedRouteFields [Text]
  deriving (Eq, Show)


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
  return $ case decodeUtf8' content of
    Left err -> Left $ InvalidUtf8 $ pack $ show err
    Right source -> readManifest source


-- | Format error messages.
formatErrors :: Error -> [Text]
formatErrors (InvalidUtf8 details) =
  ["The manifest is not valid UTF-8: " <> details]
formatErrors (TomlErrors es) = Data.List.NonEmpty.toList $ fmap pack es
formatErrors (EnvironmentPredicateError e) =
  [Dojang.Syntax.EnvironmentPredicate.Parser.errorBundlePretty e]
formatErrors (FilePathExpressionError e) =
  [Dojang.Syntax.FilePathExpression.Parser.errorBundlePretty e]
formatErrors (FileRouteBranchError fileType path index reason) =
  [ prefix <> formatReason reason
  ]
 where
  prefix =
    "Detailed "
      <> ( case fileType of
             Directory -> "directory"
             File -> "file"
             Symlink -> "symbolic link"
         )
      <> " route branch "
      <> pack (show $ index + 1)
      <> " for "
      <> pack (decodePath path)
      <> " "
  formatReason MissingRouteCondition =
    "must specify either moniker or when."
  formatReason ConflictingRouteConditions =
    "cannot specify both moniker and when."
  formatReason (UnknownRouteMoniker name) =
    "refers to undefined moniker " <> original name.name <> "."
  formatReason (UnexpectedRouteFields fields) =
    "contains unexpected field(s): " <> intercalate ", " fields <> "."
formatErrors (InvalidRoutePathError fileType routePath) =
  [ routeKind fileType
      <> " route name "
      <> pack (decodePath routePath)
      <> " must be relative and must not contain parent traversal."
  ]
 where
  routeKind Directory = "Directory"
  routeKind File = "File"
  routeKind Symlink = "Symbolic-link"
formatErrors (DuplicateNormalizedRoutePathError firstPath secondPath) =
  [ "Route names "
      <> pack (decodePath firstPath)
      <> " and "
      <> pack (decodePath secondPath)
      <> " normalize to the same path.  Use one canonical route name."
  ]
formatErrors (RepositoryIdError message) = [message]


mapManifest :: Manifest' -> Either Error Manifest
mapManifest manifest' =
  case repositoryIdResult of
    Left e -> Left e
    Right repositoryId' -> case monikersResult of
      Left e -> Left e
      Right monikers' ->
        case mapFileRouteMap monikers' manifest'.dirs manifest'.files of
          Left e -> Left e
          Right fileRoutes' ->
            case mapHooks monikers' manifest'.hooks of
              Left e -> Left e
              Right hooks' ->
                Right $
                  Manifest repositoryId' monikers' fileRoutes' ignores' hooks'
 where
  repositoryIdResult =
    traverse
      (first RepositoryIdError . parseRepositoryId)
      manifest'.repositoryId
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
      Right $
        HashMap.fromList
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
      Right $
        normalizePredicate $
          And [os, arch, kernel, kernelRelease, all', any', pred']
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
    [] -> case duplicateNormalizedRoute of
      Just (firstPath, secondPath) ->
        Left $ DuplicateNormalizedRoutePathError firstPath secondPath
      Nothing ->
        Right $ Map.fromList [(name, route) | (name, Right route) <- results]
 where
  results :: [(OsPath, Either Error FileRoute)]
  results =
    [ ( path
      , mapFileRouteEntry monikerMap path route Directory
      )
    | (name, route) <- toList dirs
    , let path = encodePath name
    ]
      ++ [ (path, mapFileRouteEntry monikerMap path route File)
         | (name, route) <- toList files
         , let path = encodePath name
         ]
  errors :: [Error]
  errors = lefts [r | (_, r) <- results]
  duplicateNormalizedRoute :: Maybe (OsPath, OsPath)
  duplicateNormalizedRoute = case [ distinctPaths
                                  | paths <-
                                      Map.elems $
                                        Map.fromListWith
                                          (<>)
                                          [(normalise routePath, [routePath]) | (routePath, _) <- results]
                                  , let distinctPaths = nub paths
                                  , length distinctPaths > 1
                                  ] of
    (firstPath : secondPath : _) : _ -> Just (firstPath, secondPath)
    _ -> Nothing


mapFileRouteEntry
  :: MonikerMap
  -> OsPath
  -> FileRoute'
  -> FileType
  -> Either Error FileRoute
mapFileRouteEntry monikerMap routePath route fileType
  | isAbsolute routePath
      || isWindowsRootedOrDriveQualified (decodePath routePath)
      || parentPath `elem` splitDirectories routePath =
      Left $ InvalidRoutePathError fileType routePath
  | otherwise = mapFileRoute monikerMap routePath route fileType
 where
  parentPath = encodePath ".."


isWindowsRootedOrDriveQualified :: FilePath -> Bool
isWindowsRootedOrDriveQualified route =
  Windows.hasDrive route
    || case route of
      initial : _ -> Windows.isPathSeparator initial
      [] -> False


encodePath :: String -> OsPath
encodePath = unsafePerformIO . encodeFS


decodePath :: OsPath -> FilePath
decodePath = unsafePerformIO . decodeFS


mapFileRoute
  :: MonikerMap
  -> OsPath
  -> FileRoute'
  -> FileType
  -> Either Error FileRoute
mapFileRoute monikerMap _ (CompactFileRoute route) fileType =
  case errors of
    e : _ -> Left $ FilePathExpressionError e
    _ ->
      Right $
        fileRoute
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
    | (name, expr) <- toList route
    ]
  errors :: [ParseErrorBundle Text Void]
  errors = lefts [r | (_, Just r) <- results]
mapFileRoute monikerMap routePath (DetailedFileRoute branches) fileType =
  fileRoutePreservingOrder (`HashMap.lookup` monikerMap)
    <$> traverse (uncurry mapBranch) (zip [0 ..] branches)
    <*> pure fileType
 where
  mapBranch
    :: Int
    -> FileRouteBranch'
    -> Either Error (EnvironmentPredicate, Maybe FilePathExpression)
  mapBranch index branch
    | not $ null branch.routeUnexpectedFields =
        Left $
          FileRouteBranchError
            fileType
            routePath
            index
            (UnexpectedRouteFields branch.routeUnexpectedFields)
    | otherwise = do
        predicate <- case (branch.routeMoniker, branch.routeCondition) of
          (Nothing, Nothing) -> branchError index MissingRouteCondition
          (Just _, Just _) -> branchError index ConflictingRouteConditions
          (Just name, Nothing)
            | HashMap.member name monikerMap -> Right $ Moniker name
            | otherwise -> branchError index $ UnknownRouteMoniker name
          (Nothing, Just condition) ->
            first
              EnvironmentPredicateError
              ( normalizePredicate
                  <$> parseEnvironmentPredicate
                    (branchSource index "when")
                    condition
              )
        path <- traverse parseDetailedPath branch.routePath
        pure (predicate, path)
   where
    parseDetailedPath "" = Right $ BareComponent ""
    parseDetailedPath expression =
      first FilePathExpressionError $
        parseFilePathExpression (branchSource index "path") expression
  branchError index =
    Left . FileRouteBranchError fileType routePath index
  branchSource index field =
    ( case fileType of
        Directory -> "dirs."
        File -> "files."
        Symlink -> "symlinks."
    )
      <> decodePath routePath
      <> "["
      <> show index
      <> "]."
      <> field


mapHooks :: MonikerMap -> Maybe Internal.Hooks' -> Either Error HookMap
mapHooks _ Nothing = Right Map.empty
mapHooks monikers (Just hooks') =
  case errors of
    e : _ -> Left e
    _ ->
      Right $
        Map.fromList
          [ (hookType, hookList)
          | (hookType, Right hookList) <- results
          , not (null hookList)
          ]
 where
  results :: [(HookType, Either Error [Hook])]
  results =
    [ (PreApply, mapHookList monikers hooks'.preApply)
    , (PreFirstApply, mapHookList monikers hooks'.preFirstApply)
    , (PostFirstApply, mapHookList monikers hooks'.postFirstApply)
    , (PostApply, mapHookList monikers hooks'.postApply)
    ]
  errors :: [Error]
  errors = lefts [r | (_, r) <- results]


mapHookList :: MonikerMap -> Maybe [Internal.Hook'] -> Either Error [Hook]
mapHookList _ Nothing = Right []
mapHookList monikers (Just hooks') =
  case errors of
    e : _ -> Left e
    _ -> Right [hook | Right hook <- results]
 where
  results :: [Either Error Hook]
  results = mapHook monikers <$> hooks'
  errors :: [Error]
  errors = lefts results


mapHook :: MonikerMap -> Internal.Hook' -> Either Error Hook
mapHook monikers hook' =
  case conditionResult of
    Left e -> Left e
    Right cond ->
      Right
        Hook
          { command = encodePath $ unpack hook'.command
          , args = maybe [] id hook'.args
          , condition = cond
          , workingDirectory = encodePath <$> hook'.workingDirectory
          , ignoreFailure = maybe False id hook'.ignoreFailure
          }
 where
  conditionResult :: Either Error EnvironmentPredicate
  conditionResult = case hook'.moniker of
    Just monikerName ->
      -- If moniker is specified, it takes precedence
      case HashMap.lookup monikerName monikers of
        Just pred' -> Right pred'
        Nothing -> Right $ Moniker monikerName
    Nothing ->
      -- Otherwise, parse the condition (when) field
      case hook'.condition of
        Nothing -> Right Always
        Just condText ->
          first EnvironmentPredicateError $
            parseEnvironmentPredicate "" condText
