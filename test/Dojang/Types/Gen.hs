{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dojang.Types.Gen
  ( arbitraryManifest
  , architecture
  , ciText
  , fileRoute
  , fileRoute'
  , fileRouteMap
  , emptyMonikerNameText
  , environment
  , environmentPredicate
  , environmentVariable
  , factKey
  , factValue
  , filePathExpression
  , hook
  , hookMap
  , invalidMonikerNameText
  , kernel
  , manifest
  , manifest'
  , manifestVariableMap
  , manifestVariableName
  , monikerMap
  , monikerMap'
  , monikerName
  , monikerNameText
  , monikerNameTextHavingInvalidCharacterWithIndex
  , monikerNameTextStartingWithNonLetter
  , monikerNameWithCIText
  , monikerNameError
  , operatingSystem
  , osPath
  , osString
  , specificity
  ) where

import Data.Char (isAlpha, isAlphaNum, isAscii, isControl, toUpper)
import Data.List (nub)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String (IsString (fromString))
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (lookup)

import Data.CaseInsensitive (CI, mk)
import Data.HashMap.Strict (HashMap, fromList, keys, lookup)
import Data.Text (Text, cons, length, pack)
import Data.Text qualified as Text
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (Range, constant, constantFrom, singleton)
import System.OsPath (OsPath, encodeFS, joinPath)
import System.OsString (OsString)
import Test.Hspec.Hedgehog (MonadGen)

import Data.Map.Strict qualified as Map
import Dojang.MonadFileSystem (FileType (..))
import Dojang.Types.Environment
  ( Architecture (..)
  , Environment
  , FactKey
  , FactValue
  , Kernel (..)
  , OperatingSystem (..)
  , emptyEnvironment
  , parseFactKey
  )
import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (..)
  , normalizePredicate
  )
import Dojang.Types.EnvironmentPredicate.Specificity (Specificity (..))
import Dojang.Types.FilePathExpression
  ( EnvironmentVariable
  , FilePathExpression (..)
  )
import Dojang.Types.FileRoute qualified as FileRoute
import Dojang.Types.FileRouteMap (FileRouteMap)
import Dojang.Types.Hook
  ( Hook (..)
  , HookMap
  , HookPolicy (..)
  , allHookTypes
  , parseHookId
  , renderHookId
  )
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.ManifestVariable
  ( ManifestVariableMap
  , ManifestVariableName
  , manifestVariable
  , manifestVariablePreservingOrder
  , parseManifestVariableName
  )
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName
  ( MonikerName
  , MonikerNameError (..)
  , parseMonikerName
  )
import Dojang.Types.RepositoryId (RepositoryId, parseRepositoryId)


ciText :: (MonadGen m) => Range Int -> m Char -> m (CI Text)
ciText range char = mk <$> Gen.text range char


monikerNameText :: (MonadGen m) => m Text
monikerNameText = do
  firstChar <- Gen.text (constant 1 1) Gen.alpha
  rest <- Gen.text (constantFrom 0 0 256) Gen.alphaNum
  return $ firstChar <> rest


monikerNameWithCIText :: (MonadGen m) => m (MonikerName, CI Text)
monikerNameWithCIText = do
  name <- monikerNameText
  let Right monikerName' = parseMonikerName name
  return (monikerName', mk name)


monikerName :: (MonadGen m) => m MonikerName
monikerName = fst <$> monikerNameWithCIText


invalidMonikerNameText :: (MonadGen m) => m Text
invalidMonikerNameText =
  Gen.choice
    [ emptyMonikerNameText
    , monikerNameTextStartingWithNonLetter
    , fst <$> monikerNameTextHavingInvalidCharacterWithIndex
    ]


emptyMonikerNameText :: (MonadGen m) => m Text
emptyMonikerNameText = Gen.text (constant 0 0) Gen.unicodeAll


monikerNameTextStartingWithNonLetter :: (MonadGen m) => m Text
monikerNameTextStartingWithNonLetter = do
  firstChar <- Gen.text (constant 1 1) Gen.digit
  rest <- Gen.text (constantFrom 0 0 256) Gen.unicodeAll
  return $ firstChar <> rest


monikerNameTextHavingInvalidCharacterWithIndex :: (MonadGen m) => m (Text, Int)
monikerNameTextHavingInvalidCharacterWithIndex = do
  a <- Gen.text (constantFrom 1 1 256) Gen.alpha
  b <-
    Gen.text (constant 1 1) $
      Gen.filterT (\c -> c /= '-' && c /= '_' && not (isAlphaNum c)) Gen.unicodeAll
  c <- Gen.text (constantFrom 0 0 256) Gen.alphaNum
  return (a <> b <> c, Data.Text.length a)


monikerNameError :: (MonadGen m) => m MonikerNameError
monikerNameError =
  Gen.choice
    [ return Empty
    , return StartingWithNonLetter
    , HavingInvalidCharacter <$> Gen.integral (constantFrom 0 0 256)
    ]


operatingSystem :: (MonadGen m) => m OperatingSystem
operatingSystem =
  Gen.choice
    [ return Android
    , return FreeBSD
    , return Linux
    , return MacOS
    , return NetBSD
    , return OpenBSD
    , return Windows
    , OtherOS <$> ciText (constant 0 256) Gen.unicodeAll
    ]


architecture :: (MonadGen m) => m Architecture
architecture =
  Gen.choice
    [ return AArch64
    , return X86
    , return X86_64
    , Etc <$> ciText (constant 0 256) Gen.unicodeAll
    ]


kernelName :: (MonadGen m) => m (CI Text)
kernelName = ciText (constant 1 50) Gen.unicodeAll


kernelRelease :: (MonadGen m) => m (CI Text)
kernelRelease = ciText (constant 1 50) Gen.unicodeAll


kernel :: (MonadGen m) => m Kernel
kernel = Kernel <$> kernelName <*> kernelRelease


environment :: (MonadGen m) => m Environment
environment = emptyEnvironment <$> operatingSystem <*> architecture <*> kernel


factKey :: (MonadGen m) => m FactKey
factKey = do
  key <- monikerNameText
  case parseFactKey key of
    Left _ -> factKey
    Right key' -> return key'


factValue :: (MonadGen m) => m FactValue
factValue = fromString . Text.unpack <$> Gen.text (constant 0 50) Gen.unicodeAll


environmentPredicate :: (MonadGen m) => m EnvironmentPredicate
environmentPredicate = environmentPredicate' 5


environmentPredicate' :: (MonadGen m) => Int -> m EnvironmentPredicate
environmentPredicate' maxDepth =
  Gen.choice $
    [ return Always
    , Moniker <$> monikerName
    , OperatingSystem <$> operatingSystem
    , Architecture <$> architecture
    , KernelName <$> kernelName
    , KernelRelease <$> kernelRelease
    , KernelReleasePrefix <$> (ciText (constant 0 50) Gen.unicodeAll)
    , KernelReleaseSuffix <$> (ciText (constant 0 50) Gen.unicodeAll)
    , Fact <$> factKey <*> factValue
    , FactDefined <$> factKey
    ]
      ++ if maxDepth < 1
        then []
        else
          [ Not <$> environmentPredicate' nextMaxDepth
          , And
              <$> Gen.nonEmpty
                (constant 0 nextMaxDepth)
                (environmentPredicate' nextMaxDepth)
          , Or
              <$> Gen.nonEmpty
                (constant 0 nextMaxDepth)
                (environmentPredicate' nextMaxDepth)
          ]
 where
  nextMaxDepth :: Int
  nextMaxDepth = maxDepth - 1


specificity :: (MonadGen m) => m Specificity
specificity =
  Specificity
    <$> Gen.integral (constantFrom 0 0 8)
    <*> Gen.integral (constantFrom 0 0 8)


osChar :: (MonadGen m) => m Char
osChar =
  Gen.filterT (not . isControl) $
    Gen.frequency [(5, Gen.alphaNum), (3, Gen.latin1), (2, Gen.unicode)]


environmentVariable :: (MonadGen m) => m EnvironmentVariable
environmentVariable =
  Gen.frequency
    [ (8, typicalEnvVar)
    , (2, Gen.text (constantFrom 1 1 256) osChar)
    ]
 where
  typicalEnvVar :: (MonadGen m) => m EnvironmentVariable
  typicalEnvVar = do
    firstChar <- Gen.upper
    rest <-
      Gen.text
        (constantFrom 1 1 50)
        $ Gen.frequency [(8, Gen.upper), (1, Gen.digit), (1, return '_')]
    return $ cons firstChar rest


filePathExpression :: (MonadGen m) => m FilePathExpression
filePathExpression = filePathExpression' 5


filePathExpression' :: (MonadGen m) => Int -> m FilePathExpression
filePathExpression' maxDepth =
  Gen.choice $
    [ bareComponent
    , root
    , substitution
    ]
      ++ if maxDepth > 0
        then
          [ concatenation
          , pathSeparator
          , substitutionWithDefault
          , conditionalSubstitution
          ]
        else []
 where
  filePathExpr :: (MonadGen m) => m FilePathExpression
  filePathExpr = filePathExpression' (maxDepth - 1)
  bareComponent :: (MonadGen m) => m FilePathExpression
  bareComponent =
    BareComponent
      <$> Gen.text
        (constantFrom 1 1 256)
        ( Gen.filterT
            (`notElem` ['/', '\\', '{', '}', ':', '\r', '\n'])
            osChar
        )
  root :: (MonadGen m) => m FilePathExpression
  root = Root . fmap toUpper <$> Gen.maybe Gen.alpha
  concatenation :: (MonadGen m) => m FilePathExpression
  concatenation =
    Gen.filterT
      ( \case
          Concatenation (Concatenation _ _) _ -> False
          Concatenation _ (Concatenation _ _) -> False
          Concatenation (PathSeparator _ _) _ -> False
          Concatenation _ (PathSeparator _ _) -> False
          c@(Concatenation (BareComponent _) b) ->
            not (startsWithBareComponent b) && not (containsRoot c)
          c -> not $ containsRoot c
      )
      $ Concatenation
        <$> filePathExpr
        <*> filePathExpr
  pathSeparator :: (MonadGen m) => m FilePathExpression
  pathSeparator =
    PathSeparator
      <$> filePathExpr
      <*> Gen.filterT
        (\case PathSeparator _ _ -> False; e -> not $ containsRoot e)
        filePathExpr
  envVar :: (MonadGen m) => m EnvironmentVariable
  envVar = do
    first <- Gen.filterT (\c -> isAscii c && isAlpha c || c == '_') osChar
    rest <-
      Gen.text (constantFrom 0 0 256) $
        Gen.filterT (\c -> isAscii c && isAlphaNum c || c == '_') osChar
    return $ first `cons` rest
  substitution :: (MonadGen m) => m FilePathExpression
  substitution = Substitution <$> envVar
  substitutionWithDefault :: (MonadGen m) => m FilePathExpression
  substitutionWithDefault =
    SubstitutionWithDefault
      <$> envVar
      <*> Gen.filterT (\case Root _ -> False; _ -> True) filePathExpr
  conditionalSubstitution :: (MonadGen m) => m FilePathExpression
  conditionalSubstitution =
    ConditionalSubstitution
      <$> envVar
      <*> Gen.filterT (\case Root _ -> False; _ -> True) filePathExpr
  startsWithBareComponent :: FilePathExpression -> Bool
  startsWithBareComponent (BareComponent _) = True
  startsWithBareComponent (Concatenation a _) = startsWithBareComponent a
  startsWithBareComponent (PathSeparator a _) = startsWithBareComponent a
  startsWithBareComponent _ = False
  containsRoot :: FilePathExpression -> Bool
  containsRoot (Root _) = True
  containsRoot (Concatenation a b) = containsRoot a || containsRoot b
  containsRoot (PathSeparator a b) = containsRoot a || containsRoot b
  containsRoot (SubstitutionWithDefault _ expr) = containsRoot expr
  containsRoot (ConditionalSubstitution _ expr) = containsRoot expr
  containsRoot _ = False


monikerMap :: (MonadGen m) => m (HashMap MonikerName EnvironmentPredicate)
monikerMap = monikerMap' (constantFrom 0 0 10)


monikerMap'
  :: (MonadGen m) => Range Int -> m (HashMap MonikerName EnvironmentPredicate)
monikerMap' sizeRange = do
  keys' <- Gen.list sizeRange monikerName
  let cardinality = Prelude.length keys'
  values <-
    Gen.list (constant cardinality cardinality) $
      normalizePredicate
        <$> environmentPredicate
  return $ fromList $ zip keys' values


fileRoute :: (MonadGen m) => m FileRoute.FileRoute
fileRoute = do
  mm <- monikerMap' (constant 0 5)
  fileRoute' (constant 0 5) mm environmentPredicate


fileRoute'
  :: (MonadGen m)
  => Range Int
  -> MonikerMap
  -> m EnvironmentPredicate
  -> m FileRoute.FileRoute
fileRoute' predicatesNumberRange mm predGen = do
  predicates <-
    nub . map normalizePredicate <$> Gen.list predicatesNumberRange predGen
  let cardinality = Prelude.length predicates
  fileOrDir <- Gen.element [File, Directory]
  paths <-
    Gen.list (constant cardinality cardinality) $
      Gen.maybe $
        routeTarget fileOrDir filePathExpression
  return $ FileRoute.fileRoute' (`lookup` mm) (predicates `zip` paths) fileOrDir


-- | Generates a 'FileRoute.RouteTarget' whose metadata is valid for
-- the given 'FileType': symlink targets carry no mode, and directory
-- targets never carry executable modes.
routeTarget
  :: (MonadGen m)
  => FileType
  -> m FilePathExpression
  -> m FileRoute.RouteTarget
routeTarget fileOrDir exprGen = do
  expr <- exprGen
  kind <-
    Gen.frequency
      [ (4, pure FileRoute.CopyRoute)
      , (1, pure FileRoute.SymlinkRoute)
      ]
  mode <- case kind of
    FileRoute.SymlinkRoute -> pure FileRoute.DefaultMode
    FileRoute.CopyRoute ->
      Gen.frequency $
        (5, pure FileRoute.DefaultMode)
          : [ (1, pure mode')
            | mode' <- case fileOrDir of
                Directory -> [FileRoute.Private, FileRoute.ReadOnly]
                _ ->
                  [ FileRoute.Private
                  , FileRoute.Executable
                  , FileRoute.PrivateExecutable
                  , FileRoute.ReadOnly
                  ]
            ]
  return $ FileRoute.RouteTarget expr mode kind


osString :: (MonadGen m) => Range Int -> m Char -> m OsString
osString range = fmap (unsafePerformIO . encodeFS) . Gen.string range


osPath :: (MonadGen m) => Range Int -> m OsPath
osPath range =
  joinPath
    <$> Gen.list
      range
      ( osString
          (constant 1 100)
          ( Gen.filterT
              (\c -> c `notElem` "/\\:" && not (isControl c))
              Gen.unicode
          )
      )


fileRouteMap :: (MonadGen m) => Range Int -> MonikerMap -> m FileRouteMap
fileRouteMap range monikers =
  Gen.map range $ do
    key <- osPath (singleton 1)
    value <- representableRoute
    return (key, value)
 where
  monikerPredicates :: [EnvironmentPredicate]
  monikerPredicates =
    fmap Moniker $ case keys monikers of
      [] ->
        let Right undefined' = parseMonikerName $ pack "undefined"
        in [undefined']
      names -> names
  representableRoute :: (MonadGen m) => m FileRoute.FileRoute
  representableRoute =
    fileRoute' (constant 0 5) monikers $ Gen.element monikerPredicates


arbitraryFileRouteMap
  :: (MonadGen m) => Range Int -> MonikerMap -> m FileRouteMap
arbitraryFileRouteMap range monikers =
  Gen.map range $ do
    key <- osPath (singleton 1)
    predicates <- Gen.list (constant 0 5) environmentPredicate
    let cardinality = Prelude.length predicates
    fileOrDir <- Gen.element [File, Directory]
    paths <-
      Gen.list (constant cardinality cardinality) $
        Gen.maybe $
          routeTarget fileOrDir arbitraryFilePathExpression
    let value =
          FileRoute.fileRoute'
            (`lookup` monikers)
            (predicates `zip` paths)
            fileOrDir
    return (key, value)
 where
  arbitraryFilePathExpression =
    Gen.frequency
      [ (1, pure $ BareComponent $ pack "")
      , (9, filePathExpression)
      ]


ignoreMap' :: (MonadGen m) => Range Int -> m (Map.Map OsPath [String])
ignoreMap' range = Gen.map range $ do
  path <- osPath (singleton 1)
  patterns <- Gen.list (constantFrom 1 1 5) $ Gen.string (constant 0 20) osChar
  pure (path, patterns)


ignoreMap :: (MonadGen m) => m (Map.Map OsPath [String])
ignoreMap = ignoreMap' (constantFrom 0 0 5)


manifestVariableName :: (MonadGen m) => m ManifestVariableName
manifestVariableName = do
  first <- Gen.element $ '_' : ['A' .. 'Z'] ++ ['a' .. 'z']
  rest <-
    Gen.text
      (constantFrom 0 0 32)
      (Gen.element $ '_' : ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'])
  let Right name = parseManifestVariableName $ cons first rest
  return name


manifestVariableMap
  :: (MonadGen m) => Range Int -> MonikerMap -> m ManifestVariableMap
manifestVariableMap range monikers = Gen.map range $ do
  name <- manifestVariableName
  variable <- Gen.frequency [(2, compact), (8, detailed)]
  return (name, variable)
 where
  value =
    Gen.frequency
      [ (1, pure $ BareComponent $ pack "")
      , (9, filePathExpression)
      ]
  compact = manifestVariable monikers <$> value
  detailed = do
    count <- Gen.integral $ constantFrom 1 1 4
    predicates <-
      Gen.list (singleton count) $ normalizePredicate <$> environmentPredicate' 2
    values <- Gen.list (singleton count) value
    return $
      manifestVariablePreservingOrder
        (`lookup` monikers)
        (NonEmpty.fromList $ zip predicates values)


manifest' :: forall m. (MonadGen m) => Range Int -> Range Int -> m Manifest
manifest' monikerMapRange fileRouteMapRange = do
  repositoryId <- Gen.maybe repositoryIdGen
  monikers <- monikerMap' monikerMapRange
  variables <- manifestVariableMap (constantFrom 0 0 5) monikers
  fileRoutes <- fileRouteMap fileRouteMapRange monikers
  ignorePatterns <- ignoreMap
  hooks <- hookMap (constant 0 3)
  return $
    Manifest repositoryId monikers variables fileRoutes ignorePatterns hooks


manifest :: (MonadGen m) => m Manifest
manifest = manifest' (constantFrom 0 0 5) (constantFrom 0 0 5)


arbitraryManifest :: (MonadGen m) => m Manifest
arbitraryManifest = do
  repositoryId <- Gen.maybe repositoryIdGen
  monikers <- monikerMap' (constantFrom 0 0 5)
  variables <- manifestVariableMap (constantFrom 0 0 5) monikers
  fileRoutes <- arbitraryFileRouteMap (constantFrom 0 0 5) monikers
  ignorePatterns <- ignoreMap' (constantFrom 1 1 5)
  hooks <- hookMap (constant 0 3)
  return $
    Manifest repositoryId monikers variables fileRoutes ignorePatterns hooks


repositoryIdGen :: (MonadGen m) => m RepositoryId
repositoryIdGen = do
  groups <-
    traverse
      (\length' -> Gen.text (constant length' length') Gen.hexit)
      [8, 4, 4, 4, 12]
  let Right repositoryId =
        parseRepositoryId $ Text.intercalate (Text.singleton '-') groups
  return repositoryId


hook :: (MonadGen m) => m Hook
hook = do
  policy' <- Gen.element [HookAlways, HookOnce, HookOnChange]
  identifierText <- Gen.text (constant 1 16) Gen.alphaNum
  let Right identifier = parseHookId $ cons 'h' identifierText
      stateful = policy' /= HookAlways
  cmd <- osPath (constant 1 3)
  argsCount <- Gen.integral (constant 0 5)
  args' <- Gen.list (singleton argsCount) $ Gen.text (constant 1 20) Gen.alphaNum
  cond <- normalizePredicate <$> environmentPredicate' 2
  workDir <- Gen.maybe filePathExpression
  ignoreFail <- Gen.bool
  return
    Hook
      { hookId = if stateful then Just identifier else Nothing
      , policy = policy'
      , changeKey =
          if policy' == HookOnChange then Just identifierText else Nothing
      , command = cmd
      , args = args'
      , condition = cond
      , workingDirectory = workDir
      , ignoreFailure = ignoreFail
      }


hookMap :: (MonadGen m) => Range Int -> m HookMap
hookMap range = do
  hookTypes <- Gen.subsequence allHookTypes
  pairs <- mapM generateHooks hookTypes
  return $ Map.fromList [(ht, hs) | (ht, hs) <- pairs, not (null hs)]
 where
  generateHooks hookType = do
    hooks <- Gen.list range hook
    return (hookType, uniqueStatefulIds hooks)
  uniqueStatefulIds = zipWith assignIdentifier [0 :: Int ..]
  assignIdentifier
    index
    hook'@(Hook currentIdentifier policy' key cmd args' cond workDir ignoreFail)
      | policy' == HookAlways = hook'
      | otherwise =
          let base = maybe (pack "hook") renderHookId currentIdentifier
              Right identifier =
                parseHookId $ base <> Text.singleton '-' <> pack (show index)
          in Hook
               (Just identifier)
               policy'
               key
               cmd
               args'
               cond
               workDir
               ignoreFail
