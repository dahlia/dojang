{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dojang.Types.Gen
  ( architecture
  , ciText
  , fileRoute
  , fileRoute'
  , fileRouteMap
  , emptyMonikerNameText
  , environment
  , environmentPredicate
  , environmentVariable
  , filePathExpression
  , invalidMonikerNameText
  , kernel
  , manifest
  , manifest'
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
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (lookup)

import Data.CaseInsensitive (CI, mk)
import Data.HashMap.Strict (HashMap, fromList, keys, lookup)
import Data.Map.Strict (empty)
import Data.Text (Text, cons, length, pack)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (Range, constant, constantFrom, singleton)
import System.OsPath (OsPath, joinPath)
import System.OsString (OsString, encodeFS)
import Test.Hspec.Hedgehog (MonadGen)

import Dojang.MonadFileSystem (FileType (..))
import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (Environment)
  , Kernel (..)
  , OperatingSystem (..)
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
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName
  ( MonikerName
  , MonikerNameError (..)
  , parseMonikerName
  )


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
    Gen.text (constant 1 1)
      $ Gen.filterT (\c -> c /= '-' && c /= '_' && not (isAlphaNum c)) Gen.unicodeAll
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
environment = Environment <$> operatingSystem <*> architecture <*> kernel


environmentPredicate :: (MonadGen m) => m EnvironmentPredicate
environmentPredicate = environmentPredicate' 5


environmentPredicate' :: (MonadGen m) => Int -> m EnvironmentPredicate
environmentPredicate' maxDepth =
  Gen.choice
    $ [ return Always
      , Moniker <$> monikerName
      , OperatingSystem <$> operatingSystem
      , Architecture <$> architecture
      , KernelName <$> kernelName
      , KernelRelease <$> kernelRelease
      , KernelReleasePrefix <$> (ciText (constant 0 50) Gen.unicodeAll)
      , KernelReleaseSuffix <$> (ciText (constant 0 50) Gen.unicodeAll)
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
  Gen.filterT (not . isControl)
    $ Gen.frequency [(5, Gen.alphaNum), (3, Gen.latin1), (2, Gen.unicode)]


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
  Gen.choice
    $ [ bareComponent
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
            (`notElem` ['/', '\\', '$', '{', '}', ':', '\r', '\n'])
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
      Gen.text (constantFrom 0 0 256)
        $ Gen.filterT (\c -> isAscii c && isAlphaNum c || c == '_') osChar
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
    Gen.list (constant cardinality cardinality)
      $ normalizePredicate
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
  paths <-
    Gen.list (constant cardinality cardinality) $ Gen.maybe filePathExpression
  fileOrDir <- Gen.element [File, Directory]
  return $ FileRoute.fileRoute' (`lookup` mm) (predicates `zip` paths) fileOrDir


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
    value <-
      fileRoute' (constant 0 5) monikers
        $ Gen.choice
        $ map return
        $ fmap Moniker
        $ case keys monikers of
          [] ->
            let Right undefined' = parseMonikerName $ pack "undefined"
            in [undefined']
          names -> names
    return (key, value)


manifest' :: forall m. (MonadGen m) => Range Int -> Range Int -> m Manifest
manifest' monikerMapRange fileRouteMapRange = do
  monikers <- monikerMap' monikerMapRange
  fileRoutes <- fileRouteMap fileRouteMapRange monikers
  return $ Manifest monikers fileRoutes Data.Map.Strict.empty


manifest :: (MonadGen m) => m Manifest
manifest = manifest' (constantFrom 0 0 5) (constantFrom 0 0 5)
