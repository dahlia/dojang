{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.Manifest.WriterSpec (spec) where

import qualified Data.HashMap.Strict as HashMap
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (isInfixOf, unpack)
import System.OsPath (encodeFS)
import Test.Hspec (Spec, runIO, specify)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog
  ( annotate
  , annotateShow
  , assert
  , forAll
  , hedgehog
  , (===)
  )

import Dojang.MonadFileSystem (FileType (File))
import Dojang.Syntax.Manifest.Parser (readManifest)
import Dojang.Syntax.Manifest.Writer (WriteError (..), writeManifest)
import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (Moniker, OperatingSystem)
  , normalizePredicate
  )
import Dojang.Types.FileRoute (FileRoute (FileRoute), fileRoute')
import Dojang.Types.Gen as Gen (arbitraryManifest, manifest)
import Dojang.Types.Manifest (Manifest (Manifest))
import Dojang.Types.MonikerName (parseMonikerName)


spec :: Spec
spec = do
  path <- runIO $ encodeFS "foo"
  let Right alpha = parseMonikerName "alpha"
      Right zeta = parseMonikerName "zeta"
      linux = OperatingSystem "linux"

  specify "writeManifest" $ hedgehog $ do
    manifest' <- forAll Gen.manifest
    let Right toml = writeManifest manifest'
    annotate $ unpack toml
    let Right (parsed, _) = readManifest toml
    annotateShow parsed
    parsed === manifest'

  specify "rejects route predicates that cannot be represented" $ do
    let route = fileRoute' (const Nothing) [(linux, Nothing)] File
        manifest' = Manifest HashMap.empty (Map.singleton path route) Map.empty Map.empty
    writeManifest manifest'
      `shouldBe` Left (UnrepresentableRoutePredicate path linux)

  specify "rejects route predicates that map to the same moniker" $ do
    let monikers = HashMap.singleton alpha linux
        route =
          fileRoute'
            (`HashMap.lookup` monikers)
            [(Moniker alpha, Nothing), (linux, Nothing)]
            File
        manifest' = Manifest monikers (Map.singleton path route) Map.empty Map.empty
    writeManifest manifest'
      `shouldBe` Left (DuplicateRouteMoniker path alpha)

  specify "never drops arbitrary route predicates" $ hedgehog $ do
    manifest'@(Manifest monikers routes ignores hooks) <-
      forAll Gen.arbitraryManifest
    case writeManifest manifest' of
      Left (UnrepresentableRoutePredicate routePath predicate) -> do
        let Just (FileRoute _ predicates _) = Map.lookup routePath routes
        assert $
          predicate `elem` (normalizePredicate . fst <$> predicates)
        assert $
          all
            ((/= predicate) . normalizePredicate)
            (HashMap.elems monikers)
      Left (DuplicateRouteMoniker routePath moniker) -> do
        let Just (FileRoute _ predicates _) = Map.lookup routePath routes
            resolve predicate = case normalizePredicate predicate of
              Moniker name -> Just name
              normalizedPredicate ->
                listToMaybe $
                  sort
                    [ name
                    | (name, definition) <- HashMap.toList monikers
                    , normalizePredicate definition == normalizedPredicate
                    ]
            matches =
              [ ()
              | (predicate, _) <- predicates
              , resolve predicate == Just moniker
              ]
        assert $ length matches > 1
      Right toml -> do
        annotate $ unpack toml
        let Right (Manifest monikers' routes' ignores' hooks', _) =
              readManifest toml
            routeShape (FileRoute _ predicates fileType) =
              (sort $ show . snd <$> predicates, fileType)
        monikers' === monikers
        Map.map routeShape routes' === Map.map routeShape routes
        ignores' === ignores
        hooks' === hooks

  specify "selects deterministically between equivalent monikers" $ do
    let monikers = HashMap.fromList [(zeta, linux), (alpha, linux)]
        route = fileRoute' (`HashMap.lookup` monikers) [(linux, Nothing)] File
        manifest' = Manifest monikers (Map.singleton path route) Map.empty Map.empty
        Right toml = writeManifest manifest'
    toml `shouldSatisfy` isInfixOf "alpha = \"\""
