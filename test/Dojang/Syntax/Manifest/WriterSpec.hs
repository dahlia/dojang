{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.Manifest.WriterSpec (spec) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
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
import Dojang.Syntax.Manifest.Parser (formatErrors, readManifest)
import Dojang.Syntax.Manifest.Writer (insertRepositoryId, writeManifest)
import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (Always, Architecture, Moniker, OperatingSystem, Or)
  , normalizePredicate
  )
import Dojang.Types.FilePathExpression (FilePathExpression (BareComponent))
import Dojang.Types.FileRoute
  ( FileRoute (FileRoute)
  , fileRoute'
  , fileRoutePreservingOrder
  )
import Dojang.Types.Gen as Gen
  ( arbitraryManifest
  , architecture
  , manifest
  , operatingSystem
  )
import Dojang.Types.Manifest (Manifest (Manifest))
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.RepositoryId
  ( parseRepositoryId
  )


spec :: Spec
spec = do
  path <- runIO $ encodeFS "foo"
  let Right alpha = parseMonikerName "alpha"
      Right zeta = parseMonikerName "zeta"
      linux = OperatingSystem "linux"

  specify "writes a repository identity before manifest tables" $ do
    let Right repositoryId =
          parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        manifest' = Manifest (Just repositoryId) mempty mempty mempty mempty
    writeManifest manifest'
      `shouldSatisfy` isInfixOf
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\""

  specify "adds an identity without rewriting existing manifest text" $ do
    let Right repositoryId =
          parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        source = "#:schema example.json\n\n# Keep this comment.\n[monikers]\n"
        expected =
          "#:schema example.json\n\n"
            <> "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n\n"
            <> "# Keep this comment.\n[monikers]\n"
    insertRepositoryId repositoryId source `shouldBe` expected
    insertRepositoryId repositoryId expected `shouldBe` expected

  specify "does not mistake a nested identity for the repository identity" $ do
    let Right repositoryId =
          parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        source =
          "[monikers]\n"
            <> "repository-id = { os = \"linux\" }\n"
        expected =
          "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n\n"
            <> source
    insertRepositoryId repositoryId source `shouldBe` expected

  specify "does not mistake multiline string contents for an identity" $ do
    let Right repositoryId =
          parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        source =
          "description = \"\"\"\n"
            <> "repository-id = \"example text\"\n"
            <> "\"\"\"\n\n"
            <> "[monikers]\n"
        expected =
          "repository-id = \"123e4567-e89b-42d3-a456-426614174000\"\n\n"
            <> source
    insertRepositoryId repositoryId source `shouldBe` expected

  specify "round-trips representable manifests" $ hedgehog $ do
    manifest' <- forAll Gen.manifest
    let toml = writeManifest manifest'
    annotate $ unpack toml
    case readManifest toml of
      Left err -> annotateShow (formatErrors err) >> assert False
      Right (parsed, _) -> do
        annotateShow parsed
        parsed === manifest'

  specify "preserves arbitrary manifest route semantics" $ hedgehog $ do
    manifest'@(Manifest repositoryId monikers routes ignores hooks) <-
      forAll Gen.arbitraryManifest
    let toml = writeManifest manifest'
    annotate $ unpack toml
    case readManifest toml of
      Left err -> annotateShow (formatErrors err) >> assert False
      Right
        (parsed@(Manifest repositoryId' monikers' routes' ignores' hooks'), _) -> do
          annotateShow parsed
          let routeShape (FileRoute _ predicates fileType) =
                ( [ (normalizePredicate predicate, destination)
                  | (predicate, destination) <- predicates
                  ]
                , fileType
                )
          repositoryId' === repositoryId
          monikers' === monikers
          Map.map routeShape routes' === Map.map routeShape routes
          ignores' === ignores
          hooks' === hooks

  specify "keeps compact routes when every condition is a unique moniker" $ do
    let monikers = HashMap.singleton alpha linux
        route = fileRoute' (`HashMap.lookup` monikers) [(Moniker alpha, Nothing)] File
        manifest' =
          Manifest Nothing monikers (Map.singleton path route) Map.empty Map.empty
        toml = writeManifest manifest'
    toml `shouldSatisfy` isInfixOf "foo.alpha = \"\""
    toml `shouldSatisfy` isInfixOf "alpha = \"\""

  specify "keeps detailed routes when compacting would change tie order" $
    hedgehog $ do
      os <- forAll Gen.operatingSystem
      let predicate = OperatingSystem os
          monikers = HashMap.fromList [(alpha, predicate), (zeta, predicate)]
          route =
            fileRoutePreservingOrder
              (`HashMap.lookup` monikers)
              [(Moniker alpha, Just "first"), (Moniker zeta, Just "second")]
              File
          manifest' =
            Manifest Nothing monikers (Map.singleton path route) Map.empty Map.empty
          toml = writeManifest manifest'
      annotate $ unpack toml
      assert $ isInfixOf "[[files.foo]]" toml
      let Right (parsed, _) = readManifest toml
      parsed === manifest'

  specify "uses an inline condition instead of looking back to a moniker" $ do
    let monikers = HashMap.singleton alpha linux
        route = fileRoute' (`HashMap.lookup` monikers) [(linux, Nothing)] File
        manifest' =
          Manifest Nothing monikers (Map.singleton path route) Map.empty Map.empty
        toml = writeManifest manifest'
    toml `shouldSatisfy` isInfixOf "[[files.foo]]"
    toml `shouldSatisfy` isInfixOf "when = \"os = linux\""
    let Right (parsed, _) = readManifest toml
    parsed `shouldBe` manifest'

  specify "preserves colliding moniker and inline conditions" $ do
    let monikers = HashMap.singleton alpha linux
        route =
          fileRoute'
            (`HashMap.lookup` monikers)
            [(Moniker alpha, Nothing), (linux, Just "target")]
            File
        manifest' =
          Manifest Nothing monikers (Map.singleton path route) Map.empty Map.empty
        toml = writeManifest manifest'
    toml `shouldSatisfy` isInfixOf "[[files.foo]]"
    toml `shouldSatisfy` isInfixOf "moniker = \"alpha\""
    toml `shouldSatisfy` isInfixOf "when = \"os = linux\""
    let Right (parsed, _) = readManifest toml
    parsed `shouldBe` manifest'

  specify "preserves repeated route conditions" $ hedgehog $ do
    os <- forAll Gen.operatingSystem
    let predicate = OperatingSystem os
        route =
          fileRoute'
            (const Nothing)
            [(predicate, Nothing), (predicate, Just "target")]
            File
        manifest' =
          Manifest Nothing HashMap.empty (Map.singleton path route) Map.empty Map.empty
        toml = writeManifest manifest'
    annotate $ unpack toml
    let Right (parsed, _) = readManifest toml
    parsed === manifest'

  specify "normalizes predicates without changing detailed branch order" $
    hedgehog $ do
      os <- forAll Gen.operatingSystem
      arch <- forAll Gen.architecture
      let reducible = Or [Always, OperatingSystem os]
          route =
            fileRoute'
              (const Nothing)
              [(reducible, Just "first"), (Architecture arch, Just "second")]
              File
          manifest' =
            Manifest
              Nothing
              HashMap.empty
              (Map.singleton path route)
              Map.empty
              Map.empty
          toml = writeManifest manifest'
      annotate $ unpack toml
      let Right (Manifest _ _ parsedRoutes _ _, _) = readManifest toml
          Just (FileRoute _ parsedPredicates _) = Map.lookup path parsedRoutes
      parsedPredicates
        === [ (Always, Just "first")
            , (Architecture arch, Just "second")
            ]

  specify "preserves explicitly constructed dispatch order" $ hedgehog $ do
    os <- forAll Gen.operatingSystem
    let route =
          FileRoute
            (const Nothing)
            [ (Always, Just "general")
            , (OperatingSystem os, Just "specific")
            ]
            File
        manifest' =
          Manifest Nothing HashMap.empty (Map.singleton path route) Map.empty Map.empty
        toml = writeManifest manifest'
    annotate $ unpack toml
    let Right (parsed, _) = readManifest toml
    parsed === manifest'

  specify "distinguishes empty destinations from null routes" $ do
    let route =
          fileRoutePreservingOrder
            (const Nothing)
            [ (Always, Just $ BareComponent "")
            , (linux, Nothing)
            ]
            File
        manifest' =
          Manifest Nothing HashMap.empty (Map.singleton path route) Map.empty Map.empty
        toml = writeManifest manifest'
    toml `shouldSatisfy` isInfixOf "path = \"\""
    let Right (parsed, _) = readManifest toml
    parsed `shouldBe` manifest'

  specify "writes equivalent moniker maps deterministically" $ do
    let first = HashMap.fromList [(zeta, linux), (alpha, linux)]
        second = HashMap.fromList [(alpha, linux), (zeta, linux)]
        route monikers =
          fileRoute' (`HashMap.lookup` monikers) [(linux, Nothing)] File
        manifestFor monikers =
          Manifest
            Nothing
            monikers
            (Map.singleton path $ route monikers)
            Map.empty
            Map.empty
    writeManifest (manifestFor first) `shouldBe` writeManifest (manifestFor second)
