{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.Manifest.WriterSpec (spec) where

import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Text (Text, isInfixOf, unpack)
import qualified Hedgehog.Gen as Hedgehog
import qualified Hedgehog.Range as Range
import System.OsPath (encodeFS, (</>))
import Test.Hspec
  ( Spec
  , anyIOException
  , runIO
  , shouldReturn
  , shouldThrow
  , specify
  )
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
import qualified Dojang.MonadFileSystem as FileSystem
import Dojang.Syntax.Manifest.Parser (formatErrors, readManifest)
import Dojang.Syntax.Manifest.Writer
  ( WriteError
      ( DuplicateStatefulHookId
      , InvalidHookConfiguration
      , UnrepresentableVariableMoniker
      )
  , insertRepositoryId
  , writeManifest
  , writeManifestFile
  )
import Dojang.TestUtils (withTempDir)
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
import Dojang.Types.Hook
  ( Hook (..)
  , HookPolicy (..)
  , HookType (PreApply)
  , parseHookId
  , renderHookId
  )
import Dojang.Types.Manifest (Manifest (Manifest))
import Dojang.Types.ManifestVariable
  ( manifestVariablePreservingOrder
  , parseManifestVariableName
  )
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.RepositoryId
  ( parseRepositoryId
  )


writeValidManifest :: Manifest -> Text
writeValidManifest manifest' = case writeManifest manifest' of
  Left err -> error $ show err
  Right source -> source


spec :: Spec
spec = do
  path <- runIO $ encodeFS "foo"
  let Right alpha = parseMonikerName "alpha"
      Right zeta = parseMonikerName "zeta"
      linux = OperatingSystem "linux"

  specify "writes a repository identity before manifest tables" $ do
    let Right repositoryId =
          parseRepositoryId "123e4567-e89b-42d3-a456-426614174000"
        manifest' = Manifest (Just repositoryId) mempty mempty mempty mempty mempty
    writeValidManifest manifest'
      `shouldSatisfy` isInfixOf
        "repository-id = \"123e4567-e89b-42d3-a456-426614174000\""

  specify "rejects a variable moniker with a different resolver" $ do
    let Right variableName = parseManifestVariableName "ROOT"
        globalMonikers = HashMap.singleton alpha linux
        variable =
          manifestVariablePreservingOrder
            ( \name ->
                if name == alpha
                  then Just $ OperatingSystem "windows"
                  else Nothing
            )
            ((Moniker alpha, BareComponent "value") :| [])
        manifest' =
          Manifest
            Nothing
            globalMonikers
            (Map.singleton variableName variable)
            Map.empty
            Map.empty
            Map.empty
    writeManifest manifest'
      `shouldBe` Left (UnrepresentableVariableMoniker variableName alpha)

  specify "finds a nested variable moniker resolver mismatch" $ do
    let Right variableName = parseManifestVariableName "ROOT"
        globalMonikers =
          HashMap.fromList
            [(alpha, Moniker zeta), (zeta, OperatingSystem "linux")]
        variable =
          manifestVariablePreservingOrder
            ( \name -> case name of
                _ | name == alpha -> Just $ Moniker zeta
                _ | name == zeta -> Just $ OperatingSystem "windows"
                _ -> Nothing
            )
            ((Moniker alpha, BareComponent "value") :| [])
        manifest' =
          Manifest
            Nothing
            globalMonikers
            (Map.singleton variableName variable)
            Map.empty
            Map.empty
            Map.empty
    writeManifest manifest'
      `shouldBe` Left (UnrepresentableVariableMoniker variableName zeta)

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
    let toml = writeValidManifest manifest'
    annotate $ unpack toml
    case readManifest toml of
      Left err -> annotateShow (formatErrors err) >> assert False
      Right (parsed, _) -> do
        annotateShow parsed
        parsed === manifest'

  specify "rejects arbitrary invalid hook policy configurations" $ hedgehog $ do
    invalidKind <-
      forAll $
        Hedgehog.element
          ( [ "always-change"
            , "once-id"
            , "once-change"
            , "on-change-id"
            , "on-change-key"
            , "on-change-empty-key"
            ]
              :: [Text]
          )
    key <- forAll $ Hedgehog.text (Range.linear 1 40) Hedgehog.alphaNum
    let Right identifier = parseHookId "writer-test"
        (hookIdentifier, policy', changeKey') = case invalidKind of
          "always-change" -> (Nothing, HookAlways, Just key)
          "once-id" -> (Nothing, HookOnce, Nothing)
          "once-change" -> (Just identifier, HookOnce, Just key)
          "on-change-id" -> (Nothing, HookOnChange, Just key)
          "on-change-key" -> (Just identifier, HookOnChange, Nothing)
          _ -> (Just identifier, HookOnChange, Just "")
        invalidHook =
          Hook
            { hookId = hookIdentifier
            , policy = policy'
            , changeKey = changeKey'
            , command = path
            , args = []
            , condition = Always
            , workingDirectory = Nothing
            , ignoreFailure = False
            }
        manifest' =
          Manifest
            Nothing
            HashMap.empty
            Map.empty
            Map.empty
            Map.empty
            (Map.singleton PreApply [invalidHook])
    case writeManifest manifest' of
      Left (InvalidHookConfiguration PreApply 1 _) -> assert True
      result -> annotateShow result >> assert False

  specify "rejects arbitrary duplicate stateful hook IDs" $ hedgehog $ do
    identifierSuffix <-
      forAll $ Hedgehog.text (Range.linear 1 40) Hedgehog.alphaNum
    firstPolicy <-
      forAll $ Hedgehog.element ([HookOnce, HookOnChange] :: [HookPolicy])
    secondPolicy <-
      forAll $ Hedgehog.element ([HookOnce, HookOnChange] :: [HookPolicy])
    let identifierText = "h" <> identifierSuffix
        Right identifier = parseHookId identifierText
        makeHook policy' =
          Hook
            { hookId = Just identifier
            , policy = policy'
            , changeKey =
                if policy' == HookOnChange then Just "revision" else Nothing
            , command = path
            , args = []
            , condition = Always
            , workingDirectory = Nothing
            , ignoreFailure = False
            }
        manifest' =
          Manifest
            Nothing
            HashMap.empty
            Map.empty
            Map.empty
            Map.empty
            ( Map.singleton
                PreApply
                [makeHook firstPolicy, makeHook secondPolicy]
            )
    case writeManifest manifest' of
      Left (DuplicateStatefulHookId PreApply duplicate) ->
        renderHookId duplicate === identifierText
      result -> annotateShow result >> assert False

  specify "does not write an invalid hook configuration" $
    withTempDir $ \tmpDir _ -> do
      outputName <- encodeFS "invalid.toml"
      let invalidHook =
            Hook
              { hookId = Nothing
              , policy = HookOnce
              , changeKey = Nothing
              , command = path
              , args = []
              , condition = Always
              , workingDirectory = Nothing
              , ignoreFailure = False
              }
          manifest' =
            Manifest
              Nothing
              HashMap.empty
              Map.empty
              Map.empty
              Map.empty
              (Map.singleton PreApply [invalidHook])
          outputPath = tmpDir </> outputName
      writeManifestFile manifest' outputPath `shouldThrow` anyIOException
      FileSystem.exists outputPath `shouldReturn` False

  specify "preserves arbitrary manifest route semantics" $ hedgehog $ do
    manifest'@(Manifest repositoryId monikers variables routes ignores hooks) <-
      forAll Gen.arbitraryManifest
    let toml = writeValidManifest manifest'
    annotate $ unpack toml
    case readManifest toml of
      Left err -> annotateShow (formatErrors err) >> assert False
      Right
        ( parsed@(Manifest repositoryId' monikers' variables' routes' ignores' hooks')
          , _
          ) -> do
          annotateShow parsed
          let routeShape (FileRoute _ predicates fileType) =
                ( [ (normalizePredicate predicate, destination)
                  | (predicate, destination) <- predicates
                  ]
                , fileType
                )
          repositoryId' === repositoryId
          monikers' === monikers
          variables' === variables
          Map.map routeShape routes' === Map.map routeShape routes
          ignores' === ignores
          hooks' === hooks

  specify "keeps compact routes when every condition is a unique moniker" $ do
    let monikers = HashMap.singleton alpha linux
        route = fileRoute' (`HashMap.lookup` monikers) [(Moniker alpha, Nothing)] File
        manifest' =
          Manifest
            Nothing
            monikers
            Map.empty
            (Map.singleton path route)
            Map.empty
            Map.empty
        toml = writeValidManifest manifest'
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
            Manifest
              Nothing
              monikers
              Map.empty
              (Map.singleton path route)
              Map.empty
              Map.empty
          toml = writeValidManifest manifest'
      annotate $ unpack toml
      assert $ isInfixOf "[[files.foo]]" toml
      let Right (parsed, _) = readManifest toml
      parsed === manifest'

  specify "uses an inline condition instead of looking back to a moniker" $ do
    let monikers = HashMap.singleton alpha linux
        route = fileRoute' (`HashMap.lookup` monikers) [(linux, Nothing)] File
        manifest' =
          Manifest
            Nothing
            monikers
            Map.empty
            (Map.singleton path route)
            Map.empty
            Map.empty
        toml = writeValidManifest manifest'
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
          Manifest
            Nothing
            monikers
            Map.empty
            (Map.singleton path route)
            Map.empty
            Map.empty
        toml = writeValidManifest manifest'
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
          Manifest
            Nothing
            HashMap.empty
            Map.empty
            (Map.singleton path route)
            Map.empty
            Map.empty
        toml = writeValidManifest manifest'
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
              Map.empty
              (Map.singleton path route)
              Map.empty
              Map.empty
          toml = writeValidManifest manifest'
      annotate $ unpack toml
      let Right (Manifest _ _ _ parsedRoutes _ _, _) = readManifest toml
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
          Manifest
            Nothing
            HashMap.empty
            Map.empty
            (Map.singleton path route)
            Map.empty
            Map.empty
        toml = writeValidManifest manifest'
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
          Manifest
            Nothing
            HashMap.empty
            Map.empty
            (Map.singleton path route)
            Map.empty
            Map.empty
        toml = writeValidManifest manifest'
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
            Map.empty
            (Map.singleton path $ route monikers)
            Map.empty
            Map.empty
    writeValidManifest (manifestFor first)
      `shouldBe` writeValidManifest (manifestFor second)
