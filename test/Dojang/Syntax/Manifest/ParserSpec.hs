{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.Manifest.ParserSpec (spec) where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text, isInfixOf, unpack)
import Data.Text qualified as Text
import System.OsPath (OsPath, encodeFS)
import Test.Hspec (Expectation, Spec, expectationFailure, runIO, specify)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)

import Dojang.MonadFileSystem (FileType (..))
import Dojang.Syntax.Manifest.Parser
  ( DetailedRouteError (..)
  , Error (..)
  , formatErrors
  , readManifest
  )
import Dojang.Types.Environment (Environment (Environment), Kernel (Kernel))
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.FilePathExpression (toPathText)
import Dojang.Types.FileRoute (FileRoute (..), dispatch)
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.MonikerName (parseMonikerName)


detailedManifest :: Text -> Text
detailedManifest branch =
  Text.unlines
    [ "[dirs]"
    , ""
    , "[files]"
    , "foo = ["
    , "  " <> branch <> ","
    , "]"
    , ""
    , "[ignores]"
    , ""
    , "[monikers.known]"
    , "os = \"linux\""
    ]


expectDetailedRouteError
  :: OsPath -> DetailedRouteError -> Text -> Expectation
expectDetailedRouteError path expected toml = case readManifest toml of
  Left (FileRouteBranchError routeType path' index reason) ->
    (routeType, path', index, reason) `shouldBe` (File, path, 0, expected)
  Left err ->
    expectationFailure $ show $ unpack <$> formatErrors err
  Right _ -> expectationFailure "Expected the detailed route to be rejected."


spec :: Spec
spec = do
  home <- runIO $ encodeFS "home"
  bashrc <- runIO $ encodeFS ".bashrc"
  gitconfig <- runIO $ encodeFS ".gitconfig"
  foo <- runIO $ encodeFS "foo"
  let Right posix = parseMonikerName "posix"

  specify "accepts compact and detailed routes in the same manifest" $ do
    let toml =
          Text.unlines
            [ "[dirs.home]"
            , "posix = \"$HOME\""
            , ""
            , "[[files.\".bashrc\"]]"
            , "moniker = \"posix\""
            , "path = \"$HOME/.bashrc\""
            , ""
            , "[[files.\".bashrc\"]]"
            , "when = \"os = windows\""
            , ""
            , "[ignores]"
            , ""
            , "[monikers.posix]"
            , "os = [\"linux\", \"macos\"]"
            ]
        Right (Manifest _ routes _ _, warnings) = readManifest toml
        Just homeRoute = Map.lookup home routes
        Just bashrcRoute = Map.lookup bashrc routes
        routeShape :: FileRoute -> [(String, Maybe Text)]
        routeShape route =
          sort
            [ (show predicate, toPathText <$> path)
            | (predicate, path) <- route.predicates
            ]
    warnings `shouldBe` ([] :: [Text])
    homeRoute.fileType `shouldBe` Directory
    routeShape homeRoute
      `shouldBe` [(show $ Moniker posix, Just "$HOME")]
    bashrcRoute.fileType `shouldBe` File
    routeShape bashrcRoute
      `shouldBe` sort
        [ (show $ Moniker posix, Just "$HOME/.bashrc")
        , (show $ OperatingSystem "windows", Nothing)
        ]

  specify "accepts detailed routes written as inline table arrays" $ do
    let toml =
          Text.unlines
            [ "[dirs]"
            , ""
            , "[files]"
            , "\".gitconfig\" = ["
            , "  { when = \"arch = aarch64\", path = \"$HOME/.gitconfig\" },"
            , "]"
            , ""
            , "[ignores]"
            , ""
            , "[monikers]"
            ]
        Right (Manifest _ routes _ _, _) = readManifest toml
        Just route = Map.lookup gitconfig routes
    [ (show predicate, toPathText <$> path)
      | (predicate, path) <- route.predicates
      ]
      `shouldBe` [(show $ Architecture "aarch64", Just "$HOME/.gitconfig")]

  specify "preserves detailed branch order at equal specificity" $ do
    let toml =
          Text.unlines
            [ "[dirs]"
            , ""
            , "[[files.foo]]"
            , "when = \"arch = 'x86_64'\""
            , "path = \"architecture-first\""
            , ""
            , "[[files.foo]]"
            , "when = \"os = linux\""
            , "path = \"operating-system-second\""
            , ""
            , "[ignores]"
            , ""
            , "[monikers]"
            ]
    case readManifest toml of
      Left err -> expectationFailure $ show $ unpack <$> formatErrors err
      Right (Manifest _ routes _ _, _) -> do
        let Just route = Map.lookup foo routes
        [ (show predicate, toPathText <$> path)
          | (predicate, path) <- route.predicates
          ]
          `shouldBe` [ (show $ Architecture "x86_64", Just "architecture-first")
                     , (show $ OperatingSystem "linux", Just "operating-system-second")
                     ]

  specify "normalizes detailed conditions before routing" $ do
    let toml =
          detailedManifest
            "{ when = \"os = plan9 || always\", path = \"target\" }"
    case readManifest toml of
      Left err -> expectationFailure $ show $ unpack <$> formatErrors err
      Right (Manifest _ routes _ _, _) -> do
        let Just route = Map.lookup foo routes
        route.predicates `shouldBe` [(Always, Just "target")]
        dispatch
          (Environment "linux" "x86_64" $ Kernel "Linux" "6.0")
          route
          `shouldBe` ([Just "target"], [])

  specify "rejects a detailed branch without a condition" $ do
    expectDetailedRouteError
      foo
      MissingRouteCondition
      (detailedManifest "{ path = \"$HOME/foo\" }")

  specify "rejects a detailed branch with both condition fields" $ do
    expectDetailedRouteError
      foo
      ConflictingRouteConditions
      ( detailedManifest
          "{ moniker = \"known\", when = \"os = linux\" }"
      )

  specify "rejects an unknown moniker in a detailed branch" $ do
    let Right missing = parseMonikerName "missing"
    expectDetailedRouteError
      foo
      (UnknownRouteMoniker missing)
      (detailedManifest "{ moniker = \"missing\" }")

  specify "rejects unexpected detailed branch fields" $ do
    expectDetailedRouteError
      foo
      (UnexpectedRouteFields ["bogus"])
      (detailedManifest "{ when = \"always\", bogus = true }")

  specify "identifies invalid detailed branch predicates" $ do
    let result = readManifest $ detailedManifest "{ when = \"os =\" }"
    case result of
      Left err ->
        formatErrors err
          `shouldSatisfy` any (isInfixOf "files.foo[0].when")
      Right _ -> expectationFailure "Expected the predicate to be rejected."

  specify "identifies invalid detailed branch paths" $ do
    let result =
          readManifest $ detailedManifest "{ when = \"always\", path = \"\" }"
    case result of
      Left err ->
        formatErrors err
          `shouldSatisfy` any (isInfixOf "files.foo[0].path")
      Right _ -> expectationFailure "Expected the path to be rejected."
