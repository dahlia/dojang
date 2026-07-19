{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.Manifest.ParserSpec (spec) where

import Control.Monad (forM_)
import Data.List (sort)
import Data.List.NonEmpty qualified as NonEmpty
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
import Dojang.Types.Environment (Kernel (Kernel), emptyEnvironment)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.FilePathExpression
  ( FilePathExpression (BareComponent, PathSeparator, Substitution)
  , toPathText
  )
import Dojang.Types.FileRoute
  ( FileRoute (..)
  , RouteTarget (..)
  , dispatch
  , routeTarget
  )
import Dojang.Types.Hook (Hook (..), HookType (PreApply))
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.ManifestVariable
  ( ManifestVariable (..)
  , parseManifestVariableName
  )
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.RepositoryId
  ( parseRepositoryId
  )


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


isHookConfigurationError :: Error -> Bool
isHookConfigurationError (HookConfigurationError _) = True
isHookConfigurationError _ = False


spec :: Spec
spec = do
  home <- runIO $ encodeFS "home"
  bashrc <- runIO $ encodeFS ".bashrc"
  gitconfig <- runIO $ encodeFS ".gitconfig"
  foo <- runIO $ encodeFS "foo"
  let Right posix = parseMonikerName "posix"

  specify "reads compact and conditional manifest variables" $ do
    let toml =
          Text.unlines
            [ "[vars]"
            , "CONFIG_HOME = \"${XDG_CONFIG_HOME:-$HOME/.config}\""
            , "PROFILE_HOME = ["
            , "  { moniker = \"posix\", value = \"$CONFIG_HOME/posix\" },"
            , "  { when = \"fact.class = personal\", value = \"$HOME/personal\" },"
            , "]"
            , "[dirs]"
            , "[files]"
            , "[ignores]"
            , "[monikers.posix]"
            , "os = \"linux\""
            ]
        Right configHome = parseManifestVariableName "CONFIG_HOME"
        Right profileHome = parseManifestVariableName "PROFILE_HOME"
    case readManifest toml of
      Left err -> expectationFailure $ show $ unpack <$> formatErrors err
      Right (manifest', warnings) -> do
        warnings `shouldBe` []
        fmap
          ( fmap (\(predicate, value) -> (predicate, toPathText value))
              . NonEmpty.toList
              . (.branches)
          )
          manifest'.variables
          `shouldBe` Map.fromList
            [
              ( configHome
              , [(Always, "${XDG_CONFIG_HOME:-$HOME/.config}")]
              )
            ,
              ( profileHome
              ,
                [ (Moniker posix, "$CONFIG_HOME/posix")
                , (Fact "class" "personal", "$HOME/personal")
                ]
              )
            ]

  specify "rejects invalid conditional manifest-variable branches" $ do
    let prefix value =
          Text.unlines
            [ "[vars]"
            , "CONFIG_HOME = " <> value
            , "[dirs]"
            , "[files]"
            , "[ignores]"
            , "[monikers]"
            ]
        invalid =
          [ "[]"
          , "[{ value = \"home\" }]"
          , "[{ moniker = \"missing\", value = \"home\" }]"
          , "[{ moniker = \"known\", when = \"always\", value = \"home\" }]"
          , "[{ when = \"always\" }]"
          , "[{ when = \"always\", value = \"home\", extra = true }]"
          ]
    forM_ invalid $ \value ->
      case readManifest (prefix value) of
        Left _ -> return ()
        Right _ -> expectationFailure $ "Expected rejection: " <> unpack value

  specify "reads a repository identity" $ do
    let repositoryIdText = "123e4567-e89b-42d3-a456-426614174000"
        Right expected = parseRepositoryId repositoryIdText
        toml =
          Text.unlines
            [ "repository-id = \"" <> repositoryIdText <> "\""
            , "[dirs]"
            , "[files]"
            , "[ignores]"
            , "[monikers]"
            ]
    case readManifest toml of
      Left err -> expectationFailure $ show $ unpack <$> formatErrors err
      Right (manifest, _) -> manifest.repositoryId `shouldBe` Just expected

  specify "accepts hooks across command lifecycles" $ do
    let toml =
          Text.unlines
            [ "[dirs]"
            , "[files]"
            , "[ignores]"
            , "[monikers]"
            , "[[hooks.pre-reflect]]"
            , "id = \"refresh-cache\""
            , "policy = \"on-change\""
            , "change-key = \"v2\""
            , "command = \"refresh\""
            , "[[hooks.post-unmanage]]"
            , "id = \"notify\""
            , "policy = \"once\""
            , "command = \"notify\""
            ]
    case readManifest toml of
      Left err -> expectationFailure $ show $ unpack <$> formatErrors err
      Right _ -> return ()

  specify "parses hook working directories as file-path expressions" $ do
    let toml =
          Text.unlines
            [ "[dirs]"
            , "[files]"
            , "[ignores]"
            , "[monikers]"
            , "[[hooks.pre-apply]]"
            , "command = \"refresh\""
            , "working-directory = \"$TOOLS/hooks\""
            ]
    case readManifest toml of
      Left err -> expectationFailure $ show $ unpack <$> formatErrors err
      Right (manifest', _) ->
        case Map.lookup PreApply manifest'.hooks of
          Just [hook] ->
            hook.workingDirectory
              `shouldBe` Just
                (PathSeparator (Substitution "TOOLS") (BareComponent "hooks"))
          hooks -> expectationFailure $ "Unexpected hooks: " <> show hooks

  specify "rejects invalid stateful hook configurations" $ do
    let prefix =
          Text.unlines
            [ "[dirs]"
            , "[files]"
            , "[ignores]"
            , "[monikers]"
            , "[[hooks.pre-apply]]"
            ]
        invalidHooks =
          [ "policy = \"once\"\ncommand = \"run\""
          , "id = \"2bad\"\npolicy = \"once\"\ncommand = \"run\""
          , "id = \"changed\"\npolicy = \"on-change\"\ncommand = \"run\""
          , "change-key = \"v2\"\ncommand = \"run\""
          ]
    forM_ invalidHooks $ \hookSource ->
      case readManifest (prefix <> hookSource) of
        Left err -> isHookConfigurationError err `shouldBe` True
        Right _ -> expectationFailure "Expected an invalid hook to be rejected."

  specify "rejects duplicate stateful hook identities within one event" $ do
    let toml =
          Text.unlines
            [ "[dirs]"
            , "[files]"
            , "[ignores]"
            , "[monikers]"
            , "[[hooks.pre-status]]"
            , "id = \"prepare\""
            , "policy = \"once\""
            , "command = \"first\""
            , "[[hooks.pre-status]]"
            , "id = \"prepare\""
            , "policy = \"once\""
            , "command = \"second\""
            ]
    case readManifest toml of
      Left err -> isHookConfigurationError err `shouldBe` True
      Right _ -> expectationFailure "Expected duplicate hook IDs to be rejected."

  specify "rejects route names containing parent traversal" $ do
    let toml =
          Text.unlines
            [ "[dirs]"
            , ""
            , "[files]"
            , "\"../outside\" = [{ when = \"always\", path = \"target\" }]"
            , ""
            , "[ignores]"
            , ""
            , "[monikers]"
            ]
    case readManifest toml of
      Left err ->
        formatErrors err
          `shouldSatisfy` any ("parent traversal" `isInfixOf`)
      Right _ -> expectationFailure "Expected the traversing route to be rejected."

  specify "rejects rooted and drive-qualified Windows route names" $ do
    forM_ ["C:foo", "C:\\foo", "\\foo"] $ \routeName -> do
      let toml =
            Text.unlines
              [ "[dirs]"
              , ""
              , "[files]"
              , "'" <> Text.pack routeName <> "' = [{ when = \"always\", path = \"target\" }]"
              , ""
              , "[ignores]"
              , ""
              , "[monikers]"
              ]
      case readManifest toml of
        Left err ->
          formatErrors err
            `shouldSatisfy` any ("must be relative" `isInfixOf`)
        Right _ ->
          expectationFailure $
            "Expected the Windows route to be rejected: " <> routeName

  specify "rejects route names that collide after normalization" $ do
    let toml =
          Text.unlines
            [ "[dirs]"
            , ""
            , "[files]"
            , "foo = [{ when = \"always\", path = \"first\" }]"
            , "\"./foo\" = [{ when = \"always\", path = \"second\" }]"
            , ""
            , "[ignores]"
            , ""
            , "[monikers]"
            ]
    case readManifest toml of
      Left err ->
        formatErrors err
          `shouldSatisfy` any ("normalize to the same path" `isInfixOf`)
      Right _ ->
        expectationFailure "Expected normalized duplicate routes to be rejected."

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
        Right (Manifest _ _ _ routes _ _, warnings) = readManifest toml
        Just homeRoute = Map.lookup home routes
        Just bashrcRoute = Map.lookup bashrc routes
        routeShape :: FileRoute -> [(String, Maybe Text)]
        routeShape route =
          sort
            [ (show predicate, toPathText . (.expression) <$> path)
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
        Right (Manifest _ _ _ routes _ _, _) = readManifest toml
        Just route = Map.lookup gitconfig routes
    [ (show predicate, toPathText . (.expression) <$> path)
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
      Right (Manifest _ _ _ routes _ _, _) -> do
        let Just route = Map.lookup foo routes
        [ (show predicate, toPathText . (.expression) <$> path)
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
      Right (Manifest _ _ _ routes _ _, _) -> do
        let Just route = Map.lookup foo routes
        route.predicates `shouldBe` [(Always, Just "target")]
        dispatch
          (emptyEnvironment "linux" "x86_64" $ Kernel "Linux" "6.0")
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

  specify "accepts an explicit empty detailed branch path" $ do
    let result =
          readManifest $ detailedManifest "{ when = \"always\", path = \"\" }"
    case result of
      Left err -> expectationFailure $ show $ unpack <$> formatErrors err
      Right (Manifest _ _ _ routes _ _, _) -> do
        let Just route = Map.lookup foo routes
        route.predicates
          `shouldBe` [(Always, Just $ routeTarget $ BareComponent "")]

  specify "identifies invalid detailed branch paths" $ do
    let result =
          readManifest $ detailedManifest "{ when = \"always\", path = \"$\" }"
    case result of
      Left err ->
        formatErrors err
          `shouldSatisfy` any (isInfixOf "files.foo[0].path")
      Right _ -> expectationFailure "Expected the path to be rejected."
