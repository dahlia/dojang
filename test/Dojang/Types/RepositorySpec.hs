{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.RepositorySpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (linear)
import System.Info (os)

import System.OsPath (encodeFS, (</>))
import Test.Hspec (Spec, expectationFailure, runIO, specify)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldNotSatisfy
  , shouldReturn
  , shouldSatisfy
  )
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.MonadFileSystem (FileType (..))
import Dojang.Types.Environment
  ( Kernel (..)
  , emptyEnvironment
  )
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression
  ( FilePathExpression (BareComponent, PathSeparator, Substitution)
  )
import Dojang.Types.FileRoute (fileRoute, fileRoutePreservingOrder)
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.ManifestVariable
  ( formatVariableResolutionError
  , lookupVariable
  , manifestVariable
  , parseManifestVariableName
  , resolveManifestVariables
  )
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.Repository
  ( Repository (..)
  , RouteMapWarning (OverlapDestinationPathsWarning)
  , RouteResult (..)
  , findOverlappingRouteResults
  , overlaps
  , routePaths
  , routePathsWithVariables
  )


win :: Bool
win = os == "mingw32"


spec :: Spec
spec = do
  root <- runIO $ encodeFS $ if win then "C:\\" else "/"
  foo <- runIO $ encodeFS "foo"
  bar <- runIO $ encodeFS "bar"
  baz <- runIO $ encodeFS "baz"
  qux <- runIO $ encodeFS "qux"
  src <- runIO $ encodeFS "src"
  dst <- runIO $ encodeFS "dst"
  nested <- runIO $ encodeFS "nested"
  inter <- runIO $ encodeFS ".dojang"

  specify "routePaths" $ do
    let Right any' = parseMonikerName "any"
    let monikers = [(any', Always)] :: MonikerMap
    let dirRoute expr = fileRoute monikers [(any', Just expr)] Directory
    let manifest =
          Manifest
            { repositoryId = Nothing
            , monikers = monikers
            , variables = mempty
            , fileRoutes =
                [ (foo, dirRoute $ if win then "C:\\dst\\foo" else "/dst/foo")
                ,
                  ( bar
                  , dirRoute $
                      if win then "C:\\dst\\foo\\bar" else "/dst/foo/bar"
                  )
                ,
                  ( baz
                  , dirRoute $
                      if win
                        then "C:\\dst\\foo\\bar\\baz"
                        else "/dst/foo/bar/baz"
                  )
                ,
                  ( qux
                  , dirRoute $
                      if win then "C:\\dst\\foo\\qux" else "/dst/foo/qux"
                  )
                ]
            , ignorePatterns =
                [ (foo, ["bar"])
                ]
            , hooks = mempty
            }
    let repo =
          Repository
            { sourcePath = root </> src
            , intermediatePath = root </> src </> inter
            , manifest = manifest
            }
    let env =
          emptyEnvironment "linux" "x86_64" $ Kernel "Linux" "4.19.0-16-amd64"
    let provenance =
          [ ("operating-system", "linux")
          , ("architecture", "x86_64")
          , ("kernel-name", "Linux")
          , ("kernel-release", "4.19.0-16-amd64")
          ]
    routePaths repo env (const $ return Nothing)
      `shouldReturn` (
                       [ RouteResult
                           (root </> src </> bar)
                           bar
                           (root </> dst </> foo </> bar)
                           Directory
                           (if win then "C:\\dst\\foo\\bar" else "/dst/foo/bar")
                           provenance
                       , RouteResult
                           (root </> src </> baz)
                           baz
                           (root </> dst </> foo </> bar </> baz)
                           Directory
                           ( if win
                               then "C:\\dst\\foo\\bar\\baz"
                               else "/dst/foo/bar/baz"
                           )
                           provenance
                       , RouteResult
                           (root </> src </> foo)
                           foo
                           (root </> dst </> foo)
                           Directory
                           (if win then "C:\\dst\\foo" else "/dst/foo")
                           provenance
                       , RouteResult
                           (root </> src </> qux)
                           qux
                           (root </> dst </> foo </> qux)
                           Directory
                           (if win then "C:\\dst\\foo\\qux" else "/dst/foo/qux")
                           provenance
                       ]
                     ,
                       [ OverlapDestinationPathsWarning
                           bar
                           (root </> dst </> foo </> bar)
                           [(baz, root </> dst </> foo </> bar </> baz)]
                       , OverlapDestinationPathsWarning
                           foo
                           (root </> dst </> foo)
                           [(qux, root </> dst </> foo </> qux)]
                       ]
                     )

  specify "routePaths expands nested manifest variables" $ do
    let Right destinationName = parseManifestVariableName "DESTINATION"
        variables =
          Map.singleton destinationName $
            manifestVariable mempty $
              PathSeparator (Substitution "BASE") (BareComponent "nested")
        route =
          fileRoutePreservingOrder
            (const Nothing)
            [(Always, Just $ Substitution "DESTINATION")]
            Directory
        manifest' =
          Manifest
            { repositoryId = Nothing
            , monikers = mempty
            , variables = variables
            , fileRoutes = Map.singleton foo route
            , ignorePatterns = mempty
            , hooks = mempty
            }
        repo = Repository (root </> src) (root </> src </> inter) manifest'
        env =
          emptyEnvironment "linux" "x86_64" $
            Kernel "Linux" "4.19.0-16-amd64"
        inherited variable =
          pure $
            if variable == "BASE"
              then Just $ root </> dst
              else Nothing
    resolution <- resolveManifestVariables env variables inherited
    case resolution of
      Left err ->
        expectationFailure $ Text.unpack $ formatVariableResolutionError err
      Right variableEnvironment -> do
        (routes, warnings) <-
          routePathsWithVariables repo env $ lookupVariable variableEnvironment
        warnings `shouldBe` []
        case routes of
          [result] -> do
            result.destinationPath `shouldBe` root </> dst </> nested
            result.routeDefinition `shouldBe` "$DESTINATION"
            Map.member "var.DESTINATION" result.routeProvenance `shouldBe` True
            Map.member "env.BASE" result.routeProvenance `shouldBe` True
          _ -> expectationFailure $ "Unexpected routes: " <> show routes

  if win
    then return ()
    else specify "routePaths distinguishes native non-UTF-8 environment values" $
      hedgehog $ do
        firstByte <- forAll $ Gen.word8 (linear 0x80 0xfe)
        secondByte <- forAll $ Gen.word8 (linear (firstByte + 1) 0xff)
        firstValue <-
          liftIO $ encodeFS [toEnum $ 0xdc00 + fromIntegral firstByte]
        secondValue <-
          liftIO $ encodeFS [toEnum $ 0xdc00 + fromIntegral secondByte]
        let Right any' = parseMonikerName "any"
        let monikers = [(any', Always)] :: MonikerMap
        let route =
              fileRoute monikers [(any', Just $ Substitution "VALUE")] File
        let manifest =
              Manifest
                { repositoryId = Nothing
                , monikers = monikers
                , variables = mempty
                , fileRoutes = [(foo, route)]
                , ignorePatterns = mempty
                , hooks = mempty
                }
        let repo = Repository (root </> src) (root </> inter) manifest
        let env =
              emptyEnvironment "linux" "x86_64" $ Kernel "Linux" "6.1.0"
        (firstRoutes, _) <-
          liftIO $ routePaths repo env (const $ return $ Just firstValue)
        (secondRoutes, _) <-
          liftIO $ routePaths repo env (const $ return $ Just secondValue)
        case (firstRoutes, secondRoutes) of
          ( [RouteResult _ _ _ _ _ firstProvenance]
            , [RouteResult _ _ _ _ _ secondProvenance]
            ) ->
              (firstProvenance == secondProvenance) === False
          _ ->
            fail "Expected exactly one route result for each environment value."

  specify "findOverlappingRouteResults" $ do
    findOverlappingRouteResults
      [ RouteResult (root </> src </> foo) foo (root </> foo) Directory "" mempty
      , RouteResult (root </> src </> bar) bar (root </> bar) Directory "" mempty
      , RouteResult
          (root </> src </> baz)
          baz
          (root </> foo </> baz)
          Directory
          ""
          mempty
      , RouteResult
          (root </> src </> qux)
          qux
          (root </> foo </> baz </> qux)
          File
          ""
          mempty
      ]
      `shouldBe` [
                   ( (baz, root </> foo </> baz)
                   , [(qux, root </> foo </> baz </> qux)]
                   )
                 ,
                   ( (foo, root </> foo)
                   ,
                     [ (baz, root </> foo </> baz)
                     , (qux, root </> foo </> baz </> qux)
                     ]
                   )
                 ]

  specify "overlaps" $ do
    foo `shouldSatisfy` overlaps foo
    foo `shouldSatisfy` (`overlaps` (foo </> bar))
    foo `shouldSatisfy` (`overlaps` (foo </> bar </> baz))
    foo `shouldNotSatisfy` (`overlaps` bar)
    foo `shouldNotSatisfy` (`overlaps` (bar </> foo))
    (foo </> bar) `shouldNotSatisfy` (`overlaps` foo)
    (foo </> bar) `shouldSatisfy` (`overlaps` (foo </> bar))
    (foo </> bar) `shouldSatisfy` (`overlaps` (foo </> bar </> baz))
    (foo </> bar) `shouldNotSatisfy` (`overlaps` (foo </> baz))
