{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.RepositorySpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (linear)
import System.Info (os)

import System.OsPath (encodeFS, (</>))
import Test.Hspec (Spec, runIO, specify)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldNotSatisfy
  , shouldReturn
  , shouldSatisfy
  )
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.MonadFileSystem (FileType (..))
import Dojang.Types.Environment (Environment (..), Kernel (..))
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression (FilePathExpression (Substitution))
import Dojang.Types.FileRoute (fileRoute)
import Dojang.Types.Manifest (Manifest (..))
import Dojang.Types.MonikerMap (MonikerMap)
import Dojang.Types.MonikerName (parseMonikerName)
import Dojang.Types.Repository
  ( Repository (..)
  , RouteMapWarning (OverlapDestinationPathsWarning)
  , RouteResult (..)
  , findOverlappingRouteResults
  , overlaps
  , routePaths
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
  inter <- runIO $ encodeFS ".dojang"

  specify "routePaths" $ do
    let Right any' = parseMonikerName "any"
    let monikers = [(any', Always)] :: MonikerMap
    let dirRoute expr = fileRoute monikers [(any', Just expr)] Directory
    let manifest =
          Manifest
            { repositoryId = Nothing
            , monikers = monikers
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
    let env = Environment "linux" "x86_64" $ Kernel "Linux" "4.19.0-16-amd64"
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
                , fileRoutes = [(foo, route)]
                , ignorePatterns = mempty
                , hooks = mempty
                }
        let repo = Repository (root </> src) (root </> inter) manifest
        let env =
              Environment "linux" "x86_64" $ Kernel "Linux" "6.1.0"
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
