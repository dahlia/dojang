{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.RouteOwnershipSpec (spec) where

import Data.List (isPrefixOf, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (Down))
import Data.Set qualified as Set

import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsPath
  ( OsPath
  , encodeFS
  , joinPath
  , normalise
  , splitDirectories
  , (</>)
  )
import Test.Hspec (Spec, describe, runIO, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Hedgehog (annotateShow, assert, forAll, hedgehog, (===))

import Dojang.MonadFileSystem (FileType (Directory, File))
import Dojang.Types.FileRoute
  ( RouteKind (CopyRoute, SymlinkRoute)
  , RouteMode (DefaultMode)
  )
import Dojang.Types.Repository (RouteResult (..))
import Dojang.Types.RouteOwnership
  ( ExpectedState (..)
  , OwnershipError (..)
  , ownerOf
  , selectOwnership
  )


mkRoute :: OsPath -> OsPath -> OsPath -> FileType -> RouteKind -> RouteResult
mkRoute src name destination fileType' kind' =
  RouteResult
    (src </> name)
    name
    destination
    fileType'
    DefaultMode
    kind'
    ""
    mempty


spec :: Spec
spec = do
  src <- runIO $ encodeFS "src"
  dst <- runIO $ encodeFS "dst"
  a <- runIO $ encodeFS "a"
  b <- runIO $ encodeFS "b"
  c <- runIO $ encodeFS "c"
  d <- runIO $ encodeFS "d"

  describe "selectOwnership" $ do
    specify "assigns each destination to its own route" $ do
      let routeA = mkRoute src a (dst </> a) Directory CopyRoute
      let routeB = mkRoute src b (dst </> b) File CopyRoute
      let Right state = selectOwnership [routeA, routeB]
      state.owners
        `shouldBe` Map.fromList
          [ (normalise $ dst </> a, routeA)
          , (normalise $ dst </> b, routeB)
          ]
      state.boundaries `shouldBe` Set.empty
      state.nestedUnder `shouldBe` Map.empty

    specify "records nested copied routes under their ancestors" $ do
      let routeA = mkRoute src a (dst </> a) Directory CopyRoute
      let routeB = mkRoute src b (dst </> a </> b) Directory CopyRoute
      let routeC = mkRoute src c (dst </> a </> b </> c) File CopyRoute
      let Right state = selectOwnership [routeA, routeB, routeC]
      state.nestedUnder
        `shouldBe` Map.fromList
          [
            ( normalise $ dst </> a
            ,
              [ normalise $ dst </> a </> b
              , normalise $ dst </> a </> b </> c
              ]
            )
          ,
            ( normalise $ dst </> a </> b
            , [normalise $ dst </> a </> b </> c]
            )
          ]

    specify "rejects two routes with one destination" $ do
      let routeA = mkRoute src a (dst </> a) Directory CopyRoute
      let routeB = mkRoute src b (dst </> a) Directory CopyRoute
      selectOwnership [routeA, routeB]
        `shouldBe` Left (DuplicateDestinationOwner (normalise $ dst </> a) a b)

    specify "records symlink destinations as boundaries" $ do
      let routeA = mkRoute src a (dst </> a) Directory SymlinkRoute
      let Right state = selectOwnership [routeA]
      state.boundaries `shouldBe` Set.singleton (normalise $ dst </> a)

    specify "rejects routes under a symlink boundary" $ do
      let routeA = mkRoute src a (dst </> a) Directory SymlinkRoute
      let routeB = mkRoute src b (dst </> a </> b) File CopyRoute
      selectOwnership [routeA, routeB]
        `shouldBe` Left
          ( RouteThroughLinkBoundary
              b
              (normalise $ dst </> a </> b)
              (normalise $ dst </> a)
          )

  describe "ownerOf" $ do
    specify "resolves the most-specific containing route" $ do
      let routeA = mkRoute src a (dst </> a) Directory CopyRoute
      let routeB = mkRoute src b (dst </> a </> b) Directory CopyRoute
      let Right state = selectOwnership [routeA, routeB]
      (.routeName) <$> ownerOf state (dst </> a </> c) `shouldBe` Just a
      (.routeName) <$> ownerOf state (dst </> a </> b) `shouldBe` Just b
      (.routeName) <$> ownerOf state (dst </> a </> b </> c) `shouldBe` Just b
      ownerOf state (dst </> d) `shouldBe` Nothing

    specify "ownership properties under arbitrary nesting" $ hedgehog $ do
      let components = [a, b, c, d]
      destinations <-
        forAll $
          fmap (nub . map normalise) $
            Gen.list (Range.linear 1 8) $
              joinPath . (dst :)
                <$> Gen.list (Range.linear 1 4) (Gen.element components)
      let named =
            [ mkRoute src destination destination Directory CopyRoute
            | destination <- destinations
            ]
      case selectOwnership named of
        Left err -> annotateShow err >> assert False
        Right state -> do
          -- Every route's own destination resolves to itself:
          sequence_
            [ ((.routeName) <$> ownerOf state route.destinationPath)
                === Just route.routeName
            | route <- named
            ]
          -- Any probe path resolves to the deepest containing destination:
          probe <-
            forAll $
              normalise . joinPath . (dst :)
                <$> Gen.list (Range.linear 1 5) (Gen.element components)
          let containing =
                [ route
                | route <- named
                , splitDirectories route.destinationPath
                    `isPrefixOf` splitDirectories probe
                ]
          let deepest = case sortOn
                (Down . length . splitDirectories . (.destinationPath))
                containing of
                route : _ -> Just route.routeName
                [] -> Nothing
          ((.routeName) <$> ownerOf state probe) === deepest
