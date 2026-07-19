{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.ManagedTargetSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.List (permutations)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (linear)
import System.Info (os)
import System.OsPath (OsPath, encodeFS, normalise, (</>))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.MonadFileSystem qualified as FileSystem
import Dojang.Types.Context
  ( FileCorrespondence (..)
  , FileDeltaKind (Unchanged)
  , FileEntry (..)
  , FileStat (File, Missing)
  , ManagedCorrespondence (..)
  )
import Dojang.Types.FileRoute
  ( RouteKind (CopyRoute, SymlinkRoute)
  , RouteMode (DefaultMode)
  )
import Dojang.Types.ManagedTarget
  ( CurrentEntry (..)
  , CurrentRoute (..)
  , ManagedTarget (..)
  , OrphanReason (..)
  , SynchronizationCommand (..)
  , TargetFingerprint (..)
  , classifyOrphan
  , equalDestinationPath
  , makeCurrentEntries
  , makeCurrentRoutes
  , mergeConvergedTargets
  , selectOrphanRecords
  , unreachableSnapshots
  )
import Dojang.Types.Repository qualified as Repository


spec :: Spec
spec = do
  describe "equalDestinationPath" $ do
    it "uses case-insensitive native path semantics on Windows" $ do
      upper <- encodeFS "C:/Users/Alice/App"
      lower <- encodeFS "c:\\users\\alice\\app"
      equalDestinationPath upper lower `shouldBe` (os == "mingw32")

  describe "makeCurrentRoutes" $ do
    it "normalizes route-map keys and route names" $ do
      rawRouteName <- encodeFS "./config"
      source <- encodeFS "/repository/config"
      destination <- encodeFS "/destination"
      let route =
            Repository.RouteResult
              source
              rawRouteName
              destination
              FileSystem.File
              DefaultMode
              CopyRoute
              "definition"
              Map.empty
      routes <- makeCurrentRoutes [route]
      let normalizedRouteName = normalise rawRouteName
      case Map.lookup normalizedRouteName routes of
        Just current -> current.routeName `shouldBe` normalizedRouteName
        Nothing -> fail "The normalized current route was not indexed."

  describe "makeCurrentEntries" $ do
    it "excludes intermediate-only directory entries but keeps file routes" $ do
      routeName <- encodeFS "config"
      entryName <- encodeFS "removed.toml"
      sourceRoot <- fixturePath "repository/config"
      intermediateRoot <- fixturePath "intermediate/config"
      destinationRoot <- fixturePath "destination"
      let correspondence relative =
            FileCorrespondence
              (FileEntry (sourceRoot </> relative) Missing)
              Unchanged
              (FileEntry (intermediateRoot </> relative) $ File 7)
              (FileEntry (destinationRoot </> relative) $ File 7)
              Unchanged
      let managed fileType relative =
            ManagedCorrespondence
              ( Repository.RouteResult
                  sourceRoot
                  routeName
                  destinationRoot
                  fileType
                  DefaultMode
                  CopyRoute
                  "route-definition"
                  Map.empty
              )
              relative
              (correspondence relative)
      makeCurrentEntries
        [ managed FileSystem.Directory entryName
        , managed FileSystem.File mempty
        ]
        `shouldBe` Set.singleton (CurrentEntry routeName routeName)

  describe "classifyOrphan" $ do
    it "orphanizes an entry no longer produced by its active directory route" $ do
      routeName <- encodeFS "config"
      nested <- encodeFS "ignored.toml"
      destinationRoot <- fixturePath "destination"
      target <- fixtureTarget "route-definition" "unused"
      let nestedTarget =
            target
              { routeName = routeName
              , sourcePath = routeName </> nested
              , routeType = FileSystem.Directory
              , destinationPath = destinationRoot </> nested
              }
      let route =
            CurrentRoute
              routeName
              destinationRoot
              "route-definition"
              FileSystem.Directory
              CopyRoute
      classifyOrphan (Map.singleton routeName route) Set.empty nestedTarget
        `shouldBe` Just EntryRemoved

    it "treats a route kind change as a route change" $ do
      target <- fixtureTarget "route-definition" "destination"
      route <- fixtureRoute "route-definition" "destination"
      classifyOrphan
        (Map.singleton target.routeName route{kind = SymlinkRoute})
        (Set.singleton $ CurrentEntry target.routeName target.sourcePath)
        target
        `shouldBe` Just RouteChanged

    it "uses native destination equality when classifying a route" $ do
      upper <- encodeFS "C:/Users/Alice/App"
      lower <- encodeFS "c:\\users\\alice\\app"
      target <- fixtureTarget "route-definition" "unused"
      let target' =
            ManagedTarget
              target.targetId
              target.routeName
              target.sourcePath
              target.routeType
              CopyRoute
              DefaultMode
              upper
              target.snapshotPath
              target.routeDefinition
              target.routeProvenance
              target.fingerprint
              target.updatedBy
              target.updatedTime
      let route =
            CurrentRoute
              target.routeName
              lower
              target.routeDefinition
              target.routeType
              CopyRoute
      classifyOrphan
        (Map.singleton target.routeName route)
        (activeEntry target')
        target'
        `shouldBe` if os == "mingw32" then Nothing else Just DestinationChanged

    it "classifies arbitrary destination changes as environment changes" $
      hedgehog $ do
        suffix <- forAll $ Gen.text (linear 1 30) Gen.alphaNum
        target <- liftIO $ fixtureTarget "route-definition" "old"
        route <- liftIO $ fixtureRoute "route-definition" (suffix <> "-new")
        classifyOrphan
          (Map.singleton target.routeName route)
          (activeEntry target)
          target
          === Just DestinationChanged

    it "distinguishes removed routes from changed definitions" $ do
      target <- fixtureTarget "old-definition" "destination"
      changed <- fixtureRoute "new-definition" "destination"
      classifyOrphan Map.empty Set.empty target `shouldBe` Just RouteRemoved
      classifyOrphan
        (Map.singleton target.routeName changed)
        (activeEntry target)
        target
        `shouldBe` Just RouteChanged

    it "keeps a nested entry active under the same directory route" $ do
      routeName <- encodeFS "config"
      nested <- encodeFS "nested"
      destinationRoot <- fixturePath "destination"
      target <- fixtureTarget "route-definition" "unused"
      let nestedTarget =
            target
              { routeName = routeName
              , sourcePath = routeName </> nested
              , routeType = FileSystem.Directory
              , destinationPath = destinationRoot </> nested
              }
      let route =
            CurrentRoute
              routeName
              destinationRoot
              "route-definition"
              FileSystem.Directory
              CopyRoute
      classifyOrphan
        (Map.singleton routeName route)
        (activeEntry nestedTarget)
        nestedTarget
        `shouldBe` Nothing

    it "orphanizes a file record when its route becomes a directory" $ do
      routeName <- encodeFS "config"
      destination <- fixturePath "destination"
      target <- fixtureTarget "route-definition" "destination"
      let fileTarget =
            target
              { routeName = routeName
              , sourcePath = routeName
              , destinationPath = destination
              }
      let directoryRoute =
            CurrentRoute
              routeName
              destination
              "route-definition"
              FileSystem.Directory
              CopyRoute
      classifyOrphan
        (Map.singleton routeName directoryRoute)
        (activeEntry fileTarget)
        fileTarget
        `shouldBe` Just RouteChanged

    it "rejects a fresh active record that replaced a selected orphan" $ do
      stale <- fixtureTarget "old-definition" "destination"
      fresh <- fixtureTarget "new-definition" "destination"
      current <- fixtureRoute "new-definition" "destination"
      let identifiers = Set.singleton stale.targetId
      selectOrphanRecords
        (Map.singleton fresh.routeName current)
        (activeEntry fresh)
        identifiers
        (Map.singleton fresh.targetId fresh)
        `shouldBe` Nothing

    it "rejects any selection whose record disappeared before reloading" $
      hedgehog $ do
        suffixes <-
          forAll $ Gen.list (linear 0 8) $ Gen.text (linear 1 12) Gen.alphaNum
        missingSuffix <- forAll $ Gen.text (linear 1 12) Gen.alphaNum
        target <- liftIO $ fixtureTarget "definition" "destination"
        let presentIds = ("present-" <>) <$> suffixes
        let missingId = "missing-" <> missingSuffix
        let records =
              Map.fromList
                [ (identifier, target{targetId = identifier})
                | identifier <- presentIds
                ]
        let identifiers = Set.insert missingId $ Map.keysSet records
        selectOrphanRecords Map.empty Set.empty identifiers records === Nothing

  describe "unreachableSnapshots" $ do
    it "does not treat unrelated relative paths as overlapping" $ do
      first <- encodeFS "first"
      second <- encodeFS "second"
      unreachableSnapshots (Set.singleton first) [second] `shouldBe` [second]

    it "keeps a child snapshot covered by a retained directory record" $ do
      parent <- fixturePath "snapshot/config"
      child <- fixtureChild parent "app.toml"
      unreachableSnapshots (Set.singleton parent) [child] `shouldBe` []

    it "is order-independent and never removes a kept path or its ancestor" $
      hedgehog $ do
        names <- forAll $ Gen.list (linear 1 8) $ Gen.text (linear 1 12) Gen.alphaNum
        root <- liftIO $ fixturePath "snapshot"
        paths <- traverse (liftIO . fixtureChild root) names
        let kept = Set.fromList $ take (length paths `div` 2) paths
        let removed = drop (length paths `div` 2) paths
        let results = unreachableSnapshots kept <$> permutations removed
        case results of
          [] -> True === False
          result : rest -> do
            rest === replicate (length rest) result
            Set.null (Set.intersection kept $ Set.fromList result) === True

  describe "mergeConvergedTargets" $ do
    it "preserves every record whose operation did not converge" $
      hedgehog $ do
        completed <- forAll Gen.bool
        target <- liftIO $ fixtureTarget "definition" "destination"
        let existing = Map.singleton target.targetId target
        let observations =
              if completed then [(target.targetId, Nothing)] else []
        let (updated, superseded) =
              mergeConvergedTargets existing observations
        if completed
          then do
            updated === Map.empty
            superseded === [target]
          else do
            updated === existing
            superseded === []

    it "returns an old baseline when a converged record replaces it" $ do
      old <- fixtureTarget "definition" "destination"
      replacementPath <- fixturePath "snapshot/replacement"
      let replacement = old{snapshotPath = replacementPath}
      let (updated, superseded) =
            mergeConvergedTargets
              (Map.singleton old.targetId old)
              [(old.targetId, Just replacement)]
      updated `shouldBe` Map.singleton old.targetId replacement
      superseded `shouldBe` [old]


fixtureTarget :: Text -> Text -> IO ManagedTarget
fixtureTarget definition destinationName = do
  route <- encodeFS "config"
  source <- encodeFS "config"
  destination <- fixturePath destinationName
  snapshot <- fixturePath "snapshot/config"
  return $
    ManagedTarget
      { targetId = "target-id"
      , routeName = route
      , sourcePath = source
      , routeType = FileSystem.File
      , routeKind = CopyRoute
      , declaredMode = DefaultMode
      , destinationPath = destination
      , snapshotPath = snapshot
      , routeDefinition = definition
      , routeProvenance = Map.empty
      , fingerprint = FileFingerprint 3 "abc"
      , updatedBy = Applied
      , updatedTime = timestamp
      }


fixtureRoute :: Text -> Text -> IO CurrentRoute
fixtureRoute definition destinationName = do
  route <- encodeFS "config"
  destination <- fixturePath destinationName
  return $
    CurrentRoute
      { routeName = route
      , destinationPath = destination
      , routeDefinition = definition
      , fileType = FileSystem.File
      , kind = CopyRoute
      }


activeEntry :: ManagedTarget -> Set.Set CurrentEntry
activeEntry target =
  Set.singleton $ CurrentEntry target.routeName target.sourcePath


fixturePath :: Text -> IO OsPath
fixturePath value = do
  root <-
    encodeFS $
      if os == "mingw32"
        then "C:\\tmp\\dojang-managed-target"
        else "/tmp/dojang-managed-target"
  relative <- encodeFS $ Text.unpack value
  return $ root </> relative


fixtureChild
  :: OsPath
  -> Text
  -> IO OsPath
fixtureChild root name = do
  child <- encodeFS $ Text.unpack name
  return $ root </> child


timestamp :: UTCTime
timestamp =
  parseTimeOrError
    True
    defaultTimeLocale
    "%Y-%m-%dT%H:%M:%SZ"
    "2026-07-14T00:00:00Z"
