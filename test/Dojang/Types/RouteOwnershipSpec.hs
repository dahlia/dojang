{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.RouteOwnershipSpec (spec) where

import Data.List (isPrefixOf, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (Down))
import Data.Set qualified as Set

import Control.Monad.Except (catchError)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory.OsPath (createDirectory, createDirectoryLink)
import System.OsPath
  ( OsPath
  , encodeFS
  , joinPath
  , normalise
  , splitDirectories
  , (</>)
  )
import Test.Hspec (Spec, describe, it, runIO, specify, xit)
import Test.Hspec.Expectations.Pretty (shouldBe)


#ifdef mingw32_HOST_OS
import Test.Hspec.Expectations.Pretty (shouldSatisfy)
#endif
import Test.Hspec.Hedgehog (annotateShow, assert, forAll, hedgehog, (===))

import Dojang.MonadFileSystem (FileType (Directory, File))
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Codec (identityCodecSpec)
import Dojang.Types.FileRoute
  ( RouteKind (CopyRoute, SymlinkRoute)
  , RouteMode (DefaultMode)
  )
import Dojang.Types.Repository (RouteResult (..))
import Dojang.Types.RouteOwnership
  ( ExpectedState (..)
  , OwnershipError (..)
  , ownedExclusions
  , ownerOf
  , selectOwnership
  , verifyResolvedIdentities
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
    identityCodecSpec


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
      let Right state = selectOwnership src [routeA, routeB]
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
      let Right state = selectOwnership src [routeA, routeB, routeC]
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
      selectOwnership src [routeA, routeB]
        `shouldBe` Left (DuplicateDestinationOwner (normalise $ dst </> a) a b)

    specify "records symlink destinations as boundaries" $ do
      let routeA = mkRoute src a (dst </> a) Directory SymlinkRoute
      let Right state = selectOwnership src [routeA]
      state.boundaries `shouldBe` Set.singleton (normalise $ dst </> a)

    specify "rejects a lexically aliased source and destination" $ do
      let outside = mkRoute dst a (dst </> a </> c) Directory CopyRoute
      -- The destination lies inside the route's own source tree, but the
      -- source is outside the repository root, so the per-route check
      -- reports it:
      selectOwnership src [outside{sourcePath = dst </> a}]
        `shouldBe` Left
          ( SourceDestinationAliased
              a
              (normalise $ dst </> a)
              (normalise $ dst </> a </> c)
          )

    specify "rejects destinations inside the repository root" $ do
      let routeA = mkRoute src a (src </> a) Directory CopyRoute
      selectOwnership src [routeA]
        `shouldBe` Left
          (DestinationInsideRepository a (normalise $ src </> a))
      let routeB = mkRoute src b (src </> b </> c) Directory CopyRoute
      selectOwnership src [routeB]
        `shouldBe` Left
          (DestinationInsideRepository b (normalise $ src </> b </> c))

    specify "rejects routes under a single-file destination" $ do
      let routeA = mkRoute src a (dst </> a) File CopyRoute
      let routeB = mkRoute src b (dst </> a </> b) File CopyRoute
      selectOwnership src [routeA, routeB]
        `shouldBe` Left
          ( NestedUnderFileRoute
              b
              (normalise $ dst </> a </> b)
              (normalise $ dst </> a)
          )

    specify "rejects routes under a symlink boundary" $ do
      let routeA = mkRoute src a (dst </> a) Directory SymlinkRoute
      let routeB = mkRoute src b (dst </> a </> b) File CopyRoute
      selectOwnership src [routeA, routeB]
        `shouldBe` Left
          ( RouteThroughLinkBoundary
              b
              (normalise $ dst </> a </> b)
              (normalise $ dst </> a)
          )

  describe "verifyResolvedIdentities" $ do
    symlinkAvailable <- runIO $ withTempDir $ \tmpDir _ ->
      ( do
          createDirectory $ tmpDir </> a
          createDirectoryLink (tmpDir </> a) (tmpDir </> b)
          return True
      )
        `catchError` const (return False)
    let symIt = if symlinkAvailable then it else xit

    symIt "rejects destinations that resolve into the source" $
      withTempDir $ \tmpDir _ -> do
        createDirectory $ tmpDir </> src
        createDirectory $ tmpDir </> src </> a
        createDirectoryLink (tmpDir </> src </> a) (tmpDir </> b)
        let route =
              mkRoute (tmpDir </> src) a (tmpDir </> b) Directory CopyRoute
        let Right state = selectOwnership (tmpDir </> src) [route]
        result <- verifyResolvedIdentities (tmpDir </> src) state
        case result of
          Left (DestinationInsideRepository name _) -> name `shouldBe` a
          other -> fail $ "Unexpected result: " <> show other

    symIt "accepts an already deployed symlink destination" $
      withTempDir $ \tmpDir _ -> do
        createDirectory $ tmpDir </> src
        createDirectory $ tmpDir </> src </> a
        createDirectoryLink (tmpDir </> src </> a) (tmpDir </> b)
        let route =
              mkRoute (tmpDir </> src) a (tmpDir </> b) Directory SymlinkRoute
        let Right state = selectOwnership (tmpDir </> src) [route]
        result <- verifyResolvedIdentities (tmpDir </> src) state
        result `shouldBe` Right ()

    symIt "rejects a missing destination under an aliased ancestor" $
      withTempDir $ \tmpDir _ -> do
        createDirectory $ tmpDir </> src
        createDirectory $ tmpDir </> src </> a
        createDirectoryLink (tmpDir </> src </> a) (tmpDir </> b)
        let route =
              mkRoute
                (tmpDir </> src)
                a
                (tmpDir </> b </> c </> d)
                Directory
                CopyRoute
        let Right state = selectOwnership (tmpDir </> src) [route]
        result <- verifyResolvedIdentities (tmpDir </> src) state
        case result of
          Left (DestinationInsideRepository name _) -> name `shouldBe` a
          other -> fail $ "Unexpected result: " <> show other

    symIt "rejects destinations that resolve to one tree" $
      withTempDir $ \tmpDir _ -> do
        createDirectory $ tmpDir </> src
        createDirectory $ tmpDir </> src </> a
        createDirectory $ tmpDir </> src </> b
        createDirectory $ tmpDir </> c
        createDirectoryLink (tmpDir </> c) (tmpDir </> d)
        let routeA =
              mkRoute (tmpDir </> src) a (tmpDir </> c) Directory CopyRoute
        let routeB =
              mkRoute (tmpDir </> src) b (tmpDir </> d) Directory CopyRoute
        let Right state = selectOwnership (tmpDir </> src) [routeA, routeB]
        result <- verifyResolvedIdentities (tmpDir </> src) state
        case result of
          Left (DuplicateDestinationOwner _ _ _) -> return ()
          other -> fail $ "Unexpected result: " <> show other

    symIt "accepts a replaceable leaf link at a nested destination" $
      withTempDir $ \tmpDir _ -> do
        createDirectory $ tmpDir </> src
        createDirectory $ tmpDir </> src </> a
        createDirectory $ tmpDir </> src </> b
        createDirectory $ tmpDir </> c
        createDirectory $ tmpDir </> d
        -- The nested destination itself is a symlink pointing elsewhere;
        -- it is a replaceable entry, not part of the ancestor chain:
        createDirectoryLink (tmpDir </> d) (tmpDir </> c </> b)
        let ancestor =
              mkRoute (tmpDir </> src) a (tmpDir </> c) Directory CopyRoute
        let nested =
              mkRoute (tmpDir </> src) b (tmpDir </> c </> b) File CopyRoute
        let Right state = selectOwnership (tmpDir </> src) [ancestor, nested]
        result <- verifyResolvedIdentities (tmpDir </> src) state
        result `shouldBe` Right ()

    symIt "rejects a nested destination below an ancestor leaf link" $
      withTempDir $ \tmpDir _ -> do
        createDirectory $ tmpDir </> src
        createDirectory $ tmpDir </> src </> a
        createDirectory $ tmpDir </> src </> b
        createDirectory $ tmpDir </> c
        createDirectory $ tmpDir </> d
        -- The ancestor's own destination is a link elsewhere; a nested
        -- route below it would operate on the linked tree while the
        -- ancestor replaces the link itself:
        createDirectoryLink (tmpDir </> d) (tmpDir </> c </> a)
        let ancestor =
              mkRoute
                (tmpDir </> src)
                a
                (tmpDir </> c </> a)
                Directory
                CopyRoute
        let nested =
              mkRoute
                (tmpDir </> src)
                b
                (tmpDir </> c </> a </> b)
                File
                CopyRoute
        let Right state = selectOwnership (tmpDir </> src) [ancestor, nested]
        result <- verifyResolvedIdentities (tmpDir </> src) state
        case result of
          Left (NestedDestinationsDiverged nested' ancestor') -> do
            nested' `shouldBe` b
            ancestor' `shouldBe` a
          other -> fail $ "Unexpected result: " <> show other

    symIt "rejects nested destinations that escape their ancestor" $
      withTempDir $ \tmpDir _ -> do
        createDirectory $ tmpDir </> src
        createDirectory $ tmpDir </> src </> a
        createDirectory $ tmpDir </> src </> b
        createDirectory $ tmpDir </> c
        createDirectory $ tmpDir </> c </> d
        createDirectory $ tmpDir </> d
        -- The nested route's lexical ancestor chain crosses a link that
        -- resolves outside the ancestor's destination:
        createDirectoryLink (tmpDir </> d) (tmpDir </> c </> d </> a)
        let ancestor =
              mkRoute (tmpDir </> src) a (tmpDir </> c) Directory CopyRoute
        let nested =
              mkRoute
                (tmpDir </> src)
                b
                (tmpDir </> c </> d </> a </> b)
                Directory
                CopyRoute
        let Right state = selectOwnership (tmpDir </> src) [ancestor, nested]
        result <- verifyResolvedIdentities (tmpDir </> src) state
        case result of
          Left (NestedDestinationsDiverged nested' ancestor') -> do
            nested' `shouldBe` b
            ancestor' `shouldBe` a
          other -> fail $ "Unexpected result: " <> show other

    symIt "accepts destinations that resolve elsewhere" $
      withTempDir $ \tmpDir _ -> do
        createDirectory $ tmpDir </> src
        createDirectory $ tmpDir </> src </> a
        createDirectory $ tmpDir </> c
        createDirectoryLink (tmpDir </> c) (tmpDir </> b)
        let route =
              mkRoute (tmpDir </> src) a (tmpDir </> b) Directory CopyRoute
        let Right state = selectOwnership (tmpDir </> src) [route]
        result <- verifyResolvedIdentities (tmpDir </> src) state
        result `shouldBe` Right ()

  describe "ownedExclusions" $ do
    specify "returns nested roots relative to the owner" $ do
      let routeA = mkRoute src a (dst </> a) Directory CopyRoute
      let routeB = mkRoute src b (dst </> a </> b </> c) Directory CopyRoute
      let Right state = selectOwnership src [routeA, routeB]
      ownedExclusions state (dst </> a) `shouldBe` [b </> c]
      ownedExclusions state (dst </> a </> b </> c) `shouldBe` []

  describe "ownerOf" $ do
    specify "resolves the most-specific containing route" $ do
      let routeA = mkRoute src a (dst </> a) Directory CopyRoute
      let routeB = mkRoute src b (dst </> a </> b) Directory CopyRoute
      let Right state = selectOwnership src [routeA, routeB]
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
      case selectOwnership src named of
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

  caseVariantSpec src dst a b

-- Case-variant destinations name one filesystem entry on Windows but two
-- distinct entries on POSIX, so the expected ownership differs by platform.
#ifdef mingw32_HOST_OS
caseVariantSpec :: OsPath -> OsPath -> OsPath -> OsPath -> Spec
caseVariantSpec src dst a b =
  describe "case-variant destinations (Windows)" $ do
    upperDst <- runIO $ encodeFS "DST"
    upperA <- runIO $ encodeFS "A"

    specify "rejects case-variant duplicate destinations" $ do
      let routeA = mkRoute src a (dst </> a) Directory CopyRoute
      let routeB = mkRoute src b (upperDst </> upperA) Directory CopyRoute
      selectOwnership src [routeA, routeB] `shouldSatisfy` isDuplicateOwner

    specify "records case-variant nested destinations" $ do
      let routeA = mkRoute src a (dst </> a) Directory CopyRoute
      let routeB = mkRoute src b (upperDst </> upperA </> b) Directory CopyRoute
      let Right state = selectOwnership src [routeA, routeB]
      Map.lookup (normalise $ dst </> a) state.nestedUnder
        `shouldBe` Just [normalise $ upperDst </> upperA </> b]

    specify "resolves owners for case-variant probes" $ do
      let routeA = mkRoute src a (dst </> a) Directory CopyRoute
      let Right state = selectOwnership src [routeA]
      (.routeName)
        <$> ownerOf state (upperDst </> upperA </> b)
        `shouldBe` Just a


isDuplicateOwner :: Either OwnershipError ExpectedState -> Bool
isDuplicateOwner (Left DuplicateDestinationOwner{}) = True
isDuplicateOwner _ = False
#else
caseVariantSpec :: OsPath -> OsPath -> OsPath -> OsPath -> Spec
caseVariantSpec src dst a b =
  describe "case-variant destinations (POSIX)" $ do
    upperDst <- runIO $ encodeFS "DST"
    upperA <- runIO $ encodeFS "A"

    specify "keeps case-variant destinations distinct" $ do
      let routeA = mkRoute src a (dst </> a) Directory CopyRoute
      let routeB = mkRoute src b (upperDst </> upperA) Directory CopyRoute
      let Right state = selectOwnership src [routeA, routeB]
      Map.keysSet state.owners
        `shouldBe` Set.fromList
          [normalise $ dst </> a, normalise $ upperDst </> upperA]
      state.nestedUnder `shouldBe` Map.empty
#endif
