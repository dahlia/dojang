{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.RouteMetadataSpec (spec) where

import Data.List (nub)

import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)

import Dojang.Types.FilePathExpression (FilePathExpression (BareComponent))
import Dojang.Types.RouteMetadata
  ( RouteKind (CopyRoute, SymlinkRoute)
  , RouteMode
    ( DefaultMode
    , Executable
    , Private
    , PrivateExecutable
    , ReadOnly
    )
  , RouteTarget (..)
  , parseRouteKind
  , parseRouteMode
  , posixDirectoryModeBits
  , posixFileModeBits
  , renderRouteKind
  , renderRouteMode
  , routeTarget
  )


spec :: Spec
spec = do
  describe "RouteMode" $ do
    specify "parseRouteMode" $ do
      parseRouteMode "default" `shouldBe` Just DefaultMode
      parseRouteMode "private" `shouldBe` Just Private
      parseRouteMode "executable" `shouldBe` Just Executable
      parseRouteMode "private-executable" `shouldBe` Just PrivateExecutable
      parseRouteMode "read-only" `shouldBe` Just ReadOnly
      parseRouteMode "" `shouldBe` Nothing
      parseRouteMode "Private" `shouldBe` Nothing
      parseRouteMode "0600" `shouldBe` Nothing
      parseRouteMode "readonly" `shouldBe` Nothing

    specify "renderRouteMode round-trips through parseRouteMode" $ do
      let modes = [minBound .. maxBound] :: [RouteMode]
      [parseRouteMode $ renderRouteMode m | m <- modes]
        `shouldBe` [Just m | m <- modes]
      length (nub $ renderRouteMode <$> modes) `shouldBe` length modes

    specify "posixFileModeBits" $ do
      posixFileModeBits DefaultMode `shouldBe` Nothing
      posixFileModeBits Private `shouldBe` Just 0o600
      posixFileModeBits Executable `shouldBe` Just 0o755
      posixFileModeBits PrivateExecutable `shouldBe` Just 0o700
      posixFileModeBits ReadOnly `shouldBe` Just 0o444

    specify "posixDirectoryModeBits" $ do
      posixDirectoryModeBits DefaultMode `shouldBe` Nothing
      posixDirectoryModeBits Private `shouldBe` Just 0o700
      -- Execute bits on directories mean traversal, not program execution;
      -- executable modes are meaningless for directory routes:
      posixDirectoryModeBits Executable `shouldBe` Nothing
      posixDirectoryModeBits PrivateExecutable `shouldBe` Nothing
      posixDirectoryModeBits ReadOnly `shouldBe` Just 0o555

  describe "RouteKind" $ do
    specify "parseRouteKind" $ do
      parseRouteKind "copy" `shouldBe` Just CopyRoute
      parseRouteKind "symlink" `shouldBe` Just SymlinkRoute
      parseRouteKind "" `shouldBe` Nothing
      parseRouteKind "Copy" `shouldBe` Nothing
      parseRouteKind "link" `shouldBe` Nothing

    specify "renderRouteKind round-trips through parseRouteKind" $ do
      let kinds = [minBound .. maxBound] :: [RouteKind]
      [parseRouteKind $ renderRouteKind k | k <- kinds]
        `shouldBe` [Just k | k <- kinds]

  describe "RouteTarget" $ do
    specify "routeTarget defaults to a plain copied route" $ do
      let expr = BareComponent "foo"
      routeTarget expr `shouldBe` RouteTarget expr DefaultMode CopyRoute
      (routeTarget expr).expression `shouldBe` expr
      (routeTarget expr).mode `shouldBe` DefaultMode
      (routeTarget expr).kind `shouldBe` CopyRoute
