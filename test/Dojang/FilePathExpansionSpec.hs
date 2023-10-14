{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.FilePathExpansionSpec
  ( spec
  ) where

import Dojang.FilePathExpansion (expandFilePath)
import Dojang.Types.FilePathExpression (FilePathExpression (..), (++), (+/+))

import Data.Text (Text, unpack)
import GHC.IO.Unsafe (unsafePerformIO)
import System.OsPath (pathSeparator, (</>))
import System.OsString (OsString, encodeUtf, pack)
import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)

import Prelude hiding ((++))


encode :: Text -> IO OsString
encode = encodeUtf . unpack


os :: String -> OsString
os = unsafePerformIO . encodeUtf


spec :: Spec
spec = do
  describe "expandFilePath" $ do
    specify "BareComponent" $ do
      expanded <- expandFilePath (BareComponent "foo") undefined encode
      expanded `shouldBe` os "foo"
    specify "Root" $ do
      expanded <- expandFilePath (Root Nothing) undefined undefined
      expanded `shouldBe` pack [pathSeparator]
      expanded' <- expandFilePath (Root $ Just 'C') undefined undefined
      expanded' `shouldBe` (os "C:" <> pack [pathSeparator])
      expandedAbs <-
        expandFilePath
          (Root Nothing +/+ BareComponent "foo" +/+ BareComponent "bar")
          undefined
          encode
      expandedAbs `shouldBe` pack [pathSeparator] </> os "foo" </> os "bar"
      expandedAbs' <-
        expandFilePath
          (Root (Just 'C') +/+ BareComponent "foo" +/+ BareComponent "bar")
          undefined
          encode
      expandedAbs'
        `shouldBe` os "C:"
        <> pack [pathSeparator]
        </> os "foo"
        </> os "bar"
    specify "Concatenation" $ do
      expanded <-
        expandFilePath
          (Substitution "FOO" ++ BareComponent "bar")
          (\e -> return $ if e == "FOO" then Just (os "baz") else Nothing)
          encode
      expanded `shouldBe` os "baz" <> os "bar"
      expanded' <-
        expandFilePath
          (BareComponent "foo" ++ Substitution "BAR")
          (\e -> return $ if e == "BAR" then Just (os "qux") else Nothing)
          encode
      expanded' `shouldBe` os "foo" <> os "qux"
    specify "PathSeparator" $ do
      expanded <-
        expandFilePath
          (Substitution "FOO" +/+ BareComponent "bar")
          (\e -> return $ if e == "FOO" then Just (os "baz") else Nothing)
          encode
      expanded `shouldBe` os "baz" </> os "bar"
      expanded' <-
        expandFilePath
          (BareComponent "foo" +/+ Substitution "BAR")
          (\e -> return $ if e == "BAR" then Just (os "qux") else Nothing)
          encode
      expanded' `shouldBe` os "foo" </> os "qux"
    specify "Substitution" $ do
      expanded <-
        expandFilePath
          (Substitution "FOO")
          (\e -> return $ if e == "FOO" then Just (os "bar") else Nothing)
          encode
      expanded `shouldBe` os "bar"
      notFound <-
        expandFilePath
          (Substitution "FOO")
          (const $ return Nothing)
          encode
      notFound `shouldBe` mempty
    specify "SubstitutionWithDefault" $ do
      expanded <-
        expandFilePath
          (SubstitutionWithDefault "FOO" (BareComponent "baz"))
          (\e -> return $ if e == "FOO" then Just (os "bar") else Nothing)
          encode
      expanded `shouldBe` os "bar"
      empty <-
        expandFilePath
          (SubstitutionWithDefault "FOO" (Substitution "BAR"))
          (\e -> return $ Just $ if e == "FOO" then mempty else os "qux")
          encode
      empty `shouldBe` os "qux"
      notFound <-
        expandFilePath
          (SubstitutionWithDefault "FOO" (BareComponent "bar"))
          (\e -> return $ if e == "FOO" then Nothing else Just $ os "qux")
          encode
      notFound `shouldBe` os "bar"
    specify "ConditionalSubstitution" $ do
      expanded <-
        expandFilePath
          (ConditionalSubstitution "FOO" (BareComponent "baz"))
          (\e -> return $ if e == "FOO" then Just (os "bar") else Nothing)
          encode
      expanded `shouldBe` os "baz"
      expanded' <-
        expandFilePath
          (ConditionalSubstitution "FOO" (Substitution "BAZ"))
          ( return . \case
              "FOO" -> Just (os "bar")
              "BAZ" -> Just (os "qux")
              _ -> Nothing
          )
          encode
      expanded' `shouldBe` os "qux"
      empty <-
        expandFilePath
          (ConditionalSubstitution "FOO" (BareComponent "baz"))
          (const $ return $ Just mempty)
          encode
      empty `shouldBe` mempty
      notFound <-
        expandFilePath
          (ConditionalSubstitution "FOO" (BareComponent "baz"))
          (const $ return Nothing)
          encode
      notFound `shouldBe` mempty
