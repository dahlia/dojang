{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.EnvironmentPredicate.WriterSpec (spec) where

import Data.Text (Text, unpack)
import Test.Hspec (Spec, describe, it, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Hedgehog (annotateShow, footnote, forAll, hedgehog, (===))

import Dojang.Syntax.EnvironmentPredicate.Parser
  ( errorBundlePretty
  , parseEnvironmentPredicate
  )
import Dojang.Syntax.EnvironmentPredicate.Writer (writeEnvironmentPredicate)
import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (..)
  , normalizePredicate
  )
import Dojang.Types.Gen qualified as Gen
import Dojang.Types.MonikerName (parseMonikerName)


write :: EnvironmentPredicate -> Text
write = writeEnvironmentPredicate


spec :: Spec
spec = do
  describe "writeEnvironmentPredicate" $ do
    specify "simple expressions" $ do
      write Always `shouldBe` "always"
      write (Not Always) `shouldBe` "never"
      write (OperatingSystem "linux") `shouldBe` "os = linux"
      write (Architecture "x86_64")
        `shouldBe` "arch = \"x86_64\""
      let Right fooBar = parseMonikerName "foo-bar"
      write (Moniker fooBar) `shouldBe` "moniker = \"foo-bar\""

    it "escape special characters in string literal" $ do
      write (OperatingSystem "\\'\"\b\f\n\r\t\v\0\x01\xff\xffff")
        `shouldBe` "os = \"\\\\'\\\"\\b\\f\\n\\r\\t\\v\\0\\x01\\xff\\uffff\""
      write (OperatingSystem "\\'\b\f\n\r\t\v\0\x01\xff\xffff")
        `shouldBe` "os = \"\\\\'\\b\\f\\n\\r\\t\\v\\0\\x01\\xff\\uffff\""

    specify "string literal is around by quotes if needed" $ do
      write (OperatingSystem "test") `shouldBe` "os = test"
      write (OperatingSystem "te st") `shouldBe` "os = \"te st\""
      write (OperatingSystem "") `shouldBe` "os = \"\""

    specify "string literal is around by single quotes if it has double quote"
      $ do
        write (OperatingSystem "\"test\"") `shouldBe` "os = '\"test\"'"
        write (OperatingSystem "\"test'") `shouldBe` "os = \"\\\"test'\""

    specify "homogenous predicates in Or are expressed using in operator" $ do
      write (Or [OperatingSystem "linux", OperatingSystem "macos"])
        `shouldBe` "os in (linux, macos)"
      write (Or [Architecture "x86_64", Architecture "aarch64"])
        `shouldBe` "arch in (\"x86_64\", aarch64)"
      let Right foo = parseMonikerName "foo"
      let Right bar = parseMonikerName "bar"
      write (Or [Moniker foo, Moniker bar]) `shouldBe` "moniker in (foo, bar)"

    specify
      ( "homogenous negative predicates in And are expressed using "
          ++ "not in operator"
      )
      $ do
        write
          (And [Not $ OperatingSystem "linux", Not $ OperatingSystem "macos"])
          `shouldBe` "os not in (linux, macos)"
        write (And [Not $ Architecture "x86_64", Not $ Architecture "aarch64"])
          `shouldBe` "arch not in (\"x86_64\", aarch64)"
        let Right foo = parseMonikerName "foo"
        let Right bar = parseMonikerName "bar"
        write (And [Not $ Moniker foo, Not $ Moniker bar])
          `shouldBe` "moniker not in (foo, bar)"

    it "properly wrap sub-expressions with parentheses" $ do
      let Right foo = parseMonikerName "foo"
      let Right bar = parseMonikerName "bar"
      write
        ( And
            [ Or [OperatingSystem "linux", Architecture "aarch64"]
            , Or [Moniker foo, Moniker bar]
            ]
        )
        `shouldBe` "(os = linux || arch = aarch64) && moniker in (foo, bar)"

    specify "yields what parseEnvironmentPredicate can parse" $ hedgehog $ do
      predicate <- normalizePredicate <$> forAll Gen.environmentPredicate
      let predText = write predicate
      annotateShow predText
      let parseResult = parseEnvironmentPredicate "test" predText
      case parseResult of
        Left err -> footnote $ unpack $ errorBundlePretty err
        Right _ -> return ()
      let Right pred' = parseResult
      pred' === predicate
