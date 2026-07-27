{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.MergeDriverSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Hedgehog (forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Exit (ExitCode (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog, (===))

import Dojang.Types.ExternalCommand (EnvironmentNameCase (..))
import Dojang.Types.MergeDriver
  ( MergeDriverConfigurationError (..)
  , MergeDriverExit (..)
  , MergeDriverNameError (..)
  , classifyMergeDriverExit
  , expandMergeDriverCommandNative
  , makeMergeDriverSpec
  , parseMergeDriverName
  , renderMergeDriverName
  , resolveMergeDriverEnvironmentNative
  )


spec :: Spec
spec = do
  describe "parseMergeDriverName" $ do
    it "accepts and round-trips arbitrary portable names" $
      hedgehog $ do
        first <- forAll $ Gen.element $ ['a' .. 'z'] <> ['A' .. 'Z']
        rest <-
          forAll $
            Gen.string
              (Range.linear 0 80)
              (Gen.element $ ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_")
        let name = Text.pack $ first : rest
        (renderMergeDriverName <$> parseMergeDriverName name)
          === Right name

    it "rejects empty, digit-leading, and nonportable names" $ do
      parseMergeDriverName "" `shouldBe` Left EmptyMergeDriverName
      parseMergeDriverName "2way" `shouldBe` Left InvalidMergeDriverNameStart
      parseMergeDriverName "three way"
        `shouldBe` Left InvalidMergeDriverNameCharacter

  describe "makeMergeDriverSpec" $ do
    it "accepts an in-place result with an optional destination argument" $ do
      let withoutDestination =
            makeMergeDriverSpec
              ["git", "merge-file", "{result}", "{base}", "{source}"]
              ["PATH"]
              Map.empty
              [1]
              [2]
          withDestination =
            makeMergeDriverSpec
              [ "tool"
              , "{source}"
              , "{base}"
              , "{destination}"
              , "{result}"
              ]
              []
              Map.empty
              [3]
              []
      withoutDestination `shouldSatisfy` isRight
      withDestination `shouldSatisfy` isRight

    it "requires whole source, base, and result placeholders exactly once" $ do
      invalid ["tool", "{base}", "{result}"]
        `shouldBe` Left MissingMergeSourcePlaceholder
      invalid ["tool", "{source}", "{source}", "{base}", "{result}"]
        `shouldBe` Left DuplicateMergeSourcePlaceholder
      invalid ["tool", "{source}", "{result}"]
        `shouldBe` Left MissingMergeBasePlaceholder
      invalid ["tool", "{source}", "{base}", "{base}", "{result}"]
        `shouldBe` Left DuplicateMergeBasePlaceholder
      invalid ["tool", "{source}", "{base}"]
        `shouldBe` Left MissingMergeResultPlaceholder
      invalid ["tool", "{source}", "{base}", "{result}", "{result}"]
        `shouldBe` Left DuplicateMergeResultPlaceholder

    it "allows at most one destination placeholder" $
      invalid
        [ "tool"
        , "{source}"
        , "{base}"
        , "{destination}"
        , "{destination}"
        , "{result}"
        ]
        `shouldBe` Left DuplicateMergeDestinationPlaceholder

    it "rejects embedded placeholders and placeholders in the executable" $ do
      invalid ["{source}", "{base}", "{result}"]
        `shouldBe` Left (MergePlaceholderInExecutable "{source}")
      invalid ["tool", "prefix-{source}", "{base}", "{result}"]
        `shouldBe` Left (EmbeddedMergePlaceholder "prefix-{source}")

    it "requires disjoint positive exit-code lists without duplicates" $ do
      make [0] [] `shouldBe` Left (InvalidMergeDriverExitCode 0)
      make [-1] [] `shouldBe` Left (InvalidMergeDriverExitCode (-1))
      make [1, 1] [] `shouldBe` Left (DuplicateMergeDriverExitCode 1)
      make [1] [1] `shouldBe` Left (AmbiguousMergeDriverExitCode 1)

  describe "expandMergeDriverCommandNative" $
    it "preserves arbitrary native paths as complete arguments" $
      hedgehog $ do
        source <- forAll nativePath
        base <- forAll nativePath
        destination <- forAll nativePath
        result <- forAll nativePath
        let Right driver =
              makeMergeDriverSpec
                [ "tool"
                , "--source"
                , "{source}"
                , "--base"
                , "{base}"
                , "--destination"
                , "{destination}"
                , "--result"
                , "{result}"
                ]
                []
                Map.empty
                [1]
                [2]
        expandMergeDriverCommandNative
          driver
          source
          base
          destination
          result
          === ( "tool"
              ,
                [ "--source"
                , source
                , "--base"
                , base
                , "--destination"
                , destination
                , "--result"
                , result
                ]
              )

  describe "classifyMergeDriverExit" $ do
    it "distinguishes success, unresolved, cancellation, and failure" $ do
      let driver = validSpec [1, 3] [2]
      classifyMergeDriverExit driver ExitSuccess
        `shouldBe` MergeDriverResolved
      classifyMergeDriverExit driver (ExitFailure 3)
        `shouldBe` MergeDriverUnresolved
      classifyMergeDriverExit driver (ExitFailure 2)
        `shouldBe` MergeDriverCanceled
      classifyMergeDriverExit driver (ExitFailure 4)
        `shouldBe` MergeDriverFailed 4

  describe "resolveMergeDriverEnvironmentNative" $ do
    it "preserves case-distinct inherited values on POSIX" $ do
      let Right driver =
            makeMergeDriverSpec
              ["tool", "{source}", "{base}", "{result}"]
              ["http_proxy", "HTTP_PROXY"]
              Map.empty
              []
              []
      resolveMergeDriverEnvironmentNative
        CaseSensitiveEnvironment
        [("http_proxy", "lower"), ("HTTP_PROXY", "upper"), ("TOKEN", "secret")]
        driver
        `shouldBe` [("HTTP_PROXY", "upper"), ("http_proxy", "lower")]

    it "lets fixed values override inherited values on Windows" $ do
      let Right driver =
            makeMergeDriverSpec
              ["tool", "{source}", "{base}", "{result}"]
              ["Path"]
              (Map.singleton "PATH" "fixed")
              []
              []
      resolveMergeDriverEnvironmentNative
        CaseInsensitiveEnvironment
        [("Path", "host")]
        driver
        `shouldBe` [("PATH", "fixed")]

    it "resolves case-colliding fixed names deterministically on Windows" $ do
      let Right driver =
            makeMergeDriverSpec
              ["tool", "{source}", "{base}", "{result}"]
              []
              (Map.fromList [("PATH", "upper"), ("Path", "title")])
              []
              []
      resolveMergeDriverEnvironmentNative
        CaseInsensitiveEnvironment
        []
        driver
        `shouldBe` [("Path", "title")]
 where
  invalid command =
    makeMergeDriverSpec command [] Map.empty [1] [2]
  make unresolved canceled =
    makeMergeDriverSpec
      ["tool", "{source}", "{base}", "{result}"]
      []
      Map.empty
      unresolved
      canceled
  validSpec unresolved canceled =
    case make unresolved canceled of
      Right value -> value
      Left err -> error $ show err
  nativePath =
    Gen.string
      (Range.linear 0 200)
      (Gen.filter (/= '\NUL') Gen.unicodeAll)
  isRight (Right _) = True
  isRight _ = False
