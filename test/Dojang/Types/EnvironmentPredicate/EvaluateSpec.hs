{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.EnvironmentPredicate.EvaluateSpec (spec) where

import Data.HashMap.Strict (HashMap)
import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (/==), (===))

import Dojang.Gen qualified as Gen
import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (Environment)
  , OperatingSystem (..)
  )
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.EnvironmentPredicate.Evaluate
  ( EvaluationWarning (..)
  , evaluate
  )
import Dojang.Types.MonikerName (MonikerName, parseMonikerName)


spec :: Spec
spec = do
  describe "EvaluationWarning" $ do
    specify "Eq" $ hedgehog $ do
      warning <- forAll Gen.evaluationWarning
      warning' <- forAll Gen.evaluationWarning
      (warning == warning') /== (warning /= warning')

    specify "Show" $ hedgehog $ do
      warning <- forAll Gen.evaluationWarning
      warning' <- forAll Gen.evaluationWarning
      showList [warning, warning'] ""
        === ("[" ++ show warning ++ "," ++ show warning' ++ "]")

  describe "evaluate" $ do
    let (Right linuxAmd64) = parseMonikerName "linux-amd64"
    let (Right linuxArm64) = parseMonikerName "linux-arm64"
    let (Right macosArm64) = parseMonikerName "macos-arm64"
    let (Right linuxAmd64') = parseMonikerName "linux-amd64-alias"
    let (Right nonExistentMoniker) = parseMonikerName "non-existent"
    let monikerMap =
          [ (linuxAmd64, And [OperatingSystem Linux, Architecture X86_64])
          , (linuxArm64, And [OperatingSystem Linux, Architecture AArch64])
          , (macosArm64, And [OperatingSystem MacOS, Architecture AArch64])
          , (linuxAmd64', Moniker linuxAmd64)
          ]
            :: HashMap MonikerName EnvironmentPredicate
    let environment = Environment Linux X86_64
    let eval = evaluate environment monikerMap

    specify "Always"
      $ eval Always
      `shouldBe` (True, [])

    specify "OperatingSystem" $ do
      eval (OperatingSystem Linux) `shouldBe` (True, [])
      eval (OperatingSystem MacOS) `shouldBe` (False, [])
      eval (OperatingSystem $ OtherOS "other")
        `shouldBe` ( False
                   , [UnrecognizedOperatingSystem $ OtherOS "other"]
                   )

    specify "Architecture" $ do
      eval (Architecture X86_64) `shouldBe` (True, [])
      eval (Architecture AArch64) `shouldBe` (False, [])
      eval (Architecture $ Etc "etc")
        `shouldBe` ( False
                   , [UnrecognizedArchitecture $ Etc "etc"]
                   )

    specify "Moniker" $ do
      eval (Moniker linuxAmd64) `shouldBe` (True, [])
      eval (Moniker linuxAmd64') `shouldBe` (True, [])
      eval (Moniker linuxArm64) `shouldBe` (False, [])
      eval (Moniker macosArm64) `shouldBe` (False, [])
      eval (Moniker nonExistentMoniker)
        `shouldBe` (False, [UndefinedMoniker nonExistentMoniker])

    specify "Not" $ do
      eval (Not $ OperatingSystem Linux) `shouldBe` (False, [])
      eval (Not $ Architecture X86_64) `shouldBe` (False, [])
      eval (Not $ Moniker linuxAmd64) `shouldBe` (False, [])
      eval (Not $ Moniker linuxArm64) `shouldBe` (True, [])
      eval (Not $ Moniker linuxAmd64') `shouldBe` (False, [])

    specify "And" $ do
      eval (And [OperatingSystem Linux]) `shouldBe` (True, [])
      eval (And [Architecture X86_64]) `shouldBe` (True, [])
      eval (And [OperatingSystem Linux, Architecture X86_64])
        `shouldBe` (True, [])
      eval
        (And [OperatingSystem Linux, Architecture X86_64, Moniker linuxAmd64])
        `shouldBe` (True, [])
      eval
        (And [OperatingSystem Linux, Architecture AArch64, Moniker linuxArm64])
        `shouldBe` (False, [])
      eval
        ( And
            [ OperatingSystem Linux
            , Architecture X86_64
            , Moniker nonExistentMoniker
            ]
        )
        `shouldBe` (False, [UndefinedMoniker nonExistentMoniker])
      eval
        (And [OperatingSystem Linux, Architecture AArch64])
        `shouldBe` (False, [])
      eval
        (And [OperatingSystem MacOS, Architecture X86_64])
        `shouldBe` (False, [])
      eval
        (And [OperatingSystem MacOS, Architecture AArch64])
        `shouldBe` (False, [])

    specify "Or"
      $ do
        eval (Or [OperatingSystem Linux]) `shouldBe` (True, [])
        eval (Or [Architecture X86_64]) `shouldBe` (True, [])
        eval
          (Or [OperatingSystem Linux, Architecture X86_64])
          `shouldBe` (True, [])
        eval
          (Or [OperatingSystem Linux, Architecture X86_64, Moniker linuxAmd64])
          `shouldBe` (True, [])
        eval
          (Or [OperatingSystem Linux, Architecture AArch64, Moniker linuxArm64])
          `shouldBe` (True, [])
        eval
          ( Or
              [ OperatingSystem Linux
              , Architecture X86_64
              , Moniker nonExistentMoniker
              ]
          )
          `shouldBe` (True, [UndefinedMoniker nonExistentMoniker])
