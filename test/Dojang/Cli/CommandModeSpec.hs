{-# LANGUAGE ImportQualifiedPost #-}

module Dojang.Cli.CommandModeSpec (spec) where

import Hedgehog.Gen qualified as Gen
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.Cli.CommandMode
  ( CommandMode (CommandMode)
  , ambiguousRepositoryExitCode
  , environmentCommandMode
  , initializationCommandMode
  , metaCommandMode
  , migrationCommandMode
  , repositoryCommandMode
  )
import Dojang.ExitCodes (machineStateError)


spec :: Spec
spec = describe "command repository selection" $ do
  it "validates state for init without automatically selecting a checkout" $
    initializationCommandMode `shouldBe` CommandMode True False

  it "validates state for migrate without automatically selecting a checkout" $
    migrationCommandMode `shouldBe` CommandMode True False

  it "requires a repository only when env reads the repository env file" $
    hedgehog $ do
      ignoreEnvFile <- forAll Gen.bool
      environmentCommandMode ignoreEnvFile
        === if ignoreEnvFile then metaCommandMode else repositoryCommandMode

  it "uses the machine-state exit code for ambiguous repositories" $
    ambiguousRepositoryExitCode `shouldBe` machineStateError
