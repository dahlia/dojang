{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# HLINT ignore "Use min" #-}
{-# HLINT ignore "Use max" #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Dojang.Types.EnvironmentSpec (spec) where

import Dojang.Types.Environment
  ( Architecture (..)
  , OperatingSystem (..)
  )
import Dojang.Types.Gen qualified as Gen

import Data.CaseInsensitive (original)
import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.String (IsString (fromString))
import Data.Text (unpack)
import Hedgehog.Gen (unicodeAll)
import Hedgehog.Range (constantFrom)
import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (/==), (===))


spec :: Spec
spec = do
  describe "OperatingSystem" $ do
    specify "HasField \"identifier\"" $ do
      (Linux).identifier `shouldBe` "linux"
      (OtherOS "other-os").identifier `shouldBe` "other-os"

    specify "IsString" $ do
      fromString "linux" `shouldBe` Linux
      fromString "other-os" `shouldBe` OtherOS "other-os"

    specify "HasField \"identifier\", IsString" $ hedgehog $ do
      os' <- forAll Gen.operatingSystem
      fromString (unpack $ original os'.identifier) === os'

    specify "Eq" $ hedgehog $ do
      os <- forAll Gen.operatingSystem
      os' <- forAll Gen.operatingSystem
      (os == os') /== (os /= os')

    specify "Ord" $ do
      hedgehog $ do
        os <- forAll Gen.operatingSystem
        os' <- forAll Gen.operatingSystem
        (os <= os') /== (os > os')
        (os < os') /== (os >= os')
        (os <= os) === True
        (os >= os) === True
      hedgehog $ do
        a <- forAll $ Gen.ciText (constantFrom 0 0 256) unicodeAll
        b <- forAll $ Gen.ciText (constantFrom 0 0 256) unicodeAll
        compare (OtherOS a) (OtherOS b) === compare a b
        min (OtherOS a) (OtherOS b) === OtherOS (min a b)
        max (OtherOS a) (OtherOS b) === OtherOS (max a b)

    specify "Read, Show" $ hedgehog $ do
      os <- forAll Gen.operatingSystem
      read (show os) === os
      read (showList [os] "") === [os]

    specify "Hashable" $ hedgehog $ do
      os <- forAll Gen.operatingSystem
      os' <- forAll Gen.operatingSystem
      (hashWithSalt 0 os == hashWithSalt 0 os') === (os == os')

  describe "Architecture" $ do
    specify "HasField \"identifier\"" $ do
      (X86_64).identifier `shouldBe` "x86_64"
      (Etc "etc").identifier `shouldBe` "etc"

    specify "IsString" $ do
      fromString "x86_64" `shouldBe` X86_64
      fromString "etc" `shouldBe` Etc "etc"

    specify "HasField \"identifier\", IsString" $ hedgehog $ do
      arch <- forAll Gen.architecture
      fromString (unpack $ original arch.identifier) === arch

    specify "Eq" $ hedgehog $ do
      arch <- forAll Gen.architecture
      arch' <- forAll Gen.architecture
      (arch == arch') /== (arch /= arch')

    specify "Ord" $ do
      hedgehog $ do
        arch <- forAll Gen.architecture
        arch' <- forAll Gen.architecture
        (arch <= arch') /== (arch > arch')
        (arch < arch') /== (arch >= arch')
        (arch <= arch) === True
        (arch >= arch) === True
      hedgehog $ do
        a <- forAll $ Gen.ciText (constantFrom 0 0 256) unicodeAll
        b <- forAll $ Gen.ciText (constantFrom 0 0 256) unicodeAll
        compare (Etc a) (Etc b) === compare a b
        min (Etc a) (Etc b) === Etc (min a b)
        max (Etc a) (Etc b) === Etc (max a b)

    specify "Read, Show" $ hedgehog $ do
      arch <- forAll Gen.architecture
      read (show arch) === arch
      read (showList [arch] "") === [arch]

    specify "Hashable" $ hedgehog $ do
      arch <- forAll Gen.architecture
      arch' <- forAll Gen.architecture
      (hashWithSalt 0 arch == hashWithSalt 0 arch') === (arch == arch')

  describe "Environment" $ do
    specify "Eq" $ hedgehog $ do
      env <- forAll Gen.environment
      env' <- forAll Gen.environment
      (env == env') /== (env /= env')

    specify "Ord" $ hedgehog $ do
      env <- forAll Gen.environment
      env' <- forAll Gen.environment
      (env <= env') /== (env > env')
      (env < env') /== (env >= env')
      (env <= env) === True
      (env >= env) === True
      min env env' === if env <= env' then env else env'
      max env env' === if env >= env' then env else env'

    specify "Read, Show" $ hedgehog $ do
      env <- forAll Gen.environment
      read (show env) === env
      read (showList [env] "") === [env]

    specify "Hashable" $ hedgehog $ do
      env <- forAll Gen.environment
      env' <- forAll Gen.environment
      (hash env == hash env') === (env == env')
      (hashWithSalt 0 env == hashWithSalt 0 env') === (env == env')
