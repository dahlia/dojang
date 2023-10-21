module Dojang.Syntax.Manifest.WriterSpec (spec) where

import Data.Text (unpack)
import Test.Hspec (Spec, specify)
import Test.Hspec.Hedgehog (annotate, annotateShow, forAll, hedgehog, (===))

import Dojang.Syntax.Manifest.Parser (readManifest)
import Dojang.Syntax.Manifest.Writer (writeManifest)
import Dojang.Types.Gen as Gen (manifest)


spec :: Spec
spec = do
  specify "writeManifest" $ hedgehog $ do
    manifest' <- forAll Gen.manifest
    let toml = writeManifest manifest'
    annotate $ unpack toml
    let Right (parsed, _) = readManifest toml
    annotateShow parsed
    parsed === manifest'
