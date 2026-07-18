{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.ManifestVariableSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsString (OsString, encodeUtf)
import Test.Hspec (Spec, expectationFailure, specify)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldSatisfy
  )
import Test.Hspec.Hedgehog (annotateShow, assert, forAll, hedgehog, (===))

import Dojang.Types.Environment
  ( Environment
  , Kernel (Kernel)
  , emptyEnvironment
  , parseFactKey
  )
import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (Always, Fact, OperatingSystem)
  )
import Dojang.Types.FilePathExpression (FilePathExpression (..))
import Dojang.Types.FilePathExpression.Expansion
  ( ExpansionWarning (UndefinedEnvironmentVariable)
  , VariableLookup (..)
  )

import Dojang.Types.ManifestVariable
  ( ManifestVariable
  , ManifestVariableName
  , VariableResolutionError (..)
  , dispatchManifestVariable
  , formatVariableResolutionError
  , lookupVariable
  , manifestVariable
  , manifestVariablePreservingOrder
  , parseManifestVariableName
  , renderManifestVariableName
  , resolveManifestVariables
  )


os :: String -> IO OsString
os = encodeUtf


name :: Text.Text -> ManifestVariableName
name source = case parseManifestVariableName source of
  Right parsed -> parsed
  Left message -> error $ Text.unpack message


linuxEnvironment :: Environment
linuxEnvironment =
  emptyEnvironment "linux" "x86_64" $ Kernel "Linux" "6.0.0"


inherited :: [(Text.Text, String)] -> Text.Text -> IO (Maybe OsString)
inherited values variable = traverse os $ lookup variable values


spec :: Spec
spec = do
  specify "accepts portable process-environment variable names" $ do
    renderManifestVariableName <$> parseManifestVariableName "CONFIG_HOME"
      `shouldBe` Right "CONFIG_HOME"
    renderManifestVariableName <$> parseManifestVariableName "_private2"
      `shouldBe` Right "_private2"

  specify "rejects nonportable manifest variable names" $ do
    traverse parseManifestVariableName ["", "2HOME", "HOME-DIR", "홈"]
      `shouldBe` Left "Manifest variable names must match [A-Za-z_][A-Za-z0-9_]*."

  specify "round-trips generated portable names" $ hedgehog $ do
    first <- forAll $ Gen.element $ '_' : ['A' .. 'Z'] ++ ['a' .. 'z']
    rest <-
      forAll $
        Gen.text
          (Range.linear 0 64)
          (Gen.element $ '_' : ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'])
    let source = Text.cons first rest
    fmap renderManifestVariableName (parseManifestVariableName source)
      === Right source

  specify "prefers selected manifest values and falls back when none match" $ do
    manifestValue <- os "manifest"
    processValue <- os "process"
    let variables =
          Map.fromList
            [ (name "SELECTED", manifestVariable mempty $ BareComponent "manifest")
            ,
              ( name "UNSELECTED"
              , manifestVariablePreservingOrder
                  (const Nothing)
                  ((OperatingSystem "windows", BareComponent "windows") :| [])
              )
            ]
    resolved <-
      resolveManifestVariables
        linuxEnvironment
        variables
        (inherited [("SELECTED", "process"), ("UNSELECTED", "process")])
    case resolved of
      Left err -> expectationFailure $ Text.unpack $ formatVariableResolutionError err
      Right environment -> do
        selected <- lookupVariable environment "SELECTED"
        unselected <- lookupVariable environment "UNSELECTED"
        selected.value `shouldBe` Just manifestValue
        unselected.value `shouldBe` Just processValue

  specify "stops evaluating conditional branches after the first match" $ do
    let Right factKey = parseFactKey "class"
        variable =
          manifestVariablePreservingOrder
            (const Nothing)
            ( (Always, BareComponent "selected")
                :| [(Fact factKey "work", BareComponent "unreachable")]
            )
    dispatchManifestVariable linuxEnvironment variable
      `shouldBe` (Just $ BareComponent "selected", [])

  specify "resolves dependencies independently of declaration order" $ hedgehog $ do
    count <- forAll $ Gen.int $ Range.linear 2 30
    shuffled <- forAll $ Gen.shuffle [0 .. count - 1]
    let variableName :: Int -> ManifestVariableName
        variableName index = name $ "V" <> Text.pack (show index)
        entry :: Int -> (ManifestVariableName, ManifestVariable)
        entry 0 =
          (variableName 0, manifestVariable mempty $ BareComponent "root")
        entry index =
          ( variableName index
          , manifestVariable mempty $
              Substitution $
                renderManifestVariableName $
                  variableName $
                    index - 1
          )
        variables = Map.fromList $ entry <$> shuffled
    resolved <-
      liftIO $
        resolveManifestVariables linuxEnvironment variables (const $ pure Nothing)
    case resolved of
      Left err -> annotateShow err >> assert False
      Right environment -> do
        result <-
          liftIO $
            lookupVariable environment $
              renderManifestVariableName $
                variableName $
                  count - 1
        expected <- liftIO $ os "root"
        result.value === Just expected

  specify "reports direct and indirect dependency cycles deterministically" $ do
    let direct =
          Map.singleton
            (name "A")
            (manifestVariable mempty $ Substitution "A")
        indirect =
          Map.fromList
            [ (name "A", manifestVariable mempty $ Substitution "B")
            , (name "B", manifestVariable mempty $ Substitution "C")
            , (name "C", manifestVariable mempty $ Substitution "B")
            ]
    directResult <-
      resolveManifestVariables linuxEnvironment direct (const $ pure Nothing)
    indirectResult <-
      resolveManifestVariables linuxEnvironment indirect (const $ pure Nothing)
    cycleMessage directResult
      `shouldSatisfy` Text.isInfixOf "A -> A"
    cycleMessage indirectResult
      `shouldSatisfy` Text.isInfixOf "A -> B -> C -> B"

  specify "does not follow an unused conditional substitution into a cycle" $ do
    let variables =
          Map.fromList
            [
              ( name "A"
              , manifestVariable mempty $
                  ConditionalSubstitution "MISSING" $
                    Substitution "B"
              )
            , (name "B", manifestVariable mempty $ Substitution "A")
            ]
    result <-
      resolveManifestVariables linuxEnvironment variables (const $ pure Nothing)
    case result of
      Left err -> expectationFailure $ Text.unpack $ formatVariableResolutionError err
      Right environment -> do
        a <- lookupVariable environment "A"
        a.value `shouldBe` Just mempty

  specify "propagates nested missing-value warnings without exposing values" $ do
    secret <- os "sensitive-value"
    let variables =
          Map.fromList
            [ (name "MISSING_REF", manifestVariable mempty $ Substitution "ABSENT")
            , (name "SECRET_REF", manifestVariable mempty $ Substitution "SECRET")
            ]
    result <-
      resolveManifestVariables
        linuxEnvironment
        variables
        (\variable -> pure $ if variable == "SECRET" then Just secret else Nothing)
    case result of
      Left err -> expectationFailure $ Text.unpack $ formatVariableResolutionError err
      Right environment -> do
        missing <- lookupVariable environment "MISSING_REF"
        secretResult <- lookupVariable environment "SECRET_REF"
        missing.warnings `shouldBe` [UndefinedEnvironmentVariable "ABSENT"]
        Map.member "var.SECRET_REF" secretResult.provenance `shouldBe` True
        Map.member "env.SECRET" secretResult.provenance `shouldBe` True
        Map.elems secretResult.provenance
          `shouldSatisfy` all (not . Text.isInfixOf "sensitive-value")


cycleMessage
  :: Either VariableResolutionError environment -> Text.Text
cycleMessage (Left err@VariableCycle{}) = formatVariableResolutionError err
cycleMessage _ = ""
