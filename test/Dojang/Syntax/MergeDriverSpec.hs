{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.MergeDriverSpec (spec) where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.FilePath qualified as FilePath
import System.OsPath (encodeFS)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Dojang.Syntax.MergeDriver
  ( Error (..)
  , formatError
  , readMergeDriverConfig
  , readMergeDriverConfigFile
  )
import Dojang.TestUtils (withTempDir)
import Dojang.Types.MergeDriver
  ( MergeDriverConfig (..)
  , MergeDriverConfigurationError (..)
  , MergeDriverLookupError (..)
  , MergeDriverNameError (..)
  , MergeDriverSpec (..)
  , lookupMergeDriver
  , renderMergeDriverName
  )


spec :: Spec
spec = do
  describe "readMergeDriverConfig" $ do
    it "reads a default driver and its complete structured command" $ do
      config <- parse validDocument
      renderMergeDriverName config.defaultDriver `shouldBe` "git"
      (_, driver) <- lookup' Nothing config
      driver.command
        `shouldBe` ("git" :| ["merge-file", "{result}", "{base}", "{source}"])
      driver.inheritedEnvironment `shouldBe` ["PATH", "HOME"]
      driver.environment `shouldBe` Map.singleton "LC_ALL" "C"
      driver.unresolvedExitCodes `shouldBe` [1, 3]
      driver.canceledExitCodes `shouldBe` [2]

    it "allows an explicit driver to override the configured default" $ do
      config <-
        parse $
          validDocument
            <> "\n[merge-drivers.gui]\n"
            <> "command = [\"gui\", \"{source}\", \"{base}\", "
            <> "\"{destination}\", \"{result}\"]\n"
            <> "unresolved-exit-codes = []\n"
            <> "canceled-exit-codes = [130]\n"
      (name, _) <- lookup' (Just "gui") config
      renderMergeDriverName name `shouldBe` "gui"

    it "rejects invalid or missing default drivers" $ do
      readMergeDriverConfig
        ( "default-driver = \"2way\"\n"
            <> "[merge-drivers.git]\n"
            <> validDriver
        )
        `shouldBe` Left
          (InvalidDefaultMergeDriverName "2way" InvalidMergeDriverNameStart)
      readMergeDriverConfig
        ( "default-driver = \"missing\"\n"
            <> "[merge-drivers.git]\n"
            <> validDriver
        )
        `shouldBe` Left (UnknownDefaultMergeDriver "missing")

    it "rejects missing outcome declarations and malformed drivers" $ do
      readMergeDriverConfig
        ( "default-driver = \"git\"\n"
            <> "[merge-drivers.git]\n"
            <> "command = [\"git\", \"{source}\", \"{base}\", \"{result}\"]\n"
        )
        `shouldSatisfy` \case
          Left (TomlErrors _) -> True
          _ -> False
      readMergeDriverConfig
        ( "default-driver = \"git\"\n"
            <> "[merge-drivers.git]\n"
            <> "command = [\"git\", \"{source}\", \"{result}\"]\n"
            <> "unresolved-exit-codes = [1]\n"
            <> "canceled-exit-codes = [2]\n"
        )
        `shouldBe` Left
          (InvalidMergeDriver "git" MissingMergeBasePlaceholder)

    it "rejects unknown fields instead of accepting typos" $
      readMergeDriverConfig
        ( "default-driver = \"git\"\n"
            <> "default-drivr = \"git\"\n"
            <> "\n[merge-drivers.git]\n"
            <> validDriver
        )
        `shouldSatisfy` \case
          Left (TomlWarnings (_ : _)) -> True
          _ -> False

    it "reports invalid and unknown explicit lookups" $ do
      config <- parse validDocument
      lookupMergeDriver (Just "") config
        `shouldBe` Left
          (InvalidLookupMergeDriverName EmptyMergeDriverName)
      lookupMergeDriver (Just "missing") config
        `shouldBe` Left (UnknownMergeDriverName "missing")

  describe "readMergeDriverConfigFile" $ do
    it "reads a valid UTF-8 configuration" $
      withTempDir $ \_ tmpDirString -> do
        let nativePath = tmpDirString FilePath.</> "merge-drivers.toml"
        path <- encodeFS nativePath
        ByteString.writeFile nativePath validDocumentBytes
        result <- readMergeDriverConfigFile path
        result `shouldSatisfy` \case
          Right _ -> True
          Left _ -> False

    it "rejects invalid UTF-8" $
      withTempDir $ \_ tmpDirString -> do
        let nativePath = tmpDirString FilePath.</> "merge-drivers.toml"
        path <- encodeFS nativePath
        ByteString.writeFile nativePath "\x80"
        result <- readMergeDriverConfigFile path
        result `shouldSatisfy` \case
          Left (InvalidUtf8 _) -> True
          _ -> False

  describe "formatError" $
    it "produces a user-facing default-driver diagnostic" $
      formatError (UnknownDefaultMergeDriver "missing")
        `shouldBe` "The default merge driver 'missing' is not configured."
 where
  parse source = case readMergeDriverConfig source of
    Left err -> fail $ show err
    Right value -> return value
  lookup' requested config = case lookupMergeDriver requested config of
    Left err -> fail $ show err
    Right value -> return value


validDocument :: Text.Text
validDocument =
  "default-driver = \"git\"\n"
    <> "\n"
    <> "[merge-drivers.git]\n"
    <> validDriver
    <> "inherit-environment = [\"PATH\", \"HOME\"]\n"
    <> "\n"
    <> "[merge-drivers.git.environment]\n"
    <> "LC_ALL = \"C\"\n"


validDriver :: Text.Text
validDriver =
  "command = [\"git\", \"merge-file\", \"{result}\", \"{base}\", \"{source}\"]\n"
    <> "unresolved-exit-codes = [1, 3]\n"
    <> "canceled-exit-codes = [2]\n"


validDocumentBytes :: ByteString.ByteString
validDocumentBytes = Text.encodeUtf8 validDocument
