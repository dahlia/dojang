{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.MergeDriverSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.OsPath (encodeFS, (</>))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Dojang.MonadFileSystem qualified as FileSystem
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
      withTempDir $ \tmpDir _ -> do
        name <- encodeFS "merge-drivers.toml"
        let path = tmpDir </> name
        FileSystem.writeFile path validDocumentBytes
        result <- readMergeDriverConfigFile path
        result `shouldSatisfy` \case
          Right _ -> True
          Left _ -> False

    it "rejects invalid UTF-8" $
      withTempDir $ \tmpDir _ -> do
        name <- encodeFS "merge-drivers.toml"
        let path = tmpDir </> name
        FileSystem.writeFile path "\x80"
        result <- readMergeDriverConfigFile path
        result `shouldSatisfy` \case
          Left (InvalidUtf8 _) -> True
          _ -> False

  describe "formatError" $
    it "formats every merge-driver configuration error" $
      forM_ formattedErrors $ \(err, message) ->
        formatError err `shouldBe` message
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


formattedErrors :: [(Error, Text.Text)]
formattedErrors =
  [
    ( InvalidUtf8 "decoder failure"
    , "The merge-driver configuration is not valid UTF-8: decoder failure."
    )
  ,
    ( TomlErrors $ "first error" :| ["second error"]
    , "The merge-driver configuration is not valid TOML:\n"
        <> "first error\nsecond error\n"
    )
  ,
    ( TomlWarnings ["unknown field"]
    , "The merge-driver configuration has unknown or unused fields:\n"
        <> "unknown field\n"
    )
  ,
    ( InvalidDefaultMergeDriverName "2way" InvalidMergeDriverNameStart
    , "Invalid default merge-driver name '2way': "
        <> "merge-driver names must start with an ASCII letter."
    )
  ,
    ( InvalidMergeDriverName "" EmptyMergeDriverName
    , "Invalid merge-driver name '': merge-driver names cannot be empty."
    )
  ,
    ( InvalidMergeDriverName "bad.name" InvalidMergeDriverNameCharacter
    , "Invalid merge-driver name 'bad.name': merge-driver names may contain "
        <> "only ASCII letters, digits, hyphens, and underscores."
    )
  , driverError EmptyMergeDriverCommand "the command cannot be empty."
  , driverError
      EmptyMergeDriverExecutable
      "the command executable cannot be empty."
  , driverError
      (MergePlaceholderInExecutable "tool-{source}")
      "the executable 'tool-{source}' cannot contain a merge placeholder."
  , driverError
      MissingMergeSourcePlaceholder
      "the command must contain one whole-argument '{source}' placeholder."
  , driverError
      DuplicateMergeSourcePlaceholder
      "the command must not contain more than one '{source}' placeholder."
  , driverError
      MissingMergeBasePlaceholder
      "the command must contain one whole-argument '{base}' placeholder."
  , driverError
      DuplicateMergeBasePlaceholder
      "the command must not contain more than one '{base}' placeholder."
  , driverError
      DuplicateMergeDestinationPlaceholder
      "the command must not contain more than one '{destination}' placeholder."
  , driverError
      MissingMergeResultPlaceholder
      "the command must contain one whole-argument '{result}' placeholder."
  , driverError
      DuplicateMergeResultPlaceholder
      "the command must not contain more than one '{result}' placeholder."
  , driverError
      (EmbeddedMergePlaceholder "--source={source}")
      ( "the argument '--source={source}' embeds a placeholder; "
          <> "placeholders must occupy a whole argument."
      )
  , driverError
      (InvalidMergeDriverExitCode 256)
      "exit code 256 must be between 1 and 255."
  , driverError
      (DuplicateMergeDriverExitCode 7)
      "exit code 7 is listed more than once."
  , driverError
      (AmbiguousMergeDriverExitCode 9)
      "exit code 9 denotes both an unresolved and a canceled merge."
  , driverError
      (InvalidMergeDriverEnvironmentName "BAD-NAME")
      "the environment name 'BAD-NAME' is not portable."
  , driverError
      (DuplicateMergeDriverInheritedEnvironmentName "PATH")
      "the environment name 'PATH' is inherited more than once."
  ,
    ( UnknownDefaultMergeDriver "missing"
    , "The default merge driver 'missing' is not configured."
    )
  ]
 where
  driverError err message =
    (InvalidMergeDriver "driver" err, "Invalid merge driver 'driver': " <> message)
