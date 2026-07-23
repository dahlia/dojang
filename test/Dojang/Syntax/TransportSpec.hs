{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.TransportSpec (spec) where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Hedgehog (forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.FilePath qualified as FilePath
import System.OsPath (encodeFS)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog, (===))

import Dojang.Syntax.Transport
  ( Error (..)
  , formatError
  , readTransportConfig
  , readTransportConfigFile
  )
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Transport
  ( EnvironmentNameCase (..)
  , TransportConfigurationError (..)
  , TransportLookupError (..)
  , TransportNameError (..)
  , TransportSpec (..)
  , expandTransportCommand
  , lookupTransport
  , resolveTransportEnvironment
  )


spec :: Spec
spec = do
  describe "readTransportConfig" $ do
    it "reads a structured external transport" $ do
      let source =
            "[transports.git]\n"
              <> "command = [\"git\", \"clone\", \"--\", "
              <> "\"{source}\", \"{destination}\"]\n"
              <> "inherit-environment = [\"HOME\", \"PATH\", \"SSH_AUTH_SOCK\"]\n"
              <> "\n"
              <> "[transports.git.environment]\n"
              <> "GIT_TERMINAL_PROMPT = \"1\"\n"
      config <- case readTransportConfig source of
        Left err -> fail $ show err
        Right value -> return value
      transport <- case lookupTransport "git" config of
        Left err -> fail $ show err
        Right value -> return value
      transport.command
        `shouldBe` ("git" :| ["clone", "--", "{source}", "{destination}"])
      transport.inheritedEnvironment
        `shouldBe` ["HOME", "PATH", "SSH_AUTH_SOCK"]
      transport.environment
        `shouldBe` Map.fromList [("GIT_TERMINAL_PROMPT", "1")]

    it "preserves arbitrary sources and destinations as whole arguments" $
      hedgehog $ do
        source <- forAll $ Gen.text (Range.linear 0 200) Gen.unicodeAll
        destination <- forAll $ Gen.text (Range.linear 0 200) Gen.unicodeAll
        let document =
              "[transports.copy]\n"
                <> "command = [\"copy-tool\", \"--source\", "
                <> "\"{source}\", \"--destination\", \"{destination}\"]\n"
        config <- case readTransportConfig document of
          Left err -> fail $ show err
          Right value -> return value
        transport <- case lookupTransport "copy" config of
          Left err -> fail $ show err
          Right value -> return value
        expandTransportCommand transport source destination
          === ( "copy-tool"
              , ["--source", source, "--destination", destination]
              )

    it "rejects reserved and invalid transport names" $ do
      readTransportConfig
        "[transports.directory]\ncommand = [\"x\", \"{source}\", \"{destination}\"]\n"
        `shouldBe` Left (InvalidTransportName "directory" ReservedTransportName)
      readTransportConfig
        "[transports.\"not a name\"]\ncommand = [\"x\", \"{source}\", \"{destination}\"]\n"
        `shouldBe` Left (InvalidTransportName "not a name" InvalidTransportNameCharacter)
      readTransportConfig
        "[transports.archive]\ncommand = [\"x\", \"{source}\", \"{destination}\"]\n"
        `shouldBe` Left (InvalidTransportName "archive" ReservedTransportName)
      readTransportConfig
        "[transports.\"1copy\"]\ncommand = [\"x\", \"{source}\", \"{destination}\"]\n"
        `shouldBe` Left (InvalidTransportName "1copy" InvalidTransportNameStart)

    it "rejects missing, repeated, and embedded placeholders" $ do
      readTransportConfig "[transports.x]\ncommand = [\"x\", \"{destination}\"]\n"
        `shouldBe` Left (InvalidTransport "x" MissingSourcePlaceholder)
      readTransportConfig
        "[transports.x]\ncommand = [\"x\", \"{source}\", \"{source}\", \"{destination}\"]\n"
        `shouldBe` Left (InvalidTransport "x" DuplicateSourcePlaceholder)
      readTransportConfig
        "[transports.x]\ncommand = [\"x\", \"prefix-{source}\", \"{destination}\"]\n"
        `shouldBe` Left (InvalidTransport "x" (EmbeddedPlaceholder "prefix-{source}"))
      readTransportConfig "[transports.x]\ncommand = [\"x\", \"{source}\"]\n"
        `shouldBe` Left (InvalidTransport "x" MissingDestinationPlaceholder)
      readTransportConfig
        "[transports.x]\ncommand = [\"x\", \"{source}\", \"{destination}\", \"{destination}\"]\n"
        `shouldBe` Left (InvalidTransport "x" DuplicateDestinationPlaceholder)
      readTransportConfig
        "[transports.x]\ncommand = [\"{source}\", \"{destination}\"]\n"
        `shouldBe` Left
          (InvalidTransport "x" (PlaceholderInExecutable "{source}"))

    it "rejects an empty command and invalid environment names" $ do
      readTransportConfig "[transports.x]\ncommand = []\n"
        `shouldBe` Left (InvalidTransport "x" EmptyTransportCommand)
      readTransportConfig
        "[transports.x]\ncommand = [\"\", \"{source}\", \"{destination}\"]\n"
        `shouldBe` Left (InvalidTransport "x" EmptyTransportExecutable)
      readTransportConfig
        ( "[transports.x]\n"
            <> "command = [\"x\", \"{source}\", \"{destination}\"]\n"
            <> "inherit-environment = [\"NOT-PORTABLE\"]\n"
        )
        `shouldBe` Left (InvalidTransport "x" (InvalidEnvironmentName "NOT-PORTABLE"))
      readTransportConfig
        ( "[transports.x]\n"
            <> "command = [\"x\", \"{source}\", \"{destination}\"]\n"
            <> "inherit-environment = [\"PATH\", \"PATH\"]\n"
        )
        `shouldBe` Left
          (InvalidTransport "x" (DuplicateInheritedEnvironmentName "PATH"))
      readTransportConfig
        ( "[transports.x]\n"
            <> "command = [\"x\", \"{source}\", \"{destination}\"]\n"
            <> "inherit-environment = [\"PATH\", \"Path\"]\n"
        )
        `shouldSatisfy` \case
          Right _ -> True
          Left _ -> False
      readTransportConfig
        ( "[transports.x]\n"
            <> "command = [\"x\", \"{source}\", \"{destination}\"]\n"
            <> "[transports.x.environment]\n"
            <> "\"NOT-PORTABLE\" = \"1\"\n"
        )
        `shouldBe` Left
          (InvalidTransport "x" (InvalidEnvironmentName "NOT-PORTABLE"))

    it "rejects unknown fields instead of ignoring a typo" $ do
      readTransportConfig
        ( "[transports.x]\n"
            <> "command = [\"x\", \"{source}\", \"{destination}\"]\n"
            <> "inherit-enviroment = [\"HOME\"]\n"
        )
        `shouldSatisfy` \case
          Left (TomlWarnings (_ : _)) -> True
          _ -> False

    it "reports invalid and unknown transport lookups" $ do
      let Right config =
            readTransportConfig
              "[transports.x]\ncommand = [\"x\", \"{source}\", \"{destination}\"]\n"
      lookupTransport "" config
        `shouldBe` Left (InvalidLookupTransportName EmptyTransportName)
      lookupTransport "missing" config
        `shouldBe` Left (UnknownTransportName "missing")

  describe "resolveTransportEnvironment" $ do
    it "preserves case-distinct inherited names on POSIX" $ do
      let Right config =
            readTransportConfig
              ( "[transports.x]\n"
                  <> "command = [\"x\", \"{source}\", \"{destination}\"]\n"
                  <> "inherit-environment = [\"http_proxy\", \"HTTP_PROXY\", "
                  <> "\"MISSING\"]\n"
              )
          Right transport = lookupTransport "x" config
      resolveTransportEnvironment
        CaseSensitiveEnvironment
        [ ("http_proxy", "lower")
        , ("HTTP_PROXY", "upper")
        , ("SECRET_TOKEN", "s3cret")
        ]
        transport
        `shouldBe` [("HTTP_PROXY", "upper"), ("http_proxy", "lower")]

    it "lets fixed values override inherited values on Windows" $ do
      let Right config =
            readTransportConfig
              ( "[transports.x]\n"
                  <> "command = [\"x\", \"{source}\", \"{destination}\"]\n"
                  <> "inherit-environment = [\"Path\"]\n"
                  <> "[transports.x.environment]\n"
                  <> "PATH = \"fixed\"\n"
              )
          Right transport = lookupTransport "x" config
      resolveTransportEnvironment
        CaseInsensitiveEnvironment
        [("Path", "host")]
        transport
        `shouldBe` [("PATH", "fixed")]

  describe "readTransportConfigFile" $ do
    it "reads a valid UTF-8 transport file" $
      withTempDir $ \_ tmpDirString -> do
        let nativePath = tmpDirString FilePath.</> "transports.toml"
        path <- encodeFS nativePath
        ByteString.writeFile
          nativePath
          "[transports.x]\ncommand = [\"x\", \"{source}\", \"{destination}\"]\n"
        result <- readTransportConfigFile path
        result `shouldSatisfy` \case
          Right config ->
            case lookupTransport "x" config of
              Right _ -> True
              Left _ -> False
          Left _ -> False

    it "rejects invalid UTF-8" $
      withTempDir $ \_ tmpDirString -> do
        let nativePath = tmpDirString FilePath.</> "transports.toml"
        path <- encodeFS nativePath
        ByteString.writeFile nativePath "\x80"
        result <- readTransportConfigFile path
        result `shouldSatisfy` \case
          Left (InvalidUtf8 _) -> True
          _ -> False

  describe "formatError" $ do
    it "formats invalid transport names for a user-facing diagnostic" $
      formatError (InvalidTransportName "archive" ReservedTransportName)
        `shouldBe` ( "Invalid transport name 'archive': the names 'directory' "
                       <> "and 'archive' are reserved for built-in transports."
                   )
