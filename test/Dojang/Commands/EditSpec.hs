{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Commands.EditSpec (spec) where

import Control.Exception (bracket_)
import Data.Map.Strict qualified as Map
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.Info (os)
import System.OsPath (OsPath, decodeFS, encodeFS, (</>))

import Test.Hspec (Spec, describe, it, sequential)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldReturn)
import Prelude hiding (writeFile)

import Dojang.App (App, AppEnv (AppEnv), runAppWithoutLogging)
import Dojang.Commands.Edit (defaultEditor, editWithCodecRuntime, getEditor)
import Dojang.MonadFileSystem
  ( FileType (Directory, File)
  , MonadFileSystem (createDirectories, writeFile)
  )
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Codec
  ( CodecConfiguration (CodecConfiguration)
  , CodecDefinition (CodecDefinition)
  , CodecSpec (CodecSpec)
  , ReflectPolicy (ReflectReject)
  , parseCodecName
  )
import Dojang.Types.Codec.Evaluate
  ( CacheScope (PersistentCache)
  , CodecDryRunPolicy (EvaluatePurely)
  , CodecImplementation (CodecImplementation)
  , CodecInputs (..)
  , CodecRequirements (CodecRequirements)
  , CodecRuntime (CodecRuntime)
  , EvaluationMode (NormalEvaluation)
  , codecRegistry
  , revealBytes
  )
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression (FilePathExpression (Substitution))
import Dojang.Types.FileRoute
  ( RouteKind (CopyRoute)
  , RouteMode (DefaultMode)
  , RouteTarget (RouteTarget)
  , fileRoutePreservingOrder
  )
import Dojang.Types.Manifest (Manifest (Manifest))
import Dojang.Types.RepositoryId (parseRepositoryId)


-- | Helper to run a test with a specific environment variable setting.
withEnvVar :: String -> Maybe String -> IO a -> IO a
withEnvVar name value action = do
  oldValue <- lookupEnv name
  bracket_
    (setOrUnset value)
    (setOrUnset oldValue)
    action
 where
  setOrUnset :: Maybe String -> IO ()
  setOrUnset (Just v) = setEnv name v
  setOrUnset Nothing = unsetEnv name


-- | Helper to run a test with multiple environment variables set.
withEnvVars :: [(String, Maybe String)] -> IO a -> IO a
withEnvVars [] action = action
withEnvVars ((name, value) : rest) action =
  withEnvVar name value (withEnvVars rest action)


spec :: Spec
spec = do
  describe "defaultEditor" $ do
    it "is vi on POSIX" $ do
      if os == "mingw32"
        then defaultEditor `shouldBe` "notepad"
        else defaultEditor `shouldBe` "vi"

  -- These tests must run sequentially because they modify global env vars.
  sequential $ describe "getEditor" $ do
    it "returns --editor option when provided" $ do
      withEnvVars [("VISUAL", Just "emacs"), ("EDITOR", Just "nano")] $ do
        editor <- getEditor (Just "vim")
        editor `shouldBe` Just "vim"

    it "returns VISUAL when --editor not provided" $ do
      withEnvVars [("VISUAL", Just "emacs"), ("EDITOR", Just "nano")] $ do
        editor <- getEditor Nothing
        editor `shouldBe` Just "emacs"

    it "returns EDITOR when VISUAL not set" $ do
      withEnvVars [("VISUAL", Nothing), ("EDITOR", Just "nano")] $ do
        editor <- getEditor Nothing
        editor `shouldBe` Just "nano"

    it "returns Nothing when no env vars set and no option" $ do
      withEnvVars [("VISUAL", Nothing), ("EDITOR", Nothing)] $ do
        editor <- getEditor Nothing
        editor `shouldBe` Nothing

  sequential $ describe "editWithCodecRuntime" $ do
    it "treats a converged rendered route as unchanged" $
      withCodecFile $ \appEnv missingEditor _ runtime ->
        runAppWithoutLogging
          appEnv
          ( editWithCodecRuntime
              runtime
              (Just missingEditor)
              True
              False
              False
              True
              False
              Nothing
              []
          )
          `shouldReturn` ExitSuccess

    it "uses the supplied codec runtime for automatic apply" $
      if os == "mingw32"
        then return ()
        else withCodecFile $ \appEnv _ destination runtime ->
          runAppWithoutLogging
            appEnv
            ( editWithCodecRuntime
                runtime
                (Just "true")
                False
                True
                False
                True
                False
                Nothing
                [destination]
            )
            `shouldReturn` ExitSuccess

    it "treats a converged forced ignored codec file as unchanged" $
      withIgnoredCodecFile $ \appEnv missingEditor runtime ->
        runAppWithoutLogging
          appEnv
          ( editWithCodecRuntime
              runtime
              (Just missingEditor)
              True
              True
              False
              True
              False
              Nothing
              []
          )
          `shouldReturn` ExitSuccess


withCodecFile
  :: (AppEnv -> FilePath -> OsPath -> CodecRuntime (App IO) -> IO a)
  -> IO a
withCodecFile action = withTempDir $ \root _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestName <- encodeFS "dojang.toml"
  envName <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "encoded"
  destinationName <- encodeFS "destination"
  missingEditorName <- encodeFS "missing-editor"
  let repository = root </> sourceDir
      source = repository </> routeName
      intermediate = repository </> intermediateDir </> routeName
      destination = root </> destinationName
      Just codecName = parseCodecName "test-codec"
      codecSpec = CodecSpec codecName $ CodecConfiguration mempty
      route =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $
                RouteTarget
                  (Substitution "DEST")
                  DefaultMode
                  CopyRoute
                  codecSpec
            )
          ]
          File
      Right repositoryId =
        parseRepositoryId "b23e4567-e89b-42d3-a456-426614174000"
      manifest' =
        Manifest
          (Just repositoryId)
          mempty
          mempty
          (Map.singleton routeName route)
          mempty
          mempty
      appEnv =
        AppEnv
          repository
          False
          (Just intermediateDir)
          (root </> stateDir)
          manifestName
          envName
          False
          False
      implementation =
        CodecImplementation
          (CodecDefinition codecName "test-1" ReflectReject)
          (const $ Right $ CodecRequirements mempty mempty [])
          (\inputs -> Right $ revealBytes inputs.rawSource <> ":rendered")
          Nothing
          PersistentCache
          EvaluatePurely
      runtime =
        CodecRuntime
          (codecRegistry [implementation])
          NormalEvaluation
          (const $ return $ Left "unexpected external input")
  createDirectories $ repository </> intermediateDir
  writeManifestFile manifest' $ repository </> manifestName
  writeFile source "raw"
  writeFile intermediate "raw:rendered"
  writeFile destination "raw:rendered"
  destinationValue <- decodeFS destination
  missingEditor <- decodeFS $ root </> missingEditorName
  withEnvVar "DEST" (Just destinationValue) $
    action appEnv missingEditor destination runtime


withIgnoredCodecFile
  :: (AppEnv -> FilePath -> CodecRuntime (App IO) -> IO a)
  -> IO a
withIgnoredCodecFile action = withTempDir $ \root _ -> do
  sourceDir <- encodeFS "source"
  intermediateDir <- encodeFS ".dojang"
  manifestName <- encodeFS "dojang.toml"
  envName <- encodeFS "dojang-env.toml"
  stateDir <- encodeFS ".state"
  routeName <- encodeFS "tree"
  fileName <- encodeFS "ignored"
  destinationName <- encodeFS "destination"
  missingEditorName <- encodeFS "missing-editor"
  let repository = root </> sourceDir
      sourceRoot = repository </> routeName
      source = sourceRoot </> fileName
      intermediate = repository </> intermediateDir </> routeName </> fileName
      destinationRoot = root </> destinationName
      destination = destinationRoot </> fileName
      Just codecName = parseCodecName "test-codec"
      codecSpec = CodecSpec codecName $ CodecConfiguration mempty
      route =
        fileRoutePreservingOrder
          (const Nothing)
          [
            ( Always
            , Just $
                RouteTarget
                  (Substitution "DEST")
                  DefaultMode
                  CopyRoute
                  codecSpec
            )
          ]
          Directory
      Right repositoryId =
        parseRepositoryId "c33e4567-e89b-42d3-a456-426614174000"
      manifest' =
        Manifest
          (Just repositoryId)
          mempty
          mempty
          (Map.singleton routeName route)
          (Map.singleton routeName ["*"])
          mempty
      appEnv =
        AppEnv
          repository
          False
          (Just intermediateDir)
          (root </> stateDir)
          manifestName
          envName
          False
          False
      implementation =
        CodecImplementation
          (CodecDefinition codecName "test-1" ReflectReject)
          (const $ Right $ CodecRequirements mempty mempty [])
          (\inputs -> Right $ revealBytes inputs.rawSource <> ":rendered")
          Nothing
          PersistentCache
          EvaluatePurely
      runtime =
        CodecRuntime
          (codecRegistry [implementation])
          NormalEvaluation
          (const $ return $ Left "unexpected external input")
  createDirectories $ repository </> intermediateDir </> routeName
  createDirectories sourceRoot
  createDirectories destinationRoot
  writeManifestFile manifest' $ repository </> manifestName
  writeFile source "raw"
  writeFile intermediate "raw:rendered"
  writeFile destination "raw:rendered"
  destinationValue <- decodeFS destinationRoot
  missingEditor <- decodeFS $ root </> missingEditorName
  withEnvVar "DEST" (Just destinationValue) $
    action appEnv missingEditor runtime
