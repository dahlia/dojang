{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Commands.ReconciliationSpec (spec) where

import Control.Exception (bracket_)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (singleton)
import qualified Data.Map.Strict as Map
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath (OsPath, encodeFS, (</>))
import Test.Hspec (Spec, describe, it, sequential)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldThrow)
import Prelude hiding (readFile, writeFile)

import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear)
import Test.Hspec.Hedgehog (MonadGen, evalIO, forAll, hedgehog, (===))

import Dojang.App (AppEnv (..), runAppWithoutLogging)
import Dojang.Commands.Apply (apply)
import Dojang.Commands.Reflect (reflect)
import Dojang.ExitCodes
  ( accidentalDeletionWarning
  , conflictError
  )
import Dojang.MonadFileSystem
  ( DryRunIO
  , MonadFileSystem (..)
  , dryRunIO
  )
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (Always))
import Dojang.Types.FilePathExpression (FilePathExpression (Substitution))
import Dojang.Types.Manifest (manifest)
import Dojang.Types.MonikerName (parseMonikerName)


data EntryState
  = EntryMissing
  | EntryDirectory
  | EntryFile ByteString
  deriving (Eq, Show)


data CommandDirection = ApplyDirection | ReflectDirection


data FixturePaths = FixturePaths
  { source :: OsPath
  , intermediate :: OsPath
  , destination :: OsPath
  }


entryState :: (MonadGen m) => m EntryState
entryState =
  Gen.choice
    [ return EntryMissing
    , return EntryDirectory
    , EntryFile <$> Gen.bytes (linear 0 8)
    ]


spec :: Spec
spec = sequential $ do
  describe "command reconciliation" $ do
    it "rejects an unforced apply deletion before mutation"
      $ withFixture
        EntryMissing
        (EntryFile "base")
        (EntryFile "base")
      $ \appEnv paths -> do
        runAppWithoutLogging appEnv (apply False [])
          `shouldThrow` (== accidentalDeletionWarning)
        states <- readFixture paths
        states
          `shouldBe` [EntryMissing, EntryFile "base", EntryFile "base"]

    it "rejects an unforced two-sided reflect conflict before mutation"
      $ withFixture
        (EntryFile "source")
        (EntryFile "base")
        (EntryFile "destination")
      $ \appEnv paths -> do
        runAppWithoutLogging appEnv (reflect False True False Nothing [])
          `shouldThrow` (== conflictError)
        states <- readFixture paths
        states
          `shouldBe` [ EntryFile "source"
                     , EntryFile "base"
                     , EntryFile "destination"
                     ]

    it "is directionally symmetric, convergent, and idempotent" $ hedgehog $ do
      sourceState <- forAll entryState
      intermediateState <- forAll entryState
      destinationState <- forAll entryState

      (applyResult, applySecondResult) <-
        evalIO $
          runFixture
            ApplyDirection
            sourceState
            intermediateState
            destinationState
      (reflectResult, reflectSecondResult) <-
        evalIO $
          runFixture
            ReflectDirection
            destinationState
            intermediateState
            sourceState

      applyResult === (ExitSuccess, replicate 3 sourceState)
      reflectResult === (ExitSuccess, replicate 3 sourceState)
      applySecondResult === applyResult
      reflectSecondResult === reflectResult


runFixture
  :: CommandDirection
  -> EntryState
  -> EntryState
  -> EntryState
  -> IO ((ExitCode, [EntryState]), (ExitCode, [EntryState]))
runFixture direction sourceState intermediateState destinationState =
  withFixture sourceState intermediateState destinationState $ \appEnv paths -> do
    dryRunIO $ do
      firstExit <- runCommand direction appEnv
      firstState <- readFixture paths
      secondExit <- runCommand direction appEnv
      secondState <- readFixture paths
      return ((firstExit, firstState), (secondExit, secondState))


withFixture
  :: EntryState
  -> EntryState
  -> EntryState
  -> (AppEnv -> FixturePaths -> IO a)
  -> IO a
withFixture sourceState intermediateState destinationState action =
  withTempDir $ \tmpDir _ -> do
    sourceDir <- encodeFS "source"
    intermediateDir <- encodeFS ".dojang"
    manifestFilename <- encodeFS "dojang.toml"
    envFilename <- encodeFS "dojang-env.toml"
    routeName <- encodeFS "managed-file"
    destinationName <- encodeFS "destination"
    homeName <- encodeFS "home"
    let repository = tmpDir </> sourceDir
    let sourcePath = repository </> routeName
    let intermediatePath = repository </> intermediateDir </> routeName
    let destinationPath = tmpDir </> destinationName
    let paths = FixturePaths sourcePath intermediatePath destinationPath
    let home = tmpDir </> homeName
    let Right always = parseMonikerName "always"
    let manifest' =
          manifest
            (singleton always Always)
            (Map.singleton routeName [(always, Just $ Substitution "DEST")])
            mempty
            mempty
            mempty
    let appEnv =
          AppEnv
            repository
            intermediateDir
            manifestFilename
            envFilename
            False
            False

    createDirectories $ repository </> intermediateDir
    createDirectories home
    writeManifestFile manifest' $ repository </> manifestFilename
    writeEntry sourcePath sourceState
    writeEntry intermediatePath intermediateState
    writeEntry destinationPath destinationState

    withEnvVars
      [ ("DEST", Just destinationPath)
      , ("HOME", Just home)
      , ("USERPROFILE", Just home)
      ]
      $ action appEnv paths


runCommand :: CommandDirection -> AppEnv -> DryRunIO ExitCode
runCommand ApplyDirection appEnv =
  runAppWithoutLogging appEnv $ apply True []
runCommand ReflectDirection appEnv =
  runAppWithoutLogging appEnv $ reflect True True False Nothing []


writeEntry :: OsPath -> EntryState -> IO ()
writeEntry _ EntryMissing = return ()
writeEntry path EntryDirectory = createDirectories path
writeEntry path (EntryFile contents) = writeFile path contents


readEntry :: (MonadFileSystem m) => OsPath -> m EntryState
readEntry path = do
  directory <- isDirectory path
  if directory
    then return EntryDirectory
    else do
      present <- exists path
      if present
        then EntryFile <$> readFile path
        else return EntryMissing


readFixture :: (MonadFileSystem m) => FixturePaths -> m [EntryState]
readFixture paths =
  mapM readEntry [paths.source, paths.intermediate, paths.destination]


withEnvVars :: [(String, Maybe OsPath)] -> IO a -> IO a
withEnvVars [] action = action
withEnvVars ((name, value) : rest) action = do
  oldValue <- lookupEnv name
  bracket_
    (setOrUnset value)
    (maybe (unsetEnv name) (setEnv name) oldValue)
    (withEnvVars rest action)
 where
  setOrUnset Nothing = unsetEnv name
  setOrUnset (Just value') = decodePath value' >>= setEnv name
