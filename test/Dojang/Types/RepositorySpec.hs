{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.RepositorySpec (spec) where

import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (writeFile)

import Data.Either (isLeft)
import Data.Map.Strict (Map)
import System.FilePath (combine)
import System.IO.Error
  ( doesNotExistErrorType
  , ioeGetErrorString
  , ioeGetErrorType
  , ioeGetFileName
  )
import System.OsPath (OsPath, encodeFS, normalise, (</>))
import System.OsString (OsString)
import Test.Hspec (Spec, describe, it, runIO, specify)
import Test.Hspec.Expectations.Pretty
  ( shouldBe
  , shouldNotSatisfy
  , shouldSatisfy
  )

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (withTempDir)
import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (Environment)
  , OperatingSystem (..)
  )
import Dojang.Types.EnvironmentPredicate.Evaluate
  ( EvaluationWarning (UndefinedMoniker)
  )
import Dojang.Types.FilePathExpression
  ( EnvironmentVariable
  , FilePathExpression (..)
  , (+/+)
  )
import Dojang.Types.FileRoute (FileType (..))
import Dojang.Types.FileRouteSpec (monikerMap)
import Dojang.Types.Manifest (manifest)
import Dojang.Types.MonikerName (MonikerName, parseMonikerName)
import Dojang.Types.Repository (FilePair (..), Repository (..), pairFiles)


spec :: Spec
spec = do
  foo <- runIO $ encodeFS "foo"
  bar <- runIO $ encodeFS "bar"
  baz <- runIO $ encodeFS "baz"
  manifestFilename' <- runIO $ encodeFS "dojang.toml"

  let Right posix = parseMonikerName "posix"
  let Right undefined' = parseMonikerName "undefined"
  let Right windows = parseMonikerName "windows"

  let fileRoutes =
        [
          ( baz
          ,
            [ (undefined', Just $ Root Nothing +/+ BareComponent "baz")
            , (windows, Just $ Substitution "BAZ")
            ]
          )
        ]
          :: Map OsPath [(MonikerName, Maybe FilePathExpression)]
  let dirRoutes =
        [ (foo, [(posix, Just $ Substitution "FOO"), (windows, Nothing)])
        ,
          ( bar
          ,
            [ (posix, Just $ Substitution "BAR")
            , (windows, Just $ Root $ Just 'B')
            ]
          )
        ]
          :: Map OsPath [(MonikerName, Maybe FilePathExpression)]

  let manifest' = Dojang.Types.Manifest.manifest monikerMap fileRoutes dirRoutes

  let repositoryFixture tmpDir = do
        -- The tree of the repository fixture looks like this:
        -- .
        -- ├── bar/
        -- │   └── foo
        -- ├── baz
        -- ├── dojang.toml
        -- └── foo/
        --     ├── bar/
        --     │   └── foo
        --     ├── baz/
        --     └── foo
        -- Total 4 directories and 5 files
        Right () <- writeManifestFile manifest' $ tmpDir </> manifestFilename'
        Right () <- createDirectory $ tmpDir </> foo
        Right () <- writeFile (tmpDir </> foo </> foo) ""
        Right () <- createDirectory $ tmpDir </> foo </> bar
        Right () <- writeFile (tmpDir </> foo </> bar </> foo) ""
        Right () <- createDirectory $ tmpDir </> foo </> baz
        Right () <- createDirectory $ tmpDir </> bar
        Right () <- writeFile (tmpDir </> bar </> foo) ""
        Right () <- writeFile (tmpDir </> baz) ""
        let repo = Repository tmpDir manifestFilename' manifest'
        repo.manifestFilename `shouldBe` manifestFilename'
        return repo

  describe "pairFiles" $ do
    specify "POSIX" $ withTempDir $ \tmpDir _ -> do
      repo <- repositoryFixture tmpDir
      let env = Environment Linux AArch64
      (Right files, warnings) <- pairFiles repo env getEnv
      warnings `shouldBe` []
      sort files
        `shouldBe` [ dirPair (tmpDir </> bar) (osPath "/var/bar")
                   , filePair (tmpDir </> bar </> foo) (osPath "/var/bar/foo")
                   , dirPair (tmpDir </> foo) (osPath "/opt/foo")
                   , dirPair (tmpDir </> foo </> bar) (osPath "/opt/foo/bar")
                   , filePair
                      (tmpDir </> foo </> bar </> foo)
                      (osPath "/opt/foo/bar/foo")
                   , dirPair (tmpDir </> foo </> baz) (osPath "/opt/foo/baz")
                   , filePair (tmpDir </> foo </> foo) (osPath "/opt/foo/foo")
                   ]

    specify "Windows" $ withTempDir $ \tmpDir _ -> do
      repo <- repositoryFixture tmpDir
      let env = Environment Windows X86_64
      (Right files, warnings) <- pairFiles repo env getEnv
      warnings `shouldBe` [UndefinedMoniker undefined']
      sort files
        `shouldBe` [ dirPair (tmpDir </> bar) (osPath "B:\\")
                   , filePair (tmpDir </> bar </> foo) (osPath "B:\\foo")
                   , filePair (tmpDir </> baz) (osPath "C:\\baz")
                   ]

    it "fails if no corresponding file" $ withTempDir $ \tmpDir tmpDir' -> do
      -- When the file does not exist:
      repo <- repositoryFixture tmpDir
      Right () <- removeFile $ tmpDir </> baz
      let env = Environment Windows X86_64
      (result, _) <- pairFiles repo env getEnv
      result `shouldSatisfy` isLeft
      let (Left e) = result
      ioeGetErrorString e `shouldBe` "No such file"
      ioeGetFileName e `shouldBe` Just (tmpDir' `combine` "baz")

      -- When the file is a directory:
      Right () <- createDirectory $ tmpDir </> baz
      (Left e', _) <- pairFiles repo env getEnv
      ioeGetErrorString e' `shouldBe` "Is a directory"
      ioeGetFileName e' `shouldBe` Just (tmpDir' `combine` "baz")

    it "fails if no corresponding dir" $ withTempDir $ \tmpDir tmpDir' -> do
      -- When the directory does not exist:
      let repo = Repository tmpDir manifestFilename' manifest'
      let env = Environment Linux X86_64
      (Left e, _) <- pairFiles repo env getEnv
      ioeGetErrorType e `shouldBe` doesNotExistErrorType
      ioeGetFileName e `shouldBe` Just (tmpDir' `combine` "bar")

      -- When the directory is a file:
      Right () <- writeFile (tmpDir </> bar) ""
      (Left e', _) <- pairFiles repo env getEnv
      ioeGetFileName e' `shouldBe` Just (tmpDir' `combine` "bar")

  describe "FilePair" $ do
    let fooBar = FilePair foo bar File
    let barBaz = FilePair bar baz File
    let bazFoo = FilePair baz foo Directory

    specify "source" $ do
      source fooBar `shouldBe` foo
      source barBaz `shouldBe` bar
      source bazFoo `shouldBe` baz

    specify "destination" $ do
      destination fooBar `shouldBe` bar
      destination barBaz `shouldBe` baz
      destination bazFoo `shouldBe` foo

    specify "fileType" $ do
      fileType fooBar `shouldBe` File
      fileType barBaz `shouldBe` File
      fileType bazFoo `shouldBe` Directory

    specify "Eq" $ do
      fooBar `shouldSatisfy` (== fooBar)
      fooBar `shouldNotSatisfy` (== barBaz)
      fooBar `shouldNotSatisfy` (== bazFoo)
      fooBar `shouldNotSatisfy` (/= fooBar)
      fooBar `shouldSatisfy` (/= barBaz)
      fooBar `shouldSatisfy` (/= bazFoo)

    specify "Ord" $ do
      sort [fooBar, barBaz, bazFoo] `shouldBe` [barBaz, bazFoo, fooBar]
      max fooBar fooBar `shouldBe` fooBar
      max fooBar barBaz `shouldBe` fooBar
      min fooBar fooBar `shouldBe` fooBar
      min fooBar barBaz `shouldBe` barBaz

    specify "Show" $ do
      show fooBar
        `shouldBe` ( "FilePair {source = \"foo\", "
                      ++ "destination = \"bar\", fileType = File}"
                   )
      show ([fooBar, barBaz, bazFoo] :: [FilePair])
        `shouldBe` ( "[FilePair {source = \"foo\", destination = \"bar\", "
                      ++ "fileType = File}"
                      ++ ",FilePair {source = \"bar\", destination = \"baz\", "
                      ++ "fileType = File}"
                      ++ ",FilePair {source = \"baz\", destination = \"foo\", "
                      ++ "fileType = Directory}]"
                   )


getEnv :: (MonadFileSystem m) => EnvironmentVariable -> m (Maybe OsString)
getEnv envVar = case envVar of
  "FOO" -> Just <$> encodePath "/opt/foo"
  "BAR" -> Just <$> encodePath "/var/bar"
  "BAZ" -> Just <$> encodePath "C:\\baz"
  _ -> return Nothing


osPath :: String -> OsPath
osPath = normalise . unsafePerformIO . encodeFS


filePair :: OsPath -> OsPath -> FilePair
filePair src dst = FilePair src dst File


dirPair :: OsPath -> OsPath -> FilePair
dirPair src dst = FilePair src dst Directory
