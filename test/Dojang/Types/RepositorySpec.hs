{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.RepositorySpec (spec) where

import Data.List (sort)
import System.IO.Error (ioeGetErrorString, ioeGetFileName)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (writeFile)

import Data.Map.Strict (Map, fromList)
import System.FilePath (combine)
import System.OsPath (OsPath, encodeFS, normalise, (</>))
import System.OsString (OsString)
import Test.Hspec (Spec, describe, it, runIO, specify)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldReturn, shouldThrow)

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (Entry (..), makeFixtureTree, withTempDir)
import Dojang.Types.FilePathExpression
  ( EnvironmentVariable
  , FilePathExpression (..)
  , (+/+)
  )
import Dojang.Types.FileRouteSpec (monikerMap)
import Dojang.Types.Manifest (manifest)
import Dojang.Types.MonikerName (MonikerName, parseMonikerName)
import Dojang.Types.Repository
  ( FileEntry (..)
  , FileStat (..)
  , Repository (..)
  , listFiles
  , makeCorrespondBetweenTwoDirs
  )


spec :: Spec
spec = do
  foo <- runIO $ encodeFS "foo"
  bar <- runIO $ encodeFS "bar"
  baz <- runIO $ encodeFS "baz"
  intermediateDir <- runIO $ encodeFS ".dojang"
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
        () <- writeManifestFile manifest' $ tmpDir </> manifestFilename'
        () <- createDirectory $ tmpDir </> foo
        () <- writeFile (tmpDir </> foo </> foo) "asdf"
        () <- createDirectory $ tmpDir </> foo </> bar
        () <- writeFile (tmpDir </> foo </> bar </> foo) "asdf asdf"
        () <- createDirectory $ tmpDir </> foo </> baz
        () <- createDirectory $ tmpDir </> bar
        () <- writeFile (tmpDir </> bar </> foo) "foobar"
        () <- writeFile (tmpDir </> baz) "foo bar baz"
        let repo = Repository tmpDir (tmpDir </> intermediateDir) manifestFilename' manifest'
        repo.manifestFilename `shouldBe` manifestFilename'
        return repo

  describe "listFiles" $ do
    it "lists files recursively" $ withTempDir $ \tmpDir _ -> do
      _ <- repositoryFixture tmpDir
      files <- listFiles tmpDir
      sort files
        `shouldBe` [ FileEntry bar Directory
                   , FileEntry (bar </> foo) $ File 6
                   , FileEntry baz $ File 11
                   , FileEntry manifestFilename' $ File 235
                   , FileEntry foo Directory
                   , FileEntry (foo </> bar) Directory
                   , FileEntry (foo </> bar </> foo) $ File 9
                   , FileEntry (foo </> baz) Directory
                   , FileEntry (foo </> foo) $ File 4
                   ]

    it "only accepts a directory path" $ withTempDir $ \tmpDir tmpDir' -> do
      () <- writeFile (tmpDir </> foo) "foo"
      listFiles (tmpDir </> foo) `shouldThrow` \e ->
        (ioeGetErrorString e == "listFiles: path is not a directory")
          && (ioeGetFileName e == Just (tmpDir' `combine` "foo"))

  specify "makeCorrespondBetweenTwoDirs" $ withTempDir $ \tmpDir _ -> do
    -- cSpell:ignore quux
    qux <- encodePath "qux"
    quux <- encodePath "quux"
    () <-
      makeFixtureTree
        (tmpDir </> foo)
        [ (foo, F "foo")
        ,
          ( bar
          , D
              [ (foo, F "bar/foo")
              , (bar, F "bar/bar")
              , (baz, D [])
              ]
          )
        , (baz, F "baz")
        ,
          ( qux
          , D
              [ (foo, F "qux/foo")
              , (quux, F "qux/quux")
              ]
          )
        ]
    () <-
      makeFixtureTree
        (tmpDir </> bar)
        [ (foo, F "foo 2")
        ,
          ( bar
          , D
              [ (foo, F "bar/foo")
              , (baz, D [(bar, F "bar/baz/bar")])
              ]
          )
        , (baz, F "baz")
        ,
          ( qux
          , D
              [ (foo, F "qux/foo")
              , (quux, F "qux/quux 2")
              ]
          )
        ]
    makeCorrespondBetweenTwoDirs (tmpDir </> foo) (tmpDir </> bar)
      `shouldReturn` fromList
        [ (foo, (Just $ FileEntry foo $ File 3, Just $ FileEntry foo $ File 5))
        , (bar, (Just $ FileEntry bar Directory, Just $ FileEntry bar Directory))
        ,
          ( bar </> foo
          ,
            ( Just $ FileEntry (bar </> foo) $ File 7
            , Just $ FileEntry (bar </> foo) $ File 7
            )
          )
        , (bar </> bar, (Just $ FileEntry (bar </> bar) $ File 7, Nothing))
        ,
          ( bar </> baz
          ,
            ( Just $ FileEntry (bar </> baz) Directory
            , Just $ FileEntry (bar </> baz) Directory
            )
          )
        ,
          ( bar </> baz </> bar
          , (Nothing, Just $ FileEntry (bar </> baz </> bar) $ File 11)
          )
        , (baz, (Just $ FileEntry baz $ File 3, Just $ FileEntry baz $ File 3))
        ,
          ( qux
          , (Just $ FileEntry qux Directory, Just $ FileEntry qux Directory)
          )
        ,
          ( qux </> foo
          ,
            ( Just $ FileEntry (qux </> foo) $ File 7
            , Just $ FileEntry (qux </> foo) $ File 7
            )
          )
        ,
          ( qux </> quux
          ,
            ( Just $ FileEntry (qux </> quux) $ File 8
            , Just $ FileEntry (qux </> quux) $ File 10
            )
          )
        ]
    makeCorrespondBetweenTwoDirs
      (tmpDir </> baz) -- This dir does not exist
      (tmpDir </> bar)
      `shouldReturn` fromList
        [ (foo, (Nothing, Just $ FileEntry foo $ File 5))
        , (bar, (Nothing, Just $ FileEntry bar Directory))
        , (bar </> foo, (Nothing, Just $ FileEntry (bar </> foo) $ File 7))
        , (bar </> baz, (Nothing, Just $ FileEntry (bar </> baz) Directory))
        ,
          ( bar </> baz </> bar
          , (Nothing, Just $ FileEntry (bar </> baz </> bar) $ File 11)
          )
        , (baz, (Nothing, Just $ FileEntry baz $ File 3))
        , (qux, (Nothing, Just $ FileEntry qux Directory))
        , (qux </> foo, (Nothing, Just $ FileEntry (qux </> foo) $ File 7))
        , (qux </> quux, (Nothing, Just $ FileEntry (qux </> quux) $ File 10))
        ]


getEnv :: (MonadFileSystem m) => EnvironmentVariable -> m (Maybe OsString)
getEnv envVar = case envVar of
  "FOO" -> Just <$> encodePath "/opt/foo"
  "BAR" -> Just <$> encodePath "/var/bar"
  "BAZ" -> Just <$> encodePath "C:\\baz"
  _ -> return Nothing


osPath :: String -> OsPath
osPath = normalise . unsafePerformIO . encodeFS


_t :: (EnvironmentVariable -> IO (Maybe OsString), String -> OsPath)
_t = (getEnv, osPath)
