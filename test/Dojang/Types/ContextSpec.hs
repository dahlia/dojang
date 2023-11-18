{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.ContextSpec (spec) where

import Data.List (isInfixOf, sort, sortOn)
import System.IO.Error
  ( ioeGetErrorString
  , ioeGetErrorType
  , ioeGetFileName
  , ioeGetLocation
  )
import Prelude hiding (readFile, writeFile)

import Data.ByteString qualified (length)
import Data.Map.Strict (Map, fromList)
import System.FilePath (combine)
import System.OsPath (OsPath, encodeFS, (</>))
import Test.Hspec (Spec, describe, it, runIO, specify)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldReturn, shouldThrow)

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.MonadFileSystem qualified (FileType (..))
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (Entry (..), makeFixtureTree, withTempDir)
import Dojang.Types.Context
  ( Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , RouteState (..)
  , getRouteState
  , listFiles
  , makeCorrespond
  , makeCorrespondBetweenThreeDirs
  , makeCorrespondBetweenThreeFiles
  , makeCorrespondBetweenTwoDirs
  , makeCorrespondWithDestination
  , routePaths
  )
import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (..)
  , Kernel (..)
  , OperatingSystem (..)
  )
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning (..))
import Dojang.Types.FilePathExpression (FilePathExpression (..), (+/+))
import Dojang.Types.FileRouteSpec (monikerMap)
import Dojang.Types.Manifest (manifest)
import Dojang.Types.MonikerName (MonikerName, parseMonikerName)
import Dojang.Types.Repository
  ( Repository (..)
  , RouteMapWarning (..)
  , RouteResult (..)
  )
import GHC.IO.Exception (IOErrorType (InappropriateType))


spec :: Spec
spec = do
  src <- runIO $ encodeFS "src"
  inter <- runIO $ encodeFS "inter"
  dst <- runIO $ encodeFS "dst"
  foo <- runIO $ encodeFS "foo"
  bar <- runIO $ encodeFS "bar"
  baz <- runIO $ encodeFS "baz"
  qux <- runIO $ encodeFS "qux"
  -- cSpell:ignore quux corge
  quux <- runIO $ encodeFS "quux"
  corge <- runIO $ encodeFS "corge"
  intermediateDir <- runIO $ encodeFS ".dojang"
  manifestFilename' <- runIO $ encodeFS "dojang.toml"

  let Right posix = parseMonikerName "posix"
  let Right undefined' = parseMonikerName "undefined"
  let Right windows = parseMonikerName "windows"

  let fileRoutes =
        [
          ( baz
          ,
            [
              ( undefined'
              , Just $ Root Nothing +/+ BareComponent "__dojang_test_baz__"
              )
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

  let manifest' =
        Dojang.Types.Manifest.manifest
          monikerMap
          fileRoutes
          dirRoutes
          [(foo, ["ignore-*"])]

  let repositoryFixture repoPath = do
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
        writeManifestFile manifest' $ repoPath </> manifestFilename'
        createDirectory $ repoPath </> foo
        writeFile (repoPath </> foo </> foo) "asdf"
        createDirectory $ repoPath </> foo </> bar
        writeFile (repoPath </> foo </> bar </> foo) "asdf asdf"
        createDirectory $ repoPath </> foo </> baz
        createDirectory $ repoPath </> bar
        writeFile (repoPath </> bar </> foo) "foobar"
        writeFile (repoPath </> baz) "foo bar baz"
        let repo =
              Repository
                repoPath
                (repoPath </> intermediateDir)
                manifest'
        return repo

  let withContextFixture action = withTempDir $ \tmpDir _ -> do
        createDirectory $ tmpDir </> src
        createDirectory $ tmpDir </> foo
        repo <- repositoryFixture $ tmpDir </> src
        let ctx = Context
              repo
              (Environment Linux X86_64 $ Kernel "Linux" "5.10.0-8")
              $ \e ->
                return $ case e of
                  "FOO" -> Just $ tmpDir </> foo
                  "BAR" -> Just $ tmpDir </> bar
                  "BAZ" -> Just $ tmpDir </> baz
                  _ -> Nothing
        action ctx tmpDir

  specify "routePaths" $ withContextFixture $ \ctx tmpDir -> do
    (results, ws) <- routePaths ctx
    ws `shouldBe` [EnvironmentPredicateWarning $ UndefinedMoniker undefined']
    sortOn (.sourcePath) results
      `shouldBe` [ RouteResult
                    { sourcePath = tmpDir </> src </> bar
                    , routeName = bar
                    , destinationPath = tmpDir </> bar
                    , fileType = Dojang.MonadFileSystem.Directory
                    }
                 , RouteResult
                    { sourcePath = tmpDir </> src </> foo
                    , routeName = foo
                    , destinationPath = tmpDir </> foo
                    , fileType = Dojang.MonadFileSystem.Directory
                    }
                 ]
  specify "makeCorrespondWithDestination"
    $ withContextFixture
    $ \ctx tmpDir -> do
      (correspond, ws) <-
        makeCorrespondWithDestination ctx (tmpDir </> bar </> foo)
      ws `shouldBe` [EnvironmentPredicateWarning $ UndefinedMoniker undefined']
      correspond
        `shouldBe` Just
          ( FileCorrespondence
              { source =
                  FileEntry
                    (tmpDir </> src </> bar </> foo)
                    (File 6)
              , sourceDelta = Added
              , intermediate =
                  FileEntry
                    (tmpDir </> src </> intermediateDir </> bar </> foo)
                    Missing
              , destination = FileEntry (tmpDir </> bar </> foo) Missing
              , destinationDelta = Unchanged
              }
          )
      (correspond', ws') <-
        makeCorrespondWithDestination ctx (tmpDir </> corge)
      correspond' `shouldBe` Nothing
      ws' `shouldBe` ws

  specify "makeCorrespond" $ withContextFixture $ \ctx tmpDir -> do
    (corresponds, ws) <- makeCorrespond ctx
    ws `shouldBe` [EnvironmentPredicateWarning $ UndefinedMoniker undefined']
    sortOn (.source.path) corresponds
      `shouldBe` [ FileCorrespondence
                    { source =
                        FileEntry
                          (tmpDir </> src </> bar </> foo)
                          (File 6)
                    , sourceDelta = Added
                    , intermediate =
                        FileEntry
                          (tmpDir </> src </> intermediateDir </> bar </> foo)
                          Missing
                    , destination = FileEntry (tmpDir </> bar </> foo) Missing
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source =
                        FileEntry
                          (tmpDir </> src </> foo </> bar)
                          Directory
                    , sourceDelta = Added
                    , intermediate =
                        FileEntry
                          (tmpDir </> src </> intermediateDir </> foo </> bar)
                          Missing
                    , destination = FileEntry (tmpDir </> foo </> bar) Missing
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source =
                        FileEntry
                          (tmpDir </> src </> foo </> bar </> foo)
                          (File 9)
                    , sourceDelta = Added
                    , intermediate =
                        FileEntry
                          ( tmpDir
                              </> src
                              </> intermediateDir
                              </> foo
                              </> bar
                              </> foo
                          )
                          Missing
                    , destination =
                        FileEntry (tmpDir </> foo </> bar </> foo) Missing
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source =
                        FileEntry (tmpDir </> src </> foo </> baz) Directory
                    , sourceDelta = Added
                    , intermediate =
                        FileEntry
                          (tmpDir </> src </> intermediateDir </> foo </> baz)
                          Missing
                    , destination = FileEntry (tmpDir </> foo </> baz) Missing
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source =
                        FileEntry (tmpDir </> src </> foo </> foo) (File 4)
                    , sourceDelta = Added
                    , intermediate =
                        FileEntry
                          (tmpDir </> src </> intermediateDir </> foo </> foo)
                          Missing
                    , destination = FileEntry (tmpDir </> foo </> foo) Missing
                    , destinationDelta = Unchanged
                    }
                 ]

  describe "listFiles" $ do
    it "lists files recursively" $ withTempDir $ \tmpDir _ -> do
      _ <- repositoryFixture tmpDir
      files <- listFiles tmpDir []
      manifestData <- readFile $ tmpDir </> manifestFilename'
      let manifestSize = toEnum $ Data.ByteString.length manifestData
      sort files
        `shouldBe` [ FileEntry bar Directory
                   , FileEntry (bar </> foo) $ File 6
                   , FileEntry baz $ File 11
                   , FileEntry manifestFilename' $ File manifestSize
                   , FileEntry foo Directory
                   , FileEntry (foo </> bar) Directory
                   , FileEntry (foo </> bar </> foo) $ File 9
                   , FileEntry (foo </> baz) Directory
                   , FileEntry (foo </> foo) $ File 4
                   ]

    it "filters out files by ignorePatterns" $ withTempDir $ \tmpDir _ -> do
      _ <- repositoryFixture tmpDir
      files <- listFiles tmpDir ["foo/b*"]
      manifestData <- readFile $ tmpDir </> manifestFilename'
      let manifestSize = toEnum $ Data.ByteString.length manifestData
      sort files
        `shouldBe` [ FileEntry bar Directory
                   , FileEntry (bar </> foo) $ File 6
                   , FileEntry baz $ File 11
                   , FileEntry manifestFilename' $ File manifestSize
                   , FileEntry foo Directory
                   , FileEntry (foo </> foo) $ File 4
                   ]

    it "only accepts a directory path" $ withTempDir $ \tmpDir tmpDir' -> do
      () <- writeFile (tmpDir </> foo) "foo"
      listFiles (tmpDir </> foo) [] `shouldThrow` \e ->
        (ioeGetErrorType e == InappropriateType)
          && (ioeGetLocation e == "listFiles")
          && (ioeGetFileName e == Just (tmpDir' `combine` "foo"))
          && ("not a directory" `isInfixOf` show e)

  specify "makeCorrespondBetweenThreeFiles" $ withTempDir $ \tmpDir _ -> do
    let makeCorrespond_ =
          makeCorrespondBetweenThreeFiles
            (tmpDir </> inter)
            (tmpDir </> src)
            (tmpDir </> dst)

    writeFile (tmpDir </> inter) "unchanged"
    writeFile (tmpDir </> src) "unchanged"
    writeFile (tmpDir </> dst) "unchanged"
    makeCorrespond_
      `shouldReturn` FileCorrespondence
        { source = FileEntry (tmpDir </> src) $ File 9
        , sourceDelta = Unchanged
        , intermediate = FileEntry (tmpDir </> inter) $ File 9
        , destination = FileEntry (tmpDir </> dst) $ File 9
        , destinationDelta = Unchanged
        }

    writeFile (tmpDir </> src) "modified!"
    writeFile (tmpDir </> dst) "modified"
    makeCorrespond_
      `shouldReturn` FileCorrespondence
        { source = FileEntry (tmpDir </> src) $ File 9
        , sourceDelta = Modified
        , intermediate = FileEntry (tmpDir </> inter) $ File 9
        , destination = FileEntry (tmpDir </> dst) $ File 8
        , destinationDelta = Modified
        }

    removeFile (tmpDir </> inter)
    writeFile (tmpDir </> src) "added"
    writeFile (tmpDir </> dst) "added"
    makeCorrespond_
      `shouldReturn` FileCorrespondence
        { source = FileEntry (tmpDir </> src) $ File 5
        , sourceDelta = Added
        , intermediate = FileEntry (tmpDir </> inter) Missing
        , destination = FileEntry (tmpDir </> dst) $ File 5
        , destinationDelta = Added
        }

    writeFile (tmpDir </> inter) "removed"
    removeFile (tmpDir </> src)
    removeFile (tmpDir </> dst)
    makeCorrespond_
      `shouldReturn` FileCorrespondence
        { source = FileEntry (tmpDir </> src) Missing
        , sourceDelta = Removed
        , intermediate = FileEntry (tmpDir </> inter) $ File 7
        , destination = FileEntry (tmpDir </> dst) Missing
        , destinationDelta = Removed
        }

    writeFile (tmpDir </> inter) "dir is disallowed"
    createDirectory (tmpDir </> src)
    filename <- decodePath $ tmpDir </> src
    makeCorrespond_ `shouldThrow` \e ->
      ( ioeGetErrorString e
          == "makeCorrespondBetweenThreeFiles: "
          ++ "expected a file, but got a directory"
      )
        && (ioeGetFileName e == Just filename)

  specify "makeCorrespondBetweenThreeDirs" $ withTempDir $ \tmpDir _ -> do
    () <-
      makeFixtureTree
        (tmpDir </> foo)
        [ (inter, D [])
        , (src, D [])
        , (dst, D [])
        ]
    emptyCorresponds <-
      makeCorrespondBetweenThreeDirs
        (tmpDir </> foo </> inter)
        (tmpDir </> foo </> src)
        (tmpDir </> foo </> dst)
        []
    sortOn (.source.path) emptyCorresponds `shouldBe` []

    unchanged <- encodePath "unchanged"
    added <- encodePath "added"
    removed <- encodePath "removed"
    modified <- encodePath "modified"
    ignored <- encodePath "ignored"
    () <-
      makeFixtureTree
        (tmpDir </> bar)
        [
          ( inter
          , D
              [ (unchanged, D [(foo, F "foo-unchanged")])
              , (added, D [])
              ,
                ( removed
                , D
                    [ (foo, F "foo-src-removed")
                    , (bar, F "bar-dst-removed")
                    , (baz, F "baz-both-removed")
                    , (qux, D [])
                    ]
                )
              ,
                ( modified
                , D
                    [ (foo, F "foo")
                    , (bar, F "bar")
                    , (baz, F "baz")
                    , (qux, F "qux-file-to-dir")
                    , (quux, D [])
                    , (corge, F "same-size-but-different-content")
                    ]
                )
              ,
                ( ignored
                , D
                    [ (foo, F "foo-ignored-but-tracked")
                    , (baz, F "baz-ignored-but-indexed")
                    ]
                )
              ]
          )
        ,
          ( src
          , D
              [ (unchanged, D [(foo, F "foo-unchanged")])
              ,
                ( added
                , D
                    [ (foo, F "foo-src-added")
                    , (baz, F "baz-both-added")
                    , (qux, D [])
                    ]
                )
              , (removed, D [(bar, F "bar-dst-removed")])
              ,
                ( modified
                , D
                    [ (foo, F "foo-src-modified")
                    , (bar, F "bar")
                    , (baz, F "baz-src-modified")
                    , (qux, D [])
                    , (quux, F "qux-dir-to-file")
                    , (corge, F "same-length-but--different-body")
                    ]
                )
              ,
                ( ignored
                , D
                    [ (foo, F "foo-ignored-but-tracked")
                    , (bar, F "bar-ignored-but-added")
                    ]
                )
              ]
          )
        ,
          ( dst
          , D
              [ (unchanged, D [(foo, F "foo-unchanged")])
              ,
                ( added
                , D
                    [ (bar, F "bar-dst-added")
                    , (baz, F "baz-both-added")
                    , (qux, D [])
                    ]
                )
              , (removed, D [(foo, F "foo-src-removed")])
              ,
                ( modified
                , D
                    [ (foo, F "foo")
                    , (bar, F "bar-dst-modified")
                    , (baz, F "baz-dst-modified")
                    , (qux, F "qux-file-to-dir")
                    , (quux, D [])
                    , (corge, F "equivalent-length-but-different")
                    ]
                )
              ,
                ( ignored
                , D
                    [ (foo, F "foo-ignored-but-tracked")
                    , (bar, F "bar-ignored-but-added")
                    , (baz, F "baz-ignored-but-indexed")
                    ]
                )
              ]
          )
        ]
    corresponds <-
      makeCorrespondBetweenThreeDirs
        (tmpDir </> bar </> inter)
        (tmpDir </> bar </> src)
        (tmpDir </> bar </> dst)
        ["ignored"]
    sortOn (.source.path) corresponds
      `shouldBe` [ FileCorrespondence
                    { source = FileEntry added Directory
                    , sourceDelta = Unchanged
                    , intermediate = FileEntry added Directory
                    , destination = FileEntry added Directory
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source = FileEntry (added </> bar) Missing
                    , sourceDelta = Unchanged
                    , intermediate = FileEntry (added </> bar) Missing
                    , destination = FileEntry (added </> bar) (File 13)
                    , destinationDelta = Added
                    }
                 , FileCorrespondence
                    { source = FileEntry (added </> baz) (File 14)
                    , sourceDelta = Added
                    , intermediate = FileEntry (added </> baz) Missing
                    , destination = FileEntry (added </> baz) (File 14)
                    , destinationDelta = Added
                    }
                 , FileCorrespondence
                    { source = FileEntry (added </> foo) (File 13)
                    , sourceDelta = Added
                    , intermediate = FileEntry (added </> foo) Missing
                    , destination = FileEntry (added </> foo) Missing
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source = FileEntry (added </> qux) Directory
                    , sourceDelta = Added
                    , intermediate = FileEntry (added </> qux) Missing
                    , destination = FileEntry (added </> qux) Directory
                    , destinationDelta = Added
                    }
                 , FileCorrespondence
                    { source = FileEntry ignored Directory
                    , sourceDelta = Unchanged
                    , intermediate = FileEntry ignored Directory
                    , destination = FileEntry ignored Directory
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source = FileEntry (ignored </> bar) (File 21)
                    , sourceDelta = Added
                    , intermediate = FileEntry (ignored </> bar) Missing
                    , destination = FileEntry (ignored </> bar) (File 21)
                    , destinationDelta = Added
                    }
                 , FileCorrespondence
                    { source = FileEntry (ignored </> baz) Missing
                    , sourceDelta = Removed
                    , intermediate = FileEntry (ignored </> baz) (File 23)
                    , destination = FileEntry (ignored </> baz) (File 23)
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source = FileEntry (ignored </> foo) (File 23)
                    , sourceDelta = Unchanged
                    , intermediate = FileEntry (ignored </> foo) (File 23)
                    , destination = FileEntry (ignored </> foo) (File 23)
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source = FileEntry modified Directory
                    , sourceDelta = Unchanged
                    , intermediate = FileEntry modified Directory
                    , destination = FileEntry modified Directory
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source = FileEntry (modified </> bar) (File 3)
                    , sourceDelta = Unchanged
                    , intermediate = FileEntry (modified </> bar) (File 3)
                    , destination = FileEntry (modified </> bar) (File 16)
                    , destinationDelta = Modified
                    }
                 , FileCorrespondence
                    { source = FileEntry (modified </> baz) (File 16)
                    , sourceDelta = Modified
                    , intermediate = FileEntry (modified </> baz) (File 3)
                    , destination = FileEntry (modified </> baz) (File 16)
                    , destinationDelta = Modified
                    }
                 , FileCorrespondence
                    { source = FileEntry (modified </> corge) (File 31)
                    , sourceDelta = Modified
                    , intermediate = FileEntry (modified </> corge) (File 31)
                    , destination = FileEntry (modified </> corge) (File 31)
                    , destinationDelta = Modified
                    }
                 , FileCorrespondence
                    { source = FileEntry (modified </> foo) (File 16)
                    , sourceDelta = Modified
                    , intermediate = FileEntry (modified </> foo) (File 3)
                    , destination = FileEntry (modified </> foo) (File 3)
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source = FileEntry (modified </> quux) (File 15)
                    , sourceDelta = Modified
                    , intermediate = FileEntry (modified </> quux) Directory
                    , destination = FileEntry (modified </> quux) Directory
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source = FileEntry (modified </> qux) Directory
                    , sourceDelta = Modified
                    , intermediate = FileEntry (modified </> qux) (File 15)
                    , destination = FileEntry (modified </> qux) (File 15)
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source = FileEntry removed Directory
                    , sourceDelta = Unchanged
                    , intermediate = FileEntry removed Directory
                    , destination = FileEntry removed Directory
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source = FileEntry (removed </> bar) (File 15)
                    , sourceDelta = Unchanged
                    , intermediate = FileEntry (removed </> bar) (File 15)
                    , destination = FileEntry (removed </> bar) Missing
                    , destinationDelta = Removed
                    }
                 , FileCorrespondence
                    { source = FileEntry (removed </> baz) Missing
                    , sourceDelta = Removed
                    , intermediate = FileEntry (removed </> baz) (File 16)
                    , destination = FileEntry (removed </> baz) Missing
                    , destinationDelta = Removed
                    }
                 , FileCorrespondence
                    { source = FileEntry (removed </> foo) Missing
                    , sourceDelta = Removed
                    , intermediate = FileEntry (removed </> foo) (File 15)
                    , destination = FileEntry (removed </> foo) (File 15)
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source = FileEntry (removed </> qux) Missing
                    , sourceDelta = Removed
                    , intermediate = FileEntry (removed </> qux) Directory
                    , destination = FileEntry (removed </> qux) Missing
                    , destinationDelta = Removed
                    }
                 , FileCorrespondence
                    { source = FileEntry unchanged Directory
                    , sourceDelta = Unchanged
                    , intermediate = FileEntry unchanged Directory
                    , destination = FileEntry unchanged Directory
                    , destinationDelta = Unchanged
                    }
                 , FileCorrespondence
                    { source = FileEntry (unchanged </> foo) (File 13)
                    , sourceDelta = Unchanged
                    , intermediate = FileEntry (unchanged </> foo) (File 13)
                    , destination = FileEntry (unchanged </> foo) (File 13)
                    , destinationDelta = Unchanged
                    }
                 ]

  specify "makeCorrespondBetweenTwoDirs" $ withTempDir $ \tmpDir _ -> do
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
    makeCorrespondBetweenTwoDirs (tmpDir </> foo) (tmpDir </> bar) []
      `shouldReturn` fromList
        [ (foo, (FileEntry foo $ File 3, FileEntry foo $ File 5))
        , (bar, (FileEntry bar Directory, FileEntry bar Directory))
        ,
          ( bar </> foo
          , (FileEntry (bar </> foo) $ File 7, FileEntry (bar </> foo) $ File 7)
          )
        ,
          ( bar </> bar
          , (FileEntry (bar </> bar) $ File 7, FileEntry (bar </> bar) Missing)
          )
        ,
          ( bar </> baz
          ,
            ( FileEntry (bar </> baz) Directory
            , FileEntry (bar </> baz) Directory
            )
          )
        ,
          ( bar </> baz </> bar
          ,
            ( FileEntry (bar </> baz </> bar) Missing
            , FileEntry (bar </> baz </> bar) $ File 11
            )
          )
        , (baz, (FileEntry baz $ File 3, FileEntry baz $ File 3))
        ,
          ( qux
          , (FileEntry qux Directory, FileEntry qux Directory)
          )
        ,
          ( qux </> foo
          , (FileEntry (qux </> foo) $ File 7, FileEntry (qux </> foo) $ File 7)
          )
        ,
          ( qux </> quux
          ,
            ( FileEntry (qux </> quux) $ File 8
            , FileEntry (qux </> quux) $ File 10
            )
          )
        ]
    makeCorrespondBetweenTwoDirs
      (tmpDir </> baz) -- This dir does not exist
      (tmpDir </> bar)
      []
      `shouldReturn` fromList
        [ (foo, (FileEntry foo Missing, FileEntry foo $ File 5))
        , (bar, (FileEntry bar Missing, FileEntry bar Directory))
        ,
          ( bar </> foo
          ,
            ( FileEntry (bar </> foo) Missing
            , FileEntry (bar </> foo) $ File 7
            )
          )
        ,
          ( bar </> baz
          ,
            ( FileEntry (bar </> baz) Missing
            , FileEntry (bar </> baz) Directory
            )
          )
        ,
          ( bar </> baz </> bar
          ,
            ( FileEntry (bar </> baz </> bar) Missing
            , FileEntry (bar </> baz </> bar) $ File 11
            )
          )
        , (baz, (FileEntry baz Missing, FileEntry baz $ File 3))
        , (qux, (FileEntry qux Missing, FileEntry qux Directory))
        ,
          ( qux </> foo
          , (FileEntry (qux </> foo) Missing, FileEntry (qux </> foo) $ File 7)
          )
        ,
          ( qux </> quux
          ,
            ( FileEntry (qux </> quux) Missing
            , FileEntry (qux </> quux) $ File 10
            )
          )
        ]

  specify "getRouteState" $ withContextFixture $ \ctx tmpDir -> do
    (state, ws) <- getRouteState ctx (tmpDir </> src)
    ws `shouldBe` [EnvironmentPredicateWarning $ UndefinedMoniker undefined']
    state `shouldBe` NotRouted
    (state', ws') <- getRouteState ctx (tmpDir </> foo </> foo)
    ws' `shouldBe` ws
    state' `shouldBe` Routed foo
    ignoreMe <- encodePath "ignore-me"
    (state'', ws'') <- getRouteState ctx (tmpDir </> foo </> ignoreMe)
    ws'' `shouldBe` ws
    state'' `shouldBe` Ignored foo "ignore-*"
    let ctx' = ctx{environment = ctx.environment{operatingSystem = FreeBSD}}
    (stateOnNullRouting, ws''') <- getRouteState ctx' (tmpDir </> src)
    ws''' `shouldBe` ws
    stateOnNullRouting `shouldBe` NotRouted
