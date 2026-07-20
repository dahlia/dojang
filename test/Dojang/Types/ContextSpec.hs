{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.ContextSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (find, isInfixOf, sort, sortOn)
import System.IO.Error
  ( ioeGetErrorType
  , ioeGetFileName
  , ioeGetLocation
  )
import Prelude hiding (readFile, writeFile)

import Control.Monad.Except (MonadError (catchError))
import Data.ByteString qualified (length)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map, fromList)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (linear)
import System.Directory qualified
import System.Directory.OsPath (createDirectoryLink, createFileLink)
import System.FilePath (combine)
import System.OsPath (OsPath, encodeFS, normalise, (</>))
import Test.Hspec
  ( Spec
  , describe
  , expectationFailure
  , it
  , runIO
  , sequential
  , specify
  , xit
  )
import Test.Hspec.Expectations.Pretty (shouldBe, shouldReturn, shouldThrow)
import Test.Hspec.Hedgehog (MonadGen, forAll, hedgehog, (===))

import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.MonadFileSystem qualified (FileType (..))
import Dojang.Syntax.Manifest.Writer (writeManifestFile)
import Dojang.TestUtils (Entry (..), makeFixtureTree, withTempDir)
import Dojang.Types.Context
  ( CandidateRoute (..)
  , Context (..)
  , FileCorrespondence (..)
  , FileDeltaKind (..)
  , FileEntry (..)
  , FileStat (..)
  , ManagedCorrespondence (..)
  , RouteMatch (..)
  , RouteState (..)
  , calculateSpecificity
  , filterBySpecificity
  , findMatchingRoutes
  , getIgnoredFiles
  , getRouteState
  , getUnregisteredFiles
  , listFiles
  , makeCorrespond
  , makeCorrespondBetweenThreeDirs
  , makeCorrespondBetweenThreeFiles
  , makeCorrespondBetweenTwoDirs
  , makeCorrespondWithDestination
  , makeManagedCorrespond
  , routePaths
  )
import Dojang.Types.Environment
  ( Architecture (..)
  , Environment (..)
  , Kernel (..)
  , OperatingSystem (..)
  , emptyEnvironment
  )
import Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (Always)
  )
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning (..))
import Dojang.Types.FilePathExpression (FilePathExpression (..), (+/+))
import Dojang.Types.FilePathExpression.Expansion (simpleVariableGetter)
import Dojang.Types.FileRoute
  ( RouteKind (CopyRoute, SymlinkRoute)
  , RouteMode (DefaultMode)
  , RouteTarget (RouteTarget)
  , fileRoutePreservingOrder
  )
import Dojang.Types.FileRouteSpec (monikerMap)
import Dojang.Types.Manifest (Manifest (..), manifest)
import Dojang.Types.MonikerName (MonikerName, parseMonikerName)
import Dojang.Types.Repository
  ( Repository (..)
  , RouteMapWarning (..)
  , RouteResult (..)
  )
import Dojang.Types.RouteOwnership
  ( OwnershipError (DuplicateDestinationOwner)
  )
import GHC.IO.Exception (IOErrorType (InappropriateType))


data TestFileState
  = TestMissing
  | TestDirectory
  | TestFile ByteString.ByteString
  | TestSymlink TestLinkTarget
  deriving (Eq, Show)


data TestLinkTarget = TestFileTarget | TestDirectoryTarget
  deriving (Eq, Show)


expectedDelta :: TestFileState -> TestFileState -> FileDeltaKind
expectedDelta TestMissing TestMissing = Unchanged
expectedDelta TestMissing _ = Added
expectedDelta _ TestMissing = Removed
expectedDelta TestDirectory TestDirectory = Unchanged
expectedDelta (TestFile a) (TestFile b)
  | a == b = Unchanged
  | otherwise = Modified
expectedDelta (TestSymlink a) (TestSymlink b)
  | a == b = Unchanged
  | otherwise = Modified
expectedDelta _ _ = Modified


testFileState :: (MonadGen m) => Bool -> m TestFileState
testFileState symlinkAvailable =
  Gen.choice $
    [ return TestMissing
    , return TestDirectory
    , TestFile <$> Gen.bytes (linear 0 64)
    ]
      ++ [ TestSymlink
             <$> Gen.element
               ([TestFileTarget, TestDirectoryTarget] :: [TestLinkTarget])
         | symlinkAvailable
         ]


observeTransitionBothWays
  :: TestFileState
  -> TestFileState
  -> IO (FileDeltaKind, FileDeltaKind)
observeTransitionBothWays intermediateState currentState =
  withTempDir $ \tmpDir _ -> do
    inter <- encodePath "inter"
    src <- encodePath "src"
    dst <- encodePath "dst"
    interDir <- encodePath "inter-dir"
    srcDir <- encodePath "src-dir"
    dstDir <- encodePath "dst-dir"
    entry <- encodePath "entry"
    fileTarget <- encodePath "file-target"
    directoryTarget <- encodePath "directory-target"
    let fileTargetPath = tmpDir </> fileTarget
    let directoryTargetPath = tmpDir </> directoryTarget
    writeFile fileTargetPath "target"
    createDirectory directoryTargetPath
    createDirectory $ tmpDir </> interDir
    createDirectory $ tmpDir </> srcDir
    createDirectory $ tmpDir </> dstDir
    materializeState
      fileTargetPath
      directoryTargetPath
      (tmpDir </> inter)
      intermediateState
    materializeState
      fileTargetPath
      directoryTargetPath
      (tmpDir </> src)
      currentState
    materializeState
      fileTargetPath
      directoryTargetPath
      (tmpDir </> interDir </> entry)
      intermediateState
    materializeState
      fileTargetPath
      directoryTargetPath
      (tmpDir </> srcDir </> entry)
      currentState
    single <-
      makeCorrespondBetweenThreeFiles
        (tmpDir </> inter)
        (tmpDir </> src)
        (tmpDir </> dst)
    directory <-
      makeCorrespondBetweenThreeDirs
        (tmpDir </> interDir)
        (tmpDir </> srcDir)
        (tmpDir </> dstDir)
        []
        []
    let directoryDelta =
          maybe Unchanged (.sourceDelta) $
            find ((== entry) . (.source.path)) directory
    return (single.sourceDelta, directoryDelta)
 where
  materializeState
    :: OsPath
    -> OsPath
    -> OsPath
    -> TestFileState
    -> IO ()
  materializeState _ _ _ TestMissing = return ()
  materializeState _ _ path TestDirectory = createDirectory path
  materializeState _ _ path (TestFile contents) = writeFile path contents
  materializeState fileTarget _ path (TestSymlink TestFileTarget) =
    createFileLink fileTarget path
  materializeState _ directoryTarget path (TestSymlink TestDirectoryTarget) =
    createDirectoryLink directoryTarget path


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

  symlinkAvailable <- runIO $ withTempDir $ \tmpDir _ ->
    ( do
        writeFile (tmpDir </> foo) ""
        createFileLink (tmpDir </> foo) (tmpDir </> bar)
        return True
    )
      `catchError` const (return False)
  let symIt = if symlinkAvailable then it else xit

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
          mempty

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
              (emptyEnvironment Linux X86_64 $ Kernel "Linux" "5.10.0-8")
              $ simpleVariableGetter
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
    sortOn (.sourcePath) ((\route -> route{routeProvenance = mempty}) <$> results)
      `shouldBe` [ RouteResult
                     { sourcePath = tmpDir </> src </> bar
                     , routeName = bar
                     , destinationPath = tmpDir </> bar
                     , fileType = Dojang.MonadFileSystem.Directory
                     , mode = DefaultMode
                     , kind = CopyRoute
                     , routeDefinition = "$BAR"
                     , routeProvenance = mempty
                     }
                 , RouteResult
                     { sourcePath = tmpDir </> src </> foo
                     , routeName = foo
                     , destinationPath = tmpDir </> foo
                     , fileType = Dojang.MonadFileSystem.Directory
                     , mode = DefaultMode
                     , kind = CopyRoute
                     , routeDefinition = "$FOO"
                     , routeProvenance = mempty
                     }
                 ]
  specify "makeCorrespondWithDestination" $
    withContextFixture $
      \ctx tmpDir -> do
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
        let ctx' = ctx{environment = ctx.environment{operatingSystem = Windows}}
        (correspond', ws') <-
          makeCorrespondWithDestination ctx' (tmpDir </> baz)
        ws' `shouldBe` [EnvironmentPredicateWarning $ UndefinedMoniker undefined']
        correspond'
          `shouldBe` Just
            ( FileCorrespondence
                { source = FileEntry (tmpDir </> src </> baz) (File 11)
                , sourceDelta = Added
                , intermediate =
                    FileEntry (tmpDir </> src </> intermediateDir </> baz) Missing
                , destination = FileEntry (tmpDir </> baz) Missing
                , destinationDelta = Unchanged
                }
            )
        (correspond'', ws'') <-
          makeCorrespondWithDestination ctx (tmpDir </> corge)
        correspond'' `shouldBe` Nothing
        ws'' `shouldBe` ws

  specify "makeCorrespond" $ withContextFixture $ \ctx tmpDir -> do
    Right (corresponds, ws) <- makeCorrespond ctx
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

  specify "makeManagedCorrespond excludes nested-owned subtrees" $
    withTempDir $ \tmpDir _ -> do
      outer <- encodePath "outer"
      inner <- encodePath "inner"
      xName <- encodePath "x"
      yName <- encodePath "y"
      zName <- encodePath "z"
      bName <- encodePath "b"
      cName <- encodePath "c"
      aName <- encodePath "a"
      let nestedManifest =
            manifest
              monikerMap
              mempty
              [ (outer, [(posix, Just $ Substitution "OUTER_DST")])
              , (inner, [(posix, Just $ Substitution "INNER_DST")])
              ]
              mempty
              mempty
      createDirectory $ tmpDir </> src
      createDirectory $ tmpDir </> src </> outer
      writeFile (tmpDir </> src </> outer </> xName) "x"
      createDirectory $ tmpDir </> src </> outer </> bName
      createDirectory $ tmpDir </> src </> outer </> bName </> cName
      writeFile
        (tmpDir </> src </> outer </> bName </> cName </> yName)
        "y"
      createDirectory $ tmpDir </> src </> inner
      writeFile (tmpDir </> src </> inner </> zName) "z"
      let repo =
            Repository
              (tmpDir </> src)
              (tmpDir </> src </> intermediateDir)
              nestedManifest
      let ctx = Context
            repo
            (emptyEnvironment Linux X86_64 $ Kernel "Linux" "5.10.0-8")
            $ simpleVariableGetter
            $ \e ->
              return $ case e of
                "OUTER_DST" -> Just $ tmpDir </> aName
                "INNER_DST" -> Just $ tmpDir </> aName </> bName </> cName
                _ -> Nothing
      Right (managed, _) <- makeManagedCorrespond ctx
      sortOn id [(m.route.routeName, m.relativePath) | m <- managed]
        `shouldBe` [ (inner, zName)
                   , (outer, bName)
                   , (outer, xName)
                   ]

  -- Mutates the process working directory; must not run concurrently
  -- with other working-directory users:
  sequential $
    specify "makeManagedCorrespond absolutizes deployment-link sources" $
      withTempDir $ \tmpDir tmpDirFP -> do
        linked <- encodePath "linked"
        relativeRepo <- encodePath "relative-repo"
        let nestedManifest =
              manifest
                monikerMap
                [(linked, [(posix, Just $ Substitution "LINK_DST")])]
                mempty
                mempty
                mempty
        -- Switch the linked file route to a symlink kind:
        let linkRoute =
              fileRoutePreservingOrder
                (const Nothing)
                [
                  ( Always
                  , Just $
                      RouteTarget (Substitution "LINK_DST") DefaultMode SymlinkRoute
                  )
                ]
                Dojang.MonadFileSystem.File
        let manifest'' =
              nestedManifest{fileRoutes = fromList [(linked, linkRoute)]}
        createDirectory $ tmpDir </> relativeRepo
        writeFile (tmpDir </> relativeRepo </> linked) "linked"
        -- A repository opened through a relative path must still produce an
        -- absolute link target, because the stored target must stay valid
        -- regardless of the working directory:
        System.Directory.withCurrentDirectory tmpDirFP $ do
          let repo =
                Repository
                  relativeRepo
                  (relativeRepo </> intermediateDir)
                  manifest''
          let ctx = Context
                repo
                (emptyEnvironment Linux X86_64 $ Kernel "Linux" "5.10.0-8")
                $ simpleVariableGetter
                $ \e ->
                  return $
                    if e == "LINK_DST" then Just (tmpDir </> bar) else Nothing
          Right (managed, _) <- makeManagedCorrespond ctx
          case managed of
            [m] ->
              m.correspondence.source.path
                `shouldBe` normalise (tmpDir </> relativeRepo </> linked)
            other -> expectationFailure $ "Unexpected: " <> show other

  specify "makeManagedCorrespond rejects duplicate destinations" $
    withTempDir $ \tmpDir _ -> do
      outer <- encodePath "outer"
      inner <- encodePath "inner"
      aName <- encodePath "a"
      let clashManifest =
            manifest
              monikerMap
              mempty
              [ (outer, [(posix, Just $ Substitution "CLASH_DST")])
              , (inner, [(posix, Just $ Substitution "CLASH_DST")])
              ]
              mempty
              mempty
      createDirectory $ tmpDir </> src
      createDirectory $ tmpDir </> src </> outer
      createDirectory $ tmpDir </> src </> inner
      let repo =
            Repository
              (tmpDir </> src)
              (tmpDir </> src </> intermediateDir)
              clashManifest
      let ctx = Context
            repo
            (emptyEnvironment Linux X86_64 $ Kernel "Linux" "5.10.0-8")
            $ simpleVariableGetter
            $ \e ->
              return $ case e of
                "CLASH_DST" -> Just $ tmpDir </> aName
                _ -> Nothing
      result <- makeManagedCorrespond ctx
      case result of
        Left (DuplicateDestinationOwner dst' _ _) ->
          dst' `shouldBe` normalise (tmpDir </> aName)
        other -> expectationFailure $ "Unexpected result: " <> show other

  symIt "skips deployment-link routes in destination scans" $
    withTempDir $ \tmpDir _ -> do
      linked <- encodePath "linked-dir"
      dstName <- encodePath "deployed"
      innerName <- encodePath "inner"
      -- A directory route deployed as a symbolic link is a traversal
      -- boundary: its destination must not be enumerated, and listing it
      -- as a directory would throw anyway.
      let linkRoute =
            fileRoutePreservingOrder
              (const Nothing)
              [
                ( Always
                , Just $
                    RouteTarget (Substitution "LINK_DST") DefaultMode SymlinkRoute
                )
              ]
              Dojang.MonadFileSystem.Directory
      let manifest'' =
            (manifest monikerMap mempty mempty mempty mempty)
              { fileRoutes = fromList [(linked, linkRoute)]
              , ignorePatterns = fromList [(linked, ["*"])]
              }
      createDirectory $ tmpDir </> src
      createDirectory $ tmpDir </> src </> linked
      writeFile (tmpDir </> src </> linked </> innerName) "inner"
      createDirectoryLink (tmpDir </> src </> linked) (tmpDir </> dstName)
      let repo =
            Repository
              (tmpDir </> src)
              (tmpDir </> src </> intermediateDir)
              manifest''
      let ctx = Context
            repo
            (emptyEnvironment Linux X86_64 $ Kernel "Linux" "5.10.0-8")
            $ simpleVariableGetter
            $ \e ->
              return $
                if e == "LINK_DST" then Just (tmpDir </> dstName) else Nothing
      getIgnoredFiles ctx `shouldReturn` []
      getUnregisteredFiles ctx `shouldReturn` Right []

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

    writeFile (tmpDir </> inter) "file to directory"
    createDirectory (tmpDir </> src)
    makeCorrespond_
      `shouldReturn` FileCorrespondence
        { source = FileEntry (tmpDir </> src) Directory
        , sourceDelta = Modified
        , intermediate = FileEntry (tmpDir </> inter) $ File 17
        , destination = FileEntry (tmpDir </> dst) Missing
        , destinationDelta = Removed
        }

  describe "file delta semantics" $ do
    let representativeStates =
          [TestMissing, TestDirectory, TestFile "contents"]
            ++ if symlinkAvailable
              then
                [ TestSymlink TestFileTarget
                , TestSymlink TestDirectoryTarget
                ]
              else []
    forM_ representativeStates $ \intermediateState ->
      forM_ representativeStates $ \currentState ->
        it
          ( "classifies "
              ++ show intermediateState
              ++ " to "
              ++ show currentState
          )
          $ do
            (singleDelta, directoryDelta) <-
              observeTransitionBothWays intermediateState currentState
            let expected = expectedDelta intermediateState currentState
            singleDelta `shouldBe` expected
            directoryDelta `shouldBe` expected

    it "keeps single-file and directory deltas equivalent" $ hedgehog $ do
      intermediateState <- forAll $ testFileState symlinkAvailable
      currentState <- forAll $ testFileState symlinkAvailable
      (singleDelta, directoryDelta) <-
        liftIO $ observeTransitionBothWays intermediateState currentState
      let expected = expectedDelta intermediateState currentState
      singleDelta === expected
      directoryDelta === expected

    it "compares arbitrary equal file contents" $ hedgehog $ do
      contents <- forAll $ Gen.bytes (linear 0 64)
      (singleDelta, directoryDelta) <-
        liftIO $
          observeTransitionBothWays
            (TestFile contents)
            (TestFile contents)
      singleDelta === Unchanged
      directoryDelta === Unchanged

    it "compares arbitrary unequal same-size file contents" $ hedgehog $ do
      prefix <- forAll $ Gen.bytes (linear 0 63)
      let intermediateContents = prefix <> ByteString.singleton 0
      let currentContents = prefix <> ByteString.singleton 1
      (singleDelta, directoryDelta) <-
        liftIO $
          observeTransitionBothWays
            (TestFile intermediateContents)
            (TestFile currentContents)
      singleDelta === Modified
      directoryDelta === Modified

    symIt "observes symlinks without following their targets" $
      withTempDir $ \tmpDir _ -> do
        fileTarget <- encodePath "file-target"
        directoryTarget <- encodePath "directory-target"
        interLink <- encodePath "inter-link"
        srcLink <- encodePath "src-link"
        dstLink <- encodePath "dst-link"
        writeFile (tmpDir </> fileTarget) "target"
        createDirectory $ tmpDir </> directoryTarget
        createFileLink (tmpDir </> fileTarget) (tmpDir </> interLink)
        createFileLink (tmpDir </> fileTarget) (tmpDir </> srcLink)
        createDirectoryLink
          (tmpDir </> directoryTarget)
          (tmpDir </> dstLink)
        correspond <-
          makeCorrespondBetweenThreeFiles
            (tmpDir </> interLink)
            (tmpDir </> srcLink)
            (tmpDir </> dstLink)
        correspond
          `shouldBe` FileCorrespondence
            { source =
                FileEntry (tmpDir </> srcLink) $ Symlink (tmpDir </> fileTarget)
            , sourceDelta = Unchanged
            , intermediate =
                FileEntry (tmpDir </> interLink) $ Symlink (tmpDir </> fileTarget)
            , destination =
                FileEntry
                  (tmpDir </> dstLink)
                  (Symlink $ tmpDir </> directoryTarget)
            , destinationDelta = Modified
            }

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
        []
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

  describe "calculateSpecificity" $ do
    it "returns 0 for exact match" $ withTempDir $ \tmpDir _ -> do
      let route =
            RouteResult
              { sourcePath = tmpDir </> foo
              , routeName = foo
              , destinationPath = tmpDir </> bar
              , fileType = Dojang.MonadFileSystem.Directory
              , mode = DefaultMode
              , kind = CopyRoute
              , routeDefinition = ""
              , routeProvenance = mempty
              }
      calculateSpecificity (tmpDir </> bar) route `shouldBe` 0

    it "returns 1 for direct child" $ withTempDir $ \tmpDir _ -> do
      let route =
            RouteResult
              { sourcePath = tmpDir </> foo
              , routeName = foo
              , destinationPath = tmpDir </> bar
              , fileType = Dojang.MonadFileSystem.Directory
              , mode = DefaultMode
              , kind = CopyRoute
              , routeDefinition = ""
              , routeProvenance = mempty
              }
      calculateSpecificity (tmpDir </> bar </> baz) route `shouldBe` 1

    it "returns 2 for grandchild" $ withTempDir $ \tmpDir _ -> do
      let route =
            RouteResult
              { sourcePath = tmpDir </> foo
              , routeName = foo
              , destinationPath = tmpDir </> bar
              , fileType = Dojang.MonadFileSystem.Directory
              , mode = DefaultMode
              , kind = CopyRoute
              , routeDefinition = ""
              , routeProvenance = mempty
              }
      calculateSpecificity (tmpDir </> bar </> baz </> qux) route `shouldBe` 2

  describe "filterBySpecificity" $ do
    it "returns empty list for empty input" $ withTempDir $ \tmpDir _ -> do
      filterBySpecificity (tmpDir </> foo) [] `shouldBe` []

    it "keeps only most specific routes" $ withTempDir $ \tmpDir _ -> do
      let route1 =
            RouteResult
              { sourcePath = tmpDir </> src </> foo
              , routeName = foo
              , destinationPath = tmpDir </> bar
              , fileType = Dojang.MonadFileSystem.Directory
              , mode = DefaultMode
              , kind = CopyRoute
              , routeDefinition = ""
              , routeProvenance = mempty
              }
      let route2 =
            RouteResult
              { sourcePath = tmpDir </> src </> baz
              , routeName = baz
              , destinationPath = tmpDir </> bar </> qux
              , fileType = Dojang.MonadFileSystem.Directory
              , mode = DefaultMode
              , kind = CopyRoute
              , routeDefinition = ""
              , routeProvenance = mempty
              }
      -- route1: specificity = 2 (bar/qux/foo)
      -- route2: specificity = 1 (qux/foo)
      let target = tmpDir </> bar </> qux </> foo
      filterBySpecificity target [route1, route2] `shouldBe` [route2]

    it "keeps all routes when equally specific" $ withTempDir $ \tmpDir _ -> do
      let route1 =
            RouteResult
              { sourcePath = tmpDir </> src </> foo
              , routeName = foo
              , destinationPath = tmpDir </> dst
              , fileType = Dojang.MonadFileSystem.Directory
              , mode = DefaultMode
              , kind = CopyRoute
              , routeDefinition = ""
              , routeProvenance = mempty
              }
      let route2 =
            RouteResult
              { sourcePath = tmpDir </> src </> bar
              , routeName = bar
              , destinationPath = tmpDir </> dst
              , fileType = Dojang.MonadFileSystem.Directory
              , mode = DefaultMode
              , kind = CopyRoute
              , routeDefinition = ""
              , routeProvenance = mempty
              }
      -- Both routes have the same destinationPath, so same specificity
      let target = tmpDir </> dst </> baz
      filterBySpecificity target [route1, route2] `shouldBe` [route1, route2]

  describe "findMatchingRoutes" $ do
    it "returns NoMatch when no routes match" $ withContextFixture $ \ctx tmpDir -> do
      (result, _) <- findMatchingRoutes ctx (tmpDir </> corge)
      result `shouldBe` NoMatch

    it "returns SingleMatch for unambiguous route" $ withContextFixture $ \ctx tmpDir -> do
      (result, _) <- findMatchingRoutes ctx (tmpDir </> foo </> foo)
      case result of
        SingleMatch route -> route.routeName `shouldBe` foo
        _ -> fail "Expected SingleMatch"

    it "returns SingleMatch when filtered by specificity" $ withTempDir $ \tmpDir _ -> do
      -- Create a context with overlapping routes at different depths
      createDirectory $ tmpDir </> src
      createDirectory $ tmpDir </> dst
      createDirectory $ tmpDir </> dst </> foo
      writeFile (tmpDir </> src </> manifestFilename') ""

      -- route1 points to DST (via env var), route2 points to DST_FOO (via env var)
      -- For target dst/foo/bar, route2 is more specific (specificity=1 vs 2)
      let overlappingFileRoutes =
            [ (foo, [(posix, Just $ Substitution "DST")])
            , (bar, [(posix, Just $ Substitution "DST_FOO")])
            ]
              :: Map OsPath [(MonikerName, Maybe FilePathExpression)]
      let overlappingManifest =
            Dojang.Types.Manifest.manifest
              monikerMap
              []
              overlappingFileRoutes
              []
              mempty
      let repo =
            Repository
              (tmpDir </> src)
              (tmpDir </> src </> intermediateDir)
              overlappingManifest
      let ctx =
            Context
              repo
              (emptyEnvironment Linux X86_64 $ Kernel "Linux" "5.10.0-8")
              $ simpleVariableGetter
              $ \e -> return $ case e of
                "DST" -> Just $ tmpDir </> dst
                "DST_FOO" -> Just $ tmpDir </> dst </> foo
                _ -> Nothing
      (result, _) <- findMatchingRoutes ctx (tmpDir </> dst </> foo </> baz)
      case result of
        SingleMatch route -> route.routeName `shouldBe` bar
        _ -> fail $ "Expected SingleMatch, got " ++ show result

    it "returns AmbiguousMatch for equally specific routes" $ withTempDir $ \tmpDir _ -> do
      -- Create a context with two routes pointing to the same destination
      createDirectory $ tmpDir </> src
      createDirectory $ tmpDir </> src </> foo
      createDirectory $ tmpDir </> src </> bar
      createDirectory $ tmpDir </> dst
      writeFile (tmpDir </> src </> manifestFilename') ""

      -- Both foo and bar routes point to DST (via env var)
      let ambiguousFileRoutes =
            [ (foo, [(posix, Just $ Substitution "DST")])
            , (bar, [(posix, Just $ Substitution "DST")])
            ]
              :: Map OsPath [(MonikerName, Maybe FilePathExpression)]
      let ambiguousManifest =
            Dojang.Types.Manifest.manifest
              monikerMap
              []
              ambiguousFileRoutes
              []
              mempty
      let repo =
            Repository
              (tmpDir </> src)
              (tmpDir </> src </> intermediateDir)
              ambiguousManifest
      let ctx =
            Context
              repo
              (emptyEnvironment Linux X86_64 $ Kernel "Linux" "5.10.0-8")
              $ simpleVariableGetter
              $ \e -> return $ case e of
                "DST" -> Just $ tmpDir </> dst
                _ -> Nothing
      (result, _) <- findMatchingRoutes ctx (tmpDir </> dst </> baz)
      case result of
        AmbiguousMatch candidates -> do
          length candidates `shouldBe` 2
          -- All candidates should have same specificity
          let firstSpec = (NE.head candidates).specificity
          all (\c -> c.specificity == firstSpec) (NE.toList candidates) `shouldBe` True
        _ -> fail $ "Expected AmbiguousMatch, got " ++ show result
