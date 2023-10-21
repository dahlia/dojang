{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dojang.Types.Repository (Repository (..), FilePair (..), pairFiles) where

import Control.Monad (forM)
import System.IO.Error (ioeSetFileName)

import Data.Text (unpack)
import System.OsPath (OsPath, makeRelative, normalise, (</>))
import System.OsString (OsString)

import Data.Map.Strict (Map, toList)
import Dojang.MonadFileSystem (MonadFileSystem (..))
import Dojang.Types.Environment (Environment)
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning)
import Dojang.Types.FilePathExpression (EnvironmentVariable, FilePathExpression)
import Dojang.Types.FilePathExpression.Expansion (expandFilePath)
import Dojang.Types.FileRoute (FileType (..))
import Dojang.Types.FileRouteMap.Evaluate (evaluateRoutesWithFileTypes)
import Dojang.Types.Manifest (Manifest (..))


-- | A repository, which is a directory containing a manifest file and dotfiles.
data Repository = Repository
  { path :: OsPath
  -- ^ The path to the repository.
  , manifestFilename :: OsPath
  -- ^ The filename of the manifest file.  This is relative to the repository
  -- 'path'.
  , manifest :: Manifest
  -- ^ The manifest of the repository.
  }


-- | A pairing of a source file and a destination file.
data FilePair = FilePair
  { source :: OsPath
  -- ^ The source file path.
  , destination :: OsPath
  -- ^ The destination file path.
  , fileType :: FileType
  -- ^ Whether the file is a file or a directory.
  }
  deriving (Eq, Show)


instance Ord FilePair where
  compare a b = compare a.source b.source


-- | Corresponds source files to destination files, that is a blueprint for
-- copying files.
pairFiles
  :: (MonadFileSystem m)
  => Repository
  -- ^ The repository that contains the 'Manifest' and dotfiles.
  -> Environment
  -- ^ The environment to evaluate the 'Manifest' against.
  -> (EnvironmentVariable -> m (Maybe OsString))
  -- ^ A function that can look up an environment variable.  Used for expanding
  -- 'FilePathExpression's.
  -> m (Either IOError [FilePair], [EvaluationWarning])
  -- ^ The paired files and the warnings that occurred during evaluation.
  -- Either it fails due to an 'IOError' or it succeeds with the paired files,
  -- there could be warnings.
pairFiles repo env lookupEnv = do
  let (evaluatedRoutes, warnings) =
        evaluateRoutesWithFileTypes repo.manifest.fileRoutes env
  files <- pairFiles' evaluatedRoutes repo.path lookupEnv
  return (files, warnings)


pairFiles'
  :: forall m
   . (MonadFileSystem m)
  => Map OsPath (FilePathExpression, FileType)
  -> OsPath
  -> (EnvironmentVariable -> m (Maybe OsString))
  -> m (Either IOError [FilePair])
pairFiles' evaluatedRoutes repoRoot lookupEnv = do
  files <- forM (toList evaluatedRoutes) $ \(srcBase, (expr, fType)) -> do
    dstBase <- expandFilePath expr lookupEnv (encodePath . unpack)
    let base =
          FilePair
            { source = normalise $ repoRoot </> srcBase
            , destination = normalise dstBase
            , fileType = fType
            }
    case fType of
      File -> do
        exists' <- exists (repoRoot </> srcBase)
        isDir <- isDirectory (repoRoot </> srcBase)
        decodedName <- decodePath (repoRoot </> srcBase)
        return $ case (exists', isDir) of
          (False, _) ->
            Left $ userError "No such file" `ioeSetFileName` decodedName
          (_, True) ->
            Left $ userError "Is a directory" `ioeSetFileName` decodedName
          (_, _) -> Right [base]
      Directory -> do
        result <- listFiles (repoRoot </> srcBase) (repoRoot </> srcBase)
        case result of
          Left e -> return $ Left e
          Right files' -> do
            return
              $ Right
              $ base
              : [ FilePair
                  { source = normalise $ repoRoot </> srcBase </> relPath
                  , destination = normalise $ dstBase </> relPath
                  , fileType = fType'
                  }
                | (fType', relPath) <- files'
                ]
  return $ foldl combine (Right []) files
 where
  combine
    :: Either IOError [FilePair]
    -> Either IOError [FilePair]
    -> Either IOError [FilePair]
  combine (Left e) _ = Left e
  combine _ (Left e) = Left e
  combine (Right files1) (Right files2) = Right $ files1 ++ files2


listFiles
  :: (MonadFileSystem m)
  => OsPath
  -> OsPath
  -> m (Either IOError [(FileType, OsPath)])
listFiles root dir = do
  result <- listDirectory dir'
  case result of
    Left e -> return $ Left e
    Right files -> list files
 where
  list
    :: (MonadFileSystem m) => [OsPath] -> m (Either IOError [(FileType, OsPath)])
  list [] = return $ Right []
  list (f : fs) = do
    isDir <- isDirectory (dir' </> f)
    rest <- list fs
    if isDir
      then do
        result <- listFiles root' (dir' </> f)
        case (result, rest) of
          (Left e, _) -> return $ Left e
          (_, Left e) -> return $ Left e
          (Right files, Right rest') -> do
            return $ Right $ (Directory, relDir </> f) : files ++ rest'
      else return $ case rest of
        Left e -> Left e
        Right rest' -> Right $ (File, relDir </> f) : rest'
  root' :: OsPath
  root' = normalise root
  dir' :: OsPath
  dir' = normalise dir
  relDir :: OsPath
  relDir = makeRelative root' dir'
