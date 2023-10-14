{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.FileRoute
  ( FileRoute (..)
  , FileType (..)
  , fileRoute
  , fileRoute'
  , dispatch
  ) where

import Dojang.Evaluate (EvaluationWarning, evaluate')
import Dojang.Types.Environment (Environment)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.EnvironmentPredicate.Specificity (specificity)
import Dojang.Types.FilePathExpression (FilePathExpression)
import Dojang.Types.MonikerMap (MonikerMap, MonikerResolver)
import Dojang.Types.MonikerName (MonikerName)

import Data.HashMap.Strict (lookup)
import Data.HashSet (HashSet, singleton, union, unions)
import Data.List (sortOn)
import Data.Ord (Down (Down))

import Prelude hiding (lookup)


-- | A type that represents a file or directory.
data FileType
  = -- | A file.
    File
  | -- | A directory.
    Directory
  deriving (Eq, Show)


-- | A route that maps 'EnvironmentPredicate's to 'FilePathExpression's.
--
-- Each 'EnvironmentPredicate' represents a condition that must be met for the
-- 'FilePathExpression' to be dispatched.  The 'FilePathExpression' is
-- the path to the directory that should be dispatched to.  If it is 'Nothing',
-- then it is considered a "null route" and will not be dispatched.
--
-- Note that a null route will still be considered a match, and will be
-- dispatched as 'Nothing'.
data FileRoute = FileRoute
  { monikerResolver :: MonikerResolver
  -- ^ The 'MonikerResolver' used to resolve 'MonikerName's.
  , predicates :: [(EnvironmentPredicate, Maybe FilePathExpression)]
  -- ^ The pairs of 'EnvironmentPredicate's and 'FilePathExpression's that make
  -- up the 'FileRoute'.  If the 'FilePathExpression' is 'Nothing', then it is
  -- considered a "null route" and will not be dispatched.
  , fileType :: FileType
  -- ^ Whether the 'FileRoute' represents a file or a directory.
  }


instance Eq FileRoute where
  a == b =
    (a.fileType == b.fileType)
      && (a.predicates == b.predicates)
      && all (\m -> a.monikerResolver m == b.monikerResolver m) allMonikerRefs
   where
    monikerRefs :: EnvironmentPredicate -> HashSet MonikerName
    monikerRefs predicates' =
      case predicates' of
        Moniker monikerName -> singleton monikerName
        Or ps -> foldMap monikerRefs ps
        And ps -> foldMap monikerRefs ps
        Not p -> monikerRefs p
        OperatingSystem _ -> mempty
        Architecture _ -> mempty
        Always -> mempty
    allMonikerRefs :: HashSet MonikerName
    allMonikerRefs =
      unions (monikerRefs . fst <$> a.predicates)
        `union` unions (monikerRefs . fst <$> b.predicates)


-- | Creates a 'FileRoute' from a 'MonikerMap' and pairs of
-- 'MonikerName's and 'FilePathExpression's.
fileRoute
  :: MonikerMap
  -- ^ The 'MonikerMap' to resolve 'MonikerName's against.
  -> [(MonikerName, Maybe FilePathExpression)]
  -- ^ The pairs of 'MonikerName's and 'FilePathExpression's that make up the
  -- 'FileRoute'.
  -> FileType
  -- ^ Whether the 'FileRoute' represents a file or a directory.
  -> FileRoute
  -- ^ The resulting 'FileRoute'.
fileRoute monikerMap predicates' =
  fileRoute'
    (`lookup` monikerMap)
    [(Moniker name, pathExpr) | (name, pathExpr) <- predicates']


-- | Similar to 'FileRoute', but it takes more raw arguments: a 'MonikerResolver'
-- (instead of a 'MonikerMap'), and a pairs of 'EnvironmentPredicate' (instead
-- of 'MonikerName') and 'FilePathExpression'.
fileRoute'
  :: MonikerResolver
  -- ^ A function that resolves a 'MonikerName' to an 'EnvironmentPredicate'.
  -> [(EnvironmentPredicate, Maybe FilePathExpression)]
  -- ^ The pairs of 'EnvironmentPredicate's and 'FilePathExpression's that make
  -- up the 'FileRoute'.
  -> FileType
  -- ^ Whether the 'FileRoute' represents a file or a directory.
  -> FileRoute
  -- ^ The resulting 'FileRoute'.
fileRoute' resolver predicates' =
  FileRoute resolver $ sortOn (Down . specificity resolver . fst) predicates'


-- | Dispatches the given 'FileRoute' against the given 'Environment' and
-- 'MonikerMap'.
dispatch
  :: Environment
  -- ^ The 'Environment' to dispatch against.
  -> FileRoute
  -- ^ The 'FileRoute' to dispatch.
  -> ([Maybe FilePathExpression], [EvaluationWarning])
  -- ^ The list of 'Maybe' 'FilePathExpression's that matched
  -- the given 'Environment' and 'MonikerMap'.  Even if matches are made,
  -- their values may be 'Nothing' if the 'FilePathExpression' was
  -- a "null route".
  -- The 'snd' of the result is a list of warnings that occurred
  -- during evaluation  whether or not any 'FilePathExpression' matched.
dispatch environment (FileRoute resolver route _) =
  (matched, warnings)
 where
  eval :: EnvironmentPredicate -> (Bool, [EvaluationWarning])
  eval = evaluate' environment resolver
  evaluated :: [((Bool, [EvaluationWarning]), Maybe FilePathExpression)]
  evaluated = [(eval predicate, expr) | (predicate, expr) <- route]
  warnings :: [EvaluationWarning]
  warnings = concatMap (snd . fst) evaluated
  matched :: [Maybe FilePathExpression]
  matched = [expr | ((True, _), expr) <- evaluated]


instance Show FileRoute where
  show (FileRoute _ route fileType') =
    "FileRoute " ++ show route ++ " " ++ show fileType'
