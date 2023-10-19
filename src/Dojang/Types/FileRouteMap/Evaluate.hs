module Dojang.Types.FileRouteMap.Evaluate (evaluateRoutes) where

import Data.Map.Strict (Map, fromList, toList)
import Dojang.Types.Environment (Environment)
import Dojang.Types.EnvironmentPredicate.Evaluate (EvaluationWarning)
import Dojang.Types.FilePathExpression (FilePathExpression)
import Dojang.Types.FileRoute (dispatch)
import Dojang.Types.FileRouteMap (FileRouteMap)
import System.OsPath (OsPath)


-- | Evaluates the 'FileRouteMap' against an 'Environment'.
evaluateRoutes
  :: FileRouteMap
  -- ^ The 'FileRouteMap' to evaluate.
  -> Environment
  -- ^ The 'Environment' to evaluate against.
  -> (Map OsPath FilePathExpression, [EvaluationWarning])
  -- ^ The evaluated routes and the warnings that occurred during evaluation.
  -- Unmatched routes or null routes will not be included in the result.
evaluateRoutes fileRoutes env =
  (evaluated, warnings)
 where
  results :: [(OsPath, FilePathExpression, [EvaluationWarning])]
  results =
    [ (filePath, match, warnings')
    | (filePath, route) <- toList fileRoutes
    , let (matches, warnings') = dispatch env route
    , Just match : _ <- [matches]
    ]
  warnings :: [EvaluationWarning]
  warnings = concatMap (\(_, _, w) -> w) results
  evaluated :: Map OsPath FilePathExpression
  evaluated = fromList [(filePath, match) | (filePath, match, _) <- results]
