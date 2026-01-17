{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | This module provides disambiguation logic for resolving ambiguous source
-- paths when multiple routes map to the same destination.
module Dojang.Commands.Disambiguation
  ( AutoSelectMode (..)
  , disambiguateRoutes
  , getAutoSelectMode
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map.Strict (Map, fromList, lookup)
import System.Environment (lookupEnv)
import Prelude hiding (lookup)

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import FortyTwo.Prompts.Select (select)
import System.OsPath (OsPath)

import Dojang.App (App)
import Dojang.MonadFileSystem (MonadFileSystem (decodePath))
import Dojang.Types.Context (CandidateRoute (..))
import Dojang.Types.Repository (RouteResult (..))


-- | The mode for auto-selecting among ambiguous routes.
data AutoSelectMode
  = -- | Prompt the user interactively to select a route.
    Interactive
  | -- | Automatically select the first route without prompting.
    AutoSelectFirst
  | -- | Return an error when routes are ambiguous.
    ErrorOnAmbiguity
  deriving (Eq, Show)


-- | Get the auto-select mode from the @DOJANG_AUTO_SELECT@ environment
-- variable.
--
-- Possible values:
--
--   * @\"first\"@ - Auto-select the first route without prompting.
--   * @\"error\"@ - Return an error when routes are ambiguous.
--   * (unset or other) - Prompt the user interactively.
getAutoSelectMode :: (MonadIO m) => m AutoSelectMode
getAutoSelectMode = liftIO $ do
  env <- lookupEnv "DOJANG_AUTO_SELECT"
  return $ case env of
    Just "first" -> AutoSelectFirst
    Just "error" -> ErrorOnAmbiguity
    _ -> Interactive


-- | Disambiguate routes when multiple candidates exist.
--
-- The disambiguation strategy is:
--
-- 1. If an explicit source path is provided via @--source@, use that.
-- 2. If exactly one source file exists, auto-select it.
-- 3. Otherwise, apply the 'AutoSelectMode':
--
--    * 'Interactive' - Prompt the user to select a route.
--    * 'AutoSelectFirst' - Select the first route.
--    * 'ErrorOnAmbiguity' - Return 'Nothing' to signal an error.
disambiguateRoutes
  :: (MonadFileSystem i, MonadIO i)
  => AutoSelectMode
  -- ^ The auto-select mode.
  -> Maybe OsPath
  -- ^ An explicit source path provided via @--source@ option.
  -> NonEmpty CandidateRoute
  -- ^ The candidate routes to disambiguate.
  -> App i (Maybe RouteResult)
  -- ^ The selected route, or 'Nothing' if disambiguation failed.
disambiguateRoutes mode explicitSource candidates =
  case explicitSource of
    Just srcPath -> do
      -- Find a candidate matching the explicit source path.
      let matching =
            [c | c <- NE.toList candidates, c.route.routeName == srcPath]
      case matching of
        (c : _) -> return $ Just c.route
        [] -> return Nothing
    Nothing -> do
      -- Try to auto-select based on existence.
      let existingCandidates =
            [c | c <- NE.toList candidates, c.sourceExists]
      case existingCandidates of
        [c] -> return $ Just c.route
        _ -> case mode of
          AutoSelectFirst -> return $ Just (NE.head candidates).route
          ErrorOnAmbiguity -> return Nothing
          Interactive -> promptForRoute candidates


-- | Prompt the user to select a route from multiple candidates.
promptForRoute
  :: (MonadFileSystem i, MonadIO i)
  => NonEmpty CandidateRoute
  -> App i (Maybe RouteResult)
promptForRoute candidates = do
  -- Decode route names to strings for display.
  routeNamePairs <- forM (NE.toList candidates) $ \c -> do
    name <- decodePath c.route.routeName
    return (name, c.route)
  let routeNames = map fst routeNamePairs
  let nameToRoute :: Map String RouteResult
      nameToRoute = fromList routeNamePairs
  -- Prompt user to select.
  selected <- liftIO $ select "Multiple routes match. Select one:" routeNames
  return $ lookup selected nameToRoute
