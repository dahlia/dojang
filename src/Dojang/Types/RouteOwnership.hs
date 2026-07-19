{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Selection of one owning route per destination entry.
--
-- Before any synchronization is planned, every destination entry must have
-- exactly one route responsible for it.  The most-specific route (the one
-- with the deepest destination path) owns its whole destination subtree,
-- and broader routes must neither read nor write entries owned by routes
-- nested inside them.  A directory deployed as a symbolic link is
-- a traversal boundary: no other route may reach through it.
module Dojang.Types.RouteOwnership
  ( ExpectedState (..)
  , OwnershipError (..)
  , formatOwnershipError
  , ownerOf
  , ownedExclusions
  , selectOwnership
  ) where

import Data.List (isPrefixOf, sortOn)
import Data.Ord (Down (Down))

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import System.OsPath (OsPath, makeRelative, normalise, splitDirectories)

import Dojang.Types.FileRoute (RouteKind (SymlinkRoute))
import Dojang.Types.Repository (RouteResult (..))


-- | The per-destination route policy shared by every command: which route
-- owns each destination root, which destinations are traversal boundaries,
-- and which owned subtrees are nested inside broader routes.
data ExpectedState = ExpectedState
  { owners :: Map OsPath RouteResult
  -- ^ Normalized destination roots and the routes that own them.
  , boundaries :: Set OsPath
  -- ^ Destinations deployed as symbolic links.  No route may read or
  -- write through these paths.
  , nestedUnder :: Map OsPath [OsPath]
  -- ^ For each destination root, the normalized destination roots of
  -- other routes nested strictly inside it, in ascending order.  The
  -- owning route must exclude these subtrees from its own traversal.
  }
  deriving (Eq, Show)


-- | A route configuration that cannot be given unambiguous per-entry
-- ownership, detected before any mutation is planned.
data OwnershipError
  = -- | Two active routes expanded to the same destination path.
    -- Carries the destination and both route names.
    DuplicateDestinationOwner OsPath OsPath OsPath
  | -- | A route's destination lies strictly inside a destination that is
    -- deployed as a symbolic link.  Carries the offending route name, its
    -- destination, and the boundary destination.
    RouteThroughLinkBoundary OsPath OsPath OsPath
  deriving (Eq, Show)


-- | Formats an 'OwnershipError' for display to a user, given a function
-- that renders an 'OsPath'.
formatOwnershipError :: (OsPath -> Text) -> OwnershipError -> Text
formatOwnershipError renderPath (DuplicateDestinationOwner dst a b) =
  "Routes "
    <> renderPath a
    <> " and "
    <> renderPath b
    <> " both route to "
    <> renderPath dst
    <> "; every destination must have exactly one owning route."
formatOwnershipError renderPath (RouteThroughLinkBoundary name dst boundary) =
  "Route "
    <> renderPath name
    <> " routes to "
    <> renderPath dst
    <> ", which is inside the symbolic link deployed at "
    <> renderPath boundary
    <> "; routes cannot reach through a deployed link."


-- | Selects one owning route per destination from the active routes of
-- the current environment.  Fails when ownership would be ambiguous
-- ('DuplicateDestinationOwner') or would reach through a deployed
-- symbolic link ('RouteThroughLinkBoundary').
selectOwnership :: [RouteResult] -> Either OwnershipError ExpectedState
selectOwnership routes = do
  case duplicates of
    (dst, a, b) : _ -> Left $ DuplicateDestinationOwner dst a b
    [] -> pure ()
  case boundaryViolations of
    (name, dst, boundary) : _ ->
      Left $ RouteThroughLinkBoundary name dst boundary
    [] -> pure ()
  pure
    ExpectedState
      { owners = Map.fromList normalized
      , boundaries = boundarySet
      , nestedUnder =
          Map.fromList
            [ (dst, nested)
            | (dst, _) <- normalized
            , let nested =
                    [ dst'
                    | (dst', _) <- normalized
                    , dst' `strictlyInside` dst
                    ]
            , not $ null nested
            ]
      }
 where
  normalized :: [(OsPath, RouteResult)]
  normalized =
    sortOn fst [(normalise route.destinationPath, route) | route <- routes]
  duplicates :: [(OsPath, OsPath, OsPath)]
  duplicates =
    [ (dst, a.routeName, b.routeName)
    | ((dst, a), (dst', b)) <- zip normalized (drop 1 normalized)
    , dst == dst'
    ]
  boundarySet :: Set OsPath
  boundarySet =
    Set.fromList [dst | (dst, route) <- normalized, route.kind == SymlinkRoute]
  boundaryViolations :: [(OsPath, OsPath, OsPath)]
  boundaryViolations =
    [ (route.routeName, dst, boundary)
    | (dst, route) <- normalized
    , boundary <- Set.toAscList boundarySet
    , dst `strictlyInside` boundary
    ]


-- | Whether the first path lies strictly inside the second.
strictlyInside :: OsPath -> OsPath -> Bool
strictlyInside path ancestor =
  ancestorDirs `isPrefixOf` pathDirs && length ancestorDirs < length pathDirs
 where
  pathDirs = splitDirectories path
  ancestorDirs = splitDirectories ancestor


-- | Resolves the most-specific route owning the given destination path,
-- if any route contains it.
ownerOf :: ExpectedState -> OsPath -> Maybe RouteResult
ownerOf state path =
  case sortOn (Down . length . splitDirectories . fst) containing of
    (_, route) : _ -> Just route
    [] -> Nothing
 where
  path' = normalise path
  containing =
    [ (dst, route)
    | (dst, route) <- Map.toAscList state.owners
    , dst == path' || path' `strictlyInside` dst
    ]


-- | The subtree roots a route must exclude while enumerating its own
-- destination, relative to the route's destination path: the destinations
-- of routes nested inside it.  Paths are returned relative to the given
-- destination root.
ownedExclusions :: ExpectedState -> OsPath -> [OsPath]
ownedExclusions state destination =
  [ makeRelative destination' nested
  | nested <- Map.findWithDefault [] destination' state.nestedUnder
  ]
 where
  destination' = normalise destination
