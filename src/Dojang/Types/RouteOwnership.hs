{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , verifyResolvedIdentities
  ) where

import Data.List (isPrefixOf, sortOn)
import Data.Ord (Down (Down))

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import System.OsPath
  ( OsPath
  , makeRelative
  , normalise
  , splitDirectories
  , takeDirectory
  , takeFileName
  , (</>)
  )

import Dojang.MonadFileSystem (MonadFileSystem (canonicalizePath, exists))
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
  | -- | A route's source and destination are the same filesystem tree, or
    -- one contains the other, either lexically or after resolving symbolic
    -- links.  Carries the route name, its source path, and its destination.
    SourceDestinationAliased OsPath OsPath OsPath
  | -- | A route's destination lies inside the repository checkout (or
    -- contains it), so applying it would write into the repository itself.
    -- Carries the route name and its destination.
    DestinationInsideRepository OsPath OsPath
  | -- | Two lexically nested destinations no longer nest the same way once
    -- symbolic links are resolved, so the exclusions computed from their
    -- lexical shape would not protect the nested route's entries.  Carries
    -- the nested route's name and the ancestor route's name.
    NestedDestinationsDiverged OsPath OsPath
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
formatOwnershipError renderPath (SourceDestinationAliased name source dst) =
  "Route "
    <> renderPath name
    <> " would synchronize "
    <> renderPath source
    <> " with "
    <> renderPath dst
    <> ", but they are the same filesystem tree or one contains the other."
formatOwnershipError renderPath (DestinationInsideRepository name dst) =
  "Route "
    <> renderPath name
    <> " routes to "
    <> renderPath dst
    <> ", which overlaps the repository checkout itself."
formatOwnershipError renderPath (NestedDestinationsDiverged nested ancestor) =
  "The destination of route "
    <> renderPath nested
    <> " nests inside the destination of route "
    <> renderPath ancestor
    <> " lexically, but resolving symbolic links changes that relationship."


-- | Selects one owning route per destination from the active routes of
-- the current environment.  Fails when ownership would be ambiguous
-- ('DuplicateDestinationOwner'), would reach through a deployed symbolic
-- link ('RouteThroughLinkBoundary'), or would write into the repository
-- checkout ('DestinationInsideRepository').
selectOwnership
  :: OsPath
  -- ^ The repository checkout root (source directory).
  -> [RouteResult]
  -- ^ The active routes of the current environment.
  -> Either OwnershipError ExpectedState
selectOwnership repositoryRoot routes = do
  case duplicates of
    (dst, a, b) : _ -> Left $ DuplicateDestinationOwner dst a b
    [] -> pure ()
  case insideRepository of
    (name, dst) : _ -> Left $ DestinationInsideRepository name dst
    [] -> pure ()
  case lexicalAliases of
    (name, source, dst) : _ ->
      Left $ SourceDestinationAliased name source dst
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
  insideRepository :: [(OsPath, OsPath)]
  insideRepository =
    [ (route.routeName, dst)
    | (dst, route) <- normalized
    , let root = normalise repositoryRoot
    , dst == root || dst `strictlyInside` root || root `strictlyInside` dst
    ]
  lexicalAliases :: [(OsPath, OsPath, OsPath)]
  lexicalAliases =
    [ (route.routeName, source, dst)
    | (dst, route) <- normalized
    , let source = normalise route.sourcePath
    , source == dst
        || source `strictlyInside` dst
        || dst `strictlyInside` source
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


-- | Checks that no route's source and destination resolve to the same
-- filesystem tree once symbolic links are followed.  Lexically distinct
-- paths can still alias each other through links; this pass canonicalizes
-- every source and destination (resolving the deepest existing ancestor
-- of paths that do not exist yet) and rejects containment either way
-- before any mutation is planned.  It also rejects two destinations that
-- resolve to one tree, which lexical duplicate detection cannot see.
--
-- The destination of a symlink route is resolved only up to its parent
-- directory: the destination leaf is expected to be a link back at the
-- route's source once deployed, and following it would misreport every
-- correctly deployed link as aliasing.
verifyResolvedIdentities
  :: forall m
   . (MonadFileSystem m)
  => OsPath
  -- ^ The repository checkout root (source directory).
  -> ExpectedState
  -- ^ The ownership selection to verify.
  -> m (Either OwnershipError ())
  -- ^ 'Right' when no resolved aliasing exists; the first
  -- 'SourceDestinationAliased', 'DestinationInsideRepository', or
  -- 'DuplicateDestinationOwner' error otherwise.
verifyResolvedIdentities repositoryRoot state = do
  resolvedRoot <- resolveEffective repositoryRoot
  resolved <- mapM resolveOwner $ Map.toAscList state.owners
  case inRepository resolvedRoot resolved of
    (name, dst) : _ ->
      return $ Left $ DestinationInsideRepository name dst
    [] -> case aliases resolved of
      (name, source, dst) : _ ->
        return $ Left $ SourceDestinationAliased name source dst
      [] -> case resolvedDuplicates resolved of
        (dst, a, b) : _ ->
          return $ Left $ DuplicateDestinationOwner dst a b
        [] -> case divergedNesting resolved of
          (nested, ancestor) : _ ->
            return $ Left $ NestedDestinationsDiverged nested ancestor
          [] -> return $ Right ()
 where
  inRepository
    :: OsPath
    -> [(OsPath, OsPath, OsPath, RouteResult)]
    -> [(OsPath, OsPath)]
  inRepository resolvedRoot resolved =
    [ (route.routeName, dst)
    | (dst, dst', _, route) <- resolved
    , dst' == resolvedRoot
        || dst' `strictlyInside` resolvedRoot
        || resolvedRoot `strictlyInside` dst'
    ]
  resolveOwner
    :: (OsPath, RouteResult) -> m (OsPath, OsPath, OsPath, RouteResult)
  resolveOwner (dst, route) = do
    resolvedSource <- resolveEffective route.sourcePath
    resolvedDestination <- case route.kind of
      SymlinkRoute -> do
        parent <- resolveEffective $ takeDirectory dst
        return $ normalise $ parent </> takeFileName dst
      _ -> resolveEffective dst
    return (dst, resolvedDestination, resolvedSource, route)
  aliases
    :: [(OsPath, OsPath, OsPath, RouteResult)]
    -> [(OsPath, OsPath, OsPath)]
  aliases resolved =
    [ (route.routeName, route.sourcePath, dst)
    | (dst, dst', source', route) <- resolved
    , source' == dst'
        || source' `strictlyInside` dst'
        || dst' `strictlyInside` source'
    ]
  resolvedDuplicates
    :: [(OsPath, OsPath, OsPath, RouteResult)]
    -> [(OsPath, OsPath, OsPath)]
  resolvedDuplicates resolved =
    [ (dstA, a.routeName, b.routeName)
    | ((dstA, resolvedA, _, a), rest) <-
        zip resolved (drop 1 $ iterate (drop 1) resolved)
    , (dstB, resolvedB, _, b) <- rest
    , resolvedA == resolvedB
        || ( ( resolvedA `strictlyInside` resolvedB
                 || resolvedB `strictlyInside` resolvedA
             )
               && not (dstA `strictlyInside` dstB)
               && not (dstB `strictlyInside` dstA)
           )
    ]
  -- Lexical nesting must survive resolution in the same direction:
  -- exclusions and boundaries are computed from the lexical shape, so a
  -- nested destination escaping its ancestor through a link would be
  -- unprotected.
  divergedNesting
    :: [(OsPath, OsPath, OsPath, RouteResult)]
    -> [(OsPath, OsPath)]
  divergedNesting resolved =
    [ (nested.routeName, ancestor.routeName)
    | (dstA, resolvedA, _, nested) <- resolved
    , (dstB, resolvedB, _, ancestor) <- resolved
    , dstA `strictlyInside` dstB
    , not $ resolvedA `strictlyInside` resolvedB
    ]
  resolveEffective :: OsPath -> m OsPath
  resolveEffective path = do
    present <- exists path
    if present
      then normalise <$> canonicalizePath path
      else do
        let parent = takeDirectory path
        if parent == path
          then return $ normalise path
          else do
            resolvedParent <- resolveEffective parent
            return $ normalise $ resolvedParent </> takeFileName path


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
