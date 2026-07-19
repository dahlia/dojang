{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Per-route metadata declared by a manifest: portable file modes and
-- destination kinds.
--
-- This module intentionally has no dependency on "Dojang.MonadFileSystem" so
-- that both the route layer and the filesystem layer can share the mode
-- vocabulary without an import cycle.
module Dojang.Types.RouteMetadata
  ( RouteKind (..)
  , RouteMode (..)
  , RouteTarget (..)
  , parseRouteKind
  , parseRouteMode
  , posixDirectoryModeBits
  , posixFileModeBits
  , renderRouteKind
  , renderRouteMode
  , routeTarget
  ) where

import Data.Text (Text)

import Dojang.Types.FilePathExpression (FilePathExpression)


-- | A portable file mode that a route can declare for its destination.
-- The vocabulary is intentionally closed: each value expresses an intent
-- that can be mapped to concrete POSIX permission bits (see
-- 'posixFileModeBits' and 'posixDirectoryModeBits') and to a best-effort
-- behavior on Windows.
data RouteMode
  = -- | No declared mode; the destination mode is left unmanaged, which is
    -- the behavior of routes that declare no @mode@ at all.
    DefaultMode
  | -- | Readable and writable by the owner only (files @0600@,
    -- directories @0700@).
    Private
  | -- | Executable by everyone, writable by the owner (files @0755@).
    -- Meaningless for directory routes.
    Executable
  | -- | Executable, readable, and writable by the owner only (files
    -- @0700@).  Meaningless for directory routes.
    PrivateExecutable
  | -- | Readable by everyone, writable by no one (files @0444@,
    -- directories @0555@).
    ReadOnly
  deriving (Bounded, Enum, Eq, Ord, Show)


-- | A destination kind that a route can declare.
data RouteKind
  = -- | The destination is an ordinary copied entry, which is the behavior
    -- of routes that declare no @kind@ at all.
    CopyRoute
  | -- | The destination is a symbolic link pointing back at the route's
    -- repository source (a deployment link).
    SymlinkRoute
  deriving (Bounded, Enum, Eq, Ord, Show)


-- | The value a route branch maps to when it is not a null route:
-- a destination path expression together with the declared metadata.
data RouteTarget = RouteTarget
  { expression :: FilePathExpression
  -- ^ The destination path expression.
  , mode :: RouteMode
  -- ^ The declared portable mode of the destination.
  , kind :: RouteKind
  -- ^ The declared kind of the destination.
  }
  deriving (Eq, Show)


-- | Wraps a bare 'FilePathExpression' into a 'RouteTarget' carrying no
-- metadata ('DefaultMode' and 'CopyRoute'), which behaves exactly like
-- a route value written as a plain string.
routeTarget :: FilePathExpression -> RouteTarget
routeTarget expr = RouteTarget expr DefaultMode CopyRoute


-- | Parses the manifest spelling of a 'RouteMode'.  Returns 'Nothing' for
-- any string outside the closed vocabulary; matching is case-sensitive.
parseRouteMode :: Text -> Maybe RouteMode
parseRouteMode "default" = Just DefaultMode
parseRouteMode "private" = Just Private
parseRouteMode "executable" = Just Executable
parseRouteMode "private-executable" = Just PrivateExecutable
parseRouteMode "read-only" = Just ReadOnly
parseRouteMode _ = Nothing


-- | Renders a 'RouteMode' into its manifest spelling.
-- The inverse of 'parseRouteMode'.
renderRouteMode :: RouteMode -> Text
renderRouteMode DefaultMode = "default"
renderRouteMode Private = "private"
renderRouteMode Executable = "executable"
renderRouteMode PrivateExecutable = "private-executable"
renderRouteMode ReadOnly = "read-only"


-- | Parses the manifest spelling of a 'RouteKind'.  Returns 'Nothing' for
-- any string outside the closed vocabulary; matching is case-sensitive.
parseRouteKind :: Text -> Maybe RouteKind
parseRouteKind "copy" = Just CopyRoute
parseRouteKind "symlink" = Just SymlinkRoute
parseRouteKind _ = Nothing


-- | Renders a 'RouteKind' into its manifest spelling.
-- The inverse of 'parseRouteKind'.
renderRouteKind :: RouteKind -> Text
renderRouteKind CopyRoute = "copy"
renderRouteKind SymlinkRoute = "symlink"


-- | The exact POSIX permission bits a 'RouteMode' declares for a regular
-- file, or 'Nothing' when the mode leaves file permissions unmanaged.
posixFileModeBits :: RouteMode -> Maybe Word
posixFileModeBits DefaultMode = Nothing
posixFileModeBits Private = Just 0o600
posixFileModeBits Executable = Just 0o755
posixFileModeBits PrivateExecutable = Just 0o700
posixFileModeBits ReadOnly = Just 0o444


-- | The exact POSIX permission bits a 'RouteMode' declares for a directory,
-- or 'Nothing' when the mode leaves directory permissions unmanaged or is
-- meaningless for directories (the executable modes).
posixDirectoryModeBits :: RouteMode -> Maybe Word
posixDirectoryModeBits DefaultMode = Nothing
posixDirectoryModeBits Private = Just 0o700
posixDirectoryModeBits Executable = Nothing
posixDirectoryModeBits PrivateExecutable = Nothing
posixDirectoryModeBits ReadOnly = Just 0o555
