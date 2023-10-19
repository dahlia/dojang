{-# LANGUAGE OverloadedStrings #-}

module Dojang.Types.FilePathExpression
  ( EnvironmentVariable
  , FilePathExpression (..)
  , toPathText
  , (++)
  , (+/+)
  ) where

import Data.Char (isAlphaNum, isAscii)
import Data.String (IsString (..))
import Prelude hiding (head, null, (++))

import Data.Hashable (Hashable (hashWithSalt))
import Data.Text (Text, head, null, pack)


-- | An environment variable.
type EnvironmentVariable = Text


-- | An expression that can be used to build a file path.
data FilePathExpression
  = -- | A bare component of a file path.  E.g., @foo@ and @bar@ in @/foo/bar@.
    BareComponent Text
  | -- | The root of a file path.  E.g., @/@ on POSIX systems or @C:\@ on
    -- Windows.  The 'Maybe' 'Char' is the drive letter on Windows, if present.
    -- On POSIX systems, this is always 'Nothing'.
    Root (Maybe Char)
  | -- | A concatenation of two 'FilePathExpression's.  E.g., @${FOO}bar@ is
    -- a concatenation of @${FOO}@ and @bar@.
    Concatenation FilePathExpression FilePathExpression
  | -- | A path separator which combines two 'FilePathExpression's.  E.g.,
    -- @/@ on POSIX systems or @\@ on Windows.
    PathSeparator FilePathExpression FilePathExpression
  | -- | A simple substitution of an environment variable.  E.g., @$FOO@ or
    -- @${BAR}@.
    Substitution EnvironmentVariable
  | -- | A substitution of an environment variable with a default value for when
    -- the environment variable is not set.  E.g., @${FOO:-this value is used if
    -- FOO is not set or is empty}@.
    SubstitutionWithDefault EnvironmentVariable FilePathExpression
  | -- | A conditional substitution of an environment variable with an expression
    -- to be expanded if the environment variable is set.  E.g., @${FOO:+this
    -- value is used if FOO is set and non-empty}@.
    ConditionalSubstitution EnvironmentVariable FilePathExpression
  -- TODO: Add support for SubstringSubstitution (e.g., ${FOO:0:3})
  -- See also https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
  deriving (Eq, Show)


instance Hashable FilePathExpression where
  hashWithSalt salt (BareComponent component) =
    salt `hashWithSalt` (0 :: Int) `hashWithSalt` component
  hashWithSalt salt (Root driveLetter) =
    salt `hashWithSalt` (1 :: Int) `hashWithSalt` driveLetter
  hashWithSalt salt (Concatenation expr1 expr2) =
    salt `hashWithSalt` (2 :: Int) `hashWithSalt` expr1 `hashWithSalt` expr2
  hashWithSalt salt (PathSeparator expr1 expr2) =
    salt `hashWithSalt` (3 :: Int) `hashWithSalt` expr1 `hashWithSalt` expr2
  hashWithSalt salt (Substitution envVar) =
    salt `hashWithSalt` (4 :: Int) `hashWithSalt` envVar
  hashWithSalt salt (SubstitutionWithDefault envVar expr) =
    salt `hashWithSalt` (5 :: Int) `hashWithSalt` envVar `hashWithSalt` expr
  hashWithSalt salt (ConditionalSubstitution envVar expr) =
    salt `hashWithSalt` (6 :: Int) `hashWithSalt` envVar `hashWithSalt` expr


infixl 5 ++, +/+


-- | Concatenates two 'FilePathExpression's.  A synonym for 'Concatenation'.
(++) :: FilePathExpression -> FilePathExpression -> FilePathExpression
(++) = Concatenation
{-# INLINE (++) #-}


-- | Combines two 'FilePathExpression's with a path separator.  A synonym for
-- 'PathSeparator'.
(+/+) :: FilePathExpression -> FilePathExpression -> FilePathExpression
(+/+) = PathSeparator
{-# INLINE (+/+) #-}


-- | Converts a 'FilePathExpression' to a textual representation.
toPathText :: FilePathExpression -> Text
toPathText (Root (Just driveLetter)) = pack [driveLetter, ':', '/']
toPathText expr = toPathText' False expr


-- | Internal implementation of 'toPathText'.
toPathText'
  :: Bool
  -- ^ Whether it is followed by a 'BareComponent'.
  -> FilePathExpression
  -- ^ The 'FilePathExpression' to render.
  -> Text
  -- ^ The rendered 'FilePathExpression'.
toPathText' _ (BareComponent component) = component
toPathText' _ (Root Nothing) = "/"
toPathText' _ (Root (Just driveLetter)) = pack [driveLetter, ':']
toPathText' followedByBare (Concatenation a b) =
  toPathText' doesBStartWithBare a <> renderedB
 where
  renderedB :: Text
  renderedB = toPathText' followedByBare b
  doesBStartWithBare :: Bool
  doesBStartWithBare =
    not (null renderedB)
      && let c = head renderedB in isAscii c && isAlphaNum c || c == '_'
toPathText' followedByBare (PathSeparator a@(Root Nothing) b) =
  toPathText' False a <> toPathText' followedByBare b
toPathText' followedByBare (PathSeparator a b) =
  toPathText' False a <> "/" <> toPathText' followedByBare b
toPathText' False (Substitution envVar) = "$" <> envVar
toPathText' True (Substitution envVar) = "${" <> envVar <> "}"
toPathText' _ (SubstitutionWithDefault envVar expr) =
  "${" <> envVar <> ":-" <> toPathText' False expr <> "}"
toPathText' _ (ConditionalSubstitution envVar expr) =
  "${" <> envVar <> ":+" <> toPathText' False expr <> "}"


instance IsString FilePathExpression where
  fromString = BareComponent . pack
