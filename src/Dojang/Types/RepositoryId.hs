{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stable identities for Dojang repositories.
module Dojang.Types.RepositoryId
  ( RepositoryId
  , newRepositoryId
  , parseRepositoryId
  , repositoryIdText
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4


-- | A stable UUID that travels with a repository independently of its path.
newtype RepositoryId = RepositoryId UUID
  deriving (Eq, Ord, Show)


-- | Generates a new random version 4 repository UUID.
newRepositoryId :: (MonadIO m) => m RepositoryId
newRepositoryId = RepositoryId <$> liftIO UUID.V4.nextRandom


-- | Parses a UUID as a repository identity.
parseRepositoryId :: Text -> Either Text RepositoryId
parseRepositoryId text = case UUID.fromText text of
  Nothing -> Left "Invalid repository ID."
  Just uuid -> Right $ RepositoryId uuid


-- | Renders a repository identity as a canonical lowercase UUID.
repositoryIdText :: RepositoryId -> Text
repositoryIdText (RepositoryId uuid) = UUID.toText uuid
