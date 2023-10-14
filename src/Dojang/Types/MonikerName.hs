{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use guards" #-}

module Dojang.Types.MonikerName
  ( MonikerName
  , MonikerNameError (..)
  , parseMonikerName
  ) where

import Data.CaseInsensitive (CI, mk)
import Data.Char (isAlpha, isAlphaNum)
import Data.Hashable (Hashable)
import Data.Text (Text, findIndex, head, null)
import GHC.Records (HasField (getField))

import Prelude hiding (head, null)


-- $setup
-- >>> :seti -XOverloadedStrings -XOverloadedRecordDot


-- | A moniker name.  The name must start with a letter and contain only
-- letters, digits, hyphens, and underscores (no spaces, no special characters).
-- Non-Latin letters and digits are allowed too.  The name must not be empty.
--
-- >>> parseMonikerName "foo-bar"
-- Right (MonikerName "foo-bar")
-- >>> parseMonikerName "甲乙_丙丁"
-- Right (MonikerName "甲乙_丙丁")
--
-- Case insensitive, but the original case is preserved:
--
-- >>> parseMonikerName "FOO-BAR" == parseMonikerName "foo-bar"
-- True
-- >>> parseMonikerName "GIÁP_ẤT" == parseMonikerName "giáp_ất" -- cSpell:disable-line
-- True
newtype MonikerName = MonikerName (CI Text) deriving (Eq, Ord, Hashable, Show)


-- | A record field named @name@ for 'MonikerName':
--
-- >>> let (Right monikerName) = parseMonikerName "foo-bar"
-- >>> monikerName.name
-- "foo-bar"
instance HasField "name" MonikerName (CI Text) where
  getField (MonikerName name) = name


-- | Errors that can occur when parsing a moniker name.
data MonikerNameError
  = -- | The moniker name is empty.
    Empty
  | -- | The moniker name starts with a non-letter character.
    StartingWithNonLetter
  | -- | The moniker name contains an invalid character at the given index.
    HavingInvalidCharacter Int
  deriving (Eq, Show, Read)


-- | Parse a moniker name.
--
-- >>> parseMonikerName "foo-bar"
-- Right (MonikerName "foo-bar")
-- >>> parseMonikerName "1"
-- Left StartingWithNonLetter
-- >>> parseMonikerName "foo bar"
-- Left (HavingInvalidCharacter 3)
-- >>> parseMonikerName "甲乙_丙丁"
-- Right (MonikerName "甲乙_丙丁")
-- >>> parseMonikerName ""
-- Left Empty
parseMonikerName :: Text -> Either MonikerNameError MonikerName
parseMonikerName name =
  if null name
    then Left Empty
    else
      if isAlpha $ head name
        then case findIndex isInvalidChar name of
          Nothing -> Right $ MonikerName $ mk name
          Just i -> Left $ HavingInvalidCharacter i
        else Left StartingWithNonLetter
 where
  isInvalidChar :: Char -> Bool
  isInvalidChar c = not $ isAlphaNum c || c == '-' || c == '_'
