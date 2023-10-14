{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.EnvironmentPredicate.Writer
  ( writeEnvironmentPredicate
  )
where

import Dojang.Types.Environment (Architecture, OperatingSystem)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.MonikerName (MonikerName)

import Data.CaseInsensitive (CI (original))
import Data.Char (isAlpha, isAlphaNum, isControl)
import Data.List.NonEmpty (toList)
import Data.Text
  ( Text
  , all
  , concatMap
  , elem
  , intercalate
  , justifyRight
  , pack
  , singleton
  , uncons
  )
import Numeric (showHex)
import Prelude hiding (all, concatMap, elem, reverse)


-- | Turns an 'EnvironmentPredicate' into a string that can be used in a
-- @dojang.toml@ file.  The string can be parsed back into an
-- 'EnvironmentPredicate' using
-- 'Dojang.Syntax.EnvironmentPredicate.Parser.parseEnvironmentPredicate'.
writeEnvironmentPredicate :: EnvironmentPredicate -> Text
writeEnvironmentPredicate = fst . write


write
  :: EnvironmentPredicate
  -> (Text, Bool)
-- \^ @(when, can be split by &&)@
write Always = ("always", False)
write (OperatingSystem os) =
  ("os = " <> stringLiteral (original os.identifier), False)
write (Architecture arch) =
  ("arch = " <> stringLiteral (original arch.identifier), False)
write (Moniker moniker) =
  ("moniker = " <> stringLiteral (original moniker.name), False)
write (And predicates) =
  case (excludeOses, excludeArchitectures, excludeMonikers) of
    (_ : _, [], [])
      | length excludeOses == length predicates ->
          ( "os not in "
              <> strings [original os.identifier | os <- excludeOses]
          , False
          )
    ([], _ : _, [])
      | length excludeArchitectures == length predicates ->
          ( "arch not in "
              <> strings
                [ original arch.identifier
                | arch <- excludeArchitectures
                ]
          , False
          )
    ([], [], _ : _)
      | length excludeMonikers == length predicates ->
          ( "moniker not in "
              <> strings [original moniker.name | moniker <- excludeMonikers]
          , False
          )
    _ ->
      ( intercalate
          " && "
          [ let (when', needParentheses) = write p
            in if needParentheses then "(" <> when' <> ")" else when'
          | p <- toList predicates
          ]
      , False
      )
 where
  excludeOses :: [OperatingSystem]
  excludeOses = [os | Not (OperatingSystem os) <- toList predicates]
  excludeArchitectures :: [Architecture]
  excludeArchitectures = [arch | Not (Architecture arch) <- toList predicates]
  excludeMonikers :: [MonikerName]
  excludeMonikers = [moniker | Not (Moniker moniker) <- toList predicates]
write (Or predicates) =
  case (oses, architectures, monikers) of
    (_ : _, [], [])
      | length oses == length predicates ->
          ("os in " <> strings [original os.identifier | os <- oses], False)
    ([], _ : _, [])
      | length architectures == length predicates ->
          ( "arch in "
              <> strings [original arch.identifier | arch <- architectures]
          , False
          )
    ([], [], _ : _)
      | length monikers == length predicates ->
          ( "moniker in "
              <> strings [original moniker.name | moniker <- monikers]
          , False
          )
    _ -> (intercalate " || " (fst . write <$> toList predicates), True)
 where
  oses :: [OperatingSystem]
  oses = [os | OperatingSystem os <- toList predicates]
  architectures :: [Architecture]
  architectures = [arch | Architecture arch <- toList predicates]
  monikers :: [MonikerName]
  monikers = [moniker | Moniker moniker <- toList predicates]
write (Not Always) = ("never", False)
write (Not (OperatingSystem os)) =
  ("os != " <> stringLiteral (original os.identifier), False)
write (Not (Architecture arch)) =
  ("arch != " <> stringLiteral (original arch.identifier), False)
write (Not (Moniker moniker)) =
  ("moniker != " <> stringLiteral (original moniker.name), False)
write (Not p) = ("!(" <> fst (write p) <> ")", False)


stringLiteral :: Text -> Text
stringLiteral v = case uncons v of
  Nothing -> "\"\""
  Just (c, cs) | isAlpha c && all isAlphaNum cs -> v
  _ | '"' `elem` v && not ('\'' `elem` v) -> "'" <> escapeText False v <> "'"
  _ -> "\"" <> escapeText True v <> "\""
 where
  escapeText :: Bool -> Text -> Text
  escapeText doubleQuote = concatMap (escape doubleQuote)
  escape :: Bool -> Char -> Text
  escape True '"' = "\\\""
  escape False '\'' = "\\'"
  escape _ '\\' = "\\\\"
  escape _ '\b' = "\\b"
  escape _ '\f' = "\\f"
  escape _ '\n' = "\\n"
  escape _ '\r' = "\\r"
  escape _ '\t' = "\\t"
  escape _ '\v' = "\\v"
  escape _ '\0' = "\\0"
  escape _ c
    | not (isControl c) && c < '\x80' = singleton c
    | c <= '\xff' = "\\x" <> justifyRight 2 '0' (hex c)
    | c <= '\xffff' = "\\u" <> justifyRight 4 '0' (hex c) -- cSpell: disable-line
    | otherwise = "\\U" <> justifyRight 8 '0' (hex c)
   where
    hex :: Char -> Text
    hex = pack . (`showHex` "") . fromEnum


strings :: [Text] -> Text
strings = ("(" <>) . (<> ")") . intercalate ", " . fmap stringLiteral
