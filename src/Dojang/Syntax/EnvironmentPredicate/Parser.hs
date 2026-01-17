{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.EnvironmentPredicate.Parser
  ( Field (..)
  , FieldOp (..)
  , ParseErrorBundle
  , andExpression
  , bareStringLiteral
  , charactersInStringLiteral
  , doubleQuoteStringLiteral
  , equalOp
  , errorBundlePretty
  , expression
  , field
  , fieldOp
  , inOp
  , notEqualOp
  , notInOp
  , parse
  , parseEnvironmentPredicate
  , prefixOp
  , simpleExpression
  , singleQuoteStringLiteral
  , stringLiteral
  , strings
  , suffixOp
  ) where

import Control.Applicative (optional, (<|>))
import Control.Applicative.Combinators (choice, sepBy1, sepEndBy)
import Control.Monad (void)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.MonikerName (parseMonikerName)

import Data.CaseInsensitive (mk)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString (fromString))
import Data.Text (Text, concat, cons, pack, singleton, unpack)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, takeWhile1P, takeWhileP, try)
  , Parsec
  , label
  , many
  , parse
  , satisfy
  )
import Text.Megaparsec.Char (char, hexDigitChar, space, space1, string)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Megaparsec.Error qualified (errorBundlePretty)
import Prelude hiding (concat)


type Parser = Parsec Void Text


-- | Parse a 'EnvironmentPredicate' from a 'Text'.
parseEnvironmentPredicate
  :: FilePath
  -- ^ The name of source file.
  -> Text
  -- ^ The input text.
  -> Either (ParseErrorBundle Text Void) EnvironmentPredicate
  -- ^ The result of parsing.
parseEnvironmentPredicate = parse $ expression <* eof


-- | A parser for 'EnvironmentPredicate'.
expression :: Parser EnvironmentPredicate
expression = label "expression" $ do
  space
  subExprs <- andExpression `sepBy1` (space >> string "||" >> space)
  space
  return $ case subExprs of
    [x] -> x
    x : xs -> Or $ x :| xs
    [] -> error "sepBy1 should never return an empty list"


andExpression :: Parser EnvironmentPredicate
andExpression = do
  space
  subExprs <- simpleExpression `sepBy1` (space >> string "&&" >> space)
  space
  return $ case subExprs of
    [x] -> x
    x : xs -> And $ x :| xs
    [] -> error "sepBy1 should never return an empty list"


simpleExpression :: Parser EnvironmentPredicate
simpleExpression = do
  space
  expr <-
    choice
      [ char '(' >> space >> expression <* space <* char ')'
      , string "always" >> return Always
      , string "never" >> return (Not Always)
      , do
          void $ char '!'
          space
          void $ char '('
          space
          expr' <- expression
          space
          void $ char ')'
          return $ Not expr'
      , fieldOp
      ]
  space
  return expr


data FieldOp
  = EqualOp Text
  | NotEqualOp Text
  | PrefixOp Text
  | SuffixOp Text
  | InOp [Text]
  | NotInOp [Text]


fieldOp :: Parser EnvironmentPredicate
fieldOp = do
  space
  field' <- field
  op <-
    choice
      [ try equalOp
      , try notEqualOp
      , try inOp
      , try notInOp
      , if field' == KernelRelease' then try prefixOp else fail ""
      , if field' == KernelRelease' then try suffixOp else fail ""
      ]
  space
  return $ case op of
    EqualOp value -> makePredicate field' value
    NotEqualOp value -> Not $ makePredicate field' value
    PrefixOp prefix -> KernelReleasePrefix $ mk prefix
    SuffixOp suffix -> KernelReleaseSuffix $ mk suffix
    InOp [] -> Not Always
    InOp (x : xs) -> Or $ makePredicate field' <$> (x :| xs)
    NotInOp [] -> Always
    NotInOp (x : xs) -> And $ Not . makePredicate field' <$> (x :| xs)
 where
  makePredicate :: Field -> Text -> EnvironmentPredicate
  makePredicate OS = OperatingSystem . fromString . unpack
  makePredicate Arch = Architecture . fromString . unpack
  makePredicate Kernel = KernelName . fromString . unpack
  makePredicate KernelRelease' = KernelRelease . fromString . unpack
  makePredicate Moniker' =
    either (const $ Not Always) Moniker . parseMonikerName


equalOp :: Parser FieldOp
equalOp = do
  space
  void $ char '='
  void $ optional (char '=')
  space
  EqualOp <$> stringLiteral


notEqualOp :: Parser FieldOp
notEqualOp = do
  space
  void $ string "!="
  space
  NotEqualOp <$> stringLiteral


prefixOp :: Parser FieldOp
prefixOp = do
  space
  void $ string "^="
  space
  PrefixOp <$> stringLiteral


suffixOp :: Parser FieldOp
suffixOp = do
  space
  void $ string "$="
  space
  SuffixOp <$> stringLiteral


inOp :: Parser FieldOp
inOp = do
  space1
  void $ string "in"
  space
  InOp <$> strings


notInOp :: Parser FieldOp
notInOp = do
  space1
  void $ string "not"
  space1
  void $ string "in"
  space
  NotInOp <$> strings


strings :: Parser [Text]
strings = do
  space
  void $ char '('
  space
  members <- stringLiteral `sepEndBy` (space >> char ',' >> space)
  space
  void $ char ')'
  space
  return members


data Field = OS | Arch | Kernel | KernelRelease' | Moniker' deriving (Eq)


field :: Parser Field
field =
  choice
    [ OS <$ string "os"
    , Arch <$ string "arch"
    , KernelRelease' <$ string "kernel-release"
    , Kernel <$ string "kernel"
    , Moniker' <$ string "moniker"
    ]


stringLiteral :: Parser Text
stringLiteral = label "string literal" $ do
  space
  string' <-
    choice
      [ doubleQuoteStringLiteral
      , singleQuoteStringLiteral
      , bareStringLiteral
      ]
  space
  return string'


bareStringLiteral :: Parser Text
bareStringLiteral = label "bare string literal" $ do
  first <- satisfy isAlpha
  str <- takeWhileP Nothing isAlphaNum
  return $ first `cons` str


doubleQuoteStringLiteral :: Parser Text
doubleQuoteStringLiteral = label "double quote string literal" $ do
  void $ char '"'
  str <- charactersInStringLiteral '"'
  void $ char '"'
  return str


singleQuoteStringLiteral :: Parser Text
singleQuoteStringLiteral = label "single quote string literal" $ do
  void $ char '\''
  str <- charactersInStringLiteral '\''
  void $ char '\''
  return str


charactersInStringLiteral :: Char -> Parser Text
charactersInStringLiteral terminal = do
  chunks <-
    many
      $ takeWhile1P (Just "string character") (\c -> c /= '\\' && c /= terminal)
      <|> singleton
      <$> charEscape
  return $ concat chunks
 where
  charEscape :: Parser Char
  charEscape = do
    void $ char '\\'
    choice
      [ char '\\' >> return '\\'
      , char '\'' >> return '\''
      , char '"' >> return '"'
      , char 'b' >> return '\b'
      , char 'f' >> return '\f'
      , char 'n' >> return '\n'
      , char 'r' >> return '\r'
      , char 't' >> return '\t'
      , char 'v' >> return '\v'
      , char '0' >> return '\0'
      , do
          void $ char 'x'
          d1 <- hex
          d2 <- hex
          return $ toEnum $ d1 * 0x10 + d2
      , do
          void $ char 'u'
          d1 <- hex
          d2 <- hex
          d3 <- hex
          d4 <- hex
          return $ toEnum $ d1 * 0x1000 + d2 * 0x100 + d3 * 0x10 + d4
      , do
          void $ char 'U'
          d1 <- hex
          d2 <- hex
          d3 <- hex
          d4 <- hex
          d5 <- hex
          d6 <- hex
          d7 <- hex
          d8 <- hex
          return
            $ toEnum
            $ d1
            * 0x10000000
            + d2
            * 0x1000000
            + d3
            * 0x100000
            + d4
            * 0x10000
            + d5
            * 0x1000
            + d6
            * 0x100
            + d7
            * 0x10
            + d8
      ]
  hex :: Parser Int
  hex = do
    c <- hexDigitChar
    return
      $ if isDigit c
        then fromEnum c - 0x30
        else
          if 'A' <= c && c <= 'F'
            then fromEnum c - 0x37
            else fromEnum c - 0x57


-- | Basically the same as 'Text.Megaparsec.Error.errorBundlePretty' but
-- returns a 'Text' instead of a 'String'.
errorBundlePretty :: ParseErrorBundle Text Void -> Text
errorBundlePretty = pack . Text.Megaparsec.Error.errorBundlePretty
