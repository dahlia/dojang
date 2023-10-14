{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.FilePathExpression.Parser
  ( ParseErrorBundle
  , bareComponent
  , environmentVariable
  , errorBundlePretty
  , filePathExpression
  , filePathExpression'
  , parseFilePathExpression
  , parse
  , root
  , substitution
  , substitutionConditional
  , substitutionDefault
  , substitutionInBraces
  , substitutionModifierAfterColon
  ) where

import Dojang.Types.FilePathExpression
  ( EnvironmentVariable
  , FilePathExpression (..)
  )

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isAscii, isControl)
import Data.Text (Text, cons, pack)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, takeWhile1P, takeWhileP)
  , Parsec
  , label
  , many
  , oneOf
  , parse
  , satisfy
  , some
  , try
  , (<?>)
  )
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Megaparsec.Error qualified (errorBundlePretty)


type Parser = Parsec Void Text


-- | Parse a 'FilePathExpression' from a 'Text'.
parseFilePathExpression
  :: FilePath
  -- ^ The name of source file.
  -> Text
  -- ^ The input text.
  -> Either (ParseErrorBundle Text Void) FilePathExpression
  -- ^ The result of parsing.
parseFilePathExpression = parse $ filePathExpression <* eof


-- | A parser for 'FilePathExpression'.
filePathExpression :: Parser FilePathExpression
filePathExpression = filePathExpression' True []
{-# INLINE filePathExpression #-}


filePathExpression' :: Bool -> [Char] -> Parser FilePathExpression
filePathExpression' begin disallowedBareChars = do
  root' <- if begin then optional $ try root else return Nothing
  head' <- case root' of
    Nothing -> do
      firstComponent <- component begin disallowedBareChars
      components <- many $ component False disallowedBareChars
      return $ foldl Concatenation firstComponent components
    Just r -> do
      components <- many $ component False disallowedBareChars
      return $ case components of
        [] -> r
        c : cs -> PathSeparator r $ foldl Concatenation c cs
  betweenSeparators <- many $ try $ do
    void $ oneOf ['/', '\\']
    components' <- some $ component False disallowedBareChars
    return $ foldl1 Concatenation components'
  let topExpr = foldl PathSeparator head' betweenSeparators
  void $ optional $ oneOf ['/', '\\']
  return topExpr
{-# INLINE filePathExpression' #-}


component :: Bool -> [Char] -> Parser FilePathExpression
component begin disallowedBareChars =
  bareComponent disallowedBareChars
    <|> substitutionInBraces begin
    <|> substitution
{-# INLINE component #-}


bareComponent :: [Char] -> Parser FilePathExpression
bareComponent disallowedChars = do
  component' <- takeWhile1P (Just "component") isValidChar
  return $ BareComponent component'
 where
  isValidChar :: Char -> Bool
  isValidChar = not . isInvalidChar
  isInvalidChar :: Char -> Bool
  isInvalidChar c =
    isControl c
      || c
      == '/'
      || c
      == '\\'
      || c
      == '$'
      || c
      == '\r'
      || c
      == '\n'
      || c
      `elem` disallowedChars
{-# INLINE bareComponent #-}


root :: Parser FilePathExpression
root =
  driveLetter <|> rootSlash
 where
  driveLetter :: Parser FilePathExpression
  driveLetter = do
    letter <- satisfy (`elem` ['A' .. 'Z']) <?> "drive letter"
    void $ char ':'
    void $ satisfy (`elem` ['/', '\\'])
    return $ Root $ Just letter
  rootSlash :: Parser FilePathExpression
  rootSlash = char '/' >> return (Root Nothing)


environmentVariable :: Parser EnvironmentVariable
environmentVariable = label "environment variable" $ do
  first <- satisfy (\c -> isAscii c && isAlpha c || c == '_')
  rest <- takeWhileP Nothing (\c -> isAscii c && isAlphaNum c || c == '_')
  return $ cons first rest
{-# INLINE environmentVariable #-}


substitution :: Parser FilePathExpression
substitution =
  char '$' >> Substitution <$> environmentVariable
{-# INLINE substitution #-}


substitutionInBraces :: Bool -> Parser FilePathExpression
substitutionInBraces begin = do
  void $ string "${"
  envVar <- environmentVariable
  colon <- optional $ char ':'
  expr <- case colon of
    Nothing -> return $ Substitution envVar
    Just _ -> substitutionModifierAfterColon begin envVar
  void $ char '}'
  return expr
{-# INLINE substitutionInBraces #-}


substitutionModifierAfterColon
  :: Bool
  -> EnvironmentVariable
  -> Parser FilePathExpression
substitutionModifierAfterColon begin envVar =
  substitutionDefault begin envVar <|> substitutionConditional begin envVar
{-# INLINE substitutionModifierAfterColon #-}


substitutionDefault :: Bool -> EnvironmentVariable -> Parser FilePathExpression
substitutionDefault begin envVar = do
  void $ char '-'
  SubstitutionWithDefault envVar <$> filePathExpression' begin "}"
{-# INLINE substitutionDefault #-}


substitutionConditional
  :: Bool -> EnvironmentVariable -> Parser FilePathExpression
substitutionConditional begin envVar = do
  void $ char '+'
  ConditionalSubstitution envVar <$> filePathExpression' begin "}"
{-# INLINE substitutionConditional #-}


-- | Basically the same as 'Text.Megaparsec.Error.errorBundlePretty' but
-- returns a 'Text' instead of a 'String'.
errorBundlePretty :: ParseErrorBundle Text Void -> Text
errorBundlePretty = pack . Text.Megaparsec.Error.errorBundlePretty
