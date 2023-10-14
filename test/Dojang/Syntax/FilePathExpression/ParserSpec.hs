{-# LANGUAGE OverloadedStrings #-}

module Dojang.Syntax.FilePathExpression.ParserSpec (spec) where

import Control.Monad (forM_)
import Prelude hiding (unlines)

import Data.Text (unlines)
import Test.Hspec (Spec, specify)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.Megaparsec
  ( eeof
  , elabel
  , err
  , etok
  , etoks
  , shouldFailWith
  , shouldParse
  , ueof
  , utok
  , utoks
  )
import Text.Megaparsec (eof, parse)

import Dojang.Syntax.FilePathExpression.Parser
  ( bareComponent
  , environmentVariable
  , errorBundlePretty
  , filePathExpression
  , filePathExpression'
  , parseFilePathExpression
  , root
  , substitution
  , substitutionConditional
  , substitutionDefault
  , substitutionInBraces
  , substitutionModifierAfterColon
  )
import Dojang.Types.FilePathExpression (FilePathExpression (..), (+/+))


spec :: Spec
spec = do
  specify "environmentVariable" $ do
    let environmentVariable' = environmentVariable <* eof
    parse environmentVariable' "" "FOO_BAR" `shouldParse` "FOO_BAR"
    parse environmentVariable' "" "foo_bar" `shouldParse` "foo_bar"
    parse environmentVariable "" "_BAR_123" `shouldParse` "_BAR_123"
    parse environmentVariable "" "123_FOO"
      `shouldFailWith` err 0 (utok '1' <> elabel "environment variable")
    parse environmentVariable "" "-FOO"
      `shouldFailWith` err 0 (utok '-' <> elabel "environment variable")
    parse environmentVariable' "" "FOO-BAR"
      `shouldFailWith` err 3 (utok '-' <> eeof)
    parse environmentVariable "" "甲_乙"
      `shouldFailWith` err 0 (utok '甲' <> elabel "environment variable")
    parse environmentVariable' "" "FOO_乙"
      `shouldFailWith` err 4 (utok '乙' <> eeof)
    parse environmentVariable "" ""
      `shouldFailWith` err 0 (ueof <> elabel "environment variable")

  specify "bareComponent" $ do
    let bareComponent' = bareComponent "" <* eof
    parse bareComponent' "" "foo" `shouldParse` BareComponent "foo"
    parse bareComponent' "" "123" `shouldParse` BareComponent "123"
    parse bareComponent' "" "甲" `shouldParse` BareComponent "甲"
    parse bareComponent' "" "123-foo-乙"
      `shouldParse` BareComponent "123-foo-乙"
    parse (bareComponent "") "" "/foo"
      `shouldFailWith` err
        0
        (utok '/' <> elabel "component")
    parse bareComponent' "" "foo/"
      `shouldFailWith` err
        3
        (utok '/' <> elabel "component" <> eeof)
    parse bareComponent' "" ""
      `shouldFailWith` err
        0
        (ueof <> elabel "component")

  specify "root" $ do
    let root' = root <* eof
    parse root' "" "/" `shouldParse` Root Nothing
    parse root' "" "C:\\" `shouldParse` Root (Just 'C')
    parse root' "" "C:/" `shouldParse` Root (Just 'C')
    parse root "" ""
      `shouldFailWith` err 0 (ueof <> etok '/' <> elabel "drive letter")
    parse root "" "C" `shouldFailWith` err 1 (ueof <> etok ':')

  specify "substitution" $ do
    let substitution' = substitution <* eof
    parse substitution' "" "$FOO" `shouldParse` Substitution "FOO"
    parse substitution' "" "$FOO_BAR" `shouldParse` Substitution "FOO_BAR"
    parse substitution "" ""
      `shouldFailWith` err 0 (ueof <> etok '$')
    parse substitution "" "FOO"
      `shouldFailWith` err 0 (utok 'F' <> etok '$')
    parse substitution' "" "$FOO-"
      `shouldFailWith` err 4 (utok '-' <> eeof)

  trueFalse $ \begin -> specify ("substitutionDefault " ++ show begin) $ do
    let substitutionDefault' env = substitutionDefault begin env <* eof
    parse (substitutionDefault' "FOO") "" "-bar"
      `shouldParse` SubstitutionWithDefault "FOO" (BareComponent "bar")
    parse (substitutionDefault' "FOO") "" "-bar${BAZ}qux"
      `shouldParse` SubstitutionWithDefault
        "FOO"
        ( Concatenation
            (Concatenation (BareComponent "bar") (Substitution "BAZ"))
            $ BareComponent "qux"
        )
    parse (substitutionDefault begin "FOO") "" ""
      `shouldFailWith` err 0 (ueof <> etok '-')
    parse (substitutionDefault begin "FOO") "" "+bar"
      `shouldFailWith` err 0 (utok '+' <> etok '-')
    parse (substitutionDefault' "FOO") "" "-$BAR}"
      `shouldFailWith` err
        5
        ( utok '}'
            <> etoks "${"
            <> etok '$'
            <> elabel "component"
            <> eeof
        )
    parse (substitutionDefault' "FOO") "" "-${BAR}}"
      `shouldFailWith` err
        7
        ( utok '}'
            <> etoks "${"
            <> etok '$'
            <> elabel "component"
            <> eeof
        )

  trueFalse $ \begin -> specify ("substitutionConditional " ++ show begin) $ do
    let substitutionConditional' env = substitutionConditional begin env <* eof
    parse (substitutionConditional' "FOO") "" "+bar"
      `shouldParse` ConditionalSubstitution "FOO" (BareComponent "bar")
    parse (substitutionConditional' "FOO") "" "+bar${BAZ}qux"
      `shouldParse` ConditionalSubstitution
        "FOO"
        ( Concatenation
            (Concatenation (BareComponent "bar") (Substitution "BAZ"))
            $ BareComponent "qux"
        )
    parse (substitutionConditional begin "FOO") "" ""
      `shouldFailWith` err 0 (ueof <> etok '+')
    parse (substitutionConditional begin "FOO") "" "-bar"
      `shouldFailWith` err 0 (utok '-' <> etok '+')
    parse (substitutionConditional' "FOO") "" "+$BAR}"
      `shouldFailWith` err
        5
        ( utok '}'
            <> etoks "${"
            <> etok '$'
            <> elabel "component"
            <> eeof
        )
    parse (substitutionConditional' "FOO") "" "+${BAR}}"
      `shouldFailWith` err
        7
        ( utok '}'
            <> etoks "${"
            <> etok '$'
            <> elabel "component"
            <> eeof
        )

  trueFalse $ \begin -> specify
    ("substitutionModifierAfterColon" ++ show begin)
    $ do
      let substitutionModifierAfterColon' env =
            substitutionModifierAfterColon begin env <* eof
      parse (substitutionModifierAfterColon' "FOO") "" "-bar"
        `shouldParse` SubstitutionWithDefault "FOO" (BareComponent "bar")
      parse (substitutionModifierAfterColon' "FOO") "" "+bar"
        `shouldParse` ConditionalSubstitution "FOO" (BareComponent "bar")
      parse (substitutionModifierAfterColon begin "FOO") "" "!bar"
        `shouldFailWith` err 0 (utok '!' <> etok '+' <> etok '-')

  trueFalse $ \begin -> specify ("substitutionInBraces" ++ show begin) $ do
    let substitutionInBraces' = substitutionInBraces begin <* eof
    parse substitutionInBraces' "" "${FOO}"
      `shouldParse` Substitution "FOO"
    parse substitutionInBraces' "" "${FOO:-bar}"
      `shouldParse` SubstitutionWithDefault "FOO" (BareComponent "bar")
    parse substitutionInBraces' "" "${FOO:+bar}"
      `shouldParse` ConditionalSubstitution "FOO" (BareComponent "bar")
    parse (substitutionInBraces begin) "" ""
      `shouldFailWith` err 0 (ueof <> etoks "${")
    parse (substitutionInBraces begin) "" "${FOO-bar}"
      `shouldFailWith` err 5 (utok '-' <> etok ':' <> etok '}')

  specify "filePathExpression' False" $ do
    let filePathExpr' disallowed = filePathExpression' False disallowed <* eof
    parse (filePathExpr' []) "" "foo}"
      `shouldParse` BareComponent "foo}"
    parse (filePathExpr' "}") "" "foo}"
      `shouldFailWith` err
        3
        ( utok '}'
            <> etoks "${"
            <> etok '$'
            <> elabel "component"
            <> eeof
        )
    parse (filePathExpr' []) "" "/foo"
      `shouldFailWith` err
        0
        (utoks "/f" <> etoks "${" <> etok '$' <> elabel "component")
    parse (filePathExpr' []) "" "C:\\foo"
      `shouldParse` PathSeparator
        (BareComponent "C:")
        (BareComponent "foo")
    parse (filePathExpr' []) "" "D:/bar"
      `shouldParse` PathSeparator
        (BareComponent "D:")
        (BareComponent "bar")

  specify "filePathExpression' True" $ do
    let filePathExpr' = filePathExpression' True [] <* eof
    parse filePathExpr' "" "/foo"
      `shouldParse` PathSeparator (Root Nothing) (BareComponent "foo")
    parse filePathExpr' "" "C:\\foo"
      `shouldParse` PathSeparator (Root $ Just 'C') (BareComponent "foo")
    parse filePathExpr' "" "D:/bar"
      `shouldParse` PathSeparator (Root $ Just 'D') (BareComponent "bar")

  specify "filePathExpression" $ do
    let filePathExpr = filePathExpression <* eof
    parse filePathExpr "" "/" `shouldParse` Root (Nothing)
    parse filePathExpr "" "A" `shouldParse` BareComponent "A"
    parse filePathExpr "" "A/" `shouldParse` BareComponent "A"
    parse filePathExpr "" "A:\\" `shouldParse` Root (Just 'A')
    parse filePathExpr "" "A:/" `shouldParse` Root (Just 'A')
    parse filePathExpr "" "foo" `shouldParse` BareComponent "foo"
    parse filePathExpr "" "foo/" `shouldParse` BareComponent "foo"
    parse filePathExpr "" "foo/bar"
      `shouldParse` PathSeparator (BareComponent "foo") (BareComponent "bar")
    parse filePathExpr "" "foo/bar/"
      `shouldParse` PathSeparator (BareComponent "foo") (BareComponent "bar")
    parse filePathExpr "" "foo/bar/baz"
      `shouldParse` PathSeparator
        (PathSeparator (BareComponent "foo") (BareComponent "bar"))
        (BareComponent "baz")
    parse filePathExpr "" "foo${BAR}baz/qux"
      `shouldParse` PathSeparator
        ( Concatenation
            (Concatenation (BareComponent "foo") (Substitution "BAR"))
            $ BareComponent "baz"
        )
        (BareComponent "qux")
    parse filePathExpr "" "foo/bar${BAZ}qux"
      `shouldParse` PathSeparator
        (BareComponent "foo")
        ( Concatenation
            (Concatenation (BareComponent "bar") (Substitution "BAZ"))
            $ BareComponent "qux"
        )
    parse filePathExpr "" "foo/${BAR:-bar}/${BAZ:+baz}"
      `shouldParse` PathSeparator
        ( PathSeparator
            (BareComponent "foo")
            (SubstitutionWithDefault "BAR" (BareComponent "bar"))
        )
        (ConditionalSubstitution "BAZ" (BareComponent "baz"))
    parse filePathExpr "" "/$FOO/bar"
      `shouldParse` PathSeparator
        (PathSeparator (Root Nothing) (Substitution "FOO"))
        (BareComponent "bar")
    parse filePathExpr "" "C:\\D:\\bar"
      `shouldParse` PathSeparator
        (PathSeparator (Root $ Just 'C') (BareComponent "D:"))
        (BareComponent "bar")
    parse filePathExpr "" "${FOO:-C:\\bar}"
      `shouldParse` SubstitutionWithDefault
        "FOO"
        (PathSeparator (Root $ Just 'C') (BareComponent "bar"))

  specify "parseFilePathExpression" $ do
    parseFilePathExpression "" "foo"
      `shouldBe` Right (BareComponent "foo")
    parseFilePathExpression "" "/foo/$BAR"
      `shouldBe` Right (Root Nothing +/+ "foo" +/+ Substitution "BAR")
    parseFilePathExpression "" "C:\\foo\\$BAR"
      `shouldBe` Right (Root (Just 'C') +/+ "foo" +/+ Substitution "BAR")

  specify "errorBundlePretty" $ do
    let Left errorBundle = parseFilePathExpression "" "/foo/${BAR/baz"
    -- FIXME: The error message is not very helpful in this case.
    errorBundlePretty errorBundle
      `shouldBe` unlines
        [ "1:6:"
        , "  |"
        , "1 | /foo/${BAR/baz"
        , "  |      ^"
        , "unexpected '$'"
        , "expecting end of input"
        ]


trueFalse :: (Monad m) => (Bool -> m a) -> m ()
trueFalse = forM_ [False, True]
