{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dojang.Syntax.EnvironmentPredicate.ParserSpec (spec) where

import Dojang.Syntax.EnvironmentPredicate.Parser
  ( Field (..)
  , FieldOp (..)
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
  , parseEnvironmentPredicate
  , prefixOp
  , simpleExpression
  , singleQuoteStringLiteral
  , stringLiteral
  , strings
  , suffixOp
  )
import Dojang.Types.Environment (Architecture (..), OperatingSystem (..))
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.MonikerName (parseMonikerName)

import Control.Monad (forM_)
import Data.Text (singleton)
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


deriving instance Show Field
deriving instance Eq FieldOp
deriving instance Show FieldOp


spec :: Spec
spec = do
  specify "expression" $ do
    let p = expression <* eof
    parse p "" "os=linux" `shouldParse` OperatingSystem Linux
    parse p "" "os=windows || arch=aarch64"
      `shouldParse` Or [OperatingSystem Windows, Architecture AArch64]
    parse p "" " os=linux && arch=x86 || os=macos && arch=aarch64 "
      `shouldParse` Or
        [ And [OperatingSystem Linux, Architecture X86]
        , And [OperatingSystem MacOS, Architecture AArch64]
        ]
    parse p "" "os=linux && ( arch=x86 || os=macos ) && arch=aarch64"
      `shouldParse` And
        [ OperatingSystem Linux
        , Or [Architecture X86, OperatingSystem MacOS]
        , Architecture AArch64
        ]

  specify "andExpression" $ do
    let p = andExpression <* eof
    parse p "" "os=linux&&arch=aarch64"
      `shouldParse` And [OperatingSystem Linux, Architecture AArch64]
    parse p "" " ( always ) && ( always )" `shouldParse` And [Always, Always]

  specify "simpleExpression" $ do
    let p = simpleExpression <* eof
    parse p "" "os=linux" `shouldParse` OperatingSystem Linux
    parse p "" "(os = windows)" `shouldParse` OperatingSystem Windows
    parse p "" " ( os == macos ) " `shouldParse` OperatingSystem MacOS
    parse p "" "always" `shouldParse` Always
    parse p "" "never" `shouldParse` Not Always
    parse p "" "!(arch = x86)" `shouldParse` Not (Architecture X86)
    parse p "" "!(arch == 'x86_64')"
      `shouldParse` Not (Architecture X86_64)
    parse p "" "! ( arch != aarch64 )"
      `shouldParse` Not (Not (Architecture AArch64))

  specify "fieldOp" $ do
    let p = fieldOp <* eof
    parse p "" "os=linux" `shouldParse` OperatingSystem Linux
    parse p "" " arch = 'x86_64' " `shouldParse` Architecture X86_64
    parse p "" "kernel = Darwin" `shouldParse` KernelName "Darwin"
    parse p "" "kernel-release=\"1.2.3\"" `shouldParse` KernelRelease "1.2.3"
    let Right fooBar = parseMonikerName "foo-bar"
    parse p "" " moniker == \"foo-bar\" " `shouldParse` Moniker fooBar
    parse p "" " moniker = 'invalid moniker' " `shouldParse` Not Always
    parse p "" " os != \"windows\" " `shouldParse` Not (OperatingSystem Windows)
    parse p "" "arch!=aarch64" `shouldParse` Not (Architecture AArch64)
    parse p "" "kernel != Linux" `shouldParse` Not (KernelName "Linux")
    parse p "" "kernel-release!='4.5.6'"
      `shouldParse` Not (KernelRelease "4.5.6")
    parse p "" "moniker!='foo-bar'" `shouldParse` Not (Moniker fooBar)
    parse p "" "kernel-release ^= '1.2'" `shouldParse` KernelReleasePrefix "1.2"
    parse p "" "kernel-release$='2.3'" `shouldParse` KernelReleaseSuffix "2.3"
    parse p "" "os in ()" `shouldParse` Not Always
    parse p "" " arch  in  ( x86, \"x86_64\", ) "
      `shouldParse` Or [Architecture X86, Architecture X86_64]
    let Right baz = parseMonikerName "baz"
    parse p "" "moniker in (\"foo-bar\", baz)"
      `shouldParse` Or [Moniker fooBar, Moniker baz]
    parse p "" "os not in (linux, macos,)"
      `shouldParse` And
        [ Not $ OperatingSystem Linux
        , Not $ OperatingSystem MacOS
        ]
    parse p "" " arch not in () " `shouldParse` Always
    parse p "" "moniker not in ('foo-bar', baz,)"
      `shouldParse` And [Not $ Moniker fooBar, Not $ Moniker baz]
    parse p "" "os ^= lin"
      `shouldFailWith` err
        3
        ( utoks "^= "
            <> etoks "!="
            <> etoks "in"
            <> etoks "not"
            <> etoks "="
            <> elabel "white space"
        )

  specify "equalOp" $ do
    let p = equalOp <* eof
    parse p "" "=foo" `shouldParse` EqualOp "foo"
    parse p "" "==bar" `shouldParse` EqualOp "bar"
    parse p "" " = foo" `shouldParse` EqualOp "foo"
    parse p "" " == bar" `shouldParse` EqualOp "bar"
    parse p "" "='foo bar'" `shouldParse` EqualOp "foo bar"
    parse p "" " = \"foo bar\"" `shouldParse` EqualOp "foo bar"
    parse p "" "="
      `shouldFailWith` err
        1
        (ueof <> etok '=' <> elabel "white space" <> elabel "string literal")
    parse p "" "= "
      `shouldFailWith` err
        2
        (ueof <> elabel "white space" <> elabel "string literal")
    parse p "" "= = foo"
      `shouldFailWith` err
        2
        (utok '=' <> elabel "white space" <> elabel "string literal")

  specify "notEqualOp" $ do
    let p = notEqualOp <* eof
    parse p "" "!=foo" `shouldParse` NotEqualOp "foo"
    parse p "" " != foo" `shouldParse` NotEqualOp "foo"
    parse p "" "!='foo bar'" `shouldParse` NotEqualOp "foo bar"
    parse p "" " != \"foo bar\"" `shouldParse` NotEqualOp "foo bar"
    parse p "" "!="
      `shouldFailWith` err
        2
        (ueof <> elabel "white space" <> elabel "string literal")
    parse p "" "!= "
      `shouldFailWith` err
        3
        (ueof <> elabel "white space" <> elabel "string literal")
    parse p "" "! = foo"
      `shouldFailWith` err
        0
        (utoks "! " <> elabel "white space" <> etoks "!=")

  specify "prefixOp" $ do
    let p = prefixOp <* eof
    parse p "" "^=foo" `shouldParse` PrefixOp "foo"
    parse p "" " ^= foo" `shouldParse` PrefixOp "foo"
    parse p "" "^='foo bar'" `shouldParse` PrefixOp "foo bar"
    parse p "" " ^= \"foo bar\"" `shouldParse` PrefixOp "foo bar"
    parse p "" "^="
      `shouldFailWith` err
        2
        (ueof <> elabel "white space" <> elabel "string literal")
    parse p "" "^= "
      `shouldFailWith` err
        3
        (ueof <> elabel "white space" <> elabel "string literal")
    parse p "" "^ = foo"
      `shouldFailWith` err
        0
        (utoks "^ " <> elabel "white space" <> etoks "^=")

  specify "suffixOp" $ do
    let p = suffixOp <* eof
    parse p "" "$=foo" `shouldParse` SuffixOp "foo"
    parse p "" " $= foo" `shouldParse` SuffixOp "foo"
    parse p "" "$='foo bar'" `shouldParse` SuffixOp "foo bar"
    parse p "" " $= \"foo bar\"" `shouldParse` SuffixOp "foo bar"
    parse p "" "$="
      `shouldFailWith` err
        2
        (ueof <> elabel "white space" <> elabel "string literal")
    parse p "" "$= "
      `shouldFailWith` err
        3
        (ueof <> elabel "white space" <> elabel "string literal")
    parse p "" "$ = foo"
      `shouldFailWith` err
        0
        (utoks "$ " <> elabel "white space" <> etoks "$=")

  specify "inOp" $ do
    let p = inOp <* eof
    parse p "" " in ()" `shouldParse` InOp []
    parse p "" " in (foo)" `shouldParse` InOp ["foo"]
    parse p "" " in (foo, bar,) " `shouldParse` InOp ["foo", "bar"]
    parse p "" "in ()" `shouldFailWith` err 0 (utok 'i' <> elabel "white space")
    parse p "" " in"
      `shouldFailWith` err 3 (ueof <> etok '(' <> elabel "white space")
    parse p "" " in "
      `shouldFailWith` err 4 (ueof <> etok '(' <> elabel "white space")
    parse p "" " in ("
      `shouldFailWith` err
        5
        (ueof <> etok ')' <> elabel "string literal" <> elabel "white space")

  specify "notInOp" $ do
    let p = notInOp <* eof
    parse p "" " not in ()" `shouldParse` NotInOp []
    parse p "" " not in (foo)" `shouldParse` NotInOp ["foo"]
    parse p "" " not in (foo, bar,) " `shouldParse` NotInOp ["foo", "bar"]
    parse p "" "not in ()"
      `shouldFailWith` err 0 (utok 'n' <> elabel "white space")
    parse p "" " not" `shouldFailWith` err 4 (ueof <> elabel "white space")
    parse p "" " not "
      `shouldFailWith` err 5 (ueof <> etoks "in" <> elabel "white space")
    parse p "" " not in"
      `shouldFailWith` err 7 (ueof <> etok '(' <> elabel "white space")
    parse p "" " not in "
      `shouldFailWith` err 8 (ueof <> etok '(' <> elabel "white space")
    parse p "" " not in ("
      `shouldFailWith` err
        9
        (ueof <> etok ')' <> elabel "string literal" <> elabel "white space")

  specify "strings" $ do
    let p = strings <* eof
    parse p "" "(foo, 'bar', \"baz\")" `shouldParse` ["foo", "bar", "baz"]
    parse p "" "('foo bar', \"baz qux\",)" `shouldParse` ["foo bar", "baz qux"]
    parse p "" "( 'foo bar' , )" `shouldParse` ["foo bar"]
    parse p "" ""
      `shouldFailWith` err 0 (ueof <> etok '(' <> elabel "white space")
    parse p "" "(foo"
      `shouldFailWith` err
        4
        (ueof <> etok ')' <> etok ',' <> elabel "white space")
    parse p "" "(foo, "
      `shouldFailWith` err
        6
        (ueof <> etok ')' <> elabel "string literal" <> elabel "white space")

  specify "field" $ do
    let p = field <* eof
    parse p "" "os" `shouldParse` OS
    parse p "" "arch" `shouldParse` Arch
    parse p "" "kernel" `shouldParse` Kernel
    parse p "" "kernel-release" `shouldParse` KernelRelease'
    parse p "" "moniker" `shouldParse` Moniker'
    parse p "" "foo"
      `shouldFailWith` err
        0
        ( utoks "foo"
            <> etoks "arch"
            <> etoks "kernel"
            <> etoks "kernel-release"
            <> etoks "moniker"
            <> etoks "os"
        )

  specify "stringLiteral" $ do
    let p = stringLiteral <* eof
    parse p "" "foo" `shouldParse` "foo"
    parse p "" "'foo'" `shouldParse` "foo"
    parse p "" "\"foo\"" `shouldParse` "foo"
    parse p "" "foo bar"
      `shouldFailWith` err 4 (utok 'b' <> eeof <> elabel "white space")
    parse p "" "'foo bar'" `shouldParse` "foo bar"
    parse p "" "\"foo bar\"" `shouldParse` "foo bar"

  specify "bareStringLiteral" $ do
    let p = bareStringLiteral <* eof
    parse p "" "foo" `shouldParse` "foo"
    parse p "" "foo123" `shouldParse` "foo123"
    parse p "" "" `shouldFailWith` err 0 (ueof <> elabel "bare string literal")
    parse p "" "123foo"
      `shouldFailWith` err 0 (utok '1' <> elabel "bare string literal")
    parse p "" "foo bar" `shouldFailWith` err 3 (utok ' ' <> eeof)
    parse p "" "foo-bar" `shouldFailWith` err 3 (utok '-' <> eeof)
    parse p "" "foo_bar" `shouldFailWith` err 3 (utok '_' <> eeof)
    parse p "" "foo.bar" `shouldFailWith` err 3 (utok '.' <> eeof)

  specify "doubleQuoteStringLiteral" $ do
    let p = doubleQuoteStringLiteral <* eof
    parse p "" "\"foo\"" `shouldParse` "foo"
    parse p "" "\"foo bar\"" `shouldParse` "foo bar"
    parse p "" "\"\\\"Hello\\'\\n\\\\\"" `shouldParse` "\"Hello'\n\\"
    parse p "" "foo"
      `shouldFailWith` err 0 (utok 'f' <> elabel "double quote string literal")
    parse p "" "\"foo"
      `shouldFailWith` err
        4
        (ueof <> etok '"' <> etok '\\' <> elabel "string character")

  specify "singleQuoteStringLiteral" $ do
    let p = singleQuoteStringLiteral <* eof
    parse p "" "'foo'" `shouldParse` "foo"
    parse p "" "'foo bar'" `shouldParse` "foo bar"
    parse p "" "'\\\"Hello\\'\\n\\\\'" `shouldParse` "\"Hello'\n\\"
    parse p "" "foo"
      `shouldFailWith` err 0 (utok 'f' <> elabel "single quote string literal")
    parse p "" "'foo"
      `shouldFailWith` err
        4
        (ueof <> etok '\'' <> etok '\\' <> elabel "string character")

  forM_ (['\'', '"'] :: [Char]) $ \terminal -> do
    specify ("charactersInStringLiteral " ++ show terminal) $ do
      let p = charactersInStringLiteral terminal <* eof
      parse p "" "foo" `shouldParse` "foo"
      parse p "" "foo bar" `shouldParse` "foo bar"
      parse p "" "\\\"Hello\\'\\n\\\\" `shouldParse` "\"Hello'\n\\"
      parse p "" "\\b\\f\\n\\r\\t\\v\\0" `shouldParse` "\b\f\n\r\t\v\0"
      parse p "" "\\x41\\x42\\x43\\x44" `shouldParse` "ABCD"
      parse p "" "\\u0045\\u0046\\u0047\\u0048" `shouldParse` "EFGH" -- cSpell: disable-line
      parse p "" "\\U00000049\\U0000004A\\U0000004b" `shouldParse` "IJK"
      parse p "" "\\"
        `shouldFailWith` err
          1
          ( (ueof <> etok '"' <> etok '\'' <> etok '0' <> etok 'U' <> etok '\\')
              <> (etok 'b' <> etok 'f' <> etok 'n' <> etok 'r' <> etok 't')
              <> (etok 'u' <> etok 'v' <> etok 'x')
          )
      parse p "" (singleton terminal)
        `shouldFailWith` err
          0
          (utok terminal <> etok '\\' <> eeof <> elabel "string character")

  specify "errorBundlePretty" $ do
    let Left e = parseEnvironmentPredicate "<input>" "invalid"
    errorBundlePretty e
      `shouldBe` "<input>:1:1:\n"
        <> "  |\n"
        <> "1 | invalid\n"
        <> "  | ^^^^^^^\n"
        <> "unexpected \"invalid\"\n"
        <> "expecting expression\n"
