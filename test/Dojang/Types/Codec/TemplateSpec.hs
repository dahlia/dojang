{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dojang.Types.Codec.TemplateSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, specify)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Dojang.Types.Codec.BuiltIn (builtInCodecRuntime)
import Dojang.Types.Codec.Evaluate
  ( CodecError
  , CodecEvaluationRequest (CodecEvaluationRequest)
  , EvaluatedCodec (cacheKey, renderedBytes)
  , EvaluationMode (NormalEvaluation)
  , evaluateCodec
  , formatCodecError
  , opaqueBytes
  , reflectCodec
  , revealBytes
  )
import Dojang.Types.Codec.Template (templateCodecSpec)


spec :: Spec
spec = describe "template codec" $ do
  specify "interpolates a declared manifest variable as UTF-8" $ hedgehog $ do
    value <- forAll $ Gen.text (Range.linear 0 256) Gen.unicode
    let result =
          render
            "name = {{ vars.GIT_NAME }}\n"
            Map.empty
            (Map.singleton "GIT_NAME" $ encodeUtf8 value)
    rendered <- either (fail . Text.unpack . formatCodecError) return result
    revealBytes rendered.renderedBytes === encodeUtf8 ("name = " <> value <> "\n")

  specify "evaluates only the selected conditional branch" $ do
    let result =
          render
            ( "{% if facts.class == \"work\" %}"
                <> "{{ vars.WORK_EMAIL }}"
                <> "{% else %}personal{% endif %}"
            )
            (Map.singleton "class" "personal")
            Map.empty
    fmap (revealBytes . (.renderedBytes)) result `shouldBe` Right "personal"

  specify "does not include unrelated inputs in the cache key" $ do
    let source = "{{ vars.NAME }}"
        first = render source Map.empty $ Map.fromList [("NAME", "Ada"), ("UNUSED", "one")]
        second = render source Map.empty $ Map.fromList [("NAME", "Ada"), ("UNUSED", "two")]
    fmap (.cacheKey) first `shouldBe` fmap (.cacheKey) second

  specify "changes the cache key when a referenced input changes" $ hedgehog $ do
    prefix <- forAll $ Gen.text (Range.linear 0 128) Gen.unicode
    let source = "{{ vars.NAME }}"
        first = render source Map.empty $ Map.singleton "NAME" $ encodeUtf8 $ prefix <> "a"
        second = render source Map.empty $ Map.singleton "NAME" $ encodeUtf8 $ prefix <> "b"
    firstKey <-
      either (fail . Text.unpack . formatCodecError) (return . (.cacheKey)) first
    secondKey <-
      either (fail . Text.unpack . formatCodecError) (return . (.cacheKey)) second
    (firstKey == secondKey) === False

  specify "preserves literal Unicode text" $ hedgehog $ do
    value <-
      forAll $
        Gen.filter
          (\text -> all (not . (`Text.isInfixOf` text)) ["{{", "{%", "{#"])
          (Gen.text (Range.linear 0 256) Gen.unicode)
    let source = encodeUtf8 $ "literal: " <> value
    rendered <-
      either
        (fail . Text.unpack . formatCodecError)
        return
        (render source Map.empty Map.empty)
    revealBytes rendered.renderedBytes === source

  specify "preserves trailing and mixed newlines" $ do
    let source = "first\r\nsecond\n"
    fmap (revealBytes . (.renderedBytes)) (render source Map.empty Map.empty)
      `shouldBe` Right source

  specify "supports local assignments, finite loops, and pure filters" $ do
    let source =
          ( "{% set greeting = \"hi\" %}"
              <> "{% for item in [1, 2, 3] %}"
              <> "{{ greeting | upper }} {{ item }}"
              <> "{% endfor %}"
          )
    fmap (revealBytes . (.renderedBytes)) (render source Map.empty Map.empty)
      `shouldBe` Right "HI 1HI 2HI 3"

  forM_
    [
      ( "a later declaration"
      , "{{ sort([\"{{ int_ratio(1, 0) }}\"], eval) }}{% set eval = 0 %}"
      )
    ,
      ( "a declaration in an unselected branch"
      , ( "{% if false %}{% set eval = 0 %}{% endif %}"
            <> "{{ sort([\"{{ int_ratio(1, 0) }}\"], eval) }}"
        )
      )
    ,
      ( "a declaration in a nested scope"
      , ( "{% scope %}{% set eval = 0 %}{% endscope %}"
            <> "{{ sort([\"{{ int_ratio(1, 0) }}\"], eval) }}"
        )
      )
    ,
      ( "a loop binding"
      , ( "{% for eval in [] %}{% endfor %}"
            <> "{{ sort([\"{{ int_ratio(1, 0) }}\"], eval) }}"
        )
      )
    ]
    $ \(description, source) ->
      specify ("rejects a local referenced before " <> description) $ do
        render source Map.empty Map.empty
          `shouldSatisfy` either
            (Text.isInfixOf "unsupported source syntax" . formatCodecError)
            (const False)

  specify "keeps a local assigned by every conditional branch" $ do
    let source =
          ( "{% if facts.class == \"work\" %}"
              <> "{% set greeting = \"hello\" %}"
              <> "{% else %}{% set greeting = \"hi\" %}{% endif %}"
              <> "{{ greeting }}"
          )
    fmap
      (revealBytes . (.renderedBytes))
      (render source (Map.singleton "class" "work") Map.empty)
      `shouldBe` Right "hello"

  specify "looks up fact names case-insensitively" $ hedgehog $ do
    value <- forAll $ Gen.text (Range.linear 0 128) Gen.unicode
    let result = render "{{ facts.OS }}" (Map.singleton "os" value) Map.empty
    rendered <- either (fail . Text.unpack . formatCodecError) return result
    revealBytes rendered.renderedBytes === encodeUtf8 value

  specify "reports a missing input only when its branch is evaluated" $ do
    let result = render "first\n{{ vars.MISSING }}" Map.empty Map.empty
    result
      `shouldSatisfy` either
        ( Text.isInfixOf "missing template input vars.MISSING at line 2"
            . formatCodecError
        )
        (const False)

  specify "retains the namespace in missing-input diagnostics" $ hedgehog $ do
    namespace <- forAll $ Gen.element ["vars", "facts"]
    name <- forAll $ Gen.text (Range.linear 1 32) Gen.alpha
    let result =
          render
            (encodeUtf8 $ "{{ " <> namespace <> "." <> name <> " }}")
            Map.empty
            Map.empty
        expected = "missing template input " <> namespace <> "." <> name
    case result of
      Left err -> Text.isInfixOf expected (formatCodecError err) === True
      Right _ -> fail "missing template input unexpectedly rendered"

  specify "redacts runtime-derived dynamic lookup keys" $ hedgehog $ do
    namespace <- forAll $ Gen.element ["vars", "facts"]
    suffix <- forAll $ Gen.text (Range.linear 1 32) Gen.alphaNum
    let secret = "sensitive-runtime-key-" <> suffix
        result =
          render
            (encodeUtf8 $ "{{ " <> namespace <> "[vars.SELECTOR] }}")
            Map.empty
            (Map.singleton "SELECTOR" $ encodeUtf8 secret)
    case result of
      Left err -> do
        let message = formatCodecError err
        Text.isInfixOf secret message === False
        Text.isInfixOf
          ("missing template input " <> namespace <> ".<dynamic>")
          message
          === True
      Right _ -> fail "missing dynamic template input unexpectedly rendered"

  specify "rejects script blocks before Ginger lowers them" $ do
    let result = render "{% script %} echo(\"hello\"); {% endscript %}" Map.empty Map.empty
    result
      `shouldSatisfy` either
        (Text.isInfixOf "unsupported source syntax" . formatCodecError)
        (const False)

  specify "rejects full-whitespace-preservation script blocks" $ do
    let source = "{%+ script %} echo(\"accepted\"); {%+ endscript %}"
    render source Map.empty Map.empty
      `shouldSatisfy` either
        (Text.isInfixOf "unsupported source syntax" . formatCodecError)
        (const False)

  specify "rejects comment-prefixed script blocks" $ do
    let source = "{% # comment\nscript %} echo(\"accepted\"); {% endscript %}"
    render source Map.empty Map.empty
      `shouldSatisfy` either
        (Text.isInfixOf "unsupported source syntax" . formatCodecError)
        (const False)

  specify "allows script tag text inside comments and string literals" $ do
    let source = "{# {% script %} #}{{ \"{% script %}\" }}"
    fmap (revealBytes . (.renderedBytes)) (render source Map.empty Map.empty)
      `shouldBe` Right "{% script %}"

  forM_
    [
      ( "division by zero"
      , "{{ 1 / 0 }}"
      , "1797693134862316" <> ByteString.replicate 293 48
      )
    , ("non-terminating decimal division", "{{ 1 / 3 }}", "0.3333333333333333")
    , ("split with an empty separator", "{{ split(\"abc\", \"\") }}", "abc")
    ]
    $ \(description, source, expected) ->
      specify ("handles " <> description <> " without an exception") $ do
        fmap (revealBytes . (.renderedBytes)) (render source Map.empty Map.empty)
          `shouldBe` Right expected

  forM_
    [ ("center with empty padding", "{{ center(\"x\", 5, \"\") }}")
    , ("divisibility by zero", "{{ divisibleby(1, 0) }}")
    , ("integer division by zero", "{{ int_ratio(1, 0) }}")
    , ("modulo by zero", "{{ 1 % 0 }}")
    , ("printf with an invalid format", "{{ printf(\"%q\", 1) }}")
    , ("format with an invalid format", "{{ format(\"%q\", 1) }}")
    , ("replace with an empty search", "{{ replace(\"abc\", \"\", \"x\") }}")
    , ("replace with an omitted search", "{{ replace(\"abc\") }}")
    ]
    $ \(description, source) ->
      specify ("rejects partial " <> description <> " before evaluation") $ do
        let result = render source Map.empty Map.empty
        result
          `shouldSatisfy` either
            (Text.isInfixOf "unsupported source syntax" . formatCodecError)
            (const False)

  specify "rejects effectful built-ins before evaluation" $ do
    let result = render "{{ eval(\"{{ 1 }}\") }}" Map.empty Map.empty
    result
      `shouldSatisfy` either
        (Text.isInfixOf "unsupported source syntax" . formatCodecError)
        (const False)

  specify "rejects invalid UTF-8 source with a position" $ do
    let result = render (ByteString.pack [0xff]) Map.empty Map.empty
    result
      `shouldSatisfy` either
        (Text.isInfixOf "not valid UTF-8 at line 1, column 1" . formatCodecError)
        (const False)

  specify "identifies a variable with invalid platform encoding" $ do
    let result =
          render
            "{{ vars.NAME }}"
            Map.empty
            (Map.singleton "NAME" $ ByteString.pack [0xff])
    result
      `shouldSatisfy` either
        ( \err ->
            let message = formatCodecError err
            in Text.isInfixOf
                 "template input vars.NAME with invalid platform text"
                 message
                 && not (Text.isInfixOf "line 1, column 1" message)
        )
        (const False)

  specify "rejects reflection" $ do
    let request =
          CodecEvaluationRequest
            "git/config"
            templateCodecSpec
            (opaqueBytes "{{ vars.NAME }}")
            Map.empty
            (Map.singleton "NAME" "Ada")
        result =
          runIdentity $
            reflectCodec
              (builtInCodecRuntime NormalEvaluation)
              request
              (opaqueBytes "Grace")
    either formatCodecError (const "unexpected success") result
      `shouldBe` "Route git/config codec template rejects reflection."


render
  :: ByteString
  -> Map.Map Text.Text Text.Text
  -> Map.Map Text.Text ByteString
  -> Either CodecError EvaluatedCodec
render source facts variables =
  runIdentity $
    evaluateCodec
      (builtInCodecRuntime NormalEvaluation)
      ( CodecEvaluationRequest
          "git/config"
          templateCodecSpec
          (opaqueBytes source)
          facts
          variables
      )
      Nothing
