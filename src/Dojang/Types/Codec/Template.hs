{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Deterministic, source-aware text-template codec.
module Dojang.Types.Codec.Template
  ( secretTemplateCodecImplementation
  , secretTemplateCodecSpec
  , templateCodecDefinition
  , templateCodecImplementation
  , templateCodecSpec
  ) where

import Control.Monad (foldM)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Writer (Writer, runWriter)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder
  ( byteString
  , toLazyByteString
  , word64BE
  )
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive (foldCase)
import Data.Char (chr, isAlphaNum, isSpace)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Numeric (showHex)
import System.OsString qualified as OsString
import Text.Ginger.AST
  ( Expression (..)
  , Statement (..)
  , Template (..)
  )
import Text.Ginger.GVal
  ( GVal (..)
  , ToGVal (toGVal)
  , fromFunction
  )
import Text.Ginger.Parse
  ( ParserError (..)
  , ParserOptions (..)
  , SourcePos
  , mkParserOptions
  , parseGinger'
  , sourceColumn
  , sourceLine
  )
import Text.Ginger.Run
  ( RuntimeError (..)
  , makeContextText
  , runGingerT
  , runtimeErrorWhere
  )
import Text.Ginger.Run.Type (GingerContext (..), Run, throwHere)
import Prelude hiding (reverse)

import Dojang.Types.Codec
  ( CodecConfiguration (CodecConfiguration)
  , CodecDefinition (CodecDefinition)
  , CodecName
  , CodecSpec (CodecSpec)
  , ReflectPolicy (ReflectReject)
  , parseCodecName
  , templateCodecName
  )
import Dojang.Types.Codec.Evaluate
  ( CacheScope (PersistentCache)
  , CodecDryRunPolicy (CachedOnly, EvaluatePurely)
  , CodecFailure (..)
  , CodecFailureCategory (..)
  , CodecImplementation
  , CodecInputPresence (DeferredInput)
  , CodecInputSelection (..)
  , CodecInputs (..)
  , CodecProgram (CodecDone, CodecRequest)
  , CodecRequirements (CodecRequirements)
  , CodecSourcePosition (CodecSourcePosition)
  , ExternalInput (..)
  , ExternalInputRequest (BackendInputRequest)
  , OpaqueBytes
  , codecImplementationWithEffects
  , codecImplementationWithSourceRequirements
  , noCodecInputs
  , opaqueBytes
  , revealBytes
  )
import Dojang.Types.CodecBackend.Protocol
  ( BackendOperation (Lookup)
  )


-- | Serializable specification selecting the built-in template codec.
templateCodecSpec :: CodecSpec
templateCodecSpec = CodecSpec templateCodecName $ CodecConfiguration Map.empty


-- | Stable cache and reflection metadata for the template codec.
templateCodecDefinition :: CodecDefinition
templateCodecDefinition = CodecDefinition templateCodecName "1" ReflectReject


-- | Pure implementation of the built-in template codec.
templateCodecImplementation :: CodecImplementation
templateCodecImplementation =
  codecImplementationWithSourceRequirements
    templateCodecDefinition
    ( \configuration ->
        if configuration == CodecConfiguration Map.empty
          then Right $ CodecRequirements noCodecInputs templateVariableInputs []
          else Left "template does not accept configuration"
    )
    (\_ source -> requirementsFor source)
    renderTemplate
    Nothing
    PersistentCache
    EvaluatePurely


-- | Serializable specification selecting the secret-backed template codec.
secretTemplateCodecSpec :: CodecSpec
secretTemplateCodecSpec =
  CodecSpec secretTemplateCodecName $ CodecConfiguration Map.empty


-- | Secret-backed template implementation with command-scoped results and no
-- destination-to-source reflection.
secretTemplateCodecImplementation :: CodecImplementation
secretTemplateCodecImplementation =
  codecImplementationWithEffects
    (CodecDefinition secretTemplateCodecName "1" ReflectReject)
    validateSecretTemplateConfiguration
    secretTemplateRequirements
    renderSecretTemplate
    Nothing
    CachedOnly


secretTemplateCodecName :: CodecName
secretTemplateCodecName = case parseCodecName "secret-template" of
  Just name -> name
  Nothing -> error "The built-in secret-template codec name is nonempty."


validateSecretTemplateConfiguration
  :: CodecConfiguration -> Either CodecFailure CodecRequirements
validateSecretTemplateConfiguration configuration
  | configuration == CodecConfiguration Map.empty =
      Right $ CodecRequirements noCodecInputs templateVariableInputs []
  | otherwise = Left "secret-template does not accept configuration"


secretTemplateRequirements
  :: CodecConfiguration
  -> OpaqueBytes
  -> Either CodecFailure CodecRequirements
secretTemplateRequirements _ source = do
  template <- parseTemplate source
  analysis <- validateTemplate template
  Right $ CodecRequirements analysis.facts analysis.variables []


data Analysis = Analysis
  { facts :: CodecInputSelection
  , variables :: CodecInputSelection
  , lookupReferences :: Map CodecSourcePosition LookupReference
  , secretReferences :: Map Text SecretReference
  }


data LookupReference
  = StaticLookup Text Text
  | DynamicLookup Text


emptyAnalysis :: Analysis
emptyAnalysis = Analysis noCodecInputs templateVariableInputs Map.empty Map.empty


data SecretReference = SecretReference
  { backend :: Text
  , item :: Text
  , position :: CodecSourcePosition
  }


templateVariableInputs :: CodecInputSelection
templateVariableInputs = CodecInputSelection Map.empty False True


requirementsFor :: OpaqueBytes -> Either CodecFailure CodecRequirements
requirementsFor source = do
  template <- parseTemplate source
  analysis <- validateTemplate template
  rejectSecretReferences analysis
  return $ CodecRequirements analysis.facts analysis.variables []


parseTemplate
  :: OpaqueBytes
  -> Either CodecFailure (Template SourcePos)
parseTemplate source = do
  text <- case decodeUtf8' $ revealBytes source of
    Left _ -> Left $ failureAt InvalidSourceEncoding initialPosition
    Right value -> Right value
  case scriptTagPosition text of
    Just position -> Left $ failureAt UnsupportedSourceSyntax position
    Nothing -> Right ()
  let options =
        (mkParserOptions $ const $ pure Nothing)
          { poKeepTrailingNewline = True
          }
  case runIdentity $ parseGinger' options $ Text.unpack text of
    Left parserError -> Left $ parserFailure parserError
    Right template -> Right template


parserFailure :: ParserError -> CodecFailure
parserFailure parserError =
  failureAt InvalidSourceSyntax $
    maybe initialPosition sourcePosition parserError.peSourcePosition


validateTemplate :: Template SourcePos -> Either CodecFailure Analysis
validateTemplate template
  | isJust template.templateParent =
      unsupported $ statementPosition template.templateBody
  | not $ HashMap.null template.templateBlocks =
      unsupported $ statementPosition template.templateBody
  | otherwise =
      snd <$> validateStatement Set.empty template.templateBody emptyAnalysis


validateStatement
  :: Set Text
  -> Statement SourcePos
  -> Analysis
  -> Either CodecFailure (Set Text, Analysis)
validateStatement locals statement analysis = case statement of
  MultiS _ statements -> foldM validateNext (locals, analysis) statements
   where
    validateNext (currentLocals, currentAnalysis) next =
      validateStatement currentLocals next currentAnalysis
  ScopedS _ body -> do
    (_, withBody) <- validateStatement locals body analysis
    Right (locals, withBody)
  IndentS _ indentation body -> do
    withIndentation <- validateExpression locals indentation analysis
    validateStatement locals body withIndentation
  LiteralS _ _ -> Right (locals, analysis)
  InterpolationS _ expression -> do
    withExpression <- validateExpression locals expression analysis
    Right (locals, withExpression)
  ExpressionS _ expression -> do
    withExpression <- validateExpression locals expression analysis
    Right (locals, withExpression)
  IfS _ condition yes no -> do
    withCondition <- validateExpression locals condition analysis
    (yesLocals, withYes) <- validateStatement locals yes withCondition
    (noLocals, withNo) <- validateStatement locals no withYes
    Right (Set.intersection yesLocals noLocals, withNo)
  SwitchS _ subject cases fallback -> do
    withSubject <- validateExpression locals subject analysis
    (caseLocals, withCases) <-
      foldM
        ( \(branchLocals, current) (expression, body) -> do
            withExpression <- validateExpression locals expression current
            (bodyLocals, withBody) <-
              validateStatement locals body withExpression
            Right (bodyLocals : branchLocals, withBody)
        )
        ([], withSubject)
        cases
    (fallbackLocals, withFallback) <-
      validateStatement locals fallback withCases
    Right
      ( foldl Set.intersection fallbackLocals caseLocals
      , withFallback
      )
  ForS position indexName valueName values body
    | any reservedName $ valueName : maybeToList indexName -> unsupported position
    | otherwise -> do
        withValues <- validateExpression locals values analysis
        (_, withBody) <-
          validateStatement
            (Set.insert valueName $ maybe id Set.insert indexName locals)
            body
            withValues
        Right (locals, withBody)
  SetVarS position name expression
    | reservedName name -> unsupported position
    | otherwise -> do
        withExpression <- validateExpression locals expression analysis
        Right (Set.insert name locals, withExpression)
  NullS _ -> Right (locals, analysis)
  DefMacroS position _ _ -> unsupported position
  BlockRefS position _ -> unsupported position
  PreprocessedIncludeS position _ -> unsupported position
  TryCatchS position _ _ _ -> unsupported position


validateExpression
  :: Set Text
  -> Expression SourcePos
  -> Analysis
  -> Either CodecFailure Analysis
validateExpression locals expression analysis = case expression of
  StringLiteralE _ _ -> Right analysis
  NumberLiteralE _ _ -> Right analysis
  BoolLiteralE _ _ -> Right analysis
  NullLiteralE _ -> Right analysis
  VarE _ name
    | name == "secret" -> unsupported $ expressionPosition expression
    | name == "vars" ->
        Right $
          Analysis
            analysis.facts
            (includeAll analysis.variables)
            analysis.lookupReferences
            analysis.secretReferences
    | name == "facts" ->
        Right $
          Analysis
            (includeAll analysis.facts)
            analysis.variables
            analysis.lookupReferences
            analysis.secretReferences
    | name `Set.member` locals || name `Set.member` allowedFunctions ->
        Right analysis
    | otherwise -> unsupported $ expressionPosition expression
  ListE _ values -> foldM (flip $ validateExpression locals) analysis values
  ObjectE _ pairs -> foldM validatePair analysis pairs
   where
    validatePair current (key, value) =
      validateExpression locals key current >>= validateExpression locals value
  MemberLookupE position (VarE _ namespace) (StringLiteralE _ name)
    | namespace == "vars" ->
        Right $
          recordLookup (StaticLookup namespace name) position $
            Analysis
              analysis.facts
              (selectName id name analysis.variables)
              analysis.lookupReferences
              analysis.secretReferences
    | namespace == "facts" ->
        Right $
          recordLookup (StaticLookup namespace name) position $
            Analysis
              (selectName foldCase name analysis.facts)
              analysis.variables
              analysis.lookupReferences
              analysis.secretReferences
  MemberLookupE position (VarE _ namespace) index
    | namespace == "vars" -> do
        withIndex <-
          validateExpression locals index $
            Analysis
              analysis.facts
              (includeAll analysis.variables)
              analysis.lookupReferences
              analysis.secretReferences
        Right $ recordLookup (DynamicLookup namespace) position withIndex
    | namespace == "facts" -> do
        withIndex <-
          validateExpression locals index $
            Analysis
              (includeAll analysis.facts)
              analysis.variables
              analysis.lookupReferences
              analysis.secretReferences
        Right $ recordLookup (DynamicLookup namespace) position withIndex
  MemberLookupE _ base index ->
    validateExpression locals base analysis >>= validateExpression locals index
  CallE position (VarE _ "secret") arguments ->
    validateSecretReference position arguments analysis
  CallE position (VarE _ name) arguments
    | name `Set.member` allowedFunctions ->
        foldM
          (\current (_, argument) -> validateExpression locals argument current)
          analysis
          arguments
    | otherwise -> unsupported position
  CallE position _ _ -> unsupported position
  TernaryE _ condition yes no -> do
    withCondition <- validateExpression locals condition analysis
    withYes <- validateExpression locals yes withCondition
    validateExpression locals no withYes
  LambdaE position _ _ -> unsupported position
  DoE position _ -> unsupported position


selectName
  :: (Text -> Text) -> Text -> CodecInputSelection -> CodecInputSelection
selectName canonicalize name selection =
  CodecInputSelection
    (Map.insert (canonicalize name) DeferredInput selection.namedInputs)
    selection.includeAllInputs
    selection.manifestInputsOnly


includeAll :: CodecInputSelection -> CodecInputSelection
includeAll selection =
  CodecInputSelection selection.namedInputs True selection.manifestInputsOnly


recordLookup :: LookupReference -> SourcePos -> Analysis -> Analysis
recordLookup reference position analysis =
  analysis
    { lookupReferences =
        Map.insert
          (sourcePosition position)
          reference
          analysis.lookupReferences
    }


validateSecretReference
  :: SourcePos
  -> [(Maybe Text, Expression SourcePos)]
  -> Analysis
  -> Either CodecFailure Analysis
validateSecretReference position arguments analysis = case arguments of
  [ (Nothing, StringLiteralE _ backend)
    , (Nothing, StringLiteralE _ item)
    ]
      | not (Text.null backend)
      , not (Text.null item) ->
          Right $
            analysis
              { secretReferences =
                  Map.insert
                    (secretReferenceKey backend item)
                    (SecretReference backend item $ sourcePosition position)
                    analysis.secretReferences
              }
  _ -> unsupported position


rejectSecretReferences :: Analysis -> Either CodecFailure ()
rejectSecretReferences analysis = case Map.elems analysis.secretReferences of
  [] -> Right ()
  reference : _ -> unsupportedPosition reference.position
 where
  unsupportedPosition = Left . failureAt UnsupportedSourceSyntax


data SecretRenderResult
  = SecretRendered ByteString
  | SecretNeeded Text Text
  | SecretRenderFailed CodecFailure


renderSecretTemplate :: CodecInputs -> CodecProgram ByteString
renderSecretTemplate inputs = go Map.empty
 where
  go secrets = case renderSecretTemplateWith secrets inputs of
    SecretRendered bytes -> CodecDone $ Right bytes
    SecretRenderFailed failure -> CodecDone $ Left failure
    SecretNeeded backend item ->
      CodecRequest
        (BackendInputRequest backend (Lookup item) $ opaqueBytes "")
        (\input -> go $ Map.insert (backend, item) (revealBytes input.value) secrets)


renderSecretTemplateWith
  :: Map (Text, Text) ByteString
  -> CodecInputs
  -> SecretRenderResult
renderSecretTemplateWith secrets inputs = case prepared of
  Left failure -> SecretRenderFailed failure
  Right (template, analysis, variables, facts, decodedSecrets) ->
    let baseContext = makeContextText $ const $ toGVal ("" :: Text)
        context =
          baseContext
            { contextLookup =
                lookupSecretValue variables facts decodedSecrets
            , contextWarn = throwHere
            }
        (result, output) = runWriter $ runGingerT context template
    in case result of
         Right _ -> SecretRendered $ encodeUtf8 output
         Left runtimeError ->
           case missingIndex runtimeError >>= (`Map.lookup` analysis.secretReferences) of
             Just reference -> SecretNeeded reference.backend reference.item
             Nothing ->
               SecretRenderFailed $
                 runtimeFailure analysis.lookupReferences runtimeError
 where
  prepared = do
    template <- parseTemplate inputs.rawSource
    analysis <- validateTemplate template
    variables <- Map.traverseWithKey decodeNativeText inputs.variables
    decodedSecrets <- Map.traverseWithKey decodeSecretText secrets
    let facts = fmap decodeUtf8Lenient inputs.facts
    Right (template, analysis, variables, facts, decodedSecrets)


lookupSecretValue
  :: Map Text Text
  -> Map Text Text
  -> Map (Text, Text) Text
  -> Text
  -> Run SourcePos (Writer Text) Text (GVal (Run SourcePos (Writer Text) Text))
lookupSecretValue variables facts secrets name = case name of
  "secret" -> return $ fromFunction $ secretFunction secrets
  _ -> lookupValue variables facts name


secretFunction
  :: Map (Text, Text) Text
  -> [ ( Maybe Text
       , GVal (Run SourcePos (Writer Text) Text)
       )
     ]
  -> Run SourcePos (Writer Text) Text (GVal (Run SourcePos (Writer Text) Text))
secretFunction secrets arguments = case arguments of
  [(Nothing, backend), (Nothing, item)] ->
    let reference = (backend.asText, item.asText)
    in case Map.lookup reference secrets of
         Just value -> return $ toGVal value
         Nothing ->
           throwHere $
             IndexError $
               secretReferenceKey backend.asText item.asText
  _ -> throwHere $ IndexError "invalid-secret-reference"


decodeSecretText :: (Text, Text) -> ByteString -> Either CodecFailure Text
decodeSecretText _ bytes = case decodeUtf8' bytes of
  Left _ -> Left $ CodecInputEncodingFailure "secret value"
  Right value -> Right value


secretReferenceKey :: Text -> Text -> Text
secretReferenceKey backend item =
  "secret-request:"
    <> digestText
      ( LazyByteString.toStrict $
          toLazyByteString $
            encodeComponent backend <> encodeComponent item
      )
 where
  encodeComponent value =
    let bytes = encodeUtf8 value
    in word64BE (fromIntegral $ ByteString.length bytes) <> byteString bytes


digestText :: ByteString -> Text
digestText = Text.pack . concatMap byteHex . ByteString.unpack . SHA256.hash
 where
  byteHex byte = case showHex byte "" of
    [digit] -> ['0', digit]
    digits -> digits


renderTemplate :: CodecInputs -> Either CodecFailure ByteString
renderTemplate inputs = do
  template <- parseTemplate inputs.rawSource
  analysis <- validateTemplate template
  rejectSecretReferences analysis
  variables <- Map.traverseWithKey decodeNativeText inputs.variables
  let factValues = fmap decodeUtf8Lenient inputs.facts
      baseContext = makeContextText $ const $ toGVal ("" :: Text)
      context =
        baseContext
          { contextLookup = lookupValue variables factValues
          , contextWarn = throwHere
          }
      (result, output) = runWriter $ runGingerT context template
  case result of
    Left runtimeError ->
      Left $ runtimeFailure analysis.lookupReferences runtimeError
    Right _ -> Right $ encodeUtf8 output


lookupValue
  :: Map Text Text
  -> Map Text Text
  -> Text
  -> Run SourcePos (Writer Text) Text (GVal (Run SourcePos (Writer Text) Text))
lookupValue variables facts name = case name of
  "vars" -> return $ toGVal variables
  "facts" -> return $ caseInsensitiveDictionary facts
  _ -> throwHere $ IndexError name


caseInsensitiveDictionary :: Map Text Text -> GVal m
caseInsensitiveDictionary values =
  let base = toGVal values
  in base
       { asLookup = Just $ \name -> toGVal <$> Map.lookup (foldCase name) values
       }


decodeNativeText :: Text -> ByteString -> Either CodecFailure Text
decodeNativeText name bytes = case nativeOsString bytes of
  Nothing -> Left $ CodecInputEncodingFailure $ "vars." <> name
  Just value -> case OsString.decodeUtf value of
    Left _ -> Left $ CodecInputEncodingFailure $ "vars." <> name
    Right text -> Right $ Text.pack text


nativeOsString :: ByteString -> Maybe OsString.OsString
nativeOsString = fmap OsString.pack . codeUnits . ByteString.unpack
 where
#ifdef mingw32_HOST_OS
  codeUnits (low : high : remaining) =
    (OsString.unsafeFromChar (chr $ fromIntegral low + fromIntegral high * 0x100) :)
      <$> codeUnits remaining
  codeUnits [] = Just []
  codeUnits [_] = Nothing
#else
  codeUnits = Just . fmap (OsString.unsafeFromChar . chr . fromIntegral)
#endif


decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient bytes = case decodeUtf8' bytes of
  Left _ -> Text.empty
  Right value -> value


runtimeFailure
  :: Map CodecSourcePosition LookupReference
  -> RuntimeError SourcePos
  -> CodecFailure
runtimeFailure lookupReferences runtimeError =
  failureAt category position
 where
  position = case runtimeErrorWhere runtimeError of
    [] -> initialPosition
    positions -> sourcePosition $ last positions
  category = case missingIndex runtimeError of
    Nothing -> SourceEvaluationFailure
    Just _ ->
      MissingSourceInput $
        maybe "<dynamic>" renderLookupReference $
          Map.lookup position lookupReferences


renderLookupReference :: LookupReference -> Text
renderLookupReference reference = case reference of
  StaticLookup namespace name -> namespace <> "." <> name
  DynamicLookup namespace -> namespace <> ".<dynamic>"


missingIndex :: RuntimeError p -> Maybe Text
missingIndex runtimeError = case runtimeError of
  IndexError name -> Just $ Text.dropAround (== '"') name
  RuntimeErrorAt _ nested -> missingIndex nested
  _ -> Nothing


unsupported :: SourcePos -> Either CodecFailure a
unsupported = Left . failureAt UnsupportedSourceSyntax . sourcePosition


failureAt :: CodecFailureCategory -> CodecSourcePosition -> CodecFailure
failureAt = CodecFailureAt


sourcePosition :: SourcePos -> CodecSourcePosition
sourcePosition position =
  CodecSourcePosition (sourceLine position) (sourceColumn position)


statementPosition :: Statement SourcePos -> SourcePos
statementPosition statement = case statement of
  MultiS position _ -> position
  ScopedS position _ -> position
  IndentS position _ _ -> position
  LiteralS position _ -> position
  InterpolationS position _ -> position
  ExpressionS position _ -> position
  IfS position _ _ _ -> position
  SwitchS position _ _ _ -> position
  ForS position _ _ _ _ -> position
  SetVarS position _ _ -> position
  DefMacroS position _ _ -> position
  BlockRefS position _ -> position
  PreprocessedIncludeS position _ -> position
  NullS position -> position
  TryCatchS position _ _ _ -> position


expressionPosition :: Expression SourcePos -> SourcePos
expressionPosition expression = case expression of
  StringLiteralE position _ -> position
  NumberLiteralE position _ -> position
  BoolLiteralE position _ -> position
  NullLiteralE position -> position
  VarE position _ -> position
  ListE position _ -> position
  ObjectE position _ -> position
  MemberLookupE position _ _ -> position
  CallE position _ _ -> position
  LambdaE position _ _ -> position
  TernaryE position _ _ _ -> position
  DoE position _ -> position


initialPosition :: CodecSourcePosition
initialPosition = CodecSourcePosition 1 1


scriptTagPosition :: Text -> Maybe CodecSourcePosition
scriptTagPosition source = positionAt source <$> findTag 0 source
 where
  findTag offset remaining
    | Just afterOpening <- Text.stripPrefix "{#" remaining =
        let skipped = plainDelimitedLength "#}" afterOpening
        in findTag (offset + 2 + skipped) $ Text.drop skipped afterOpening
    | Just afterOpening <- Text.stripPrefix "{{" remaining =
        continueAfter "}}" offset afterOpening
    | Just afterOpening <- Text.stripPrefix "{%" remaining =
        if statementName afterOpening == "script"
          then Just offset
          else continueAfter "%}" offset afterOpening
    | otherwise = case Text.uncons remaining of
        Nothing -> Nothing
        Just (_, rest) -> findTag (offset + 1) rest
  continueAfter closing offset afterOpening =
    let skipped = delimitedLength closing afterOpening
    in findTag (offset + 2 + skipped) $ Text.drop skipped afterOpening


statementName :: Text -> Text
statementName =
  Text.takeWhile (\character -> isAlphaNum character || character == '_')
    . dropStatementPrefix


dropStatementPrefix :: Text -> Text
dropStatementPrefix text =
  case Text.stripPrefix "#" withoutSpacing of
    Nothing -> withoutSpacing
    Just comment -> case Text.break (== '\n') comment of
      (_, newlineAndRest) -> case Text.uncons newlineAndRest of
        Nothing -> Text.empty
        Just (_, rest) -> dropStatementPrefix rest
 where
  withoutSpacing =
    Text.dropWhile
      (\character -> isSpace character || character == '-' || character == '+')
      text


delimitedLength :: Text -> Text -> Int
delimitedLength closing = go 0 Nothing False
 where
  go lengthSoFar quote escaped remaining
    | quote == Nothing
    , Just _ <- Text.stripPrefix closing remaining =
        lengthSoFar + Text.length closing
    | otherwise = case Text.uncons remaining of
        Nothing -> lengthSoFar
        Just (character, rest) ->
          let nextQuote
                | escaped = quote
                | Just activeQuote <- quote =
                    if character == activeQuote then Nothing else quote
                | character == '\'' || character == '"' = Just character
                | otherwise = Nothing
              nextEscaped = not escaped && quote /= Nothing && character == '\\'
          in go (lengthSoFar + 1) nextQuote nextEscaped rest


plainDelimitedLength :: Text -> Text -> Int
plainDelimitedLength closing text =
  let (before, after) = Text.breakOn closing text
  in Text.length before
       + if Text.null after then 0 else Text.length closing


positionAt :: Text -> Int -> CodecSourcePosition
positionAt source offset =
  CodecSourcePosition
    (Text.count "\n" prefix + 1)
    (Text.length (Text.takeWhileEnd (/= '\n') prefix) + 1)
 where
  prefix = Text.take offset source


reservedName :: Text -> Bool
reservedName name = name == "vars" || name == "facts" || name `Set.member` allowedFunctions


allowedFunctions :: Set Text
allowedFunctions =
  Set.fromList
    [ "abs"
    , "all"
    , "any"
    , "capitalize"
    , "ceil"
    , "concat"
    , "contains"
    , "d"
    , "default"
    , "dictsort"
    , "difference"
    , "eq"
    , "equals"
    , "equalto"
    , "even"
    , "filesizeformat"
    , "floor"
    , "ge"
    , "greater"
    , "greaterEquals"
    , "greaterthan"
    , "gt"
    , "in"
    , "int"
    , "is_lt"
    , "iterable"
    , "le"
    , "length"
    , "less"
    , "lessEquals"
    , "lessthan"
    , "lower"
    , "lt"
    , "ne"
    , "nequals"
    , "num"
    , "odd"
    , "product"
    , "ratio"
    , "reverse"
    , "round"
    , "secret"
    , "slice"
    , "sort"
    , "split"
    , "str"
    , "sum"
    , "truncate"
    , "urlencode"
    , "upper"
    , "zip"
    ]
