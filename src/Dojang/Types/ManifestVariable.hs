{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Declarative variables defined by a manifest.
module Dojang.Types.ManifestVariable
  ( ManifestVariableName
  , ManifestVariable (..)
  , ManifestVariableMap
  , VariableEnvironment (..)
  , VariableResolutionError (..)
  , dispatchManifestVariable
  , formatVariableResolutionError
  , lookupVariable
  , manifestVariable
  , manifestVariablePreservingOrder
  , parseManifestVariableName
  , renderManifestVariableName
  , resolveManifestVariables
  , variableValueGetter
  ) where

import Control.Monad (forM, forM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict
  ( StateT
  , gets
  , modify'
  , runStateT
  )
import Control.Monad.Trans.Class (lift)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder
  ( toLazyByteString
  , word32BE
  )
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric (showHex)
import System.OsPath (OsString, toChar, unpack)

import Dojang.MonadFileSystem (MonadFileSystem (encodePath))
import Dojang.Types.Environment (Environment)
import Dojang.Types.EnvironmentPredicate (EnvironmentPredicate (..))
import Dojang.Types.EnvironmentPredicate.Evaluate
  ( EvaluationWarning
  , evaluate'
  )
import Dojang.Types.FilePathExpression (EnvironmentVariable, FilePathExpression)
import Dojang.Types.FilePathExpression.Expansion
  ( ExpansionWarning
  , VariableGetter
  , VariableLookup (..)
  , expandFilePathWithVariables
  )
import Dojang.Types.MonikerMap (MonikerMap, MonikerResolver)
import Dojang.Types.MonikerName (MonikerName)


-- | A portable variable name accepted by file-path expressions.
newtype ManifestVariableName = ManifestVariableName Text
  deriving (Eq, Ord, Show)


-- | A conditionally selected manifest-variable value.
data ManifestVariable = ManifestVariable
  { monikerResolver :: MonikerResolver
  -- ^ Resolver for monikers referenced by branch conditions.
  , branches :: NonEmpty (EnvironmentPredicate, FilePathExpression)
  -- ^ Ordered conditions and their declared values.
  }


-- | Manifest variables keyed by their portable names.
type ManifestVariableMap = Map ManifestVariableName ManifestVariable


-- | A failure while resolving selected manifest-variable definitions.
newtype VariableResolutionError
  = -- | Active dependency chain ending in the repeated variable.
    VariableCycle (NonEmpty ManifestVariableName)
  deriving (Eq, Show)


-- | An immutable resolved manifest-variable layer over inherited lookups.
data VariableEnvironment m = VariableEnvironment
  { resolvedValues :: Map ManifestVariableName ResolvedVariable
  -- ^ Values of every branch selected for the current environment.
  , inheritedGetter :: EnvironmentVariable -> m (Maybe OsString)
  -- ^ Process-environment lookup used when no manifest branch is selected.
  , evaluationWarnings :: [EvaluationWarning]
  -- ^ Warnings produced while selecting conditional branches.
  , shadowedVariables :: [ManifestVariableName]
  -- ^ Selected declarations that hide inherited variables of the same name.
  }


data ResolvedVariable = ResolvedVariable
  { resolvedValue :: OsString
  , resolvedWarnings :: [ExpansionWarning]
  , resolvedProvenance :: Map Text Text
  }


data ResolverState = ResolverState
  { cachedValues :: Map ManifestVariableName ResolvedVariable
  , resolvingStack :: [ManifestVariableName]
  }


instance Eq ManifestVariable where
  left == right =
    left.branches == right.branches
      && all
        (\name -> left.monikerResolver name == right.monikerResolver name)
        (HashSet.toList referencedMonikers)
   where
    referencedMonikers =
      foldMap (monikersInPredicate . fst) left.branches
        <> foldMap (monikersInPredicate . fst) right.branches


instance Show ManifestVariable where
  show variable = "ManifestVariable " <> show variable.branches


-- | Creates an unconditional manifest variable.
manifestVariable
  :: MonikerMap
  -- ^ Monikers available to the variable's condition resolver.
  -> FilePathExpression
  -- ^ Unconditional value expression.
  -> ManifestVariable
  -- ^ Manifest variable containing one unconditional branch.
manifestVariable monikers value =
  ManifestVariable (`HashMap.lookup` monikers) $ (Always, value) NonEmpty.:| []


-- | Creates a manifest variable while preserving branch declaration order.
manifestVariablePreservingOrder
  :: MonikerResolver
  -- ^ Resolver for monikers referenced by branch conditions.
  -> NonEmpty (EnvironmentPredicate, FilePathExpression)
  -- ^ Nonempty branches in declaration order.
  -> ManifestVariable
  -- ^ Conditional manifest variable.
manifestVariablePreservingOrder = ManifestVariable


-- | Selects the first manifest-variable branch matching an environment.
dispatchManifestVariable
  :: Environment
  -- ^ Environment used to evaluate branch conditions.
  -> ManifestVariable
  -- ^ Variable declaration to select.
  -> (Maybe FilePathExpression, [EvaluationWarning])
  -- ^ First selected value, if any, and predicate warnings.
dispatchManifestVariable environment variable =
  select $ NonEmpty.toList variable.branches
 where
  select [] = (Nothing, [])
  select ((predicate, value) : remaining) =
    let (matches, warnings) =
          evaluate' environment variable.monikerResolver predicate
    in if matches
         then (Just value, warnings)
         else
           let (selected, laterWarnings) = select remaining
           in (selected, warnings <> laterWarnings)


-- | Resolves all selected manifest-variable branches and their dependencies.
--
-- Resolution begins in variable-name order, is independent of TOML table
-- order, and reports cycles through the active path-expression evaluation
-- path.  Unselected declarations fall through to the inherited getter.
resolveManifestVariables
  :: forall m
   . (MonadFileSystem m)
  => Environment
  -- ^ Current environment including repository-scoped machine facts.
  -> ManifestVariableMap
  -- ^ Parsed variable declarations.
  -> (EnvironmentVariable -> m (Maybe OsString))
  -- ^ Inherited process-environment lookup.
  -> m (Either VariableResolutionError (VariableEnvironment m))
  -- ^ Resolved immutable variable environment, or a dependency cycle.
resolveManifestVariables environment variables inherited = do
  (result, finalState) <-
    runStateT
      (runExceptT $ forM_ (Map.keys selected) resolveName)
      (ResolverState Map.empty [])
  case result of
    Left err -> return $ Left err
    Right () -> do
      shadowed <- fmap concat $ forM (Map.keys selected) $ \name -> do
        inheritedValue <- inherited $ renderManifestVariableName name
        return [name | Just _ <- [inheritedValue]]
      return $
        Right $
          VariableEnvironment
            finalState.cachedValues
            inherited
            predicateWarnings
            shadowed
 where
  selections = fmap (dispatchManifestVariable environment) variables
  selected = Map.mapMaybe fst selections
  predicateWarnings = concatMap snd $ Map.elems selections

  resolveName
    :: ManifestVariableName
    -> ExceptT VariableResolutionError (StateT ResolverState m) ()
  resolveName name = do
    cached <- gets $ Map.member name . (.cachedValues)
    if cached
      then return ()
      else do
        stack <- gets (.resolvingStack)
        if name `elem` stack
          then
            throwError $
              VariableCycle $
                NonEmpty.fromList $
                  reverse stack <> [name]
          else do
            modify' $ \state ->
              state{resolvingStack = name : state.resolvingStack}
            let expression = selected Map.! name
            (value, warnings, provenance) <-
              expandFilePathWithVariables
                expression
                lookupDuringResolution
                (lift . lift . encodePath . Text.unpack)
            modify' $ \state ->
              state
                { cachedValues =
                    Map.insert
                      name
                      (ResolvedVariable value warnings provenance)
                      state.cachedValues
                , resolvingStack = case state.resolvingStack of
                    _ : remaining -> remaining
                    [] -> []
                }

  lookupDuringResolution
    :: VariableGetter (ExceptT VariableResolutionError (StateT ResolverState m))
  lookupDuringResolution variable =
    case parseManifestVariableName variable of
      Right name | Map.member name selected -> do
        resolveName name
        resolved <- gets $ (Map.! name) . (.cachedValues)
        return $ manifestLookup name resolved
      _ -> do
        value <- lift $ lift $ inherited variable
        return $ inheritedLookup variable value


-- | Looks up a name through the resolved manifest layer and inherited getter.
lookupVariable
  :: (Monad m)
  => VariableEnvironment m
  -- ^ Resolved variable environment.
  -> EnvironmentVariable
  -- ^ Name referenced by a file-path expression.
  -> m VariableLookup
  -- ^ Value, nested warnings, and privacy-preserving provenance.
lookupVariable environment variable =
  case parseManifestVariableName variable of
    Right name
      | Just resolved <- Map.lookup name environment.resolvedValues ->
          return $ manifestLookup name resolved
    _ -> do
      value <- environment.inheritedGetter variable
      return $ inheritedLookup variable value


-- | Adapts a resolved variable environment to the legacy value-only lookup.
variableValueGetter
  :: (Monad m)
  => VariableEnvironment m
  -- ^ Resolved manifest-variable environment.
  -> EnvironmentVariable
  -- ^ Variable name to look up.
  -> m (Maybe OsString)
  -- ^ Resolved value, or 'Nothing' when neither layer defines the name.
variableValueGetter environment variable =
  (.value) <$> lookupVariable environment variable


-- | Formats a manifest-variable resolution failure for a CLI diagnostic.
formatVariableResolutionError
  :: VariableResolutionError
  -- ^ Resolution failure to describe.
  -> Text
  -- ^ Human-readable diagnostic ending with a period.
formatVariableResolutionError (VariableCycle chain) =
  "Manifest variable cycle: "
    <> Text.intercalate
      " -> "
      (renderManifestVariableName <$> NonEmpty.toList chain)
    <> "."


-- | Parses a portable manifest-variable name.
--
-- Names start with an ASCII letter or underscore and otherwise contain only
-- ASCII letters, digits, or underscores.
parseManifestVariableName
  :: Text
  -- ^ Textual variable name.
  -> Either Text ManifestVariableName
  -- ^ Parsed name, or a description of the validation failure.
parseManifestVariableName value =
  case Text.uncons value of
    Just (first, rest)
      | isInitial first && Text.all isContinuation rest ->
          Right $ ManifestVariableName value
    _ -> Left invalidNameMessage
 where
  isInitial character = isAsciiLetter character || character == '_'
  isContinuation character = isInitial character || isDigit character
  isAsciiLetter character =
    isAsciiLower character || isAsciiUpper character


-- | Renders a manifest-variable name.
renderManifestVariableName
  :: ManifestVariableName
  -- ^ Variable name to render.
  -> Text
  -- ^ Portable textual spelling.
renderManifestVariableName (ManifestVariableName value) = value


invalidNameMessage :: Text
invalidNameMessage =
  "Manifest variable names must match [A-Za-z_][A-Za-z0-9_]*."


monikersInPredicate :: EnvironmentPredicate -> HashSet MonikerName
monikersInPredicate predicate = case predicate of
  Moniker name -> HashSet.singleton name
  Or predicates -> foldMap monikersInPredicate predicates
  And predicates -> foldMap monikersInPredicate predicates
  Not nested -> monikersInPredicate nested
  _ -> HashSet.empty


manifestLookup :: ManifestVariableName -> ResolvedVariable -> VariableLookup
manifestLookup name resolved =
  VariableLookup
    (Just resolved.resolvedValue)
    resolved.resolvedWarnings
    ( Map.insert
        ("var." <> renderManifestVariableName name)
        (fingerprint $ Just resolved.resolvedValue)
        resolved.resolvedProvenance
    )


inheritedLookup :: EnvironmentVariable -> Maybe OsString -> VariableLookup
inheritedLookup variable value =
  VariableLookup
    value
    []
    (Map.singleton ("env." <> variable) $ fingerprint value)


fingerprint :: Maybe OsString -> Text
fingerprint Nothing = "unset"
fingerprint (Just value) | value == mempty = "empty"
fingerprint (Just value) = "sha256:" <> sha256OsString value


sha256OsString :: OsString -> Text
sha256OsString =
  digestHex
    . SHA256.hash
    . LazyByteString.toStrict
    . toLazyByteString
    . foldMap (word32BE . fromIntegral . fromEnum . toChar)
    . unpack


digestHex :: ByteString.ByteString -> Text
digestHex = Text.pack . concatMap byteHex . ByteString.unpack
 where
  byteHex byte = case showHex byte "" of
    [digit] -> ['0', digit]
    digits -> digits
