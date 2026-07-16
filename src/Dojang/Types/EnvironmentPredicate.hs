{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | A predicate expression that can be used to match an environment, e.g.,
-- @And [OperatingSystem "linux", Architecture "x86_64"]@.
module Dojang.Types.EnvironmentPredicate
  ( EnvironmentPredicate (..)
  , normalizePredicate
  , referencedFacts
  ) where

import Data.List.NonEmpty (NonEmpty, filter, nonEmpty, nub, sortWith, toList)
import Prelude hiding (filter)

import Data.CaseInsensitive (CI)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import Data.Ord (Down (..))
import Dojang.Types.Environment
  ( Architecture
  , FactKey
  , FactValue
  , OperatingSystem
  )
import Dojang.Types.MonikerName (MonikerName)


-- | A predicate that can be used to match an environment.
data EnvironmentPredicate
  = -- | A predicate that always matches.
    Always
  | -- | A predicate that matches if the given predicate does not match.
    Not EnvironmentPredicate
  | -- | A predicate that matches if all of the given predicates match.
    And (NonEmpty EnvironmentPredicate)
  | -- | A predicate that matches if any of the given predicates match.
    Or (NonEmpty EnvironmentPredicate)
  | -- | A predicate that matches to another moniker.  Mostly used to eliminate
    -- duplication.
    Moniker MonikerName
  | -- | A predicate that matches to the given operating system (e.g. @linux@).
    -- For a list of possible values, see the 'System.Info.os' field.
    OperatingSystem OperatingSystem
  | -- | A predicate that matches to the given architecture (e.g. @x86_64@).
    -- For a list of possible values, see the 'System.Info.arch' field.
    Architecture Architecture
  | -- | A predicate that matches to the given kernel name (e.g. @Darwin@).
    KernelName (CI Text)
  | -- | A predicate that exactly matches to the given kernel release
    -- (e.g. @23.1.0@).
    KernelRelease (CI Text)
  | -- | A predicate that matches to the given kernel release prefix
    -- (e.g. @23.1@ for @23.1.0@).
    KernelReleasePrefix (CI Text)
  | -- | A predicate that matches to the given kernel release suffix
    -- (e.g. @amd64@ for @4.19.0-16-amd64@).
    KernelReleaseSuffix (CI Text)
  | -- | A predicate that matches a named machine fact.
    Fact FactKey FactValue
  | -- | A predicate that matches when a named machine fact is defined.
    FactDefined FactKey
  deriving (Show)


instance Eq EnvironmentPredicate where
  Always == Always = True
  Always == _ = False
  Not p == Not p' = p == p'
  Not _ == _ = False
  And ps == And ps' = normalizePredicateList ps == normalizePredicateList ps'
  And _ == _ = False
  Or ps == Or ps' = normalizePredicateList ps == normalizePredicateList ps'
  Or _ == _ = False
  Moniker monikerName == Moniker monikerName' = monikerName == monikerName'
  Moniker _ == _ = False
  OperatingSystem os == OperatingSystem os' = os == os'
  OperatingSystem _ == _ = False
  Architecture arch == Architecture arch' = arch == arch'
  Architecture _ == _ = False
  KernelName kernel == KernelName kernel' = kernel == kernel'
  KernelName _ == _ = False
  KernelRelease ver == KernelRelease ver' = ver == ver'
  KernelRelease _ == _ = False
  KernelReleasePrefix prefix == KernelReleasePrefix prefix' = prefix == prefix'
  KernelReleasePrefix _ == _ = False
  KernelReleaseSuffix suffix == KernelReleaseSuffix suffix' = suffix == suffix'
  KernelReleaseSuffix _ == _ = False
  Fact key value == Fact key' value' = key == key' && value == value'
  Fact _ _ == _ = False
  FactDefined key == FactDefined key' = key == key'
  FactDefined _ == _ = False


instance Hashable EnvironmentPredicate where
  hashWithSalt salt Always =
    salt `hashWithSalt` ("Always" :: Text)
  hashWithSalt salt (Not predicate) =
    salt `hashWithSalt` ("Not" :: Text) `hashWithSalt` predicate
  hashWithSalt salt (And predicates) =
    salt
      `hashWithSalt` ("And" :: Text)
      `hashWithSalt` normalizePredicateList predicates
  hashWithSalt salt (Or predicates) =
    salt
      `hashWithSalt` ("Or" :: Text)
      `hashWithSalt` normalizePredicateList predicates
  hashWithSalt salt (Moniker monikerName) =
    salt `hashWithSalt` ("Moniker" :: Text) `hashWithSalt` monikerName
  hashWithSalt salt (OperatingSystem os') =
    salt `hashWithSalt` ("OperatingSystem" :: Text) `hashWithSalt` os'
  hashWithSalt salt (Architecture arch') =
    salt `hashWithSalt` ("Architecture" :: Text) `hashWithSalt` arch'
  hashWithSalt salt (KernelName kernel) =
    salt `hashWithSalt` ("KernelName" :: Text) `hashWithSalt` kernel
  hashWithSalt salt (KernelRelease ver) =
    salt `hashWithSalt` ("KernelRelease" :: Text) `hashWithSalt` ver
  hashWithSalt salt (KernelReleasePrefix prefix) =
    salt `hashWithSalt` ("KernelReleasePrefix" :: Text) `hashWithSalt` prefix
  hashWithSalt salt (KernelReleaseSuffix suffix) =
    salt `hashWithSalt` ("KernelReleaseSuffix" :: Text) `hashWithSalt` suffix
  hashWithSalt salt (Fact key value) =
    salt
      `hashWithSalt` ("Fact" :: Text)
      `hashWithSalt` key
      `hashWithSalt` value
  hashWithSalt salt (FactDefined key) =
    salt
      `hashWithSalt` ("FactDefined" :: Text)
      `hashWithSalt` key


normalizePredicateList
  :: NonEmpty EnvironmentPredicate -> NonEmpty EnvironmentPredicate
normalizePredicateList = nub . sortWith (Down . hashWithSalt 1)


-- | Normalize an environment predicate by removing redundant predicates.
--
-- >>> normalizePredicate $ And [Not Always, Always]
-- Not Always
-- >>> normalizePredicate $ Or [Not Always, Always]
-- Always
-- >>> import Dojang.Types.MonikerName (parseMonikerName)
-- >>> let Right foo = parseMonikerName "foo"
-- >>> normalizePredicate $ And [Moniker foo, Not $ Moniker foo]
-- Not Always
-- >>> normalizePredicate $ Or [Moniker foo, Not $ Moniker foo]
-- Or (Moniker (MonikerName "foo") :| [Not (Moniker (MonikerName "foo"))])
-- >>> let Right a = parseMonikerName "a"
-- >>> let Right b = parseMonikerName "b"
-- >>> let Right c = parseMonikerName "c"
-- >>> normalizePredicate $ And [Moniker a, And [Moniker b, Moniker c]]
-- And (Moniker (MonikerName "a") :| [Moniker (MonikerName "b"),Moniker (MonikerName "c")])
normalizePredicate :: EnvironmentPredicate -> EnvironmentPredicate
normalizePredicate pred'
  | normalized == normalized2 = normalized
  | otherwise = normalizePredicate normalized2
 where
  normalized :: EnvironmentPredicate
  normalized = normalizePredicate' pred'
  normalized2 :: EnvironmentPredicate
  normalized2 = normalizePredicate' normalized


normalizePredicate' :: EnvironmentPredicate -> EnvironmentPredicate
normalizePredicate' (And [p]) = normalizePredicate' p
normalizePredicate' (And ps) =
  if Not Always `elem` ps'
    then Not Always
    else
      let filtered = filter (/= Always) ps'
      in if (`any` filtered) $ \case
           Not p' -> p' `elem` filtered && not (mayBeUndefined p')
           _ -> False
           then Not Always
           else
             let reduced =
                   maybe Always And $
                     nonEmpty
                       [ p'
                       | p <- filtered
                       , p' <- case p of
                           And ps'' -> normalizePredicate' <$> toList ps''
                           p'' -> [p'']
                       ]
             in case reduced of
                  And [p'] -> p'
                  _ -> reduced
 where
  ps' :: NonEmpty EnvironmentPredicate
  ps' = normalizePredicate' <$> ps
normalizePredicate' (Or [p]) = normalizePredicate' p
normalizePredicate' (Or ps) =
  if Always `elem` ps'
    then Always
    else
      let filtered = filter (/= Not Always) ps'
      in if (`any` filtered) $ \case
           Not p' -> p' `elem` filtered && not (mayBeUndefined p')
           _ -> False
           then Always
           else
             let reduced =
                   maybe (Not Always) Or $
                     nonEmpty
                       [ p'
                       | p <- filtered
                       , p' <- case p of
                           Or ps'' -> normalizePredicate' <$> toList ps''
                           p'' -> [p'']
                       ]
             in case reduced of
                  Or [p'] -> p'
                  _ -> reduced
 where
  ps' :: NonEmpty EnvironmentPredicate
  ps' = normalizePredicate' <$> ps
normalizePredicate' (Not (Not p)) = normalizePredicate' p
normalizePredicate' (Not p) = Not $ normalizePredicate' p
normalizePredicate' p = p


-- Fact-backed monikers cannot be distinguished from total monikers until
-- evaluation, so complements containing either form must remain explicit.
mayBeUndefined :: EnvironmentPredicate -> Bool
mayBeUndefined (Not predicate) = mayBeUndefined predicate
mayBeUndefined (And predicates) = any mayBeUndefined predicates
mayBeUndefined (Or predicates) = any mayBeUndefined predicates
mayBeUndefined (Moniker _) = True
mayBeUndefined (Fact _ _) = True
mayBeUndefined (FactDefined _) = True
mayBeUndefined _ = False


-- | Finds named machine facts referenced by a reachable predicate branch.
--
-- Monikers are followed once, so recursive definitions cannot loop forever.
referencedFacts
  :: (MonikerName -> Maybe EnvironmentPredicate)
  -> EnvironmentPredicate
  -> Set FactKey
referencedFacts resolver = go Set.empty . normalizePredicate
 where
  go visited predicate = case predicate of
    Always -> Set.empty
    Not child -> go visited child
    And children -> foldMap (go visited) children
    Or children -> foldMap (go visited) children
    Moniker name
      | name `Set.member` visited -> Set.empty
      | otherwise ->
          maybe
            Set.empty
            (go $ Set.insert name visited)
            (resolver name)
    Fact key _ -> Set.singleton key
    FactDefined key -> Set.singleton key
    OperatingSystem _ -> Set.empty
    Architecture _ -> Set.empty
    KernelName _ -> Set.empty
    KernelRelease _ -> Set.empty
    KernelReleasePrefix _ -> Set.empty
    KernelReleaseSuffix _ -> Set.empty
