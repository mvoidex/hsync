{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Sync.Base.Internal (
	swapChange, lookup, filter, filterWithKey, mapMaybe, mapEither, mapKeys,
	mapWithKey, mapMaybeWithKey,
	traverseWithKey, traverseMaybeWithKey
	) where

import Prelude hiding (map, filter, null, lookup)

import qualified Data.Map as M

import Sync.Base.Types

swapChange ∷ Change a → Change a
swapChange (ChangeLeft v) = ChangeRight v
swapChange (ChangeRight v) = ChangeLeft v
swapChange (ChangeBoth l r) = ChangeBoth r l

lookup ∷ Ord k ⇒ k → Repo k a → Maybe a
lookup key (Repo r) = M.lookup key r

filter ∷ (a → Bool) → Repo k a → Repo k a
filter p (Repo r) = Repo $ M.filter p r

filterWithKey ∷ (k → a → Bool) → Repo k a → Repo k a
filterWithKey p (Repo r) = Repo $ M.filterWithKey p r

mapMaybe ∷ (a → Maybe b) → Repo k a → Repo k b
mapMaybe fn (Repo r) = Repo $ M.mapMaybe fn r

mapEither ∷ (a → Either b c) → Repo k a → (Repo k b, Repo k c)
mapEither fn (Repo r) = (Repo b', Repo c') where
	(b', c') = M.mapEither fn r

-- | Map keys
mapKeys ∷ Ord k' ⇒ (k → k') → Repo k a → Repo k' a
mapKeys f (Repo r) = Repo $ M.mapKeys f r

-- | Map with keys
mapWithKey ∷ (k → a → b) → Repo k a → Repo k b
mapWithKey f (Repo r) = Repo $ M.mapWithKey f r

-- | Map maybe with keys
mapMaybeWithKey ∷ (k → a → Maybe b) → Repo k a → Repo k b
mapMaybeWithKey f (Repo r) = Repo $ M.mapMaybeWithKey f r

-- | Traverse with keys
traverseWithKey ∷ Applicative t ⇒ (k → a → t b) → Repo k a → t (Repo k b)
traverseWithKey f (Repo r) = Repo <$> M.traverseWithKey f r

-- | Traverse maybe with keys
traverseMaybeWithKey ∷ Applicative t ⇒ (k → a → t (Maybe b)) → Repo k a → t (Repo k b)
traverseMaybeWithKey f (Repo r) = Repo <$> M.traverseMaybeWithKey f r
