module Sync.Base (
	Repo, Diff, Patch, Merge,
	diff,
	mirror, combine,
	merge, resolve,
	patch
	) where

import Prelude.Unicode

import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple (swap)

type Repo k a = Map k a
type Diff k a = Map k (Maybe a, Maybe a)
type Patch k a = Map k (Maybe a)
type Merge k a = Map k (Either (Maybe a, Maybe a) (Maybe a))

-- | Compare repositories
diff ∷ (Ord k, Eq a) ⇒ Repo k a → Repo k a → Diff k a
diff = M.mergeWithKey
	(\_ l r → if l ≡ r then Nothing else Just (Just l, Just r))
	(M.map (\v → (Just v, Nothing)))
	(M.map (\v → (Nothing, Just v)))

-- | Make mirroring patches
--
-- > let (lm, rm) = mirror (diff l r)
-- > patch lm l ≡ r
-- > patch rm r ≡ l
mirror ∷ Diff k a → (Patch k a, Patch k a)
mirror d = (M.map snd d, M.map fst d)

-- | Make additive patches
--
-- > let (lm, rm) = combine m (diff l r)
-- > patch lm l ≡ patch rm r
combine ∷ (a → a → Either a a) → Diff k a → (Patch k a, Patch k a)
combine fn d = (M.mapMaybe (fn' ∘ swap) d, M.mapMaybe fn' d) where
	fn' (Nothing, _) = Nothing
	fn' (Just l, Nothing) = Just (Just l)
	fn' (Just l, Just r) = case fn l r of
		Left v → Just (Just v)
		Right _ → Nothing

-- | Merge two patches
merge ∷ (Ord k, Eq a) ⇒ Patch k a → Patch k a → Merge k a
merge = M.mergeWithKey (const merge') (M.map Right) (M.map Right) where
	merge' l r
		| l ≡ r = Just (Right l)
		| otherwise = Just (Left (l, r))

-- | Resolve conflicts
resolve ∷ (k → Maybe a → Maybe a → Maybe a) → Merge k a → Patch k a
resolve fn = M.mapWithKey resolve' where
	resolve' k (Left (l, r)) = fn k l r
	resolve' _ (Right v) = v

-- | Apply patch
patch ∷ Ord k ⇒ Patch k a → Repo k a → Repo k a
patch = M.mergeWithKey
	(\_ p _ → p)
	(M.mapMaybe id)
	id
