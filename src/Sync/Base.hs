{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances, UndecidableInstances #-}

module Sync.Base (
	Repo(..), Change(..), Action(..), Merged(..), Diff, Patch, Merge,
	RepoItem(..),
	swap,
	repo, toList, mapKeys, mapWithKey,
	diff,
	patch, mirror, newest, combine,
	updates, before,
	merge, resolve,
	apply,
	rebase,
	exclude
	) where

import Prelude.Unicode

import Data.Map (Map)
import qualified Data.Map as M

-- | Named items
newtype Repo k a = Repo { getRepo ∷ Map k a } deriving (Eq, Ord, Functor, Traversable, Foldable)
-- | Change between repos
data Change a = ChangeLeft a | ChangeRight a | ChangeBoth a a deriving (Eq, Ord, Functor)
-- | Action on item
data Action a = Delete | Update a deriving (Eq, Ord, Functor)
-- | Merged item
data Merged a = Conflict (Action a) (Action a) | Merged (Action a) deriving (Eq, Ord)

-- | Diff is repo if changes
type Diff k a = Repo k (Change a)
-- | Patch is repo of actions
type Patch k a = Repo k (Action a)
-- | Merge state is repo of merged
type Merge k a = Repo k (Merged a)

instance Show a ⇒ Show (Change a) where
	show (ChangeLeft v) = "⇐ " ++ show v
	show (ChangeRight v) = "⇒ " ++ show v
	show (ChangeBoth l r) = "⇔ " ++ show l ++ " " ++ show r

instance Show a ⇒ Show (Action a) where
	show Delete = "-"
	show (Update v) = "+" ++ show v

instance Show a ⇒ Show (Merged a) where
	show (Conflict l r) = "✗ " ++ show l ++ " ≠ " ++ show r
	show (Merged v) = "✓ " ++ show v

instance Functor Merged where
	fmap f (Conflict l r) = Conflict (fmap f l) (fmap f r)
	fmap f (Merged v) = Merged (fmap f v)

swap ∷ Change a → Change a
swap (ChangeLeft v) = ChangeRight v
swap (ChangeRight v) = ChangeLeft v
swap (ChangeBoth l r) = ChangeBoth r l

-- | Used for printing items
data RepoItem k a = RepoItem k a deriving (Eq, Ord)

instance Show k ⇒ Show (RepoItem k (Change a)) where
	show (RepoItem name (ChangeLeft _)) = "⇐ " ++ show name
	show (RepoItem name (ChangeRight _)) = "⇒ " ++ show name
	show (RepoItem name (ChangeBoth _ _)) = "⇔ " ++ show name

instance Show k ⇒ Show (RepoItem k (Action a)) where
	show (RepoItem name Delete) = "✗ " ++ show name
	show (RepoItem name (Update _)) = "✓ " ++ show name

instance Show k ⇒ Show (RepoItem k (Merged a)) where
	show (RepoItem name (Conflict _ _)) = "✗ " ++ show name
	show (RepoItem name (Merged _)) = "✓ " ++ show name

instance {-# OVERLAPPABLE #-} Show k ⇒ Show (RepoItem k a) where
	show (RepoItem name _) = show name

instance Show (RepoItem k a) ⇒ Show (Repo k a) where
	show (Repo r) = unlines $ do
		(k, v) ← M.toList r
		return $ show $ RepoItem k v

-- | Make repo
repo ∷ Ord k ⇒ [(k, a)] → Repo k a
repo = Repo ∘ M.fromList

-- | Get list of entities
toList ∷ Repo k a → [(k, a)]
toList (Repo r) = M.toList r

-- | Map keys
mapKeys ∷ Ord k' ⇒ (k → k') → Repo k a → Repo k' a
mapKeys f (Repo r) = Repo $ M.mapKeys f r

-- | Make with keys
mapWithKey ∷ (k → a → b) → Repo k a → Repo k b
mapWithKey f (Repo r) = Repo $ M.mapWithKey f r

-- | Compare repositories
diff ∷ (Ord k, Eq a) ⇒ Repo k a → Repo k a → Diff k a
diff (Repo l) (Repo r) = Repo $ M.mergeWithKey
	(\_ dl dr → if dl ≡ dr then Nothing else Just (ChangeBoth dl dr))
	(M.map ChangeLeft)
	(M.map ChangeRight)
	l
	r

-- | Make patch by function, which selects action based on change
patch ∷ (Change a → Maybe (Action a)) → Diff k a → (Patch k a, Patch k a)
patch fn (Repo d) = (Repo $ M.mapMaybe (fn ∘ swap) d, Repo $ M.mapMaybe fn d)

-- | Mirror changes
mirror ∷ Change a → Maybe (Action a)
mirror (ChangeLeft l) = Just $ Update l
mirror (ChangeRight _) = Just Delete
mirror (ChangeBoth l _) = Just $ Update l

-- | Select newest on conflict
newest ∷ Ord a ⇒ Change a → Maybe (Action a)
newest (ChangeLeft l) = Just $ Update l
newest (ChangeRight _) = Just Delete
newest (ChangeBoth l r)
	| l > r = Just (Update l)
	| otherwise = Nothing

-- | No delete
combine ∷ Ord a ⇒ Change a → Maybe (Action a)
combine (ChangeLeft l) = Just (Update l)
combine (ChangeRight _) = Nothing
combine (ChangeBoth l r)
	| l > r = Just (Update l)
	| otherwise = Nothing

-- | Get only updates from patch
updates ∷ Patch k a → Repo k a
updates (Repo r) = Repo $ M.mapMaybe update' r where
	update' Delete = Nothing
	update' (Update v) = Just v

-- | Sequence two patches
before ∷ (Ord k, Ord a) ⇒ Patch k a → Patch k a → Patch k a
before (Repo l) (Repo r) = Repo $ M.unionWith before' l r where
	before' (Update l') (Update r') = Update $ max l' r'
	before' _ r' = r'

-- | Merge two patches
merge ∷ (Ord k, Eq a) ⇒ Patch k a → Patch k a → Merge k a
merge (Repo l) (Repo r) = Repo $ M.mergeWithKey (const merge') (M.map Merged) (M.map Merged) l r where
	merge' ml mr
		| ml ≡ mr = Just $ Merged ml
		| otherwise = Just $ Conflict ml mr

-- | Resolve conflicts
resolve ∷ (k → Action a → Action a → Action a) → Merge k a → Patch k a
resolve fn (Repo m) = Repo $ M.mapWithKey resolve' m where
	resolve' k (Conflict l r) = fn k l r
	resolve' _ (Merged v) = v

-- | Apply patch
apply ∷ Ord k ⇒ Patch k a → Repo k a → Repo k a
apply (Repo p) (Repo r) = Repo $ M.mergeWithKey (\_ p' _ → apply' p') (M.mapMaybe apply') id p r where
	apply' Delete = Nothing
	apply' (Update v) = Just v

rebase ∷ (Ord k, Eq a) ⇒ (Change (Action a) → Maybe (Action (Action a))) → Patch k a → Patch k a → Patch k a
rebase fn b = Repo ∘ M.mapMaybe rebase' ∘ getRepo ∘ fst ∘ patch fn ∘ diff b where
	rebase' Delete = Nothing
	rebase' (Update v) = Just v

exclude ∷ (k → Bool) → Repo k a → Repo k a
exclude p (Repo r) = Repo $ M.filterWithKey (\k _ → not (p k)) r
