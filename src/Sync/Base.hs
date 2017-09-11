module Sync.Base (
	module Sync.Base.Types,

	empty, nullRepo, repo, toList, items,
	diff, patch, revert, merge, chain, rebase,
	resolve, resolveA, tryResolve, tryResolveA,
	resolved, unresolved,
	newest, newestLeft, preferLeft, preferRight,
	apply,
	exclude, include
	) where

import Prelude hiding (filter)
import Prelude.Unicode

import Control.Applicative ((<$>))
import Control.Arrow ((***), (&&&))
import Data.Either
import qualified Data.Map as M

import Sync.Base.Types
import Sync.Base.Internal

-- | Empty repo
empty ∷ Repo k a
empty = Repo M.empty

-- | Is repo empty
nullRepo ∷ Repo k a → Bool
nullRepo = M.null ∘ repoItems

-- | Make repo
repo ∷ Ord k ⇒ [(k, a)] → Repo k a
repo = Repo ∘ M.fromList

-- | Get list of entities
toList ∷ Repo k a → [(k, a)]
toList (Repo r) = M.toList r

-- | Get list of entities as @RepoItem@
items ∷ Repo k a → [RepoItem k a]
items = map (uncurry RepoItem) ∘ toList

-- | Compare repositories
diff ∷ Ord k ⇒ Repo k a → Repo k a → Diff k a
diff (Repo l) (Repo r) = Repo $ M.unions [left', right', both'] where
	left' = M.map ChangeLeft $ M.difference l r
	right' = M.map ChangeRight $ M.difference r l
	both' = M.intersectionWith ChangeBoth l r

-- | Make patches in both directions by diff
patch ∷ Eq a ⇒ Diff k a → (Patch k a, Patch k a)
patch d = (mapMaybe (fn ∘ swapChange) d, mapMaybe fn d) where
	fn (ChangeLeft l) = Just $ Create l
	fn (ChangeRight r) = Just $ Delete r
	fn (ChangeBoth l r)
		| l ≡ r = Nothing
		| otherwise = Just $ Update r l

-- | Inverse patch
revert ∷ Patch k a → Patch k a
revert = fmap revert' where
	revert' (Create v) = Delete v
	revert' (Update v v') = Update v' v
	revert' (Delete v) = Create v

-- | Merge two independent patches
merge ∷ (Ord k, Eq a) ⇒ Patch k a → Patch k a → Merge k a
merge l r = merge' <$> diff l r where
	merge' (ChangeLeft l') = Merged l'
	merge' (ChangeRight r') = Merged r'
	merge' (ChangeBoth l' r')
		| l' ≡ r' = Merged l'
		| otherwise = Conflict l' r'

-- | Chain two patches
chain ∷ (Ord k, Eq a) ⇒ Patch k a → Patch k a → Merge k a
chain l r = mapMaybe chain' $ diff l r where
	chain' (ChangeLeft l') = Just $ Merged l'
	chain' (ChangeRight r') = Just $ Merged r'
	chain' (ChangeBoth l' r')
		| ldst ≡ rsrc = Merged <$> fromPath lsrc rdst
		| otherwise = Just $ Conflict l' r'
		where
			(lsrc, ldst) = actionPath l'
			(rsrc, rdst) = actionPath r'
			actionPath (Create y) = (Nothing, Just y)
			actionPath (Update x y) = (Just x, Just y)
			actionPath (Delete x) = (Just x, Nothing)
			fromPath Nothing (Just y) = Just $ Create y
			fromPath (Just x) Nothing = Just $ Delete x
			fromPath (Just x) (Just y)
				| x ≡ y = Nothing
				| otherwise = Just $ Update x y
			fromPath Nothing Nothing = Nothing

-- | Rebase left over right
rebase ∷ (Ord k, Eq a) ⇒ Patch k a → Patch k a → Merge k a
rebase l r = mapMaybe rebase' $ diff l r where
	rebase' (ChangeLeft l') = Just $ Merged l'
	rebase' (ChangeRight _) = Nothing
	rebase' (ChangeBoth l' r')
		| l' ≡ r' = Nothing
		| otherwise = Just $ Conflict l' r'

-- | Resolve conflicts
resolve ∷ (k → Action a → Action a → Maybe (Action a)) → Merge k a → Patch k a
resolve fn = mapMaybeWithKey resolve' where
	resolve' _ (Merged v) = Just v
	resolve' k (Conflict l r) = fn k l r

-- | Resolve with applicative
resolveA ∷ Applicative t ⇒ (k → Action a → Action a → t (Maybe (Action a))) → Merge k a → t (Patch k a)
resolveA fn = traverseMaybeWithKey resolveA' where
	resolveA' _ (Merged v) = pure $ Just v
	resolveA' k (Conflict l r) = fn k l r

-- | Try resolve conflicts, producing new merged-state
tryResolve ∷ (k → Action a → Action a → Maybe (Maybe (Action a))) → Merge k a → Merge k a
tryResolve fn = mapMaybeWithKey tryResolve' where
	tryResolve' k (Conflict l r) = case fn k l r of
		Just (Just v) → Just $ Merged v
		Just Nothing → Nothing
		Nothing → Just $ Conflict l r
	tryResolve' _ (Merged v) = Just $ Merged v

-- | @tryResolve@ with applicative action
tryResolveA ∷ Applicative t ⇒ (k → Action a → Action a → t (Maybe (Maybe (Action a)))) → Merge k a → t (Merge k a)
tryResolveA fn = traverseMaybeWithKey tryResolveA' where
	tryResolveA' k (Conflict l r) = toMerged <$> fn k l r where
		toMerged (Just (Just v)) = Just $ Merged v
		toMerged (Just Nothing) = Nothing
		toMerged Nothing = Just $ Conflict l r
	tryResolveA' _ (Merged v) = pure $ Just $ Merged v

-- | Get resolved part
resolved ∷ Merge k a → Patch k a
resolved = mapMaybe resolved' where
	resolved' (Merged v) = Just v
	resolved' _ = Nothing

-- | Leave only unresolved part
unresolved ∷ Merge k a → Merge k a
unresolved = filter unresolved' where
	unresolved' (Conflict _ _) = True
	unresolved' _ = False

-- | Resolve tactic: select newest
newest ∷ Ord a ⇒ k → Action a → Action a → Maybe (Maybe (Action a))
newest _ l r = do
	ldst ← dst l
	rdst ← dst r
	return $ Just $ if ldst > rdst then l else r
	where
		dst (Create v) = Just v
		dst (Update _ v) = Just v
		dst (Delete _) = Nothing

-- | Resolve tactic: select left, if it's newer
newestLeft ∷ Ord a ⇒ k → Action a → Action a → Maybe (Maybe (Action a))
newestLeft _ l r = do
	ldst ← dst l
	rdst ← dst r
	return $ if ldst > rdst then Just l else Nothing
	where
		dst (Create v) = Just v
		dst (Update _ v) = Just v
		dst (Delete _) = Nothing

-- | Resolve tactic: prefer left
preferLeft ∷ k → Action a → Action a → Maybe (Action a)
preferLeft _ l _ = Just l

-- | Resolve tactic: prefer right
preferRight ∷ k → Action a → Action a → Maybe (Action a)
preferRight _ _ r = Just r

-- | Returns valid and invalid part, which can't be applied:
-- * Create - already exists
-- * Update - doesn't exist or not equal to previous value
-- * Delete - doesn't exist or not equal to previous value
validate ∷ (Ord k, Eq a) ⇒ Patch k a → Repo k a → (Patch k a, Patch k a)
validate (Repo p) (Repo r) = (Repo *** Repo) $ M.partitionWithKey validate' p where
	validate' key (Create _) = M.notMember key r
	validate' key (Update v _) = M.lookup key r ≡ Just v
	validate' key (Delete v) = M.lookup key r ≡ Just v

-- | Apply patch, fails if patch can't be applied
apply ∷ (Ord k, Eq a) ⇒ Patch k a → Repo k a → Repo k a
apply p r
	| not (nullRepo invalid') = error "Patch can't be applied"
	| otherwise = Repo $ M.fromList adds `M.union` repoItems r `M.difference` M.fromList deletes
	where
		invalid' = snd $ validate p r
		(adds, deletes) = (rights &&& lefts) ∘ fmap (uncurry makeAct) ∘ toList $ p
		makeAct key (Create v) = Right (key, v)
		makeAct key (Update _ v) = Right (key, v)
		makeAct key (Delete _) = Left (key, ())

-- | Filter repo, excluding matching keys
exclude ∷ (k → Bool) → Repo k a → Repo k a
exclude p = filterWithKey (\k _ → not (p k))

-- | Filter repo, leaving only matching keys
include ∷ (k → Bool) → Repo k a → Repo k a
include p = filterWithKey (\k _ → p k)
