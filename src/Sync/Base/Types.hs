{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances, UndecidableInstances #-}

module Sync.Base.Types (
	Repo(..), Change(..), Action(..), Merged(..), Diff, Patch, Merge,
	RepoItem(..)
	) where

import Data.Map (Map)
import qualified Data.Map as M

-- | Named items
newtype Repo k a = Repo { repoItems ∷ Map k a } deriving (Eq, Ord, Functor, Traversable, Foldable)
-- | Change between repos
data Change a = ChangeLeft a | ChangeRight a | ChangeBoth a a deriving (Eq, Ord, Functor)
-- | Action on item
data Action a = Create a | Update a a | Delete a deriving (Eq, Ord, Functor)
-- | Merged or conflicted
data Merged a = Merged (Action a) | Conflict (Action a) (Action a) deriving (Eq, Ord, Functor)

-- | Diff is repo if changes
type Diff k a = Repo k (Change a)
-- | Patch is repo of actions
type Patch k a = Repo k (Action a)
-- | Merge state
type Merge k a = Repo k (Merged a)

instance Show a ⇒ Show (Change a) where
	show (ChangeLeft v) = "⇐ " ++ show v
	show (ChangeRight v) = "⇒ " ++ show v
	show (ChangeBoth l r) = "⇔ " ++ show l ++ " " ++ show r

instance Show a ⇒ Show (Action a) where
	show (Create v) = "+ " ++ show v
	show (Update v v') = "* " ++ show v ++ " -> " ++ show v'
	show (Delete v) = "- " ++ show v

instance Show a ⇒ Show (Merged a) where
	show (Merged v) = "✓ " ++ show v
	show (Conflict l r) = "✗ " ++ show l ++ " ≠ " ++ show r

-- | Used for printing items
data RepoItem k a = RepoItem k a deriving (Eq, Ord)

instance Show k ⇒ Show (RepoItem k (Change a)) where
	show (RepoItem name (ChangeLeft _)) = "⇐ " ++ show name
	show (RepoItem name (ChangeRight _)) = "⇒ " ++ show name
	show (RepoItem name (ChangeBoth _ _)) = "⇔ " ++ show name

instance Show k ⇒ Show (RepoItem k (Action a)) where
	show (RepoItem name (Create _)) = "✓ " ++ show name
	show (RepoItem name (Update _ _)) = "✓ " ++ show name
	show (RepoItem name (Delete _)) = "✗ " ++ show name

instance Show k ⇒ Show (RepoItem k (Merged a)) where
	show (RepoItem name (Merged _)) = "✓ " ++ show name
	show (RepoItem name (Conflict _ _)) = "✗ " ++ show name

instance {-# OVERLAPPABLE #-} Show k ⇒ Show (RepoItem k a) where
	show (RepoItem name _) = show name

instance Show (RepoItem k a) ⇒ Show (Repo k a) where
	show (Repo r) = unlines $ do
		(k, v) ← M.toList r
		return $ show $ RepoItem k v
