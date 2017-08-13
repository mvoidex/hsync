module Sync.Mode (
	Mode(..),
	syncPatch
	) where

import Prelude.Unicode

import Sync.Base

-- | Sync mode
data Mode = Default | Mirror | New | Overwrite | Skip deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Make patch to apply on @dst@, synchronizing updates on @src@
syncPatch ∷ (Ord k, Ord a) ⇒ Mode → Patch k a → Patch k a → Merge k a
syncPatch m src dst = case m of
	Default → combined
	Mirror → revert dst `chain` src
	New → tryResolve newest combined
	Overwrite → fmap Merged ∘ resolve preferLeft $ combined
	Skip → fmap Merged ∘ resolved $ combined
	where
		combined = rebase src dst
