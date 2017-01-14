{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (
	main
	) where

import Prelude.Unicode

import Test.Hspec

import Sync.Base

main ∷ IO ()
main = hspec $
	describe "base" $ do
		it "patch mirror" $
			apply (fst $ patch mirror $ diff lrepo rrepo) lrepo ≡ rrepo
		it "patch combine" $
			let
				(l', r') = patch combine $ diff lrepo rrepo
			in
				apply l' lrepo ≡ apply r' rrepo

lrepo ∷ Repo String Int
lrepo = repo [
	("foo", 0),
	("bar", 10),
	("baz", 20)]

rrepo ∷ Repo String Int
rrepo = repo [
	("foo", 10),
	("baz", 10),
	("quux", 30)]
