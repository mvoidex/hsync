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
		it "patch" $ do
			apply (fst $ patch $ diff base other) base `shouldBe` other
			apply (snd $ patch $ diff base other) other `shouldBe` base
		it "reverts" $ do
			revert (revert patch1) `shouldBe` patch1
			revert (revert patch2) `shouldBe` patch2
		it "chains" $ do
			apply (resolved (patch1 `chain` patch2)) base `shouldBe` result
			resolved (patch1 `chain` patch2) `shouldBe` fst (patch $ diff base result)
		it "rebases" $
			(patch3 `chain` resolved (rebase patch1 patch3)) `shouldBe` merge patch1 patch3
	where
		result = apply patch2 ∘ apply patch1 $ base

base ∷ Repo String Int
base = repo [
	("foo", 0),
	("bar", 10),
	("baz", 20)]

other ∷ Repo String Int
other = repo [
	("foo", 10),
	("bar", 0),
	("quux", 40)]

patch1 ∷ Patch String Int
patch1 = repo [
	("foo", Update 0 10),
	("bar", Delete 10),
	("quux", Create 0)]

patch2 ∷ Patch String Int
patch2 = repo [
	("foo", Update 10 20),
	("quux", Update 0 10)]

patch3 ∷ Patch String Int
patch3 = repo [
	("foo", Update 0 10)]
