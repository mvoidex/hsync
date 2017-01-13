{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (
	main
	) where

import Prelude.Unicode

import qualified Data.Map as M
import Test.Hspec

import Sync.Base

main ∷ IO ()
main = hspec $
	describe "base" $ do
		it "patch mirror" $
			patch (fst $ mirror $ diff lrepo rrepo) lrepo ≡ rrepo
		it "patch combine" $
			let
				(l', r') = combine (≥) $ diff lrepo rrepo
			in
				patch l' lrepo ≡ patch r' rrepo

lrepo ∷ Repo String Int
lrepo = M.fromList [
	("foo", 0),
	("bar", 10),
	("baz", 20)]

rrepo ∷ Repo String Int
rrepo = M.fromList [
	("foo", 10),
	("baz", 10),
	("quux", 30)]
