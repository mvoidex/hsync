{-# LANGUAGE FlexibleContexts #-}

module Sync.Git (
	enumGit, git, remoteGit,

	module Sync.Base,
	module Sync.Repo
	) where

import Prelude.Unicode

import Control.Arrow
import Data.List (nub)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Time.Clock
import Data.Tuple (swap)
import System.Process
import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))

import Sync.Base hiding (swap)
import Sync.Repo
import Sync.Ssh

enumGit ∷ Location → IO (Patch Entity UTCTime)
enumGit = location git remoteGit

git ∷ FilePath → IO (Patch Entity UTCTime)
git fpath = withDir fpath $ do
	status ← lines <$> readProcess "git" ["status", "-s"] ""
	repo <$> traverse (uncurry getStat) (parseGitStatus status)
	where
		getStat True f = do
			tm ← getMTime f
			return (entity False f, Update tm)
		getStat False f = return (entity False f, Delete)

remoteGit ∷ String → FilePath → IO (Patch Entity UTCTime)
remoteGit host fpath = ssh host $ do
	cd fpath
	out ← invoke "git status -s"
	repo <$> traverse (uncurry getStat) (parseGitStatus out)
	where
		getStat True f = second Update <$> stat f
		getStat False f = return (entity False f, Delete)

data GitStatus = Untracked | Added | Modified | Renamed | Deleted deriving (Eq, Ord, Enum, Bounded)

gitStates ∷ [(GitStatus, Char)]
gitStates = [
	(Untracked, '?'),
	(Added, 'A'),
	(Modified, 'M'),
	(Renamed, 'R'),
	(Deleted, 'D')]

instance Show GitStatus where
	show = maybe undefined return ∘ flip lookup gitStates

instance Read GitStatus where
	readsPrec _ "" = []
	readsPrec _ (s:ss) = maybe [] (\st → [(st, ss)]) $ lookup s (map swap gitStates)

parseGitStatus ∷ [String] → [(Bool, FilePath)]
parseGitStatus = concatMap parse' where
	parse' f = maybe [] (uncurry toStatus) $ do
		[_, mods, from, to] ← listToMaybe (f =~ "^([AMDR? ]{2}) (.*?)(?: -> (.*?))?$" ∷ [[String]])
		let
			mod' = listToMaybe $ merge' $ nub $ mapMaybe (readMaybe ∘ return) mods
			files = filter (not ∘ null) [from, to]
		return (mod', files)
	merge' [Added, Modified] = [Modified]
	merge' [Added, Renamed] = [Added]
	merge' [Added, Deleted] = []
	merge' [Modified, Renamed] = [Renamed]
	merge' [Modified, Deleted] = [Deleted]
	merge' [Renamed, Modified] = [Renamed]
	merge' [Renamed, Deleted] = [Deleted]
	merge' s = s
	toStatus Nothing _ = []
	toStatus (Just Untracked) [f] = return (True, f)
	toStatus (Just Added) [f] = return (True, f)
	toStatus (Just Modified) [f] = return (True, f)
	toStatus (Just Renamed) [f, t] = [(False, f), (True, t)]
	toStatus (Just Deleted) [f] = return (False, f)
	toStatus _ _ = []
