{-# LANGUAGE FlexibleContexts #-}

module Sync.Git (
	enumGit, git, remoteGit,

	module Sync.Base,
	module Sync.Repo
	) where

import Prelude.Unicode

import Control.Arrow
import Control.Monad.Except
import Data.List (nub)
import Data.Maybe (mapMaybe, listToMaybe, catMaybes)
import Data.Time.Clock
import Data.Tuple (swap)
import System.Directory
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
			dir ← doesDirectoryExist f
			return (entity dir f, Update tm)
		getStat False f = do
			dir ← doesDirectoryExist f
			return (entity dir f, Delete)

remoteGit ∷ String → FilePath → IO (Patch Entity UTCTime)
remoteGit host fpath = ssh host $ do
	cd fpath
	out ← invoke "git status -s"
	repo <$> fmap catMaybes (traverse ((`catchError` const (return Nothing)) ∘ fmap Just ∘ uncurry getStat) (parseGitStatus out))
	where
		getStat True f = second Update <$> stat f
		getStat False f = return (entity False f, Delete)

data GitStatus = Ignored | Untracked | Added | Unmerged | Modified | Renamed | Deleted | Copied deriving (Eq, Ord, Enum, Bounded)

gitStates ∷ [(GitStatus, Char)]
gitStates = [
	(Ignored, '!'),
	(Untracked, '?'),
	(Added, 'A'),
	(Unmerged, 'U'),
	(Modified, 'M'),
	(Renamed, 'R'),
	(Deleted, 'D'),
	(Copied, 'C')]

instance Show GitStatus where
	show = maybe undefined return ∘ flip lookup gitStates

instance Read GitStatus where
	readsPrec _ "" = []
	readsPrec _ (s:ss) = maybe [] (\st → [(st, ss)]) $ lookup s (map swap gitStates)

parseGitStatus ∷ [String] → [(Bool, FilePath)]
parseGitStatus = concatMap parse' where
	parse' f = maybe [] (uncurry toStatus) $ do
		[_, mods, from, to] ← listToMaybe (f =~ "^([!?AUMRDC ]{2}) (.*?)(?: -> (.*?))?$" ∷ [[String]])
		let
			mod' = listToMaybe $ merge' $ nub $ map simplifyStatus $ mapMaybe (readMaybe ∘ return) mods
			files = filter (not ∘ null) [from, to]
		return (mod', files)

	simplifyStatus Ignored = Modified
	simplifyStatus Untracked = Modified
	simplifyStatus Added = Modified
	simplifyStatus Unmerged = Modified
	simplifyStatus Copied = error "Never seen this, don't know what to do, so fail"
	simplifyStatus s = s

	merge' [Modified, Renamed] = [Renamed]
	merge' [Modified, Deleted] = [Deleted]
	merge' [Renamed, Modified] = [Renamed]
	merge' [Renamed, Deleted] = [Deleted]
	merge' s = s
	toStatus Nothing _ = []
	toStatus (Just Modified) [f] = return (True, f)
	toStatus (Just Renamed) [f, t] = [(False, f), (True, t)]
	toStatus (Just Deleted) [f] = return (False, f)
	toStatus _ _ = []
