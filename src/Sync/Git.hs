{-# LANGUAGE FlexibleContexts #-}

module Sync.Git (
	enumGit, git, remoteGit,

	module Sync.Base,
	module Sync.Repo
	) where

import Prelude.Unicode

import Control.Arrow
import Control.Lens
import Control.Monad.Except
import Data.List (nub, inits)
import Data.Maybe (mapMaybe, listToMaybe, catMaybes)
import qualified Data.Set as S
import Data.Time.Clock
import Data.Tuple (swap)
import System.Directory
import System.FilePath.Posix
import System.Process
import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))

import Sync.Base hiding (swap)
import Sync.Repo
import Sync.Dir hiding (swap)
import Sync.Ssh

enumGit ∷ Location → Bool → IO (Patch Entity UTCTime)
enumGit = location git remoteGit

git ∷ FilePath → Bool → IO (Patch Entity UTCTime)
git fpath untracked = withDir fpath $ do
	status ← lines <$> readProcess "git" ["status", if untracked then "-s" else "-suno"] ""
	rgit ← traverse (uncurry getStat) (parseGitStatus status)
	udirs ← fmap concat ∘ mapM (untrackedDir ∘ view entityPath) ∘ filter isDir ∘ map fst $ rgit
	let
		parentDirs = S.toList ∘ S.fromList ∘ concatMap (parents ∘ view entityPath) ∘ filter isFile ∘ map fst $ rgit
	pdirs ← traverse (uncurry getStat) (zip (repeat True) parentDirs)
	return $ repo $ rgit ++ udirs ++ pdirs
	where
		getStat True f = do
			tm ← getMTime f
			isDir' ← doesDirectoryExist f
			return (Entity isDir' f, Update tm)
		getStat False f = do
			isDir' ← doesDirectoryExist f
			return (Entity isDir' f, Delete)
		untrackedDir d = do
			dirCts ← dir d
			return $ toList ∘ fmap Update ∘ mapKeys (over entityPath (normalise ∘ (d </>))) $ dirCts

remoteGit ∷ String → FilePath → Bool → IO (Patch Entity UTCTime)
remoteGit host fpath untracked = ssh host $ do
	cd fpath
	out ← invoke $ "git status " ++ (if untracked then "-s" else "-suno")
	rgit ← fmap catMaybes (traverse ((`catchError` const (return Nothing)) ∘ fmap Just ∘ uncurry getStat) (parseGitStatus out))
	udirs ← fmap concat ∘ mapM (untrackedDir ∘ view entityPath) ∘ filter isDir ∘ map fst $ rgit
	let
		parentDirs = S.toList ∘ S.fromList ∘ concatMap (parents ∘ view entityPath) ∘ filter isFile ∘ map fst $ rgit
	pdirs ← traverse (uncurry getStat) (zip (repeat True) parentDirs)
	return $ repo $ rgit ++ udirs ++ pdirs
	where
		getStat True f = second Update <$> stat f
		getStat False f = return (Entity False f, Delete)
		untrackedDir d = do
			cts ← invoke $ "find '" ++ d ++ "' -mindepth 1"
			r ← repo <$> fmap catMaybes (mapM (\f → fmap Just (stat f) `catchError` const (return Nothing)) cts)
			return $ toList ∘ fmap Update ∘ mapKeys (over entityPath (normalise ∘ (d </>))) $ r

parents ∷ FilePath → [FilePath]
parents = map joinPath ∘ tail ∘ inits ∘ splitDirectories ∘ takeDirectory ∘ normalise

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
		[_, mods, from', to'] ← listToMaybe (f =~ "^([!?AUMRDC ]{2}) (.*?)(?: -> (.*?))?$" ∷ [[String]])
		let
			mod' = listToMaybe $ merge' $ nub $ map simplifyStatus $ mapMaybe (readMaybe ∘ return) mods
			files = filter (not ∘ null) [from', to']
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
