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
import Data.List (nub, intercalate)
import Data.Maybe (mapMaybe, listToMaybe, catMaybes)
import Data.Time.Clock
import Data.Tuple (swap)
import System.Directory
import System.FilePath.Posix
import System.Process
import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))

import Sync.Base
import Sync.Base.Internal (mapKeys)
import Sync.Repo
import Sync.Dir
import Sync.Ssh

enumGit ∷ Location → Bool → IO (Patch Entity (Maybe UTCTime))
enumGit = location git remoteGit

git ∷ FilePath → Bool → IO (Patch Entity (Maybe UTCTime))
git fpath untracked = withDir fpath $ do
	status ← lines <$> readProcess "git" ["status", if untracked then "-s" else "-suno"] ""
	rgit ← traverse (uncurry getStat) (dropSiblings $ parseGitStatus status)
	udirs ← fmap concat ∘ mapM (untrackedDir ∘ view entityPath) ∘ filter isDir ∘ map fst $ rgit
	return $ repo $ rgit ++ udirs
	where
		stat' f = do
			tm ← getMTime f
			isDir' ← doesDirectoryExist f
			return (Entity isDir' f, tm)
		getStat (Create _) f = second (Create ∘ Just) <$> stat' f
		getStat (Update _ _) f = second (Update Nothing ∘ Just) <$> stat' f
		getStat (Delete _) f = return (Entity False f, Delete Nothing)
		untrackedDir d = do
			dirCts ← dir d
			return $ toList ∘ fmap (Create ∘ Just) ∘ mapKeys (over entityPath (normalise ∘ (d </>))) $ dirCts

remoteGit ∷ String → FilePath → Bool → IO (Patch Entity (Maybe UTCTime))
remoteGit host fpath untracked = ssh host $ do
	cd fpath
	out ← invoke $ unwords $ "git" : ["status", if untracked then "-s" else "-suno"]
	rgit ← fmap catMaybes (traverse ((`catchError` const (return Nothing)) ∘ fmap Just ∘ uncurry getStat) (dropSiblings $ parseGitStatus out))
	udirs ← fmap concat ∘ mapM (untrackedDir ∘ view entityPath) ∘ filter isDir ∘ map fst $ rgit
	return $ repo $ rgit ++ udirs
	where
		getStat (Create _) f = second (Create ∘ Just) <$> stat f
		getStat (Update _ _) f = second (Update Nothing ∘ Just) <$> stat f
		getStat (Delete _) f = return (Entity False f, Delete Nothing)
		untrackedDir d = do
			cts ← invoke $ "find '" ++ d ++ "' -mindepth 1"
			r ← repo <$> fmap catMaybes (mapM (\f → fmap Just (stat f) `catchError` const (return Nothing)) cts)
			return $ toList ∘ fmap (Create ∘ Just) ∘ mapKeys (over entityPath normalise) $ r

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

-- | Leave only files within current directory
dropSiblings ∷ [(Action (), FilePath)] → [(Action (), FilePath)]
dropSiblings = filter (not ∘ sibling ∘ snd) where
	sibling fpath = (listToMaybe ∘ splitDirectories $ fpath) ≡ Just ".."

parseGitStatus ∷ [String] → [(Action (), FilePath)]
parseGitStatus = concatMap parse' where
	parse' f = maybe [] (uncurry toStatus) $ do
		[_, mods, from', to'] ← listToMaybe (f =~ "^([!?AUMRDC ]{2}) (.*?)(?: -> (.*?))?$" ∷ [[String]])
		let
			mod' = listToMaybe $ merge' $ nub $ map simplifyStatus $ mapMaybe (readMaybe ∘ return) mods
			files = filter (not ∘ null) [from', to']
		return (mod', files)

	-- Leaves only `Added`, `Modified`, `Deleted` and `Renamed`
	simplifyStatus Ignored = Added
	simplifyStatus Untracked = Added
	simplifyStatus Unmerged = Modified
	simplifyStatus Copied = error "Never seen this, don't know what to do, so fail"
	simplifyStatus s = s

	merge' [Added, Modified] = [Added]
	merge' [Added, Deleted] = error "Impossible git status: added, then deleted"
	merge' [Added, Renamed] = error "Impossible git status: added, then renamed (should be added with new name)"
	merge' [Modified, Added] = error "Impossible git status: modified, then added"
	merge' [Modified, Deleted] = [Deleted]
	merge' [Modified, Renamed] = error "Impossible git status: modified, then renamed (should be deleted and added with new name)"
	merge' [Deleted, _] = error "Impossible git status: deleted, then smth else"
	merge' [Renamed, Added] = error "Impossible git status: renamed, then added"
	merge' [Renamed, Modified] = [Renamed]
	merge' [Renamed, Deleted] = [Deleted]
	merge' [s] = [s]
	merge' s = error $ "Impossible git status: " ++ show s

	toStatus Nothing _ = []
	toStatus (Just Added) [f] = return (Create (), f)
	toStatus (Just Modified) [f] = return (Update () (), f)
	toStatus (Just Renamed) [f, t] = [(Delete (), f), (Create (), t)]
	toStatus (Just Deleted) [f] = return (Delete (), f)
	toStatus (Just s) fs = error $ "Don't know how to convert this git status to actions, status: " ++ show s ++ ", files: " ++ intercalate ", " fs
