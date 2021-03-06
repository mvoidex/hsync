{-# LANGUAGE FlexibleContexts #-}

module Sync.GitLike (
	GitLike(..),
	enumGitLike, gitLike, remoteGitLike,
	markGitLike, remoteMarkGitLike,

	module Sync.Base,
	module Sync.Repo
	) where

import Prelude.Unicode

import Control.Arrow
import Control.Lens
import Control.Monad.Except
import Data.Either (partitionEithers)
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

data GitLike = GitLike {
	gitLikeCommand ∷ String,
	gitLikeArgs ∷ [String] }

gitLikeCmds ∷ GitLike → [String]
gitLikeCmds (GitLike cmd args) = cmd : args

gitLikeShellCmd ∷ GitLike → String
gitLikeShellCmd = unwords ∘ gitLikeCmds

enumGitLike ∷ GitLike → Location → Bool → IO (Patch Entity (Maybe UTCTime), Repo Entity UTCTime)
enumGitLike cvs = location (gitLike cvs) (remoteGitLike cvs)

gitLike ∷ GitLike → FilePath → Bool → IO (Patch Entity (Maybe UTCTime), Repo Entity UTCTime)
gitLike cvs fpath untracked = withDir fpath $ do
	status ← lines <$> readProcess (gitLikeCommand cvs) (gitLikeArgs cvs ++ ["status", if untracked then "-s" else "-suno", "."]) ""
	let
		(trackedList, untrackedList) = parseGitLikeStatus status
	rgit ← traverse (uncurry getStat) trackedList
	ugit ← filterM (fmap not ∘ pathIsSymbolicLink) untrackedList >>= traverse stat'
	udirs ← fmap concat ∘ mapM (untrackedDir ∘ view entityPath) ∘ filter isDir ∘ map fst $ ugit
	return (repo rgit, repo $ ugit ++ udirs)
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
			return $ toList ∘ mapKeys (over entityPath (normalise ∘ (d </>))) $ dirCts

remoteGitLike ∷ GitLike → String → FilePath → Bool → IO (Patch Entity (Maybe UTCTime), Repo Entity UTCTime)
remoteGitLike cvs host fpath untracked = ssh host $ do
	cd fpath
	out ← invoke $ unwords $ gitLikeCmds cvs ++ ["status", if untracked then "-s" else "-suno", "."]
	let
		(trackedList, untrackedList) = parseGitLikeStatus out
	rgit ← fmap catMaybes (traverse ((`catchError` const (return Nothing)) ∘ fmap Just ∘ uncurry getStat) trackedList)
	ugit ← filterM (fmap not ∘ isLink) untrackedList >>= traverse stat
	udirs ← fmap concat ∘ mapM (untrackedDir ∘ view entityPath) ∘ filter isDir ∘ map fst $ ugit
	return (repo rgit, repo $ ugit ++ udirs)
	where
		getStat (Create _) f = second (Create ∘ Just) <$> stat f
		getStat (Update _ _) f = second (Update Nothing ∘ Just) <$> stat f
		getStat (Delete _) f = return (Entity False f, Delete Nothing)
		untrackedDir d = do
			cts ← invoke $ "find '" ++ d ++ "' -mindepth 1 -type f -or -type d"
			r ← repo <$> fmap catMaybes (mapM (\f → fmap Just (stat f) `catchError` const (return Nothing)) cts)
			return $ toList ∘ mapKeys (over entityPath normalise) $ r

-- | Mark git file according to action performed
markGitLike ∷ GitLike → Entity → Action a → IO ()
markGitLike _ (Entity True _) _ = return ()
markGitLike _ _ (Update _ _) = return ()
markGitLike cvs (Entity False fpath) (Delete _) = void $ readProcess (gitLikeCommand cvs) (gitLikeArgs cvs ++ ["rm", "--cached", fpath]) ""
markGitLike cvs (Entity False fpath) _ = void $ readProcess (gitLikeCommand cvs) (gitLikeArgs cvs ++ ["add", fpath]) ""

-- | Mark remote git file according to action performed
remoteMarkGitLike ∷ GitLike → Entity → Action a → ProcessM ()
remoteMarkGitLike _ (Entity True _) _ = return ()
remoteMarkGitLike _ _ (Update _ _) = return ()
remoteMarkGitLike cvs (Entity False fpath) (Delete _) = invoke_ $ gitLikeShellCmd cvs ++ " rm --cached " ++ quote fpath
remoteMarkGitLike cvs (Entity False fpath) _ = invoke_ $ gitLikeShellCmd cvs ++ " add " ++ quote fpath

data GitLikeStatus = Ignored | Untracked | Added | Unmerged | Modified | Renamed | Deleted | Copied deriving (Eq, Ord, Enum, Bounded)

gitStates ∷ [(GitLikeStatus, Char)]
gitStates = [
	(Ignored, '!'),
	(Untracked, '?'),
	(Added, 'A'),
	(Unmerged, 'U'),
	(Modified, 'M'),
	(Renamed, 'R'),
	(Deleted, 'D'),
	(Copied, 'C')]

instance Show GitLikeStatus where
	show = maybe undefined return ∘ flip lookup gitStates

instance Read GitLikeStatus where
	readsPrec _ "" = []
	readsPrec _ (s:ss) = maybe [] (\st → [(st, ss)]) $ lookup s (map swap gitStates)

parseGitLikeStatus ∷ [String] → ([(Action (), FilePath)], [FilePath])
parseGitLikeStatus = partitionEithers ∘ concatMap parse' where
	parse' f = maybe [] (uncurry toStatus) $ do
		[_, mods, from', to'] ← listToMaybe (f =~ "^([!?AUMRDC ]{2}) (.*?)(?: -> (.*?))?$" ∷ [[String]])
		let
			mod' = listToMaybe $ merge' $ nub $ map simplifyStatus $ mapMaybe (readMaybe ∘ return) mods
			files = filter (not ∘ null) [from', to']
		return (mod', files)

	-- Leaves only `Added`, `Modified`, `Deleted`, `Renamed` and `Copied`
	simplifyStatus Ignored = Added
	simplifyStatus Unmerged = Modified
	simplifyStatus s = s

	merge' [Added, Modified] = [Added]
	merge' [Added, Deleted] = error "Impossible git status: AD - added, then deleted"
	merge' [Added, Renamed] = error "Impossible git status: AR - added, then renamed (should be added with new name)"
	merge' [Added, Copied] = error "Unknown git status: AC - added, then copied"
	merge' [Modified, Added] = error "Impossible git status: MA - modified, then added"
	merge' [Modified, Deleted] = [Deleted]
	merge' [Modified, Renamed] = error "Impossible git status: MR - modified, then renamed (should be deleted and added with new name)"
	merge' [Modified, Copied] = error "Unknown git status: MC - modified, then copied"
	merge' [Deleted, _] = error "Impossible git status: Dx - deleted, then smth else"
	merge' [Renamed, Added] = error "Impossible git status: RA - renamed, then added"
	merge' [Renamed, Modified] = [Renamed]
	merge' [Renamed, Deleted] = [Deleted]
	merge' [Renamed, Copied] = error "Unknown git status: RC - renamed, then copied"
	merge' [Copied, Added] = error "Impossible git status: CA - copied, then added"
	merge' [Copied, Modified] = [Copied]
	merge' [Copied, Deleted] = [Deleted]
	merge' [Copied, Renamed] = error "Unknown git status: CR - copied, then renamed"
	merge' [s] = [s]
	merge' s = error $ "Impossible git status: " ++ show s

	toStatus Nothing _ = []
	toStatus (Just Added) [f] = return $ Left (Create (), f)
	toStatus (Just Modified) [f] = return $ Left (Update () (), f)
	toStatus (Just Renamed) [f, t] = [Left (Delete (), f), Left (Create (), t)]
	toStatus (Just Copied) [_, t] = return $ Left (Update () (), t)
	toStatus (Just Deleted) [f] = return $ Left (Delete (), f)
	toStatus (Just Untracked) [f] = return (Right f)
	toStatus (Just s) fs = error $ "Don't know how to convert this git status to actions, status: " ++ show s ++ ", files: " ++ intercalate ", " fs
