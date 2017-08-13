{-# LANGUAGE FlexibleContexts #-}

module Sync.Svn (
	enumSvn, svn, remoteSvn,
	markSvn, remoteMarkSvn,

	module Sync.Base,
	module Sync.Repo
	) where

import Prelude.Unicode

import Control.Arrow
import Control.Lens
import Control.Monad.Except
import Data.Either (partitionEithers)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Time.Clock
import Data.Tuple (swap)
import System.Directory
import System.FilePath.Posix
import qualified System.FilePath as F
import System.Process
import Text.Read (readMaybe)

import Sync.Base
import Sync.Base.Internal (mapKeys)
import Sync.Repo
import Sync.Dir
import Sync.Ssh

enumSvn ∷ Location → Bool → IO (Patch Entity (Maybe UTCTime), Repo Entity UTCTime)
enumSvn = location svn remoteSvn

svn ∷ FilePath → Bool → IO (Patch Entity (Maybe UTCTime), Repo Entity UTCTime)
svn fpath untracked = withDir fpath $ do
	status ← lines <$> readProcess "svn" ("status" : if untracked then [] else ["-q"]) ""
	let
		(trackedList, untrackedList) = parseSvnStatus status
	rsvn ← traverse (uncurry getStat) trackedList
	usvn ← traverse stat' untrackedList
	udirs ← fmap concat ∘ mapM (untrackedDir ∘ view entityPath) ∘ filter isDir ∘ map fst $ usvn
	return (repo rsvn, repo $ usvn ++ udirs)
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

remoteSvn ∷ String → FilePath → Bool → IO (Patch Entity (Maybe UTCTime), Repo Entity UTCTime)
remoteSvn host fpath untracked = ssh host $ do
	cd fpath
	out ← invoke $ unwords $ "svn" : ("status" : if untracked then [] else ["-q"])
	let
		(trackedList, untrackedList) = parseSvnStatus out
	rsvn ← fmap catMaybes (traverse ((`catchError` const (return Nothing)) ∘ fmap Just ∘ uncurry getStat) trackedList)
	usvn ← traverse stat untrackedList
	udirs ← fmap concat ∘ mapM (untrackedDir ∘ view entityPath) ∘ filter isDir ∘ map fst $ usvn
	return (repo rsvn, repo $ usvn ++ udirs)
	where
		getStat (Create _) f = second (Create ∘ Just) <$> stat f
		getStat (Update _ _) f = second (Update Nothing ∘ Just) <$> stat f
		getStat (Delete _) f = return (Entity False f, Delete Nothing)
		untrackedDir d = do
			cts ← invoke $ "find '" ++ d ++ "' -mindepth 1"
			r ← repo <$> fmap catMaybes (mapM (\f → fmap Just (stat f) `catchError` const (return Nothing)) cts)
			return $ toList ∘ mapKeys (over entityPath normalise) $ r

-- | Mark svn file according to action performed
markSvn ∷ Entity → Action a → IO ()
markSvn _ (Update _ _) = return ()
markSvn (Entity False fpath) (Delete _) = void $ readProcess "svn" ["rm", "--keep-local", fpath] ""
markSvn (Entity True fpath) (Delete _) = void $ readProcess "svn" ["rm", "--keep-local", fpath] ""
markSvn (Entity False fpath) _ = void $ readProcess "svn" ["add", fpath] ""
markSvn (Entity True fpath) _ = void $ readProcess "svn" ["add", "-N", fpath] ""

-- | Mark remote svn file according to action performed
remoteMarkSvn ∷ Entity → Action a → ProcessM ()
remoteMarkSvn _ (Update _ _) = return ()
remoteMarkSvn (Entity False fpath) (Delete _) = invoke_ $ "svn rm --keep-local " ++ quote fpath
remoteMarkSvn (Entity True fpath) (Delete _) = invoke_ $ "svn rm --keep-local " ++ quote fpath
remoteMarkSvn (Entity False fpath) _ = invoke_ $ "svn add " ++ quote fpath
remoteMarkSvn (Entity True fpath) _ = invoke_ $ "svn add -N " ++ quote fpath

data SvnStatus = Added | Conflicted | Deleted | Ignored | Modified | Replaced | Untracked | Missing deriving (Eq, Ord, Enum, Bounded)

svnStates ∷ [(SvnStatus, Char)]
svnStates = [
	(Added, 'A'),
	(Conflicted, 'C'),
	(Deleted, 'D'),
	(Ignored, 'I'),
	(Modified, 'M'),
	(Replaced, 'R'),
	(Untracked, '?'),
	(Missing, '!')]

instance Show SvnStatus where
	show = maybe undefined return ∘ flip lookup svnStates

instance Read SvnStatus where
	readsPrec _ "" = []
	readsPrec _ (s:ss) = maybe [] (\st → [(st, ss)]) $ lookup s (map swap svnStates)

parseSvnStatus ∷ [String] → ([(Action (), FilePath)], [FilePath])
parseSvnStatus = partitionEithers ∘ mapMaybe parse' where
	parse' str = fmap ((`toStatus` file) ∘ simplifyStatus) ∘ readMaybe $ [s] where
		(s:_, rest) = splitAt 8 str
		file = joinPath $ F.splitDirectories rest

	-- Leaves only `Added`, `Modified` and`Deleted`
	simplifyStatus Conflicted = Modified
	simplifyStatus Ignored = Added
	simplifyStatus Replaced = Modified
	simplifyStatus Missing = Deleted
	simplifyStatus s = s

	toStatus Added f = Left (Create (), f)
	toStatus Modified f = Left (Update () (), f)
	toStatus Deleted f = Left (Delete (), f)
	toStatus Untracked f = Right f
	toStatus status f = error $ "Don't know how to convert this svn status to actions, status: " ++ show status ++ ", file: " ++ f
