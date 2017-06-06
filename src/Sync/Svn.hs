{-# LANGUAGE FlexibleContexts #-}

module Sync.Svn (
	enumSvn, svn, remoteSvn,

	module Sync.Base,
	module Sync.Repo
	) where

import Prelude.Unicode

import Control.Arrow
import Control.Lens
import Control.Monad.Except
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

enumSvn ∷ Location → Bool → IO (Patch Entity (Maybe UTCTime))
enumSvn = location svn remoteSvn

svn ∷ FilePath → Bool → IO (Patch Entity (Maybe UTCTime))
svn fpath untracked = withDir fpath $ do
	status ← lines <$> readProcess "svn" ("status" : if untracked then [] else ["-q"]) ""
	rsvn ← traverse (uncurry getStat) (parseSvnStatus status)
	udirs ← fmap concat ∘ mapM (untrackedDir ∘ view entityPath) ∘ filter isDir ∘ map fst $ rsvn
	return $ repo $ rsvn ++ udirs
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

remoteSvn ∷ String → FilePath → Bool → IO (Patch Entity (Maybe UTCTime))
remoteSvn host fpath untracked = ssh host $ do
	cd fpath
	out ← invoke $ unwords $ "svn" : ("status" : if untracked then [] else ["-q"])
	rsvn ← fmap catMaybes (traverse ((`catchError` const (return Nothing)) ∘ fmap Just ∘ uncurry getStat) (parseSvnStatus out))
	udirs ← fmap concat ∘ mapM (untrackedDir ∘ view entityPath) ∘ filter isDir ∘ map fst $ rsvn
	return $ repo $ rsvn ++ udirs
	where
		getStat (Create _) f = second (Create ∘ Just) <$> stat f
		getStat (Update _ _) f = second (Update Nothing ∘ Just) <$> stat f
		getStat (Delete _) f = return (Entity False f, Delete Nothing)
		untrackedDir d = do
			cts ← invoke $ "find '" ++ d ++ "' -mindepth 1"
			r ← repo <$> fmap catMaybes (mapM (\f → fmap Just (stat f) `catchError` const (return Nothing)) cts)
			return $ toList ∘ fmap (Create ∘ Just) ∘ mapKeys (over entityPath normalise) $ r

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

parseSvnStatus ∷ [String] → [(Action (), FilePath)]
parseSvnStatus = mapMaybe parse' where
	parse' str = fmap ((`toStatus` file) ∘ simplifyStatus) ∘ readMaybe $ [s] where
		(s:_, rest) = splitAt 8 str
		file = joinPath $ F.splitDirectories rest

	-- Leaves only `Added`, `Modified` and`Deleted`
	simplifyStatus Conflicted = Modified
	simplifyStatus Ignored = Added
	simplifyStatus Replaced = Modified
	simplifyStatus Untracked = Added
	simplifyStatus Missing = Deleted
	simplifyStatus s = s

	toStatus Added f = (Create (), f)
	toStatus Modified f = (Update () (), f)
	toStatus Deleted f = (Delete (), f)
	toStatus status f = error $ "Don't know how to convert this svn status to actions, status: " ++ show status ++ ", file: " ++ f
