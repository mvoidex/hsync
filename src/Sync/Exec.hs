module Sync.Exec (
	sync, Marker(..), mark, order,

	module Sync.Base,
	module Sync.Repo
	) where

import Prelude.Unicode

import Control.Exception (SomeException)
import Control.Lens
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch
import Data.List
import Data.Ord (compare)
import System.FilePath.Posix
import System.Directory

import Sync.Base
import Sync.Repo
import Sync.Ssh

-- | Sync changes
sync ∷ (String → IO ()) → Patch Entity a → Location → Location → IO ()
sync writeLine p (Local l) (Local r) = forM_ (order p) (safe writeLine sync') where
	sync' (Entity False fpath) (Delete _) = removeFile $ r </> fpath
	sync' (Entity True fpath) (Delete _) = removeDirectory $ r </> fpath
	sync' (Entity False fpath) _ = do
		createDirectoryIfMissing True (takeDirectory (r </> fpath))
		copyFileWithMetadata (l </> fpath) (r </> fpath)
	sync' (Entity True fpath) _ = createDirectoryIfMissing True (r </> fpath)
sync writeLine p (Local l) (Remote host r) = sftp host r $ forM_ (order p) (safe writeLine sync') where
	sync' (Entity False fpath) (Delete _) = rm fpath
	sync' (Entity True fpath) (Delete _) = rmdir fpath
	sync' (Entity False fpath) _ = mkdirs (takeDirectory fpath) >> put False (l </> fpath) fpath
	sync' (Entity True fpath) _ = mkdir fpath
sync writeLine p (Remote host l) (Local r) = sftp host l $ forM_ (order p) (safe writeLine sync') where
	sync' (Entity False fpath) (Delete _) = liftIO $ removeFile $ r </> fpath
	sync' (Entity True fpath) (Delete _) = liftIO $ removeDirectory $ r </> fpath
	sync' (Entity False fpath) _ = get False fpath (r </> fpath)
	sync' (Entity True fpath) _ = liftIO $ createDirectory (r </> fpath)
sync writeLine p (Remote lhost l) (Remote rhost r) = ssh lhost $ send sftpToRight >> forM_ (order p) (safe writeLine sync') where
	sync' (Entity False fpath) (Delete _) = rm fpath
	sync' (Entity True fpath) (Delete _) = rmdir fpath
	sync' (Entity False fpath) _ = mkdirs (takeDirectory fpath) >> put False (l </> fpath) fpath
	sync' (Entity True fpath) _ = mkdir fpath
	sftpToRight = "sftp " ++ rhost ++ ":" ++ quote r

data Marker a = Marker {
	localMark ∷ Entity → Action a → IO (),
	remoteMark ∷ Entity → Action a → ProcessM () }

-- | Mark changes in version control system (git/svn)
mark ∷ Marker a → Patch Entity a → Location → IO ()
mark m p (Local path) = withDir path $ forM_ (order p) mark' where
	mark' (RepoItem e act) = localMark m e act
mark m p (Remote host path) = ssh host $ cd path >> forM_ (order p) mark' where
	mark' (RepoItem e act) = remoteMark m e act

-- | Order actions so that deletions goes first (from deepest to root), and additions goes after (from root to deepest)
order ∷ Patch Entity a → [RepoItem Entity (Action a)]
order = sortBy cmp ∘ items where
	cmp ∷ RepoItem Entity (Action a) → RepoItem Entity (Action a) → Ordering
	cmp (RepoItem l (Delete _)) (RepoItem r (Delete _)) = cmpPath False (view entityPath l) (view entityPath r)
	cmp (RepoItem _ (Delete _)) _ = LT
	cmp _ (RepoItem _ (Delete _)) = GT
	cmp (RepoItem l _) (RepoItem r _) = cmpPath True (view entityPath l) (view entityPath r)
	-- tricky: we want sort deletions and additions alphabetically
	-- the only difference is that deletions should goes from deepest
	-- so when one path is prefix of another, we consider child to be less in case of deletions
	cmpPath ∷ Bool → FilePath → FilePath → Ordering
	cmpPath naturalOrder lpath rpath = cmpPath' (splitDirectories lpath) (splitDirectories rpath) where
		cmpPath' ∷ [FilePath] → [FilePath] → Ordering
		cmpPath' (l:ls) (r:rs) = case compare l r of
			EQ → cmpPath' ls rs
			res → res
		cmpPath' [] [] = EQ
		cmpPath' _ [] = if naturalOrder then GT else LT
		cmpPath' [] _ = if naturalOrder then LT else GT

safe ∷ (MonadCatch m, MonadIO m) ⇒ (String → IO ()) → (Entity → Action a → m ()) → RepoItem Entity (Action a) → m ()
safe writeLine fn (RepoItem e tm) = handle (liftIO ∘ onErr) (fn e tm >> liftIO onOk) where
	onErr ∷ SomeException → IO ()
	onErr err = writeLine $ show e ++ ": " ++ show err
	onOk = writeLine $ show $ RepoItem e tm
