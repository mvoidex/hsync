module Sync.Exec (
	exec, order,

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

exec ∷ (String → IO ()) → Patch Entity a → Location → Location → IO ()
exec writeLine p (Local l) (Local r) = forM_ (order p) (safe writeLine exec') where
	exec' (Entity False fpath) (Delete _) = removeFile $ r </> fpath
	exec' (Entity True fpath) (Delete _) = removeDirectory $ r </> fpath
	exec' (Entity False fpath) _ = do
		createDirectoryIfMissing True (takeDirectory (r </> fpath))
		copyFileWithMetadata (l </> fpath) (r </> fpath)
	exec' (Entity True fpath) _ = createDirectoryIfMissing True (r </> fpath)
exec writeLine p (Local l) (Remote host r) = sftp host r $ forM_ (order p) (safe writeLine exec') where
	exec' (Entity False fpath) (Delete _) = rm fpath
	exec' (Entity True fpath) (Delete _) = rmdir fpath
	exec' (Entity False fpath) _ = mkdirs (takeDirectory fpath) >> put False (l </> fpath) fpath
	exec' (Entity True fpath) _ = mkdir fpath
exec writeLine p (Remote host l) (Local r) = sftp host l $ forM_ (order p) (safe writeLine exec') where
	exec' (Entity False fpath) (Delete _) = liftIO $ removeFile $ r </> fpath
	exec' (Entity True fpath) (Delete _) = liftIO $ removeDirectory $ r </> fpath
	exec' (Entity False fpath) _ = get False fpath (r </> fpath)
	exec' (Entity True fpath) _ = liftIO $ createDirectory (r </> fpath)
exec writeLine p (Remote lhost l) (Remote rhost r) = ssh lhost $ send sftpToRight >> forM_ (order p) (safe writeLine exec') where
	exec' (Entity False fpath) (Delete _) = rm fpath
	exec' (Entity True fpath) (Delete _) = rmdir fpath
	exec' (Entity False fpath) _ = mkdirs (takeDirectory fpath) >> put False (l </> fpath) fpath
	exec' (Entity True fpath) _ = mkdir fpath
	sftpToRight = "sftp " ++ rhost ++ ":" ++ quote r

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
safe writeLine fn (RepoItem e tm) = handle (liftIO ∘ onError) (fn e tm >> liftIO onOk) where
	onError ∷ SomeException → IO ()
	onError err = writeLine $ show e ++ ": " ++ show err
	onOk = writeLine $ show $ RepoItem e tm
