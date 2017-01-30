module Sync.Exec (
	exec,

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
import System.FilePath.Posix
import System.Directory

import Sync.Base
import Sync.Repo
import Sync.Ssh

exec ∷ (String → IO ()) → Patch Entity a → Location → Location → IO ()
exec writeLine p (Local l) (Local r) = forM_ (changes' p) (safe writeLine exec') where
	exec' (Entity False fpath) (Delete _) = removeFile $ r </> fpath
	exec' (Entity True fpath) (Delete _) = removeDirectory $ r </> fpath
	exec' (Entity False fpath) _ = copyFileWithMetadata (l </> fpath) (r </> fpath)
	exec' (Entity True fpath) _ = createDirectoryIfMissing True (r </> fpath)
exec writeLine p (Local l) (Remote host r) = sftp host r $ forM_ (changes' p) (safe writeLine exec') where
	exec' (Entity False fpath) (Delete _) = rm fpath
	exec' (Entity True fpath) (Delete _) = rmdir fpath
	exec' (Entity False fpath) _ = put False (l </> fpath) fpath
	exec' (Entity True fpath) _ = mkdir fpath
exec writeLine p (Remote host l) (Local r) = sftp host l $ forM_ (changes' p) (safe writeLine exec') where
	exec' (Entity False fpath) (Delete _) = liftIO $ removeFile $ r </> fpath
	exec' (Entity True fpath) (Delete _) = liftIO $ removeDirectory $ r </> fpath
	exec' (Entity False fpath) _ = get False fpath (r </> fpath)
	exec' (Entity True fpath) _ = liftIO $ createDirectory (r </> fpath)
exec writeLine p (Remote lhost l) (Remote rhost r) = ssh lhost $ send sftpToRight >> forM_ (changes' p) (safe writeLine exec') where
	exec' (Entity False fpath) (Delete _) = rm fpath
	exec' (Entity True fpath) (Delete _) = rmdir fpath
	exec' (Entity False fpath) _ = put False (l </> fpath) fpath
	exec' (Entity True fpath) _ = mkdir fpath
	sftpToRight = "sftp " ++ rhost ++ ":" ++ quote r

safe ∷ (MonadCatch m, MonadIO m) ⇒ (String → IO ()) → (Entity → Action a → m ()) → (Entity, Action a) → m ()
safe writeLine fn (e, tm) = handle (liftIO ∘ onError) (fn e tm >> liftIO onOk) where
	onError ∷ SomeException → IO ()
	onError err = writeLine $ show e ++ ": " ++ show err
	onOk = writeLine $ show $ RepoItem e tm

changes' ∷ Patch Entity a → [(Entity, Action a)]
changes' = sortBy cmp ∘ toList where
	cmp ∷ (Entity, Action a) → (Entity, Action a) → Ordering
	cmp (l, dl) (r, dr) = compare (entityType dl l, view entityPath l) (entityType dr r, view entityPath r)
	entityType (Delete _) = isDir
	entityType _ = isFile
