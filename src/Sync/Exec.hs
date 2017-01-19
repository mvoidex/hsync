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
import Data.Time.Clock
import System.FilePath.Posix
import System.Directory

import Sync.Base
import Sync.Repo
import Sync.Ssh

exec ∷ (String → IO ()) → Patch Entity UTCTime → Location → Location → IO ()
exec writeLine p (Local l) (Local r) = forM_ (changes' p) (safe writeLine exec') where
	exec' (Entity False fpath) Delete = removeFile $ r </> fpath
	exec' (Entity True fpath) Delete = removeDirectory $ r </> fpath
	exec' (Entity False fpath) (Update _) = copyFileWithMetadata (l </> fpath) (r </> fpath)
	exec' (Entity True fpath) (Update _) = createDirectoryIfMissing True (r </> fpath)
exec writeLine p (Local l) (Remote host r) = sftp host r $ forM_ (changes' p) (safe writeLine exec') where
	exec' (Entity False fpath) Delete = rm fpath
	exec' (Entity True fpath) Delete = rmdir fpath
	exec' (Entity False fpath) (Update _) = put False (l </> fpath) fpath
	exec' (Entity True fpath) (Update _) = mkdir fpath
exec writeLine p (Remote host l) (Local r) = sftp host l $ forM_ (changes' p) (safe writeLine exec') where
	exec' (Entity False fpath) Delete = liftIO $ removeFile $ r </> fpath
	exec' (Entity True fpath) Delete = liftIO $ removeDirectory $ r </> fpath
	exec' (Entity False fpath) (Update _) = get False fpath (r </> fpath)
	exec' (Entity True fpath) (Update _) = liftIO $ createDirectory (r </> fpath)
exec _ _ l r = error $ "Can't sync between two remotes: " ++ show l ++ "," ++ show r

safe ∷ (MonadCatch m, MonadIO m) ⇒ (String → IO ()) → (Entity → Modify UTCTime → m ()) → (Entity, Modify UTCTime) → m ()
safe writeLine fn (e, tm) = handle (liftIO ∘ onError) (fn e tm >> liftIO onOk) where
	onError ∷ SomeException → IO ()
	onError err = writeLine $ "✗ " ++ show e ++ ": " ++ show err
	onOk = writeLine $ "✓ " ++ show e

changes' ∷ Patch Entity UTCTime → [(Entity, Modify UTCTime)]
changes' = sortBy cmp ∘ toList where
	cmp ∷ (Entity, Modify UTCTime) → (Entity, Modify UTCTime) → Ordering
	cmp (l, dl) (r, dr) = compare (entityType dl l, view entityPath l) (entityType dr r, view entityPath r)
	entityType Delete = isDir
	entityType (Update _) = isFile
