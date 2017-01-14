module Sync.Exec (
	exec,

	module Sync.Base,
	module Sync.Repo
	) where

import Prelude.Unicode

import Control.Exception (SomeException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch
import Data.List
import Data.Time.Clock
import System.Directory

import Sync.Base
import Sync.Repo
import Sync.Ssh

exec ∷ (String → IO ()) → Patch Entity UTCTime → Location → Location → IO ()
exec writeLine p (Local l) (Local r) = forM_ (changes' p) (safe writeLine exec') where
	exec' (File fpath) Delete = removeFile $ atRoot r fpath
	exec' (Dir fpath) Delete = removeDirectory $ atRoot r fpath
	exec' (File fpath) (Update _) = copyFileWithMetadata (atRoot l fpath) (atRoot r fpath)
	exec' (Dir fpath) (Update _) = createDirectory (atRoot r fpath)
exec writeLine p (Local l) (Remote host r) = sftp host r $ forM_ (changes' p) (safe writeLine exec') where
	exec' (File fpath) Delete = rm (joins fpath)
	exec' (Dir fpath) Delete = rmdir (joins fpath)
	exec' (File fpath) (Update _) = put False (atRoot l fpath) (joins fpath)
	exec' (Dir fpath) (Update _) = mkdir (joins fpath)
exec writeLine p (Remote host l) (Local r) = sftp host l $ forM_ (changes' p) (safe writeLine exec') where
	exec' (File fpath) Delete = liftIO $ removeFile $ atRoot r fpath
	exec' (Dir fpath) Delete = liftIO $ removeDirectory $ atRoot r fpath
	exec' (File fpath) (Update _) = get False (joins fpath) (atRoot r fpath)
	exec' (Dir fpath) (Update _) = liftIO $ createDirectory (atRoot r fpath)
exec _ _ l r = error $ "Can't sync between two remotes: " ++ show l ++ "," ++ show r

safe ∷ (MonadCatch m, MonadIO m) ⇒ (String → IO ()) → (Entity → Modify UTCTime → m ()) → (Entity, Modify UTCTime) → m ()
safe writeLine fn (e, tm) = handle (liftIO ∘ onError) (fn e tm >> liftIO onOk) where
	onError ∷ SomeException → IO ()
	onError err = writeLine $ "✗ " ++ show e ++ ": " ++ show err
	onOk = writeLine $ "✓ " ++ show e

changes' ∷ Patch Entity UTCTime → [(Entity, Modify UTCTime)]
changes' = sortBy cmp ∘ toList where
	cmp ∷ (Entity, Modify UTCTime) → (Entity, Modify UTCTime) → Ordering
	cmp (l, dl) (r, dr) = compare (entityType dl l, entityPath l) (entityType dr r, entityPath r)
	entityType Delete = isDir
	entityType (Update _) = isFile
