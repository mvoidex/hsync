module Sync.Dir (
	enumDir, dir, remoteDir,

	module Sync.Base,
	module Sync.Repo
	) where

import Prelude.Unicode

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.Time.Clock
import System.Directory
import System.FilePath.Posix

import Sync.Base
import Sync.Repo
import Sync.Ssh

enumDir ∷ Location → IO (Repo Entity UTCTime)
enumDir = location dir remoteDir

dir ∷ FilePath → IO (Repo Entity UTCTime)
dir fpath = withDir fpath $ do
	cts ← getDir "."
	repo <$> mapM getStat cts
	where
		getDir f = getDir' f <|> return []
		getDir' f = do
			cts ← ((map (f </>) ∘ filter (∉ [".", ".."])) <$> getDirectoryContents f) >>= filterM (fmap not ∘ pathIsSymbolicLink)
			scts ← mapM getDir cts
			return $ concat $ cts : scts
		getStat f = do
			tm ← getMTime f
			efile ← doesFileExist f
			edir ← doesDirectoryExist f
			when (not efile ∧ not edir) $ fail $ "file or directory not exist: " ++ f
			return (Entity edir f, tm)

remoteDir ∷ String → FilePath → IO (Repo Entity UTCTime)
remoteDir host fpath = ssh host $ do
	cd fpath
	cts ← invoke "find . -mindepth 1 -type f -or -type d"
	repo <$> fmap catMaybes (mapM (\f → fmap Just (stat f) `catchError` const (return Nothing)) cts)
