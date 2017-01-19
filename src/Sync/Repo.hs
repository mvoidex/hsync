{-# LANGUAGE TemplateHaskell #-}

module Sync.Repo (
	Location(..), location,
	Entity(..), entityIsDir, entityPath, isFile, isDir,
	withDir,
	getMTime,
	match
	) where

import Prelude.Unicode

import Control.Lens
import Control.Exception (bracket)
import Data.Time.Clock
import System.Directory
import qualified System.FilePath as F
import System.FilePath.Posix
import Text.Regex.PCRE ((=~))

data Location = Local FilePath | Remote String FilePath deriving (Eq, Ord)

instance Show Location where
	show (Local fpath) = fpath
	show (Remote host fpath) = host ++ ":" ++ fpath

instance Read Location where
	readsPrec _ str
		| F.isAbsolute str = [(Local $ map replacePathSep str, "")]
		| otherwise = case rest of
			':':path → [(Remote host path, "")]
			_ → [(Local str, "")]
		where
			(host, rest) = break (≡ ':') str
			replacePathSep ch
				| ch ∈ F.pathSeparators = pathSeparator
				| otherwise = ch


location ∷ (FilePath → a) → (String → FilePath → a) → Location → a
location local' _ (Local fpath) = local' fpath
location _ remote' (Remote host fpath) = remote' host fpath

data Entity = Entity {
	_entityIsDir ∷ Bool,
	_entityPath ∷ FilePath }
		deriving (Eq, Ord, Read)

makeLenses ''Entity

instance Show Entity where
	show (Entity dir' fpath)
		| dir' = addTrailingPathSeparator fpath
		| otherwise = fpath

isFile ∷ Entity → Bool
isFile = not ∘ isDir

isDir ∷ Entity → Bool
isDir = view entityIsDir

withDir ∷ FilePath → IO a → IO a
withDir fpath act = bracket getCurrentDirectory setCurrentDirectory $ \_ → setCurrentDirectory fpath >> act

getMTime ∷ FilePath → IO UTCTime
getMTime f = do
	tm ← getModificationTime f
	return $ tm { utctDayTime = secondsToDiffTime (diffTimeToPicoseconds (utctDayTime tm) `div` (10 ^ (12 ∷ Integer))) }

match ∷ String → Entity → Bool
match rx = any (=~ rx) ∘ splitDirectories ∘ normalise ∘ view entityPath
