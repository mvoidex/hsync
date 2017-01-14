module Sync.Repo (
	Location(..), location,
	Entity(..), entity, entityPath, isFile, isDir,
	split,
	joins,
	atRoot,
	withDir,
	getMTime,
	match
	) where

import Prelude.Unicode

import Control.Exception (bracket)
import Data.List
import Data.Time.Clock
import System.Directory
import System.FilePath
import Text.Regex.PCRE ((=~))

data Location = Local FilePath | Remote String FilePath deriving (Eq, Ord)

instance Show Location where
	show (Local fpath) = fpath
	show (Remote host fpath) = host ++ ":" ++ fpath

instance Read Location where
	readsPrec _ str = case rest of
		':':path → [(Remote host path, "")]
		_ → [(Local str, "")]
		where
			(host, rest) = break (≡ ':') str

location ∷ (FilePath → a) → (String → FilePath → a) → Location → a
location local' _ (Local fpath) = local' fpath
location _ remote' (Remote host fpath) = remote' host fpath

data Entity = File [FilePath] | Dir [FilePath] deriving (Eq, Ord, Read)

instance Show Entity where
	show (File fpath) = intercalate "/" fpath
	show (Dir fpath) = intercalate "/" fpath ++ "/"

entity ∷ Bool → FilePath → Entity
entity False = File ∘ split
entity True = Dir ∘ split

entityPath ∷ Entity → [FilePath]
entityPath (File f) = f
entityPath (Dir d) = d

isFile ∷ Entity → Bool
isFile (File _) = True
isFile _ = False

isDir ∷ Entity → Bool
isDir (Dir _) = True
isDir _ = False

split ∷ FilePath → [FilePath]
split = splitDirectories ∘ normalise

joins ∷ [FilePath] → FilePath
joins = intercalate "/"

atRoot ∷ FilePath → [FilePath] → FilePath
atRoot root path = intercalate "/" $ split root ++ path

withDir ∷ FilePath → IO a → IO a
withDir fpath act = bracket getCurrentDirectory setCurrentDirectory $ \_ → setCurrentDirectory fpath >> act

getMTime ∷ FilePath → IO UTCTime
getMTime f = do
	tm ← getModificationTime f
	return $ tm { utctDayTime = secondsToDiffTime (diffTimeToPicoseconds (utctDayTime tm) `div` (10 ^ (12 ∷ Integer))) }

match ∷ String → Entity → Bool
match rx = any (=~ rx) ∘ entityPath
