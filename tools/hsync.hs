module Main (
	main
	) where

import Prelude.Unicode

import Control.Applicative
import Data.List (intersperse)
import Data.Monoid
import Options.Applicative
import Options.Applicative.Help.Pretty as P
import System.Console.ANSI
import System.IO
import Text.Format

import Sync.Base.Internal (mapWithKey)
import Sync.Exec
import Sync.Dir
import Sync.Git

data RepoType = Folder | Git Bool

data SyncMode = Default | Mirror | New | Overwrite | Skip deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Options = Options {
	repoSource ∷ Location,
	repoDestination ∷ Location,
	repoType ∷ RepoType,
	optionNoAction ∷ Bool,
	syncMode ∷ SyncMode,
	showDiff ∷ Bool,
	excludePats ∷ [String],
	includePats ∷ [String],
	verboseOutput ∷ Bool }

mode ∷ Parser SyncMode
mode = foldr (<|>) (pure Default) [
	pure Mirror <* switch (long "mirror" <> short 'm' <> help "mirror mode: `dst` will become in same state as `src`, i.e. new files will be deleted, unexistant will be created etc."),
	pure New <* switch (long "new" <> help "new mode: select newest file when syncing"),
	pure Overwrite <* switch (long "overwrite" <> short 'o' <> help "overwrite mode: overwrite files (even older one) on conflict"),
	pure Skip <* switch (long "skip" <> short 's' <> help "skip mode: don't perform actions for conflicts")]

options ∷ Parser Options
options =
	Options
		<$> argument auto (metavar "src" <> help "source, either local path or remote [host]:[path]")
		<*> argument auto (metavar "dst" <> help "destination, either local path either remote [host]:[path]")
		<*> typeFlags
		<*> switch (long "noaction" <> short 'n' <> help "don't perform any actions, just show what to be done")
		<*> mode
		<*> switch (long "diff" <> short 'd' <> help "show diff, doesn't perform any actions")
		<*> many (strOption (long "exclude" <> short 'e' <> help "exclude directories and files by regex"))
		<*> many (strOption (long "include" <> short 'i' <> help "include directories and files by regex"))
		<*> switch (long "verbose" <> short 'v' <> help "verbose output")
	where
		typeFlags = mkType <$>
			switch (long "git" <> help "ask git for modifications") <*>
			switch (long "untracked" <> short 'u' <> help "show untracked files, git-only")
			where
				mkType False = const Folder
				mkType True = Git

description ∷ Maybe Doc
description = Just $ vsep [
	par "synchronize destination folder state with source one",
	par "it can ask git for modifications with no need to fully traverse directories",
	P.empty,
	par "there are two main modes of syncing:",
	indent 4 $ vsep [
		text "mirror -" </> align (par "destination folder will become in same state as source" </> parens (par "created files will be deleted, modifications will be reverted etc.")),
		text "combine -" </> align (par "try to merge folder states, this can produce conflicts, which have to be resolved:"),
		indent 4 $ vsep [
			text "newest -" </> align (par "prefer file with latest modification time"),
			text "ignore -" </> align (par "don't do anything for conflicted files"),
			text "prefer left|right -" </> align (par "prefer source of destination file")
		]
	],
	P.empty,
	par "examples:",
	P.empty,
	indent 4 $ vsep $ intersperse P.empty [
		nest 4 $ vsep [
			text "hsync src dst --mirror",
			par "mirror-copy src to dst"
		],
		nest 4 $ vsep [
			text "hsync src dst --combine --newest",
			par "copy src to dst, overwrites older files, but doesn't touch newest ones; doesn't delete anything"
		],
		nest 4 $ vsep [
			text "hsync src dst --git --mirror",
			par "mirror-copy src to dst, also restores deleted files, reverts renaming etc., but NOTE, hsync doesn't interact with git"
				</> parens (par "doesn't invoke git's add, rm, mv commands")
				</> par "it only copies/deletes files"
		]
	],
	P.empty,
	underline $ text link]
	where
		par = foldr1 (</>) ∘ map text ∘ words

link ∷ String
link = "https://github.com/mvoidex/hsync"

main ∷ IO ()
main = do
	hSetEncoding stdin utf8
	hSetEncoding stdout utf8
	popts ← execParser (info (helper <*> options) (briefDesc <> header "hsync — synchronize folders" <> footerDoc description))
	validateOpts popts main'
	where
		validateOpts popts act
			| not (null (includePats popts)) ∧ not (null (excludePats popts)) = putStrLn "only 'include' or 'exclude' filters can be specified, not both"
			| otherwise = act popts
		main' opts = do
			verbose opts $ format "source: {0}" ~~ show (repoSource opts)
			verbose opts $ format "destination: {0}" ~~ show (repoDestination opts)
			verbose opts $ format "type: {0}" ~~ (case repoType opts of
				Folder → "folder"
				Git False → "git"
				Git True → "git=u")
			verbose opts $ format "getting {0}..." ~~ show (repoSource opts)
			src ← enumRepo (repoSource opts)
			verbose opts "✓"
			verbose opts $ format "getting {0}..." ~~ show (repoDestination opts)
			dst ← enumRepo (repoDestination opts)
			verbose opts "✓"
			let
				patch' = case syncMode opts of
					Default → combined
					Mirror → revert dst `chain` src
					New → tryResolve newest combined
					Overwrite → fmap Merged ∘ resolve preferLeft $ combined
					Skip → fmap Merged ∘ resolved $ combined
					where
						combined = rebase src dst
				resolved' = resolved patch'
				unresolved' = unresolved patch'

				applyPatch
					| showDiff opts = print $ diff src dst
					| not (nullRepo unresolved') = do
						putStrLn "Conflicts:"
						print unresolved'
					| optionNoAction opts = mapM_ write $ lines $ show resolved'
					| otherwise = exec write resolved' (repoSource opts) (repoDestination opts)
			applyPatch
			where
				enumRepo r = (include' ∘ exclude') <$> case repoType opts of
					Folder → (fmap Create ∘ mapWithKey dropDirTime) <$> enumDir r
					Git untracked → enumGit r untracked
				exclude' = exclude (\e → or [match pat e | pat ← excludePats opts])
				include'
					| null (includePats opts) = id
					| otherwise = include (\e → or [match pat e | pat ← includePats opts])
				dropDirTime e
					| isDir e = const Nothing
					| otherwise = Just

verbose ∷ Options → String → IO ()
verbose opts
	| verboseOutput opts = write
	| otherwise = const $ return ()

write ∷ String → IO ()
write s@('✓':_) = color Green $ putStrLn s
write s@('+':_) = color Green $ putStrLn s
write s@('✗':_) = color Red $ putStrLn s
write s@('-':_) = color Red $ putStrLn s
write s = putStrLn s

color ∷ Color → IO () → IO ()
color clr act = setSGR [SetColor Foreground Vivid clr] >> act >> setSGR []
