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

data Options = Options {
	repoSource ∷ Location,
	repoDestination ∷ Location,
	repoType ∷ RepoType,
	optionNoAction ∷ Bool,
	combineMode ∷ Bool,
	mirrorMode ∷ Bool,
	newestMode ∷ Bool,
	preferMode ∷ Maybe Bool,
	ignoreMode ∷ Bool,
	showDiff ∷ Bool,
	excludePats ∷ [String],
	verboseOutput ∷ Bool }

options ∷ Parser Options
options =
	Options
		<$> argument auto (metavar "src" <> help "source, either local path or remote [host]:[path]")
		<*> argument auto (metavar "dst" <> help "destination, either local path either remote [host]:[path]")
		<*> typeFlags
		<*> switch (long "noaction" <> short 'n' <> help "don't perform any actions, just show what to be done")
		<*> switch (long "combine" <> short 'c' <> help "combine mode, can produce conflicts")
		<*> switch (long "mirror" <> short 'm' <> help "mirror mode: `dst` will become in same state as `src`, i.e. new files will be deleted, unexistant will be created etc.")
		<*> switch (long "newest" <> help "resolving: prefer newest")
		<*> preferModeOpt
		<*> switch (long "ignore" <> help "resolving: ignore conflict, don't do anything for them")
		<*> switch (long "diff" <> short 'd' <> help "show diff, doesn't perform any actions")
		<*> many (strOption (long "exclude" <> short 'e' <> help "exclude directories and files"))
		<*> switch (long "verbose" <> short 'v' <> help "verbose output")
	where
		typeFlags = mkType <$>
			switch (long "git" <> help "ask git for modifications") <*>
			switch (long "untracked" <> short 'u' <> help "show untracked files, git-only")
			where
				mkType False = const Folder
				mkType True = Git
		preferModeOpt = optional (option parse' (long "prefer" <> help "resolving: prefer 'left' or 'right'")) where
			parse' = eitherReader $ \s → case s of
				"left" → Right True
				"right" → Right False
				_ → Left $ "invalid prefer value: '" ++ s ++ "', can be 'left' or 'right'"

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
	execParser (info (helper <*> options) (briefDesc <> header "hsync — synchronize folders" <> footerDoc description)) >>= main'
	where
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
				patch'
					| mirrorMode opts = revert dst `chain` src
					| combineMode opts = maybeIgnore ∘ maybePrefer ∘ maybeNewest $ rebase src dst
					| otherwise = error "Select mode: mirror or combine" -- TODO: Select default
				maybeNewest
					| newestMode opts = tryResolve newest
					| otherwise = id
				maybePrefer = case preferMode opts of
					Nothing → id
					Just pleft → fmap Merged ∘ resolve (if pleft then preferLeft else preferRight)
				maybeIgnore
					| ignoreMode opts = fmap Merged ∘ resolved
					| otherwise = id
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
				enumRepo r = exclude' <$> case repoType opts of
					Folder → (fmap Create ∘ mapWithKey dropDirTime) <$> enumDir r
					Git untracked → enumGit r untracked
				exclude' = exclude (\e → or [match pat e | pat ← excludePats opts])
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
