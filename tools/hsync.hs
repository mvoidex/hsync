module Main (
	main
	) where

import Prelude.Unicode

import Control.Applicative
import Data.Monoid
import Options.Applicative
import System.Console.ANSI
import System.IO
import Text.Format

import Sync.Base.Internal (mapWithKey)
import Sync.Exec
import Sync.Dir
import Sync.Git

data RepoType = Folder | Git Bool

data Options = Options {
	repoType ∷ RepoType,
	repoSource ∷ Location,
	repoDestination ∷ Location,
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
		<$> typeFlags
		<*> argument auto (metavar "src" <> help "source")
		<*> argument auto (metavar "dst" <> help "destination")
		<*> switch (long "noaction" <> short 'n' <> help "don't perform any actions")
		<*> switch (long "combine" <> short 'c' <> help "combine mode, can produce conflicts")
		<*> switch (long "mirror" <> short 'm' <> help "mirror mode: replace newer files")
		<*> switch (long "newest" <> help "resolving: prefer newest")
		<*> preferModeOpt
		<*> switch (long "ignore" <> help "resolving: ignore conflict, don't do anything for them")
		<*> switch (long "diff" <> short 'd' <> help "show diff, don't performs any actions")
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

main ∷ IO ()
main = do
	hSetEncoding stdin utf8
	hSetEncoding stdout utf8
	execParser (info (helper <*> options) (fullDesc <> header "hsync — synchronize folders")) >>= main'
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
