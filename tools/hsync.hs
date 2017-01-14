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

import Sync.Exec
import Sync.Dir
import Sync.Git

data RepoType = Folder | Git

data Options = Options {
	repoType ∷ RepoType,
	repoSource ∷ Location,
	repoDestination ∷ Location,
	optionNoAction ∷ Bool,
	combineMode ∷ Bool,
	mirrorMode ∷ Bool,
	excludePats ∷ [String],
	verboseOutput ∷ Bool }

options ∷ Parser Options
options = Options <$> typeFlag <*> srcOpt <*> dstOpt <*> noActionFlag <*> combineFlag <*> mirrorFlag <*> many excludeOpt <*> verboseFlag where
	srcOpt = argument auto (metavar "src" <> help "source")
	dstOpt = argument auto (metavar "dst" <> help "destination")
	typeFlag = flag Folder Git (long "git" <> help "ask git for modifications")
	noActionFlag = switch (long "noaction" <> short 'n' <> help "don't perform any actions")
	combineFlag = switch (long "combine" <> short 'c' <> help "combine mode")
	mirrorFlag = switch (long "mirror" <> short 'm' <> help "mirror mode: replace newer files")
	excludeOpt = strOption (long "exclude" <> short 'e' <> help "exclude directories and files")
	verboseFlag = switch (long "verbose" <> short 'v' <> help "verbose output")

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
				Git → "git")
			case repoType opts of
				Folder → do
					verbose opts $ format "getting {0}..." ~~ show (repoSource opts)
					src ← exclude' <$> enumDir (repoSource opts)
					verbose opts "✓"
					verbose opts $ format "getting {0}..." ~~ show (repoDestination opts)
					dst ← exclude' <$> enumDir (repoDestination opts)
					verbose opts "✓"
					let
						diff' = diff src dst
						patch'
							| combineMode opts = snd $ patch combine diff'
							| mirrorMode opts = snd $ patch mirror diff'
							| otherwise = snd $ patch newest diff'
					if optionNoAction opts
						then mapM_ write $ lines $ show patch'
						else exec write patch' (repoSource opts) (repoDestination opts)
				Git → do
					verbose opts $ format "getting {0}..." ~~ show (repoSource opts)
					src ← exclude' <$> enumGit (repoSource opts)
					verbose opts "✓"
					verbose opts $ format "getting {0}..." ~~ show (repoDestination opts)
					dst ← exclude' <$> enumGit (repoDestination opts)
					verbose opts "✓"
					let
						patch'
							| combineMode opts = snd $ patch combine $ diff (updates src) (updates dst)
							| mirrorMode opts = rebase mirror dst src
							| otherwise = rebase newest dst src
					if optionNoAction opts
						then mapM_ write $ lines $ show patch'
						else exec write patch' (repoSource opts) (repoDestination opts)
			where
				exclude' = exclude (\e → or [match pat e | pat ← excludePats opts])

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
