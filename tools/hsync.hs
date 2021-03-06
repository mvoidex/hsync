module Main (
	main
	) where

import Prelude.Unicode

import Control.Applicative
import Control.Arrow
import Control.Monad (when)
import Data.List (intersperse)
import Options.Applicative
import Options.Applicative.Help.Pretty as P
import System.Console.ANSI
import System.FilePath (FilePath)
import System.IO
import Text.Format

import qualified Sync.Base as R (empty)
import Sync.Base.Internal (mapWithKey)
import Sync.Arc
import Sync.Exec
import Sync.Dir
import Sync.Git
import Sync.Mode
import Sync.Svn

import Config

data RepoType = Folder | Git Bool | Svn Bool | Arc Bool deriving (Eq, Ord, Read, Show)

data Options = Options {
	repoSource ∷ Location,
	repoDestination ∷ Location,
	repoType ∷ RepoType,
	optionNoAction ∷ Bool,
	optionNoMark ∷ Bool,
	syncMode ∷ Mode,
	showDiff ∷ Bool,
	excludePats ∷ [String],
	includePats ∷ [String],
	configFile ∷ Maybe FilePath,
	verboseOutput ∷ Bool }

mode ∷ Parser Mode
mode = mirror' <|> new' <|> overwrite' <|> skip' <|> pure Default where
	mirror' = flag' Mirror (long "mirror" <> short 'm' <> help "mirror mode: `dst` will become in same state as `src`, i.e. new files will be deleted, unexistant will be created etc.")
	new' = flag' New (long "new" <> help "new mode: select newest file when syncing")
	overwrite' = flag' Overwrite (long "overwrite" <> short 'o' <> help "overwrite mode: overwrite files (even older one) on conflict")
	skip' = flag' Skip (long "skip" <> short 's' <> help "skip mode: don't perform actions for conflicts")

options ∷ Parser Options
options = setBack <$> options' <*> switch (long "back" <> short 'b' <> help "backward direction: from destination to source") where
	setBack opts True = opts { repoSource = repoDestination opts, repoDestination = repoSource opts }
	setBack opts _ = opts
	options' = Options
		<$> argument auto (metavar "src" <> help "source, either local path or remote [host]:[path]")
		<*> argument auto (metavar "dst" <> help "destination, either local path either remote [host]:[path]")
		<*> typeFlags
		<*> switch (long "noaction" <> short 'n' <> help "don't perform any actions, just show what to be done")
		<*> switch (long "nomark" <> help "don't mark synced files with version control system")
		<*> mode
		<*> switch (long "diff" <> short 'd' <> help "show diff, doesn't perform any actions")
		<*> many (strOption (long "exclude" <> short 'e' <> help "exclude directories and files by regex"))
		<*> many (strOption (long "include" <> short 'i' <> help "include directories and files by regex"))
		<*> optional (strOption (long "conf" <> help "path to config file, default is ~/.hsync"))
		<*> switch (long "verbose" <> short 'v' <> help "verbose output")
	typeFlags = arc' <|> git' <|> svn' <|> pure Folder where
		arc' = flag' Arc (long "arc" <> help "ask arc for modifications") <*> untrackedFlag
		git' = flag' Git (long "git" <> help "ask git for modifications") <*> untrackedFlag
		svn' = flag' Svn (long "svn" <> help "ask svn for modifications") <*> untrackedFlag
		untrackedFlag = switch (long "untracked" <> short 'u' <> help "show untracked files, git/svn mode")

description ∷ Maybe Doc
description = Just $ vsep [
	par "synchronize destination folder state with source one",
	par "it can ask git/svn for modifications with no need to fully traverse directories",
	P.empty,
	par "there are several modes of syncing:",
	indent 4 $ vsep [
		text "default -" </> align (par "try to merge folder states, this can produce conflicts and fail in this case"),
		text "mirror -" </> align (par "destination folder will become in same state as source" </> parens (par "created files will be deleted, modifications will be reverted etc.")),
		text "new -" </> align (par "don't delete anything, but overwrite older files at destination"),
		text "overwrite -" </> align (par "don't delete anything, overwrite files at destination"),
		text "skip -" </> align (par "don't delete anything, don't overwrite files at destination")
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
			text "hsync src dst --new",
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
				Arc False → "arc"
				Arc True → "arc=u"
				Git False → "git"
				Git True → "git=u"
				Svn False → "svn"
				Svn True → "svn=u")
			verbose opts $ format "getting {0} ..." ~~ show (repoSource opts)
			(src, usrc) ← enumRepo (repoSource opts)
			verbose opts $ format "tracked state:\n{0}" ~~ show src
			verbose opts $ format "untracked state:\n{0}" ~~ show usrc
			verbose opts "✓"
			verbose opts $ format "getting {0} ..." ~~ show (repoDestination opts)
			(dst, udst) ← enumRepo (repoDestination opts)
			verbose opts $ format "tracked state:\n{0}" ~~ show dst
			verbose opts $ format "untracked state:\n{0}" ~~ show udst
			verbose opts "✓"
			let
				patch' = syncPatch (syncMode opts) src dst
				upatch' = syncPatch (syncMode opts) usrc udst

				-- resolved parts
				resolved' = resolved patch'
				uresolved' = resolved upatch'

				-- resolved and merged into one part
				-- used to perform copy/delete actions
				merged' = resolve preferLeftModify $ merge resolved' uresolved' where
					preferLeftModify _ l (Delete _) = Just l
					preferLeftModify _ (Delete _) r = Just r
					preferLeftModify _ l (Create _) = Just l
					preferLeftModify _ (Create _) r = Just r
					preferLeftModify _ l _ = Just l

				-- unresolved parts
				unresolved' = unresolved patch'
				unuresolved' = unresolved upatch'

				applyPatch
					| showDiff opts = do
						print $ diff src dst
						print $ diff usrc udst
					| not (nullRepo unresolved') ∨ not (nullRepo unuresolved') = do
						putStrLn "Conflicts:"
						print unresolved'
						print unuresolved'
					| optionNoAction opts = mapM_ (write ∘ show) $ order merged'
					| otherwise = do
						sync write merged' (repoSource opts) (repoDestination opts)
						when canMark $ mark marker resolved' (repoDestination opts)
			applyPatch
			where
				-- Returns two repos: versioned files and unversioned
				enumRepo r = (filters' *** filters') <$> case repoType opts of
					Folder → ((,) R.empty ∘ repoAsPatch) <$> enumDir r
					Arc untracked → (mapWithKey unsetDirTime *** repoAsPatch) <$> enumArc r untracked
					Git untracked → (mapWithKey unsetDirTime *** repoAsPatch) <$> enumGit r untracked
					Svn untracked → (mapWithKey unsetDirTime *** repoAsPatch) <$> enumSvn r untracked
				repoAsPatch = fmap Create ∘ mapWithKey dropDirTime
				canMark = repoType opts ≢ Folder
				marker = case repoType opts of
					Folder → error "no need for mark"
					Arc _ → Marker markArc remoteMarkArc
					Git _ → Marker markGit remoteMarkGit
					Svn _ → Marker markSvn remoteMarkSvn
				filters' = include' ∘ exclude'
				exclude' = exclude (\e → or [match pat e | pat ← excludePats opts])
				include'
					| null (includePats opts) = id
					| otherwise = include (\e → or [match pat e | pat ← includePats opts])
				dropDirTime e
					| isDir e = const Nothing
					| otherwise = Just
				unsetDirTime e
					| isDir e = fmap (const Nothing)
					| otherwise = id

verbose ∷ Options → String → IO ()
verbose opts
	| verboseOutput opts = write
	| otherwise = const $ return ()

write ∷ String → IO ()
write = mapM_ write' ∘ lines where
	write' s@('✓':_) = color Green $ putStrLn s
	write' s@('+':_) = color Green $ putStrLn s
	write' s@('✗':_) = color Red $ putStrLn s
	write' s@('-':_) = color Red $ putStrLn s
	write' s = putStrLn s

color ∷ Color → IO () → IO ()
color clr act = setSGR [SetColor Foreground Vivid clr] >> act >> setSGR []
