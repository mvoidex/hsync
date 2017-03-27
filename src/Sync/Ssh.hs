{-# LANGUAGE DeriveDataTypeable #-}

module Sync.Ssh (
	run, ssh, sftp,
	send, wait,
	invoke, invoke_,
	close, kill,

	cd,
	stat,
	put, get, rm, rmdir, mkdir, mkdirs,
	quote,

	module Sync.Repo
	) where

import Prelude.Unicode

import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Data.Maybe (catMaybes, isJust)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Typeable
import System.Exit
import System.FilePath.Posix ((</>), splitDirectories)
import System.IO
import System.Process
import Text.Read (readMaybe)

import Sync.Repo

data ProcessError = ProcessError String [String] Int [String] deriving (Eq, Ord, Typeable)

instance Show ProcessError where
	show (ProcessError cmd args code e) = unlines $ header : e where
		header = unwords (cmd : args) ++ " ⇒ " ++ show code

instance Exception ProcessError

run ∷ String → [String] → (String → String) → ([String] → [String]) → ProcessM a → IO a
run cmd args echo' filter' act = do
	(stdin', stdout', stderr', ph) ← runInteractiveProcess cmd args Nothing Nothing
	mapM_ setHandle [stdin', stdout', stderr']
	chIn ← newChan
	chOut ← newChan
	chErr ← newChan

	_ ← forkIO $ do
		cts ← getChanContents chIn
		mapM_ (hPutStrLn stdin') ∘ catMaybes ∘ takeWhile isJust $ cts
		hClose stdin'
	forM_ [(stdout', chOut), (stderr', chErr)] $ \(h, ch) → forkIO $ flip finally (writeChan ch Nothing) $ do
		cts ← hGetContents h
		mapM_ (writeChan ch ∘ Just) ∘ filter' ∘ lines $ cts

	let
		duplex = ProcessDuplex chIn chOut chErr ph echo'

	res ← runExceptT $ flip runReaderT duplex $ do
		r ← act
		_ ← close
		return r
	case res of
		Left (code, e) → throwIO $ ProcessError cmd args code e
		Right v → return v
	where
		setHandle h = do
			hSetBuffering h LineBuffering
			hSetEncoding h utf8
			hSetNewlineMode h noNewlineTranslation

ssh ∷ String → ProcessM a → IO a
ssh host = run "ssh" [host] id id

sftp ∷ String → FilePath → ProcessM a → IO a
sftp host path = run "sftp" [host ++ ":" ++ path] ('!':) dropSftpEcho where
	dropSftpEcho = filter (not ∘ ("sftp> " `isPrefixOf`))

send ∷ String → ProcessM ()
send s = do
	cin ← asks duplexIn
	liftIO ∘ writeChan cin ∘ Just $ s

invoke ∷ String → ProcessM [String]
invoke cmd = send cmd >> wait

invoke_ ∷ String → ProcessM ()
invoke_ = void ∘ invoke

close ∷ ProcessM [String]
close = do
	cin ← asks duplexIn
	ph ← asks duplexProcess
	liftIO $ writeChan cin Nothing
	code ← liftIO $ waitForProcess ph
	out ← asks duplexOut
	err ← asks duplexErr
	o ← waitChan out
	e ← waitChan err
	result code o e
	where
		waitChan = liftIO ∘ fmap (catMaybes ∘ takeWhile isJust) ∘ getChanContents

kill ∷ ProcessM [String]
kill = do
	ph ← asks duplexProcess
	liftIO $ terminateProcess ph
	close

cd ∷ FilePath → ProcessM ()
cd fpath = invoke_ $ "cd " ++ quote fpath

stat ∷ FilePath → ProcessM (Entity, UTCTime)
stat fpath = do
	(tm : ftype) ← (words ∘ head) <$> invoke ("stat '" ++ fpath ++ "' -c '%Y %F'")
	let
		getType
			| "directory" ∈ ftype = return True
			| "file" ∈ ftype = return False
			| otherwise = throwError (0, ["unknown file type: " ++ unwords ftype])
	t ← getType
	case readMaybe tm of
		Just tm' → return (Entity t fpath, posixSecondsToUTCTime ∘ fromInteger $ tm')
		Nothing → throwError (0, ["stat: can't parse time " ++ tm])

put ∷ Bool → FilePath → FilePath → ProcessM ()
put recursive from to = invoke_ $ unwords [
	if recursive then "put -Pr" else "put -P",
	quote from,
	quote to]

get ∷ Bool → FilePath → FilePath → ProcessM ()
get recursive from to = invoke_ $ unwords [
	if recursive then "get -Pr" else "get -P",
	quote from,
	quote to]

rm ∷ FilePath → ProcessM ()
rm fpath = invoke_ $ "rm " ++ quote fpath

rmdir ∷ FilePath → ProcessM ()
rmdir fpath = invoke_ $ "rmdir " ++ quote fpath

mkdir ∷ FilePath → ProcessM ()
mkdir fpath = invoke_ $ "mkdir " ++ quote fpath

mkdirs ∷ FilePath → ProcessM ()
mkdirs fpath = do
	forM_ fpaths $ \f → send ("mkdir " ++ quote f)
	void $ wait
	where
		fpaths = scanl1 (</>) ∘ splitDirectories $ fpath

data ProcessDuplex = ProcessDuplex {
	duplexIn ∷ Chan (Maybe String),
	duplexOut ∷ Chan (Maybe String),
	duplexErr ∷ Chan (Maybe String),
	duplexProcess ∷ ProcessHandle,
	duplexEcho ∷ String → String }

type ProcessM a = ReaderT ProcessDuplex (ExceptT (Int, [String]) IO) a

result ∷ ExitCode → [String] → [String] → ProcessM [String]
result ExitSuccess out _ = return out
result (ExitFailure code) _ err = throwError (code, err)

wait ∷ ProcessM [String]
wait = do
	echo ← asks duplexEcho
	send $ echo $ "echo " ++ waitStr ++ " $?"
	send $ echo $ "echo " ++ waitStr ++ " 1>&2"
	out ← asks duplexOut
	err ← asks duplexErr
	(o, code) ← readWait out
	e ← readWait_ err
	result code o e
	where
		waitStr = "--wait--"
		readWait_ = fmap fst ∘ readWait
		readWait ch = do
			str ← liftIO $ readChan ch
			case str of
				Nothing → stop
				Just s → case stripPrefix waitStr s of
					Nothing → goon s
					Just "" → stop
					Just " 0" → stop
					Just (' ':post) → stopFail $ read post
					Just _ → stopFail 0
			where
				stop = return ([], ExitSuccess)
				stopFail e = return ([], ExitFailure e)
				goon s = first (s :) <$> readWait ch

quote ∷ String → String
quote s = "'" ++ s ++ "'"
