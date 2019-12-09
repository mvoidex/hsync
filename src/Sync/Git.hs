{-# LANGUAGE FlexibleContexts #-}

module Sync.Git (
	enumGit, git, remoteGit,
	markGit, remoteMarkGit,

	module Sync.Base,
	module Sync.Repo
	) where

import Data.Time.Clock

import Sync.Base
import Sync.Repo
import Sync.Ssh
import Sync.GitLike

gitCvs ∷ GitLike
gitCvs = GitLike "git" []

enumGit ∷ Location → Bool → IO (Patch Entity (Maybe UTCTime), Repo Entity UTCTime)
enumGit = enumGitLike gitCvs

git ∷ FilePath → Bool → IO (Patch Entity (Maybe UTCTime), Repo Entity UTCTime)
git = gitLike gitCvs

remoteGit ∷ String → FilePath → Bool → IO (Patch Entity (Maybe UTCTime), Repo Entity UTCTime)
remoteGit = remoteGitLike gitCvs

-- | Mark git file according to action performed
markGit ∷ Entity → Action a → IO ()
markGit = markGitLike gitCvs

-- | Mark remote git file according to action performed
remoteMarkGit ∷ Entity → Action a → ProcessM ()
remoteMarkGit = remoteMarkGitLike gitCvs
