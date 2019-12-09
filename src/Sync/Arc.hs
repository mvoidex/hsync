{-# LANGUAGE FlexibleContexts #-}

module Sync.Arc (
	enumArc, arc, remoteArc,
	markArc, remoteMarkArc,

	module Sync.Base,
	module Sync.Repo
	) where

import Data.Time.Clock

import Sync.Base
import Sync.Repo
import Sync.Ssh
import Sync.GitLike

arcCvs ∷ GitLike
arcCvs = GitLike "arc" []

enumArc ∷ Location → Bool → IO (Patch Entity (Maybe UTCTime), Repo Entity UTCTime)
enumArc = enumGitLike arcCvs

arc ∷ FilePath → Bool → IO (Patch Entity (Maybe UTCTime), Repo Entity UTCTime)
arc = gitLike arcCvs

remoteArc ∷ String → FilePath → Bool → IO (Patch Entity (Maybe UTCTime), Repo Entity UTCTime)
remoteArc = remoteGitLike arcCvs

-- | Mark git file according to action performed
markArc ∷ Entity → Action a → IO ()
markArc = markGitLike arcCvs

-- | Mark remote git file according to action performed
remoteMarkArc ∷ Entity → Action a → ProcessM ()
remoteMarkArc = remoteMarkGitLike arcCvs
