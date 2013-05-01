{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sortie.Command.Release
-- Copyright   :  (c) Bitbase 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  trevor@bitba.se
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Sortie.Command.Release
    (release)
where

import Control.Applicative     ((<$>))
import Data.Maybe              (isJust)

import Sortie.Context          (Context(..))
import Sortie.Leiningen        (createArtifact)
import Sortie.Project          (Project(..))
import Sortie.Utils            (die, elseM, warFileType)
import Sortie.ProjectUtils
    ( ensureMatchingProjectName
    , ensureMatchingProjectVersion )
import Sortie.SourceControl
    ( hasUncommittedChanges, getChangedFiles
    , ensureTagForHEAD )
import qualified Sortie.S3 as S3
    ( getCredentials, putFile )

release :: Context -> IO ()
release ctx@Context{project = Project{version}} =
    do { not <$> hasUncommittedChanges `elseM` changedFiles
       ; isJust <$> S3.getCredentials ctx `elseM` missingS3Env
       ; ensureMatchingProjectName ctx
       ; ensureMatchingProjectVersion ctx
       ; ensureTagForHEAD ctx version
       ; S3.putFile ctx warFileType =<< createArtifact ctx
       }
    where { changedFiles = unlines . map show <$> getChangedFiles >>=
                           (die . ("release aborted. uncommitted changes:\n" ++))
          ; missingS3Env = die "missing either AWS_ACCESS_KEY_ID or \
                               \AWS_SECRET_ACCESS_KEY environment variables"
          }