{-# LANGUAGE RecordWildCards #-}
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
import Control.Lens            ((^.))
import Data.Maybe              (isJust)
import Data.Version            (showVersion)

import Sortie.Context          (Context(..))
import Sortie.Leiningen        (createArtifact)
import Sortie.Utils            (die, warFileType)
import Sortie.SourceControl
    ( hasUncommittedChanges, getChangedFiles
    , ensureTagForHEAD )
import qualified Sortie.Project as Project
    ( name, version, s3Bucket, s3KeyPrefix )
import qualified Sortie.Leiningen as Lein
    ( getProjectName, getProjectVersion )
import qualified Sortie.S3 as S3
    ( Bucket(Bucket), connection, putFile )

dieUnless :: (a -> Bool) -> (a -> IO String) -> a -> IO ()
dieUnless p msg x | p x       = return ()
                  | otherwise = msg x >>= die

release :: Context          -- | Project execution context.
        -> IO ()
release Context{..} =
    do { dieUnless not          changedFiles    =<< hasUncommittedChanges
       ; dieUnless (== name)    nameMismatch    =<< Lein.getProjectName projectDirectory
       ; dieUnless (== version) versionMismatch =<< Lein.getProjectVersion projectDirectory
       ; dieUnless isJust       missingS3Env    =<< S3.connection
       ; ensureTagForHEAD verbosity dryRun version
       ; createArtifact verbosity dryRun projectDirectory project >>=
         S3.putFile verbosity dryRun
               (S3.Bucket s3Bucket) s3KeyPrefix warFileType
       }
    where { name        = project ^. Project.name
          ; version     = project ^. Project.version
          ; s3Bucket    = project ^. Project.s3Bucket
          ; s3KeyPrefix = project ^. Project.s3KeyPrefix

          ; changedFiles _ = ("release aborted. uncommitted changes:\n" ++) .
                             unlines . map show <$>
                             getChangedFiles

          ; nameMismatch projectName =
              return $ "mismatched project names: " ++
                     show name ++ " vs. " ++
                     show projectName

          ; versionMismatch projectVersion =
              return $ "mismatched project versions: " ++
                     showVersion version ++ " vs. " ++
                     showVersion projectVersion

          ; missingS3Env = const . return $
                           "missing either AWS_ACCESS_KEY_ID or " ++
                           "AWS_SECRET_ACCESS_KEY environment variables"
          }