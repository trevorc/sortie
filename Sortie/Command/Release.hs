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
import Control.Monad           (when)
import Data.Version            (showVersion)
import System.Exit             (exitSuccess)
import System.FilePath         ((</>))

import Sortie.Leiningen        (artifactFileName, leinDo)
import Sortie.Project          (Project)
import Sortie.Utils            (die)
import Sortie.SourceControl
    ( hasUncommittedChanges, getChangedFiles
    , listTags, versionToTag, createTag )
import qualified Sortie.Project as Project
    ( name, version, s3Bucket, s3KeyPrefix )
import qualified Sortie.Leiningen as Lein
    ( getProjectName, getProjectVersion )

dieUnless :: (a -> Bool) -> (a -> IO String) -> a -> IO ()
dieUnless p msg x | p x       = return ()
                  | otherwise = msg x >>= die

release :: Project       -- | The project data structure.
        -> FilePath      -- | The project directory containing
                         --   a project.clj and the target
                         --   subdirectory.
        -> Bool          -- | Dry run: perform no action, just show
                         --   what would be done.
        -> IO ()
release project dir dryRun =
    do { when dryRun $ putStrLn "** DRY RUN **"
       ; dieUnless not           changedFiles     =<< hasUncommittedChanges
       ; dieUnless (== name)     nameMismatch     =<< Lein.getProjectName dir
       ; dieUnless (== version)  versionMismatch  =<< Lein.getProjectVersion dir
       ; dieUnless (notElem tag) tagAlreadyExists =<< listTags
       ; when dryRun $ do { putStrLn $ "create tag " ++ tag
                          ; putStrLn $ "create artifact " ++ artifactPath
                          ; putStrLn $ "put artifact to s3://" ++ s3Bucket ++ s3Key
                          ; exitSuccess
                          }
       ; _ <- createTag version
       ; leinDo [["clean"], ["ring", "uberwar", artifactPath]]
       ; deployWarToS3
       }
    where { name        = project ^. Project.name
          ; version     = project ^. Project.version
          ; s3Bucket    = project ^. Project.s3Bucket
          ; s3KeyPrefix = project ^. Project.s3KeyPrefix
          ; tag         = versionToTag version
          ; s3Key       = s3KeyPrefix ++ artifactFileName project

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

          ; tagAlreadyExists _ =
              return $ "tag " ++ tag ++ " already exists"

          ; artifactPath = dir </> artifactFileName project

          ; deployWarToS3 = undefined
          }