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
import Data.Version            (showVersion)
import System.FilePath         ((</>))

import Sortie.Leiningen        (artifactFileName, leinDo)
import Sortie.Project          (Project)
import Sortie.Utils            (die)
import Sortie.SourceControl
    ( isWorkingTreeDirty, getChangedFiles
    , listTags, versionToTag, createTag )
import qualified Sortie.Project as Project
    ( name, version )
import qualified Sortie.Leiningen as Lein
    ( getProjectName, getProjectVersion )

dieUnless :: (a -> Bool) -> (a -> IO String) -> a -> IO ()
dieUnless p msg x | p x       = return ()
                  | otherwise = msg x >>= die

release :: Project       -- | The project data structure.
        -> FilePath      -- | The project directory containing
                         --   a project.clj and the target
                         --   subdirectory.
        -> IO ()
release project dir =
    do { dieUnless not changedFiles               =<< isWorkingTreeDirty
       ; dieUnless (== name)     nameMismatch     =<< Lein.getProjectName dir
       ; dieUnless (== version)  versionMismatch  =<< Lein.getProjectVersion dir
       ; dieUnless (notElem tag) tagAlreadyExists =<< listTags
       ; _ <- createTag version
       ; createDeploymentArtifact
       ; deployWarToS3
       }
    where { name    = project ^. Project.name
          ; version = project ^. Project.version
          ; tag     = versionToTag version

          ; changedFiles _ = ("release aborted. working tree dirty:\n" ++) .
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

          ; createDeploymentArtifact =
              leinDo [["clean"], ["ring", "uberwar",
                                  dir </> artifactFileName project]]

          ; deployWarToS3 = undefined
          }