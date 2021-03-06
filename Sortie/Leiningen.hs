{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sortie.Leiningen
-- Copyright   :  (c) Bitbase 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  trevor@bitba.se
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Sortie.Leiningen
    ( artifactFileName
    , createArtifact
    , getProjectName
    , getProjectVersion
    , leinDo
    )
where

import Control.Applicative       ((<$>))
import Control.Monad             (unless)
import Control.Monad.Loops       (allM, andM)
import Data.Maybe                (listToMaybe, mapMaybe)
import Data.Version              (Version(..), parseVersion, showVersion)
import Distribution.Package      (PackageName(PackageName))
import Distribution.Simple.Utils
    ( getDirectoryContentsRecursive, withFileContents )
import System.Directory          (doesFileExist, getModificationTime)
import System.FilePath           ((</>), takeExtension)
import Text.Printf               (printf)
import Text.ParserCombinators.ReadP (readP_to_S)

import Sortie.Context            (Context(..))
import Sortie.Project            (Project(..), getPackageName)
import Sortie.Utils
    ( Pattern, (=^~), die, elseM
    , mapButLast, mapLast
    , notice, parseMaybe, readCommand_ )

type LeiningenCommand = [String]

artifactFileName :: Project -> FilePath
artifactFileName project = printf "%s-%s.war"
                           (getPackageName . projectName $ project)
                           (showVersion    . version     $ project)

projectFileName :: FilePath
projectFileName = "project.clj"

getProjectName :: FilePath -> IO PackageName
getProjectName = parseProjectField "name" projectNamePattern (Just . PackageName)
    where projectNamePattern = "defproject[[:space:]]+\
                               \(?:[[:alnum:]_.-]+/)?\
                               \([[:alnum:]_-]+)"

getProjectVersion :: FilePath -> IO Version
getProjectVersion = parseProjectField "version" projectVersionPattern readVersion
    where { projectVersionPattern = "defproject.*\"([[:digit:]][^\"]+)\""
          ; readVersion = parseMaybe $ readP_to_S parseVersion
          }

parseProjectField :: String -> Pattern -> (String -> Maybe a) -> FilePath -> IO a
parseProjectField fld pat parser dir = withFileContents projectFilePath $
                                       maybe notFoundError return .
                                       listToMaybe . mapMaybe findField . lines
    where { projectFilePath = dir </> projectFileName
          ; notFoundError = die $ "couldn't find " ++ fld ++
                            " in " ++ projectFilePath
          ; findField line = listToMaybe (line =^~ pat) >>= parser
          }

isUpToDate :: FilePath          -- ^ Artifact file path
           -> FilePath          -- ^ Project directory
           -> IO Bool
isUpToDate artifactPath projectDirectory =
    andM [ doesFileExist artifactPath
         , do { sourceFiles <- filter isProjectFile <$>
                               getDirectoryContentsRecursive projectDirectory
              ; artifactModTime <- getModificationTime artifactPath
              ; allM (fmap (< artifactModTime) . getModificationTime)
                     sourceFiles
              }
         ]
    where isProjectFile = (`elem` [".clj", ".resources", ".xml"]) .
                          takeExtension

leinDo :: [LeiningenCommand] -> IO ()
leinDo commands = readCommand_ "lein" $ "do" : concat (joinCommands commands)
    where joinCommands = mapButLast (mapLast (++ ","))

createArtifact :: Context -> IO FilePath
createArtifact Context{verbosity, dryRun, projectDirectory, project} =
    do { notice verbosity $ "creating artifact " ++ artifactPath ++ "..."
       ; upToDate <- isUpToDate artifactPath projectDirectory
       ; unless (dryRun || upToDate) $ do
           { leinDo [["clean"], ["ring", "uberwar", fileName]]
           ; doesFileExist artifactPath `elseM` artifactNotCreated
           }
       ; notice verbosity $ if upToDate then "(up-to-date).\n" else "done.\n"
       ; return artifactPath
       }
    where { artifactPath       = projectDirectory </> "target" </> fileName
          ; fileName           = artifactFileName project
          ; artifactNotCreated = die $ "failed to create artifact at " ++
                                 artifactPath
          }
