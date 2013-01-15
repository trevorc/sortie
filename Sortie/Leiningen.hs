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
    , getProjectName
    , getProjectVersion
    , leinDo
    )
where

import Control.Lens              (view)
import Data.Maybe                (listToMaybe, mapMaybe)
import Data.Version              (Version(..), parseVersion, showVersion)
import Distribution.Package      (PackageName(PackageName))
import Distribution.Simple.Utils (withFileContents)
import System.FilePath           ((</>))
import Text.Printf               (printf)
import Text.Regex.PCRE           ((=~))
import Text.ParserCombinators.ReadP (readP_to_S)

import Sortie.Project            (Project)
import Sortie.Utils              (die, parseMaybe, readCommand_)
import qualified Sortie.Project as Project

type LeiningenCommand = [String]

artifactFileName :: Project -> FilePath
artifactFileName project = printf "%s-%s.war"
                           (getPackageName . view Project.name $ project)
                           (showVersion . view Project.version $ project)
    where getPackageName (PackageName name) = name

projectFileName :: FilePath
projectFileName = "project.clj"

getProjectName :: FilePath -> IO PackageName
getProjectName = parseProjectField "name" projectNamePattern (Just . PackageName)
    where projectNamePattern = "defproject[[:space:]]+" ++
                               "(?:[[:alnum:]_.-]+/)?" ++
                               "([[:alnum:]_-]+)"

getProjectVersion :: FilePath -> IO Version
getProjectVersion = parseProjectField "version" projectVersionPattern readVersion
    where { projectVersionPattern = "defproject.*\"([[:digit:]][^\"]+)\""
          ; readVersion = parseMaybe $ readP_to_S parseVersion
          }

parseProjectField :: String -> String -> (String -> Maybe a) -> FilePath -> IO a
parseProjectField fld pat parser dir = withFileContents projectFilePath $
                                       maybe notFoundError return .
                                       listToMaybe . mapMaybe findField . lines
    where { projectFilePath = dir </> projectFileName
          ; notFoundError = die $ "couldn't find " ++ fld ++
                            " in " ++ projectFilePath
          ; findField = match . (=~ pat)
          ; match [[_, str]] = parser str
          ; match _          = Nothing
          }

mapButLast :: (a -> a) -> [a] -> [a]
mapButLast _ []     = []
mapButLast _ [x]    = [x]
mapButLast f (x:xs) = f x : mapButLast f xs

mapLast :: (a -> a) -> [a] -> [a]
mapLast _ []     = []
mapLast f [x]    = [f x]
mapLast f (x:xs) = x : mapLast f xs

leinDo :: [LeiningenCommand] -> IO ()
leinDo commands = readCommand_ "lein" $ "do" : concat (joinCommands commands)
    where joinCommands = mapButLast (mapLast (++ ","))