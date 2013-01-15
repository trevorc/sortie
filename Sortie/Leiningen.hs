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

import Control.Applicative       ((<$>))
import Control.Lens              (view)
import Data.Maybe                (listToMaybe, mapMaybe)
import Data.Version              (Version(..), parseVersion)
import Distribution.Package      (PackageName)
import Distribution.Simple.Utils (withFileContents)
import System.FilePath           ((</>))
import Text.Printf               (printf)
import Text.Regex.Posix          ((=~))
import Text.ParserCombinators.ReadP (readP_to_S)

import Sortie.Project            (Project)
import Sortie.Utils              (die, parseMaybe, readCommand_, readMaybe)
import qualified Sortie.Project as Project

type LeiningenCommand = [String]

artifactFileName :: Project -> FilePath
artifactFileName project = printf "%s-%s.war"
                           (show . view Project.name    $ project)
                           (show . view Project.version $ project)

projectFileName :: FilePath
projectFileName = "project.clj"

getProjectName :: FilePath -> IO PackageName
getProjectName = parseProjectField "name" projectNameParser readMaybe
    where projectNameParser = "defproject"

getProjectVersion :: FilePath -> IO Version
getProjectVersion = parseProjectField "version" projectVersionPattern readVersion
    where { projectVersionPattern = "defproject.*\"([0-9][^\"]+)\""
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