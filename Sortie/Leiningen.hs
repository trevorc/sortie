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
import Control.Lens              (view)
import Control.Monad             (unless, when)
import Control.Monad.Loops       (allM, andM)
import Data.Maybe                (listToMaybe, mapMaybe)
import Data.Version              (Version(..), parseVersion, showVersion)
import Distribution.Package      (PackageName(PackageName))
import Distribution.Simple.Utils
    ( getDirectoryContentsRecursive, withFileContents )
import Distribution.Verbosity    (Verbosity)
import System.Directory          (doesFileExist, getModificationTime)
import System.FilePath           ((</>), takeExtension)
import Text.Printf               (printf)
import Text.Regex.PCRE           ((=~))
import Text.ParserCombinators.ReadP (readP_to_S)

import Sortie.Project            (Project)
import Sortie.Utils              (die, notice, parseMaybe, readCommand_)
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

isUpToDate :: FilePath          -- | Artifact file path
           -> FilePath          -- | Project directory
           -> IO Bool
isUpToDate artifactPath projectDirectory =
    do { sourceFiles <- filter ((== ".clj") . takeExtension) <$>
                        getDirectoryContentsRecursive projectDirectory
       ; andM [ doesFileExist artifactPath
              , do { artifactModTime <- getModificationTime artifactPath
                   ; allM (fmap (< artifactModTime) . getModificationTime)
                          sourceFiles
                   }
              ]
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

infixl 1 `elseM`

elseM :: Monad m => m Bool -> m () -> m ()
c `elseM` m = c >>= flip unless m

createArtifact :: Verbosity -> Bool -> FilePath -> Project -> IO FilePath
createArtifact verbosity dryRun projectDir project =
    do { notice verbosity $ "creating artifact " ++ artifactPath ++ "..."
       ; upToDate <- isUpToDate artifactPath projectDir
       ; when upToDate $ notice verbosity "(up-to-date).\n"
       ; unless (dryRun || upToDate) $ do
           { leinDo [["clean"], ["ring", "uberwar", fileName]]
           ; doesFileExist artifactPath `elseM` artifactNotCreated
           ; notice verbosity "done.\n"
           }
       ; return artifactPath
       }
    where { artifactPath       = projectDir </> "target" </> fileName
          ; fileName           = artifactFileName project
          ; artifactNotCreated = die $ "failed to create artifact at " ++
                                 artifactPath
          }