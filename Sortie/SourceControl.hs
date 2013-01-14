-----------------------------------------------------------------------------
-- |
-- Module      :  Sortie.Project.Parse
-- Copyright   :  (c) Bitbase 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  trevor@bitba.se
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Sortie.SourceControl
    ( FileStatus(..)
    , getChangedFiles
    , isWorkingTreeDirty
    , showFileStatus
    , createTag
    )
where

import Control.Applicative          ((<$>))
import Data.Functor                 ((<$))
import Data.Maybe                   (mapMaybe)
import Data.Version                 (showVersion)
import Distribution.Version         (Version)
import System.Exit                  (ExitCode(..))
import Text.ParserCombinators.ReadP (get, readP_to_S)
import Text.Regex.Posix             ((=~))

import Sortie.Utils                 (readMaybe, readCommand, readCommand_,
                                     runProcessSilently)

data FileStatus
    = New
    | Deleted
    | Modified
    | Unmerged
    | TypeChange

instance Read FileStatus where
    readsPrec _ r = do
      { (m, s) <- readP_to_S get r
      ; case m of
          { 'A' -> return (New,        s)
          ; 'D' -> return (Deleted,    s)
          ; 'M' -> return (Modified,   s)
          ; 'U' -> return (Unmerged,   s)
          ; 'T' -> return (TypeChange, s)
          ; _   -> []
          }
      }

instance Show FileStatus where show = showFileStatus

showFileStatus :: FileStatus -> String
showFileStatus New        = "N"
showFileStatus Deleted    = "D"
showFileStatus Modified   = "M"
showFileStatus Unmerged   = "U"
showFileStatus TypeChange = "T"

isWorkingTreeDirty :: IO Bool
isWorkingTreeDirty =
    (/= ExitSuccess) <$>
    runProcessSilently "git" ["update-index", "--refresh"]

diffLinePattern :: String
diffLinePattern = "^:[0-7]+ [0-7]+ [0-9a-f]+ [0-9a-f]+ (.)[0-9]*\t([^\t\n]+)$"

processDiffLine :: String -> Maybe (FilePath, FileStatus)
processDiffLine line = do { (fp, Just st) <- fmap readMaybe <$> match
                          ; return (fp, st)
                          }
    where match :: Maybe (FilePath, String)
          match = case line =~ diffLinePattern of
                    { [[_,st,fp]] -> Just (fp, st)
                    ; _           -> Nothing
                    }

getChangedFiles :: IO [(FilePath, FileStatus)]
getChangedFiles = mapMaybe processDiffLine . lines <$>
                  readCommand "git" ["diff-index", "HEAD"]

createTag :: Version -> IO String
createTag version = versionString <$
                    readCommand_ "git" ["tag", versionString, "HEAD"]
    where versionString = 'v' : showVersion version
