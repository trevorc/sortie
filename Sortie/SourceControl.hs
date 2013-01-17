{-# LANGUAGE RecordWildCards #-}
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
    , ensureTagForHEAD
    , getChangedFiles
    , hasUncommittedChanges
    , isWorkingTreeDirty
    , showFileStatus
    , tagVersion
    , listTags
    , versionToTag
    )
where

import Control.Applicative          ((<$>), (<*>))
import Control.Monad                (unless)
import Data.Maybe                   (listToMaybe, mapMaybe)
import Data.Version                 (showVersion)
import Distribution.Verbosity       (Verbosity)
import Distribution.Version         (Version)
import System.Exit                  (ExitCode(..))
import Text.ParserCombinators.ReadP (get, readP_to_S)
import Text.Printf                  (printf)
import Text.Regex.PCRE              ((=~))

import Sortie.Utils
    ( die, notice
    , readMaybe
    , readCommand, readCommand_, runProcessSilently )

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

data FileChange = FileChange
    { path   :: FilePath
    , status :: FileStatus
    }

instance Show FileChange where show = showFileChange

newtype Revision = Revision String
    deriving Eq

showFileStatus :: FileStatus -> String
showFileStatus New        = "N"
showFileStatus Deleted    = "D"
showFileStatus Modified   = "M"
showFileStatus Unmerged   = "U"
showFileStatus TypeChange = "T"

showFileChange :: FileChange -> String
showFileChange FileChange{..} = printf "%s %s" (show status) path

isWorkingTreeDirty :: IO Bool
isWorkingTreeDirty =
    (/= ExitSuccess) <$>
    runProcessSilently "git" ["update-index", "--refresh"]

hasUncommittedChanges :: IO Bool
hasUncommittedChanges = not . null <$> getChangedFiles

diffLinePattern :: String
diffLinePattern = "^:[0-7]+ [0-7]+ [0-9a-f]+ [0-9a-f]+ (.)[0-9]*\t([^\t\n]+)$"

processDiffLine :: String -> Maybe FileChange
processDiffLine line = do { (fp, st) <- match
                          ; FileChange fp <$> readMaybe st
                          }
    where match :: Maybe (FilePath, String)
          match = case line =~ diffLinePattern of
                    { [[_,st,fp]] -> Just (fp, st)
                    ; _           -> Nothing
                    }

getChangedFiles :: IO [FileChange]
getChangedFiles = mapMaybe processDiffLine . lines <$>
                  readCommand "git" ["diff-index", "HEAD"]

versionToTag :: Version -> String
versionToTag = ('v':) . showVersion

tagVersion :: Version -> IO ()
tagVersion version = readCommand_ "git" ["tag", tag, "HEAD"]
    where tag = versionToTag version

listTags :: IO [String]
listTags = lines <$> readCommand "git" ["tag", "-l"]

parseRev :: String -> IO Revision
parseRev treeish = readCommand "git" ["rev-parse", treeish] >>=
                   maybe (die $ "rev-parse failed for " ++ treeish)
                         (return . Revision) .
                   listToMaybe . lines

headRevision :: IO Revision
headRevision = parseRev "HEAD"

ensureTagForHEAD :: Verbosity   -- | Log at this verbosity.
                 -> Bool        -- | Dry run -- perform no action.
                 -> Version     -- | Version to create tag for.
                 -> IO ()
ensureTagForHEAD verbosity dryRun version =
    do { tagExists <- elem tagName <$> listTags
       ; if tagExists then ensureIsHEAD else createTag
       }
    where { tagName = versionToTag version
          ; ensureIsHEAD = do
              { isHead <- (==) <$> headRevision <*> parseRev tagName
              ; unless isHead $
                       die $ printf "tag %s already exists \
                                    \and does not point to HEAD" tagName
              }
          ; createTag = do { notice verbosity $ "creating tag " ++
                                    tagName ++ "..."
                           ; unless dryRun $ tagVersion version
                           ; notice verbosity "done.\n"
                           }
          }
