{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
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
import Distribution.Version         (Version)
import System.Exit                  (ExitCode(..))
import Text.ParserCombinators.ReadP (get, readP_to_S)
import Text.Printf                  (printf)

import Sortie.Context            (Context(..))
import Sortie.Utils
    ( (=^~)
    , Pattern
    , die, notice
    , elseM
    , maybeTuple
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
showFileChange FileChange{status, path} = printf "%s %s" (show status) path

isWorkingTreeDirty :: IO Bool
isWorkingTreeDirty =
    (/= ExitSuccess) <$>
    runProcessSilently "git" ["update-index", "--refresh"]

hasUncommittedChanges :: IO Bool
hasUncommittedChanges = not . null <$> getChangedFiles

diffLinePattern :: Pattern
diffLinePattern = "^:[0-7]+ [0-7]+ [0-9a-f]+ [0-9a-f]+ (.)[0-9]*\t([^\t\n]+)$"

processDiffLine :: String -> Maybe FileChange
processDiffLine line = do { (st, fp) <- maybeTuple $ line =^~ diffLinePattern
                          ; FileChange fp <$> readMaybe st
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

ensureTagForHEAD :: Context     -- ^ Execution context.
                 -> Version     -- ^ Version to create tag for.
                 -> IO ()
ensureTagForHEAD Context{verbosity, dryRun} version =
    do { tagExists <- elem tagName <$> listTags
       ; if tagExists
           then (==) <$> headRevision <*> parseRev tagName
                    `elseM` tagIsNotHEAD
           else do
              { notice verbosity $ "creating tag " ++ tagName ++ "..."
              ; unless dryRun $ tagVersion version
              ; notice verbosity "done.\n"
              }
       }
    where { tagName = versionToTag version
          ; tagIsNotHEAD = die $ printf "tag %s already exists \
                                        \and does not point to HEAD" tagName
          }