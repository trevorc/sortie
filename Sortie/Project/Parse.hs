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
------------------------------------------------------------------------

module Sortie.Project.Parse
    ( findAndParseProjectFile
    , projectParser
    , showProject
    )
where

import Control.Applicative       ((<$>))
import Control.Lens              (set, view)
import Control.Monad.Loops       (firstM)
import Data.List                 (inits)
import Distribution.Text         (disp, parse)
-- import Distribution.PackageDescription.Parse (readAndParseFile)
import Distribution.ParseUtils   (FieldDescr(FieldDescr), ParseResult(..),
                                  locatedErrorMsg, parseFields, ppFields,
                                  showPWarning, simpleField)
import Distribution.Simple.Utils (die, dieWithLocation, warn, withFileContents)
import Distribution.Verbosity    (normal)
import System.Directory          (getCurrentDirectory, getDirectoryContents)
import System.FilePath           ((</>), joinPath, splitDirectories)
import Text.PrettyPrint          (Doc, render)

import Sortie.Project           (Project, emptyProject)
import qualified Sortie.Project as Project


projectFileFields :: [FieldDescr Project]
projectFileFields =
    [ simpleField "name"     disp parse
      (view Project.name)    (set Project.name)
    , simpleField "version"  disp parse
      (view Project.version) (set Project.version)
    ]

projectFileName :: FilePath
projectFileName = "Sortieproject"

containingDirectories :: FilePath -> [FilePath]
containingDirectories = map joinPath . reverse . drop 1 . inits .
                        splitDirectories

findProjectFile :: IO (Maybe FilePath)
findProjectFile = containingDirectories <$> getCurrentDirectory >>=
                  fmap (fmap (</> projectFileName)) . firstM containsProjectFile
    where containsProjectFile = fmap (elem projectFileName) .
                                getDirectoryContents

projectParser :: String -> ParseResult Project
projectParser = parseFields projectFileFields emptyProject

findAndParseProjectFile :: IO Project
findAndParseProjectFile = do
  { path <- findProjectFile >>= maybe (die "could not find project file") return
  ; withFileContents path $ \s ->
      case projectParser s of
        { ParseFailed e -> uncurry (dieWithLocation path) (locatedErrorMsg e)
        ; ParseOk warnings proj -> do { mapM_ (warn normal . showPWarning path) $
                                        reverse warnings
                                      ; return proj
                                      }
        }
  }

showProject :: Project -> String
showProject = render . ppFields projectFileFields
