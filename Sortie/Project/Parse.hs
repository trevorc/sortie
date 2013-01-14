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

import Control.Applicative       (Applicative(..), (<$>), (<$))
import Control.Lens              ((^.), set, view)
import Control.Monad             (ap, foldM)
import Control.Monad.Loops       (firstM)
import Data.List                 (inits)
import Distribution.Text         (disp, parse)
import Distribution.ParseUtils   (Field(..), FieldDescr(FieldDescr),
                                  ParseResult(..), locatedErrorMsg,
                                  parseFilePathQ, parseFreeText,
                                  parseTokenQ, ppFields, showFilePath, showFreeText,
                                  showPWarning, showToken, simpleField,
                                  readFields, warning, lineNo)
import Distribution.Simple.Utils (warn, withFileContents)
import Distribution.Verbosity    (normal)
import System.Directory          (getCurrentDirectory, getDirectoryContents)
import System.FilePath           ((</>), joinPath, splitDirectories)
import Text.PrettyPrint          (($$), render, space, vcat)
import qualified Data.Map as Map

import Sortie.Project            (Environment, Project,
                                  emptyEnvironment, emptyProject)
import Sortie.Utils              (die, dieWithLocation)
import qualified Sortie.Project as Project

instance Functor     ParseResult where { fmap f m = m >>= return . f }
instance Applicative ParseResult where { pure = return ; (<*>) = ap }

projectDescriptionFields :: [FieldDescr Project]
projectDescriptionFields =
    [ simpleField "name"          disp parse
      (view Project.name)         (set Project.name)
    , simpleField "version"       disp parse
      (view Project.version)      (set Project.version)
    , simpleField "repository"    showFreeText parseFreeText
      (view Project.repository)   (set Project.repository)
    , simpleField "s3-bucket"     showToken parseTokenQ
      (view Project.s3Bucket)     (set Project.s3Bucket)
    , simpleField "s3-key-prefix" showFilePath parseFilePathQ
      (view Project.s3KeyPrefix)  (set Project.s3KeyPrefix)
    ]

environmentFields :: [FieldDescr Environment]
environmentFields =
    [ simpleField "host"              showToken parseTokenQ
      (view Project.host)             (set Project.host)
    , simpleField "user"              showToken parseTokenQ
      (view Project.user)             (set Project.user)
    , simpleField "database-name"     showToken parseTokenQ
      (view Project.databaseName)     (set Project.databaseName)
    , simpleField "database-user"     showToken parseTokenQ
      (view Project.databaseUser)     (set Project.databaseUser)
    , simpleField "database-password" showFreeText parseFreeText
      (view Project.databasePassword) (set Project.databasePassword)
    , simpleField "install-script"    showFreeText parseFreeText
      (view Project.installScript)    (set Project.installScript)
    ]

projectFileName :: FilePath
projectFileName = "Sortieproject"

containingDirectories :: FilePath -> [FilePath]
containingDirectories = map joinPath . reverse . drop 1 . inits .
                        splitDirectories

findProjectFile :: IO (Maybe FilePath)
findProjectFile = containingDirectories <$> getCurrentDirectory >>=
                  fmap (fmap (</> projectFileName)) .
                  firstM containsProjectFile
    where containsProjectFile = fmap (elem projectFileName) .
                                getDirectoryContents

projectParser :: String -> ParseResult Project
projectParser str = do { (header, body) <- span isSimpleField <$> readFields str
                       ; set Project.environments <$>
                         mapM parseBody body <*>
                         parseHeader header
                       }
    where { isSimpleField F{} = True
          ; isSimpleField _   = False
          ; parseHeader = accumFields projectDescriptionFields emptyProject
          ; parseBody (Section _ "environment" _ fields)
              = accumFields environmentFields emptyEnvironment fields
          ; parseBody _ = error "X"
          }

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
showProject project = render $ header $$ body
    where { header = ppFields projectDescriptionFields project
          ; body = vcat . map ppSection $ project ^. Project.environments
          ; ppSection env = space $$ ppFields environmentFields env
          }


{- The following methods are from Cabal:
Module      :  Distribution.ParseUtils
Copyright   :  (c) The University of Glasgow 2004
-}

accumFields :: [FieldDescr a] -> a -> [Field] -> ParseResult a
accumFields fields = foldM setField
    where { fieldMap = Map.fromList [ (name, f) | f@(FieldDescr name _ _) <- fields ]
          ; setField accum (F line name value) =
              case Map.lookup name fieldMap of
                { Just (FieldDescr _ _ setter) -> setter line value accum
                ; Nothing ->
                    accum <$ warning ("Unrecognized field " ++ name ++
                                      " on line " ++ show line)
                }
          ; setField accum f =
              accum <$ warning ("Unrecognized stanza on line " ++
                                show (lineNo f))
          }
