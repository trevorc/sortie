{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

module Sortie.Project.Parse
    ( findProjectDirectory
    , findAndParseProjectFile
    , projectParser
    , showProject
    )
where

import Control.Applicative       (Applicative(..), (<$>), (<$))
import Control.Monad             (ap, foldM, when)
import Control.Monad.Loops       (firstM)
import Data.List                 (inits)
import Distribution.Text         (disp, parse)
import Distribution.ParseUtils
    ( Field(..), FieldDescr(FieldDescr), ParseResult(..)
    , locatedErrorMsg, parseFilePathQ, parseFreeText
    , parseTokenQ, ppFields, showFilePath, showFreeText
    , showPWarning, showToken, simpleField, syntaxError
    , spaceListField, readFields, fName, warning, lineNo )
import Distribution.Compat.ReadP (ReadP, readS_to_P)
import Distribution.Verbosity    (normal)
import System.Directory
    ( getCurrentDirectory, doesFileExist )
import System.FilePath
    ( (</>), joinPath, normalise, splitDirectories )
import Text.PrettyPrint          (($$), (<+>), empty, nest, render, space, text, vcat)
import qualified Data.Map as Map

import Sortie.Project            (Environment(..), Project(..),
                                  emptyEnvironment, emptyProject)
import Sortie.Utils
    ( die, dieWithLocation
    , setFst, setSnd
    , warn
    , withFileContents )

projectDescriptionFields :: [FieldDescr Project]
projectDescriptionFields =
    [ simpleField "name"             disp parse
      projectName (\projectName p -> p {projectName})
    , simpleField "version"          disp parse
      version     (\version p     -> p {version})
    , simpleField "repository"       showFreeText parseFreeText
      repository  (\repository p  -> p {repository})
    , simpleField "s3-bucket"        disp parse
      s3Bucket    (\s3Bucket p    -> p {s3Bucket})
    , simpleField "s3-key-prefix"    showFilePath parseFilePathQ
      s3KeyPrefix (\s3KeyPrefix p -> p {s3KeyPrefix})
    , simpleField        "aws-access-key-id"
      (maybe empty disp) (Just <$> parse)
      awsAccessKeyId     (\awsAccessKeyId p     -> p {awsAccessKeyId})
    , simpleField        "aws-secret-access-key"
      (maybe empty disp) (Just <$> parse)
      awsSecretAccessKey (\awsSecretAccessKey p -> p {awsSecretAccessKey})
    ]

environmentFields :: [FieldDescr Environment]
environmentFields =
    [ simpleField      "host"                  showToken parseTokenQ
      host             (\host e             -> e {host})
    , simpleField      "user"                  showToken parseTokenQ
      execUser         (\execUser e         -> e {execUser})
    , simpleField      "database-name"         showToken parseTokenQ
      databaseName     (\databaseName e     -> e {databaseName})
    , simpleField      "database-user"         showToken parseTokenQ
      databaseUser     (\databaseUser e     -> e {databaseUser})
    , simpleField      "database-password"     showFreeText parseHaskellString
      databasePassword (\databasePassword e -> e {databasePassword})
    , simpleField      "install-script"        showFilePath parseFilePathQ
      (fst . installScript)
      (\path e@Environment{installScript} ->
           e {installScript = setFst path installScript})
    , spaceListField   "install-script-args"   showFilePath parseHaskellString
      (snd . installScript)
      (\args e@Environment{installScript} ->
           e {installScript = setSnd args installScript})
    ]

parseHaskellString :: ReadP r String
parseHaskellString = readS_to_P reads

projectFileName :: FilePath
projectFileName = "Sortieproject"

containingDirectories :: FilePath -> [FilePath]
containingDirectories = map joinPath . reverse . drop 1 . inits .
                        splitDirectories

findProjectDirectory :: IO FilePath
findProjectDirectory = containingDirectories . normalise <$>
                       getCurrentDirectory >>=
                       firstM containsProjectFile >>=
                       maybe (die "could not find project file") return
    where containsProjectFile = doesFileExist . (</> projectFileName)

findProjectFile :: IO FilePath
findProjectFile = (</> projectFileName) <$> findProjectDirectory

projectParser :: String -> ParseResult Project
projectParser str = do { (header, body) <- span isSimpleField <$> readFields str
                       ; (\v p -> p { environments = v }) . Map.fromList <$>
                         mapM parseEnvironment body <*>
                         parseHeader header
                       }
    where { isSimpleField F{} = True
          ; isSimpleField _   = False
          ; parseHeader = accumFields projectDescriptionFields emptyProject
          ; parseEnvironment f@(Section _ "environment" name fields)
              = do { when (null name) $ syntaxError (lineNo f)
                              "environment must be given a name"
                   ; ((,) name) <$>
                     accumFields environmentFields emptyEnvironment fields
                   }
          ; parseEnvironment f = syntaxError (lineNo f) $
                                 "Unknown stanza " ++ fName f

          }

findAndParseProjectFile :: IO Project
findAndParseProjectFile = do
  { path <- findProjectFile
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
          ; body = vcat . map (uncurry ppSection) . Map.toList $
                   environments project
          ; ppSection name env = space $$
                                 "environment" <+> text name $$
                                 nest 2 (ppFields environmentFields env)
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
