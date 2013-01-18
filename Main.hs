{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Bitbase 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  trevor@bitba.se
-- Stability   :  experimental
-- Portability :  portable
--
-- sortie: a simple, bespoke deployment tool
--
-----------------------------------------------------------------------------


module Main
    (main)
where

import Control.Applicative         (Applicative(..), (<$>))
import Control.Lens                ((^.))
import Control.Monad               (unless, when)
import Data.Char (toLower)
import Data.List                   ((\\))
import Data.Maybe                  (mapMaybe)
import Distribution.Simple.Setup   (fromFlag)
import Distribution.Simple.Utils   (die, topHandler)
import Distribution.Text           (display)
import System.Environment          (getArgs, getProgName)
import System.IO                   (hPutStr, stderr)
import Text.Printf                 (printf)
import Distribution.Simple.Command
    ( CommandParse(..), commandsRun, commandAddAction )
import qualified Data.Map as Map

import Sortie.Command
    ( Action , Command
    , GlobalFlags(..),  globalCommand , ReleaseFlags(..), releaseCommand
    , DeployFlags(..),  deployCommand , MigrateFlags(..), migrateCommand
    , ShowFlags(..),    showCommand
    )
import Sortie.Command.Deploy       (deploy)
import Sortie.Command.Release      (release)
import Sortie.Context              (Context(..))
import Sortie.Project.Parse
    ( findAndParseProjectFile, findProjectDirectory, showProject )
import qualified Sortie.Project as Project
    ( environments )
import qualified Paths_sortie (version)

guardNoExtraArgs :: String -> [String] -> IO ()
guardNoExtraArgs name args =
    when (length args > 0) $ die $
             "the `" ++ name ++"' command does not accept any " ++
             "positional arguments: " ++ unwords args

releaseAction :: Action ReleaseFlags
releaseAction _flags args ctx =
    guardNoExtraArgs "release" args >>
    release ctx

deployAction :: Action DeployFlags
deployAction DeployFlags{..} envs ctx@Context{project} =
    do { unless (null unknownEnvs) $ die $
                    "unrecognized environments " ++ unwords unknownEnvs
       ; mapM_ (deploy ctx) $ mapMaybe (`Map.lookup` projectEnvs) targetEnvs
       }
    where { projectEnvs = project ^. Project.environments
          ; targetEnvs = map toLower <$> envs
          ; unknownEnvs = targetEnvs \\ Map.keys projectEnvs
          }

migrateAction :: Action MigrateFlags
migrateAction = undefined

showAction :: Action ShowFlags
showAction _flags args Context{..} =
    guardNoExtraArgs "show" args >>
    putStrLn (showProject project)

commands :: [Command]
commands = [ commandAddAction releaseCommand releaseAction
           , commandAddAction deployCommand  deployAction
           , commandAddAction migrateCommand migrateAction
           , commandAddAction showCommand    showAction
           ]

run :: [String] -> IO ()
run args =
    case commandsRun globalCommand commands args of
      { CommandHelp   help          -> printCommandHelp help
      ; CommandList   opts          -> printOptionsList opts
      ; CommandErrors errs          -> printErrors errs
      ; CommandReadyToGo (fl@GlobalFlags{..}, commandParse) ->
        case commandParse of
          { _ | fromFlag globalVersion -> printVersion
          ; CommandHelp      help      -> printCommandHelp help
          ; CommandList      opts      -> printOptionsList opts
          ; CommandErrors    errs      -> printErrors errs
          ; CommandReadyToGo action    -> setupContext fl >>= action
          }
      }
    where { pr                    = hPutStr stderr
          ; liftFlag              = pure . fromFlag
          ; printCommandHelp help = getProgName >>= pr . help
          ; printOptionsList      = pr . unlines
          ; printErrors           = die . unlines
          ; printVersion          = do { prog <- getProgName
                                       ; pr $ printf "%s version %s\n"
                                            prog (display Paths_sortie.version)
                                       }
          ; setupContext GlobalFlags{..}
              = Context <$>
                findProjectDirectory <*>
                findAndParseProjectFile <*>
                liftFlag globalVerbosity <*>
                liftFlag globalDryRun
          }

main :: IO ()
main = topHandler $ getArgs >>= run
