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
import Control.Monad               (unless, when)
import Data.Char                   (toLower)
import Data.List                   ((\\), intercalate)
import Data.Maybe                  (mapMaybe)
import Distribution.Simple.Setup   (fromFlag, fromFlagOrDefault)
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
import Sortie.Project              (Project(..))
import Sortie.Project.Parse
    ( findAndParseProjectFile, findProjectDirectory, showProject )
import Sortie.Utils                (notice)
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
deployAction DeployFlags{deployTag} envs
             ctx@Context{project =
                             Project{environments, version}} =
    do { when (null envs) $ die $ "must specify at least one environment"
       ; unless (null unknownEnvs) $ die $
                    "unrecognized environments " ++ intercalate ", " unknownEnvs
       ; mapM_ (deploy ctx deployVersion) $
               mapMaybe (`Map.lookup` environments) targetEnvs
       }
    where { targetEnvs    = map toLower <$> envs
          ; unknownEnvs   = targetEnvs \\ Map.keys environments
          ; deployVersion = fromFlagOrDefault version deployTag
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
          ; CommandReadyToGo action    -> do { ctx <- setupContext fl
                                             ; printDryRun ctx
                                             ; action ctx }
          }
      }
    where { pr                    = hPutStr stderr
          ; liftFlag              = pure . fromFlag
          ; printCommandHelp help = getProgName >>= pr . help
          ; printOptionsList      = pr . unlines
          ; printErrors           = die . unlines
          ; printVersion          = do { prog <- getProgName
                                       ; pr $ printf "%s version %s\n"
                                            prog (display Paths_sortie.version) }
          ; printDryRun Context{dryRun, verbosity}
              = when dryRun $ notice verbosity "** DRY RUN **\n"
          ; setupContext GlobalFlags{globalVerbosity, globalDryRun}
              = Context <$>
                findProjectDirectory <*>
                findAndParseProjectFile <*>
                liftFlag globalVerbosity <*>
                liftFlag globalDryRun
          }

main :: IO ()
main = topHandler $ getArgs >>= run
