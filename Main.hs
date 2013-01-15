{-# LANGUAGE RecordWildCards #-}
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
import Control.Monad               (when)
import Distribution.Simple.Setup   (fromFlag)
import Distribution.Simple.Utils   (die, topHandler)
import Distribution.Text           (display)
import System.Environment          (getArgs, getProgName)
import System.IO                   (hPutStr, stderr)
import Text.Printf                 (printf)
import Distribution.Simple.Command
    ( CommandParse(..), commandsRun, commandAddAction )

import Sortie.Command
    ( Action , Command
    , GlobalFlags(..),  globalCommand , ReleaseFlags(..), releaseCommand
    , DeployFlags(..),  deployCommand , MigrateFlags(..), migrateCommand
    , ShowFlags(..),    showCommand
    )
import Sortie.Command.Release      (release)
import Sortie.Context              (Context(Context))
import Sortie.Project.Parse
    ( findAndParseProjectFile, findProjectDirectory, showProject )
import qualified Sortie.Context as Context (projectDirectory, project, verbosity)
import qualified Paths_sortie (version)

guardNoExtraArgs :: String -> [String] -> IO ()
guardNoExtraArgs name args =
    when (length args > 0) $ die $
             "the `" ++ name ++"' command does not accept any " ++
             "positional arguments: " ++ unwords args

releaseAction :: Action ReleaseFlags
releaseAction (ReleaseFlags dryRun) args Context{..} =
    guardNoExtraArgs "release" args >>
    release project projectDirectory (fromFlag dryRun)

deployAction :: Action DeployFlags
deployAction = undefined

migrateAction :: Action MigrateFlags
migrateAction = undefined

showAction :: Action ShowFlags
showAction _flags args Context{..} = guardNoExtraArgs "show" args >>
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
    where { pr = hPutStr stderr
          ; printCommandHelp help = getProgName >>= pr . help
          ; printOptionsList      = pr . unlines
          ; printErrors           = die . unlines
          ; printVersion          = do { prog <- getProgName
                                       ; pr $ printf "%s version %s\n"
                                            prog (display Paths_sortie.version)
                                       }
          ; setupContext flags    = Context <$>
                                    findProjectDirectory <*>
                                    findAndParseProjectFile <*>
                                    (pure . fromFlag $ globalVerbosity flags)
          }

main :: IO ()
main = topHandler $ getArgs >>= run
