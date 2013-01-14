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

import System.IO (hPutStr, stderr)
import Distribution.Simple.Command (CommandParse(..),
                                    commandsRun, commandAddAction)
import Distribution.Simple.Setup   (fromFlag)
import Distribution.Simple.Utils   (die, topHandler)
import Distribution.Text           (display)
import System.Environment          (getArgs, getProgName)
import Text.Printf                 (printf)

import Sortie.Command
    ( Action , Command
    , GlobalFlags(..),  globalCommand , ReleaseFlags(..), releaseCommand
    , DeployFlags(..),  deployCommand , MigrateFlags(..), migrateCommand
    )
import qualified Paths_sortie      (version)


releaseAction :: Action ReleaseFlags
releaseAction = undefined

deployAction :: Action DeployFlags
deployAction = undefined

migrateAction :: Action MigrateFlags
migrateAction = undefined

commands :: [Command (IO ())]
commands = [ commandAddAction releaseCommand  releaseAction
           , commandAddAction deployCommand   deployAction
           , commandAddAction migrateCommand  migrateAction
           ]

run :: [String] -> IO ()
run args =
    case commandsRun globalCommand commands args of
      { CommandHelp   help          -> printCommandHelp help
      ; CommandList   opts          -> printOptionsList opts
      ; CommandErrors errs          -> printErrors errs
      ; CommandReadyToGo (GlobalFlags{..}, commandParse) ->
        case commandParse of
          { _ | fromFlag globalVersion -> printVersion
          ; CommandHelp      help      -> printCommandHelp help
          ; CommandList      opts      -> printOptionsList opts
          ; CommandErrors    errs      -> printErrors errs
          ; CommandReadyToGo action    -> action
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
          }


main :: IO ()
main = topHandler $ getArgs >>= run
