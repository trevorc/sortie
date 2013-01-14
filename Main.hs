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
import Distribution.Simple.Command (CommandParse(..), CommandUI(..), Command,
                                    commandsRun, commandAddAction, option)
import Distribution.Simple.Setup   (Flag(..), fromFlag, trueArg, falseArg)
import Distribution.Simple.Utils   (die, topHandler)
import Distribution.Text           (display)
import System.Environment          (getArgs, getProgName)
import Text.Printf                 (printf)

import qualified Paths_sortie      (version)

data GlobalFlags = GlobalFlags
    { globalVersion :: Flag Bool
    }

type Action flags = flags -> [String] -> IO ()

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags
    = GlobalFlags {
        globalVersion = Flag False
      }

globalCommand :: CommandUI GlobalFlags
globalCommand
    = CommandUI {
        commandName = ""
      , commandSynopsis = ""
      , commandUsage = const $ "This program is Sortie, the simple, " ++
                       "bespoke deployment tool."
      , commandDescription =
          Just $ \prog -> "For help with a particular command, run:\n" ++
                          "  " ++ prog ++ " COMMAND --help\n\n"
      , commandDefaultFlags = defaultGlobalFlags
      , commandOptions =
          const [ option "V" ["version"]
                  "show version information"
                  globalVersion (\v flags -> flags { globalVersion = v })
                  trueArg
                ]
      }

type ReleaseFlags = Flag Bool

releaseCommand :: CommandUI ReleaseFlags
releaseCommand
    = CommandUI {
        commandName         = "release"
      , commandSynopsis     = "Cuts a new tagged release."
      , commandUsage        = noArgsUsage "release"
      , commandDefaultFlags = Flag False
      , commandDescription  =
          Just . const $
                   "Tags HEAD with the project version; creates " ++
                   "the deployment artifact; uploads the deployment artifact"
      , commandOptions =
          const [ option "n" ["dry-run"]
                  "perform no action; only show what would be done"
                  id const falseArg
                ]
      }

releaseAction :: Action ReleaseFlags
releaseAction = undefined

deployCommand :: CommandUI ()
deployCommand
    = CommandUI {
        commandName         = "deploy"
      , commandSynopsis     = "Deploys the project to an environment"
      , commandUsage        = printf "Usage: %s deploy [VERSION] [ENVIRONMENTS]"
      , commandDefaultFlags = ()
      , commandDescription  =
          Just . const $
                   "Deploys the specified version, or the version name " ++
                   "in the project file, to the given environment."
      , commandOptions = const []
      }

deployAction :: Action ()
deployAction = undefined

migrateCommand :: CommandUI ()
migrateCommand
    = CommandUI {
        commandName         = "migrate"
      , commandSynopsis     = "Run the database migrations."
      , commandUsage        = printf "Usage: %s migrate [ENVIRONMENTS]"
      , commandDefaultFlags = ()
      , commandDescription  =
          Just . const $
                   "Migrate the database in the named environments " ++
                   "or locally if none are given. Sets up the database if if " ++
                   "has not yet been created"
      , commandOptions = const []
      }

migrateAction :: Action ()
migrateAction = undefined

noArgsUsage :: String -> String -> String
noArgsUsage = printf "Usage: %s %s [FLAGS]"

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
