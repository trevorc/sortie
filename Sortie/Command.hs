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

module Sortie.Command
    ( Action
    , Command
    , GlobalFlags(..)
    , ReleaseFlags(..)
    , DeployFlags(..)
    , MigrateFlags(..)
    , globalCommand
    , releaseCommand
    , deployCommand
    , migrateCommand
    )
where

import Distribution.Simple.Command (CommandUI(..), Command, option)
import Distribution.Simple.Setup   (Flag(..), trueArg, falseArg)
import Text.Printf                 (printf)

data GlobalFlags = GlobalFlags
    { globalVersion :: Flag Bool
    }

type Action flags = flags -> [String] -> IO ()

noArgsUsage :: String -> String -> String
noArgsUsage = printf "Usage: %s %s [FLAGS]"

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

newtype ReleaseFlags = ReleaseFlags
    { getReleaseFlags :: Flag Bool }

releaseCommand :: CommandUI ReleaseFlags
releaseCommand
    = CommandUI {
        commandName         = "release"
      , commandSynopsis     = "Cuts a new tagged release."
      , commandUsage        = noArgsUsage "release"
      , commandDefaultFlags = ReleaseFlags (Flag False)
      , commandDescription  =
          Just . const $
                   "Tags HEAD with the project version; creates " ++
                   "the deployment artifact; uploads the deployment artifact.\n"
      , commandOptions =
          const [ option "n" ["dry-run"]
                  "perform no action; only show what would be done"
                  getReleaseFlags (const . ReleaseFlags)
                  falseArg
                ]
      }

newtype DeployFlags = DeployFlags ()

deployCommand :: CommandUI DeployFlags
deployCommand
    = CommandUI {
        commandName         = "deploy"
      , commandSynopsis     = "Deploys the project to an environment."
      , commandUsage        = printf "Usage: %s deploy [VERSION] [ENVIRONMENTS]"
      , commandDefaultFlags = DeployFlags ()
      , commandDescription  =
          Just . const $
                   "Deploys the specified version, or the version name " ++
                   "in the project file, to the given environment.\n"
      , commandOptions = const []
      }

newtype MigrateFlags = MigrateFlags ()

migrateCommand :: CommandUI MigrateFlags
migrateCommand
    = CommandUI {
        commandName         = "migrate"
      , commandSynopsis     = "Run the database migrations."
      , commandUsage        = printf "Usage: %s migrate [ENVIRONMENTS]"
      , commandDefaultFlags = MigrateFlags ()
      , commandDescription  =
          Just . const $
                   "Migrate the database in the named environments " ++
                   "or locally if none are given. Sets up the database if it " ++
                   "has not yet been created.\n"
      , commandOptions = const []
      }