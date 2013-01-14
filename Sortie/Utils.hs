-----------------------------------------------------------------------------
-- |
-- Module      :  Sortie.Utils
-- Copyright   :  (c) Bitbase 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  trevor@bitba.se
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Sortie.Utils
    ( die
    , dieWithLocation
    , readMaybe
    , readCommand
    , readCommand_
    , runProcessSilently
    )
where

import Control.Applicative       ((<$>))
import Control.Lens              (_4, view)
import Control.Monad             (void)
import Data.Foldable             (Foldable(..))
import Data.Traversable          (Traversable(..))
import Distribution.Simple.Utils (die, dieWithLocation, rawSystemStdout)
import Distribution.Verbosity    (normal)
import System.IO                 (IOMode(..), withFile)
import System.Exit               (ExitCode(..))
import System.Process            (CreateProcess(..), StdStream(UseHandle),
                                  createProcess, proc, waitForProcess)

instance Foldable    ((,) a) where foldMap f         = snd . fmap f
instance Traversable ((,) a) where traverse f (x, y) = ((,) x) <$> f y

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                { [(x, "")] -> Just x
                ; _         -> Nothing
                }

runProcessSilently :: FilePath -> [String] -> IO ExitCode
runProcessSilently cmd args =
    withFile "/dev/null" ReadWriteMode $ \nul -> do
      { ph <- view _4 <$> createProcess (proc cmd args)
              { std_err = UseHandle nul
              , std_in  = UseHandle nul
              , std_out = UseHandle nul
              }
      ; waitForProcess ph
      }

readCommand :: FilePath -> [String] -> IO String
readCommand = rawSystemStdout normal

readCommand_ :: FilePath -> [String] -> IO ()
readCommand_ = (void .) . readCommand

