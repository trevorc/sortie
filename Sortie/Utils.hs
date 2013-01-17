{-# OPTIONS_GHC -fno-warn-orphans #-}
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
    ( MimeType(..)
    , Verbosity
    , die
    , dieWithLocation
    , info
    , notice
    , parseMaybe
    , readMaybe
    , readCommand
    , readCommand_
    , runProcessSilently
    , warFileType
    )
where

import Control.Applicative       ((<$>))
import Control.Lens              (_4, view)
import Data.List                 (find)
import Control.Monad             (when, void)
import Data.Foldable             (Foldable(..))
import Data.Traversable          (Traversable(..))
import Distribution.Simple.Utils (die, dieWithLocation, rawSystemStdout)
import Distribution.Verbosity    (Verbosity, normal, verbose)
import System.IO                 (IOMode(..), hPutStr, hFlush, stderr, withFile)
import System.Exit               (ExitCode(..))
import System.Process            (CreateProcess(..), StdStream(UseHandle),
                                  createProcess, proc, waitForProcess)

instance Foldable    ((,) a) where foldMap f         = snd . fmap f
instance Traversable ((,) a) where traverse f (x, y) = ((,) x) <$> f y

newtype MimeType = MimeType String

warFileType :: MimeType
warFileType = MimeType "application/zip"

emitLog :: Verbosity -> Verbosity -> String -> IO ()
emitLog minVerbosity verbosity msg =
    when (verbosity >= minVerbosity) $ do
      { hPutStr stderr msg
      ; hFlush stderr
      }

notice :: Verbosity -> String -> IO ()
notice = emitLog normal

info :: Verbosity -> String -> IO ()
info = emitLog verbose

parseMaybe :: ReadS a -> String -> Maybe a
parseMaybe p s = fst <$> find (null . snd) (p s)

readMaybe :: Read a => String -> Maybe a
readMaybe = parseMaybe reads

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
