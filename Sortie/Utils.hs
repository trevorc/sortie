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
    ( (=~)
    , MimeType(..)
    , Verbosity
    , die
    , dieWithLocation
    , elseM
    , getPackageName
    , info
    , isRight
    , notice
    , parseMaybe
    , readMaybe
    , readCommand
    , readCommand_
    , runProcessSilently
    , warFileType
    , warn
    , withFileContents
    , writeCommand
    )
where

import Control.Applicative       ((<$>))
import Control.Monad             (when, unless, void)
import Data.Foldable             (Foldable(..))
import Data.List                 (find)
import Data.Traversable          (Traversable(..))
import Distribution.Package      (PackageName(PackageName))
import Distribution.Simple.Utils
    ( die, dieWithLocation, warn
    , rawSystemStdInOut, rawSystemStdout
    , withUTF8FileContents )
import Distribution.Verbosity    (Verbosity, normal, verbose)
import System.Exit               (ExitCode(..))
import System.IO                 (IOMode(..), hPutStr, hFlush, stderr, withFile)
import System.Process            (CreateProcess(..), StdStream(UseHandle),
                                  createProcess, proc, waitForProcess)
import qualified Text.Regex.PCRE as Regex ((=~))

instance Foldable    ((,) a) where foldMap f         = snd . fmap f
instance Traversable ((,) a) where traverse f (x, y) = ((,) x) <$> f y

newtype MimeType = MimeType String

infixl 1 `elseM`

elseM :: Monad m => m Bool -> m () -> m ()
c `elseM` m = c >>= flip unless m

type Match = [String]

(=~) :: String -> String -> [Match]
(=~) = (Regex.=~)

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

fourth :: (a, b, c, d) -> d
fourth (_,_,_,x) = x

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

runProcessSilently :: FilePath -> [String] -> IO ExitCode
runProcessSilently cmd args =
    withFile "/dev/null" ReadWriteMode $ \nul ->
        createProcess (proc cmd args)
                          { std_err = UseHandle nul
                          , std_in  = UseHandle nul
                          , std_out = UseHandle nul } >>=
        waitForProcess . fourth

readCommand :: FilePath -> [String] -> IO String
readCommand = rawSystemStdout normal

readCommand_ :: FilePath -> [String] -> IO ()
readCommand_ = (void .) . readCommand

writeCommand :: Verbosity
             -> FilePath -> [String]
             -> String
             -> IO ()
writeCommand verbosity cmd args input =
    do { (_, errors, status) <- rawSystemStdInOut verbosity cmd args
                                (Just (input, True)) True
       ; unless (status == ExitSuccess) $ die errors
       }


getPackageName :: PackageName -> String
getPackageName (PackageName name) = name

withFileContents :: FilePath -> (String -> IO a) -> IO a
withFileContents = withUTF8FileContents
