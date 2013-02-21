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
    ( (=^~)
    , MimeType(..)
    , Pattern
    , Verbosity
    , die
    , dieWithLocation
    , elseM
    , info
    , isRight
    , mapButLast
    , mapLast
    , maybeTuple
    , notice
    , parseMaybe
    , readMaybe
    , readCommand
    , readCommand_
    , runProcessSilently
    , setFst
    , setSnd
    , warFileType
    , warn
    , withFileContents
    , writeCommand
    )
where

import Control.Applicative       ((<$>))
import Control.Arrow             (first, second)
import Control.Monad             (when, unless, void)
import Data.Foldable             (Foldable(..))
import Data.List                 (find)
import Data.String               (IsString(..))
import Data.Traversable          (Traversable(..))
import Distribution.Simple.Utils
    ( die, dieWithLocation, warn
    , rawSystemStdInOut, rawSystemStdout
    , withUTF8FileContents )
import Distribution.Verbosity    (Verbosity, normal, verbose)
import System.Exit               (ExitCode(..))
import System.IO                 (IOMode(..), hPutStr, hFlush, stderr, withFile)
import System.Process            (CreateProcess(..), StdStream(UseHandle),
                                  createProcess, proc, waitForProcess)
import Text.Regex.PCRE           ((=~))

instance Foldable    ((,) a) where foldMap f         = snd . fmap f
instance Traversable ((,) a) where traverse f (x, y) = ((,) x) <$> f y

newtype MimeType = MimeType String

infixl 1 `elseM`

elseM :: Monad m => m Bool -> m () -> m ()
c `elseM` m = c >>= flip unless m

newtype Pattern = Pattern String
type Group = String

instance IsString Pattern where fromString = Pattern

(=^~) :: String -> Pattern -> [Group]
str =^~ (Pattern pat) = drop 1 . concat . take 1 $ str =~ pat

warFileType :: MimeType
warFileType = MimeType "application/zip"


---------------------
-- Basic Utilities --
---------------------

parseMaybe :: ReadS a -> String -> Maybe a
parseMaybe p s = fst <$> find (null . snd) (p s)

readMaybe :: Read a => String -> Maybe a
readMaybe = parseMaybe reads

setFst :: c -> (a, b) -> (c, b)
setFst = first . const

setSnd :: c -> (a, b) -> (a, c)
setSnd = second . const

maybeTuple :: [a] -> Maybe (a, a)
maybeTuple [x, y] = Just (x, y)
maybeTuple _      = Nothing

fourth :: (a, b, c, d) -> d
fourth (_,_,_,x) = x

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

mapButLast :: (a -> a) -> [a] -> [a]
mapButLast _ []     = []
mapButLast _ [x]    = [x]
mapButLast f (x:xs) = f x : mapButLast f xs

mapLast :: (a -> a) -> [a] -> [a]
mapLast _ []     = []
mapLast f [x]    = [f x]
mapLast f (x:xs) = x : mapLast f xs

-------------
-- Logging --
-------------

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


-------------------
-- I/O Utilities --
-------------------

withFileContents :: FilePath -> (String -> IO a) -> IO a
withFileContents = withUTF8FileContents

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
