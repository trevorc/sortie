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

import Sortie.Project.Parse
import Sortie.SourceControl
import Distribution.Simple.Utils

main :: IO ()
main = topHandler $
       getChangedFiles >>= mapM_ print >>
       findAndParseProjectFile >>= putStrLn . showProject
