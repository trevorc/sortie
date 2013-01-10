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
------------------------------------------------------------------------


module Main
    (main)
where

import Sortie.Project.Parse (findAndParseProjectFile, showProject)

main :: IO ()
main = findAndParseProjectFile >>= putStrLn . showProject