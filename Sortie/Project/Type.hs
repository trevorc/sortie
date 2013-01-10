{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sorties.Project.Type
-- Copyright   :  (c) Bitbase 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  trevor@bitba.se
-- Stability   :  experimental
-- Portability :  portable
--
------------------------------------------------------------------------

module Sortie.Project.Type
    ( Project(..)
    , name
    , version
    )
where

import Control.Lens            (makeLenses)
import Distribution.Package    (PackageName)
import Distribution.Version    (Version)

data Project = Project
    { _name    :: PackageName
    , _version :: Version
    } deriving (Show, Eq)

makeLenses ''Project