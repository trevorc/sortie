-----------------------------------------------------------------------------
-- |
-- Module      :  Sortie.Project
-- Copyright   :  (c) Bitbase 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  trevor@bitba.se
-- Stability   :  experimental
-- Portability :  portable
--
------------------------------------------------------------------------

module Sortie.Project
    ( Project()
    , name
    , version
    , emptyProject
    )
where

import Control.Lens
import Sortie.Project.Type
import Distribution.Package    (PackageName(PackageName))
import Distribution.Version    (Version(Version))

emptyProject :: Project
emptyProject
    = Project
      { _name    = PackageName ""
      , _version = Version [] []
      }
