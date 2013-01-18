-----------------------------------------------------------------------------
-- |
-- Module      :  Sortie.Context
-- Copyright   :  (c) Bitbase 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  trevor@bitba.se
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Sortie.Context
    ( Context(..)
    , GlobalFlags(..)
    )
where

import Distribution.Simple.Setup   (Flag)
import Distribution.Verbosity      (Verbosity)

import Sortie.Project              (Project)

data GlobalFlags = GlobalFlags
    { globalVersion   :: Flag Bool
    , globalVerbosity :: Flag Verbosity
    , globalDryRun    :: Flag Bool
    }

data Context = Context
    { projectDirectory :: FilePath
    , project          :: Project
    , verbosity        :: Verbosity
    , dryRun           :: Bool
    }
