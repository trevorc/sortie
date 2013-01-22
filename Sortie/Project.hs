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
-----------------------------------------------------------------------------

module Sortie.Project
    ( Bucket(..)
    , Environment
    , Project
    , host
    , user
    , databaseName
    , databaseUser
    , databasePassword
    , fromBucket
    , installScript
    , name
    , version
    , repository
    , s3Bucket
    , s3KeyPrefix
    , environments
    , emptyProject
    , emptyEnvironment
    )
where

import Distribution.Package    (PackageName(PackageName))
import Distribution.Version    (Version(Version))
import qualified Data.Map as Map

import Sortie.Project.Type

emptyEnvironment :: Environment
emptyEnvironment
    = Environment
      { _host             = ""
      , _user             = ""
      , _databaseName     = ""
      , _databaseUser     = ""
      , _databasePassword = ""
      , _installScript    = ""
      }

emptyProject :: Project
emptyProject
    = Project
       { _name         = PackageName ""
       , _version      = Version [] []
       , _repository   = ""
       , _s3Bucket     = Bucket ""
       , _s3KeyPrefix  = ""
       , _environments = Map.empty
       }
