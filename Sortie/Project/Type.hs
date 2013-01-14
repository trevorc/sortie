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
-----------------------------------------------------------------------------

module Sortie.Project.Type
    ( Environment(..)
    , Project(..)
    , host
    , user
    , databaseName
    , databaseUser
    , databasePassword
    , installScript
    , name
    , version
    , repository
    , s3Bucket
    , s3KeyPrefix
    , environments
    )
where

import Control.Lens            (makeLenses)
import Distribution.Package    (PackageName)
import Distribution.Version    (Version)

data Environment = Environment
     { _host             :: String
     , _user             :: String
     , _databaseName     :: String
     , _databaseUser     :: String
     , _databasePassword :: String
     , _installScript    :: String
     } deriving (Show, Eq)

data Project = Project
    { _name         :: PackageName
    , _version      :: Version
    , _repository   :: String
    , _s3Bucket     :: String
    , _s3KeyPrefix  :: String
    , _environments :: [Environment]
    } deriving (Show, Eq)

makeLenses ''Environment
makeLenses ''Project