{-# LANGUAGE OverloadedStrings #-}
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
    ( AwsToken(..), fromAwsToken
    , Bucket(..), fromBucket
    , Project(..), emptyProject
    , Environment(..), emptyEnvironment
    , getPackageName
    )
where

import Distribution.Package    (PackageName(PackageName))
import Distribution.Version    (Version(Version))
import qualified Data.Map as Map

import Sortie.Project.Type

emptyEnvironment :: Environment
emptyEnvironment
    = Environment
      { host             = ""
      , execUser         = ""
      , databaseName     = ""
      , databaseUser     = ""
      , databasePassword = ""
      , installScript    = ("", [])
      }

emptyProject :: Project
emptyProject
    = Project
       { projectName        = PackageName ""
       , version            = Version [] []
       , repository         = ""
       , s3Bucket           = Bucket ""
       , s3KeyPrefix        = ""
       , awsAccessKeyId     = Nothing
       , awsSecretAccessKey = Nothing
       , environments       = Map.empty
       }
