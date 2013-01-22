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
    ( Bucket(..)
    , Environment(..)
    , Project(..)
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
    )
where

import Control.Applicative     ((<$>))
import Control.Lens            (makeLenses)
import Data.Map                (Map)
import Distribution.Package    (PackageName)
import Distribution.ParseUtils (parseTokenQ, showToken)
import Distribution.Text       (Text(..))
import Distribution.Version    (Version)

newtype Bucket = Bucket String
    deriving Eq

instance Text Bucket where
    parse = Bucket <$> parseTokenQ
    disp (Bucket b) = showToken b

fromBucket :: Bucket -> String
fromBucket (Bucket b) = b

data Environment = Environment
     { _host             :: String
     , _user             :: String
     , _databaseName     :: String
     , _databaseUser     :: String
     , _databasePassword :: String
     , _installScript    :: String
     } deriving Eq

data Project = Project
    { _name         :: PackageName
    , _version      :: Version
    , _repository   :: String
    , _s3Bucket     :: Bucket
    , _s3KeyPrefix  :: String
    , _environments :: Map String Environment
    } deriving Eq

makeLenses ''Environment
makeLenses ''Project