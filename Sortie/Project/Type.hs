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
    , fromBucket
    , getPackageName
    )
where

import Control.Applicative     ((<$>))
import Data.Map                (Map)
import Distribution.Package    (PackageName(PackageName))
import Distribution.ParseUtils (parseTokenQ, showToken)
import Distribution.Text       (Text(..))
import Distribution.Version    (Version)

newtype Bucket = Bucket String
    deriving Eq

instance Text Bucket where
    parse = Bucket <$> parseTokenQ
    disp (Bucket b) = showToken b

getPackageName :: PackageName -> String
getPackageName (PackageName name) = name

fromBucket :: Bucket -> String
fromBucket (Bucket b) = b

data Environment = Environment
     { host             :: String
     , execUser         :: String
     , databaseName     :: String
     , databaseUser     :: String
     , databasePassword :: String
     , installScript    :: (FilePath, [String])
     } deriving Eq

data Project = Project
    { projectName  :: PackageName
    , version      :: Version
    , repository   :: String
    , s3Bucket     :: Bucket
    , s3KeyPrefix  :: String
    , environments :: Map String Environment
    } deriving Eq
