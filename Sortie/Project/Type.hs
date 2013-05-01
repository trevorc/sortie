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
    ( AwsToken(..), fromAwsToken
    , Bucket(..), fromBucket
    , Environment(..)
    , Project(..)
    , getPackageName
    )
where

import Control.Applicative     ((<$>))
import Data.ByteString         (ByteString)
import Data.Map                (Map)
import Distribution.Package    (PackageName(PackageName))
import Distribution.ParseUtils (parseTokenQ, showToken)
import Distribution.Text       (Text(..))
import Distribution.Version    (Version)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import qualified Data.Text as T (Text, pack, unpack)

newtype Bucket = Bucket T.Text
    deriving Eq

instance Text Bucket where
    parse = Bucket . T.pack <$> parseTokenQ
    disp (Bucket b) = showToken $ T.unpack b

newtype AwsToken = AwsToken ByteString
    deriving Eq

instance Text AwsToken where
    parse = AwsToken . C.pack <$> parseTokenQ
    disp (AwsToken t) = showToken $ C.unpack t

getPackageName :: PackageName -> String
getPackageName (PackageName name) = name

fromBucket :: Bucket -> String
fromBucket (Bucket b) = T.unpack b

fromAwsToken :: AwsToken -> ByteString
fromAwsToken (AwsToken t) = t

data Environment = Environment
     { host             :: String
     , execUser         :: String
     , databaseName     :: String
     , databaseUser     :: String
     , databasePassword :: String
     , installScript    :: (FilePath, [String])
     } deriving Eq

data Project = Project
    { projectName        :: PackageName
    , version            :: Version
    , repository         :: String
    , s3Bucket           :: Bucket
    , s3KeyPrefix        :: String
    , awsAccessKeyId     :: Maybe AwsToken
    , awsSecretAccessKey :: Maybe AwsToken
    , environments       :: Map String Environment
    } deriving Eq
