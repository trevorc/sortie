{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sortie.Command.Release
-- Copyright   :  (c) Bitbase 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  trevor@bitba.se
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Sortie.S3
    ( connection
    , hasKey
    , putFile
    )
where

import Control.Applicative       ((<$>), (<*>), liftA2)
import Control.Monad             ((>=>), unless)
import Network.AWS.AWSConnection (AWSConnection, amazonS3Connection)
import Network.AWS.AWSResult     (prettyReqError)
import Network.AWS.S3Object
    ( S3Object(S3Object), getObjectInfo, sendObject )
import System.FilePath           ((</>), takeFileName)
import System.Posix              (getEnv)
import System.IO                 (IOMode(ReadMode), withBinaryFile)
import Text.Printf               (printf)
import qualified Data.ByteString.Lazy as B
    ( hGetContents )

import Sortie.Context            (Context(..))
import Sortie.Project            (Bucket(Bucket), Project(..))
import Sortie.Utils              (MimeType(..), die, isRight, notice)

connection :: IO (Maybe AWSConnection)
connection = liftA2 amazonS3Connection <$>
             getEnv "AWS_ACCESS_KEY_ID" <*>
             getEnv "AWS_SECRET_ACCESS_KEY"

connectToS3 :: IO AWSConnection
connectToS3 = connection >>= maybe (die "aws keys not found") return

hasKey :: Bucket -> String -> IO Bool
hasKey (Bucket bucket) key =
    do { conn <- connectToS3
       ; fmap isRight <$>
         getObjectInfo conn $ S3Object bucket key "" [] ""
       }

putFile :: Context            -- ^ Project execution context.
        -> MimeType           -- ^ Mime type
        -> FilePath           -- ^ Path to file to upload
        -> IO ()
putFile Context{dryRun, verbosity, project =
                    Project{s3Bucket = (Bucket bucket), s3KeyPrefix}
               } (MimeType mime) path =
    do { conn <- connectToS3
       ; notice verbosity $ printf "uploading %s -> s3://%s/%s..." path bucket key
       ; unless dryRun $ withBinaryFile path ReadMode $
                B.hGetContents >=>
                sendObject conn . toObject >=>
                either uploadFailure return
       ; notice verbosity "done.\n"
       }
    where { toObject         = S3Object bucket key mime []
          ; key              = s3KeyPrefix </> takeFileName path
          ; uploadFailure    = die . ("S3 upload failure: "++) . prettyReqError
          }
