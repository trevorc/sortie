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
    ( Bucket(..)
    , connection
    , putFile )
where

import Control.Applicative       ((<$>), (<*>), liftA2)
import Control.Monad             ((>=>), unless)
import Distribution.Verbosity    (Verbosity)
import Network.AWS.AWSConnection (AWSConnection, amazonS3Connection)
import Network.AWS.AWSResult     (prettyReqError)
import Network.AWS.S3Object
    ( S3Object(S3Object), sendObject )
import System.FilePath           ((</>), takeFileName)
import System.Posix              (getEnv)
import System.IO                 (IOMode(ReadMode), withBinaryFile)
import Text.Printf               (printf)
import qualified Data.ByteString.Lazy as B
    ( hGetContents )

import Sortie.Utils              (MimeType(..), die, notice)

newtype Bucket = Bucket String

connection :: IO (Maybe AWSConnection)
connection = liftA2 amazonS3Connection <$>
             getEnv "AWS_ACCESS_KEY_ID" <*>
             getEnv "AWS_SECRET_ACCESS_KEY"

putFile :: Verbosity            -- | Verbosity
        -> Bool                 -- | Dry run (perform no action)
        -> Bucket               -- | S3 bucket name
        -> String               -- | S3 key prefix
        -> MimeType             -- | Mime type
        -> FilePath             -- | Path to file to upload
        -> IO ()
putFile verbosity dryRun (Bucket bucket) keyPrefix (MimeType mime) path =
    do { conn <- maybe missingKeysError return =<< connection
       ; notice verbosity $ printf "uploading %s -> s3://%s/%s..." path bucket key
       ; unless dryRun $ withBinaryFile path ReadMode $
                B.hGetContents >=>
                sendObject conn . toObject >=>
                either uploadFailure return
       ; notice verbosity "done.\n"
       }
    where { toObject         = S3Object bucket key mime []
          ; key              = keyPrefix </> takeFileName path
          ; uploadFailure    = die . ("S3 upload failure: "++) . prettyReqError
          ; missingKeysError = die "aws keys not found"
          }