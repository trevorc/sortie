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

import Blaze.ByteString.Builder  (fromByteString)
import Control.Applicative       ((<$>))
import Control.Monad             (unless, void)
import Crypto.Hash.MD5           (hash)
import Data.ByteString           (hGetContents)
import Data.Conduit              (($=))
import Data.Conduit.Binary       (sourceFile)
import Data.Maybe                (isJust)
import Data.Text                 (Text)
import Data.Text.Encoding        (decodeUtf8)
import Network.HTTP.Conduit      (RequestBody(RequestBodySource))
import System.FilePath           ((</>), takeFileName)
import System.IO                 (IOMode(ReadMode), hFileSize, withBinaryFile)
import Text.Printf               (printf)

import Aws ( Configuration(..), Credentials, NormalQuery
           , LogLevel(Warning), TimeInfo(Timestamp), defaultLog, simpleAws )
import qualified Aws
import qualified Aws.S3 as S3
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text (filter, pack, unpack)
import qualified Data.ByteString.Char8 as ByteString (pack)
import qualified Data.Conduit.List as CL (map)

import Sortie.Context            (Context(..))
import Sortie.Project            (Bucket(Bucket), Project(..), fromBucket)
import Sortie.Utils              (MimeType(..), die, info, notice)

connection :: IO (Maybe Credentials)
connection = Aws.loadCredentialsFromEnv

s3Config :: S3.S3Configuration NormalQuery
s3Config = Aws.defServiceConfig

connectToS3 :: IO Configuration
connectToS3 = connection >>= maybe (die "aws keys not found") (return . configure)
    where configure credentials = Configuration {
                                    credentials
                                  , logger = defaultLog Warning
                                  , timeInfo = Timestamp}

hasKey :: Bucket -> Text -> IO Bool
hasKey bucket key = isJust <$> getKeyETag bucket key

getKeyETag :: Bucket -> Text -> IO (Maybe Text)
getKeyETag (Bucket bucket) key = do
  { config <- connectToS3
  ; S3.HeadObjectMemoryResponse (S3.ObjectMetadata{S3.omETag})
      <- simpleAws config s3Config $ S3.headObject bucket key
  ; return (Just omETag)
  }

isUpToDate :: Context           -- ^ Project execution context
           -> FilePath          -- ^ Filesystem path to artifact
           -> Bucket            -- ^ S3 bucket
           -> Text              -- ^ Key within S3 bucket to uploaded
                                --   artifact
           -> IO Bool
isUpToDate Context{verbosity} path s3Bucket key = do
    { md5 <- withBinaryFile path ReadMode $ \h ->
             decodeUtf8 . Base16.encode . hash <$> hGetContents h
    ; info verbosity $ printf "md5 %s..." (Text.unpack md5)
    ; etag <- fmap (Text.filter (/= '"')) <$> getKeyETag s3Bucket key
    ; info verbosity $ printf "etag %s..." $ maybe "(null)" Text.unpack etag
    ; return $ maybe False (== md5) etag
    }

putObject :: Context -> FilePath -> MimeType -> Bucket -> Text -> IO ()
putObject Context{verbosity} path (MimeType mime) (Bucket bucket) key =
    do { config <- connectToS3
       ; size <- fromInteger <$> withBinaryFile path ReadMode hFileSize
       ; info verbosity $ printf "size %s..." (show size)
       ; let { requestBody = RequestBodySource size $
                             sourceFile path $= CL.map fromByteString
             ; por = S3.putObject bucket key requestBody
             ; poContentType = Just $ ByteString.pack mime
             }
       ; void $ Aws.simpleAws config s3Config por {S3.poContentType}
       }

putFile :: Context            -- ^ Project execution context.
        -> MimeType           -- ^ Mime type
        -> FilePath           -- ^ Path to file to upload
        -> IO ()
putFile ctx@Context{dryRun, verbosity, project =
                        Project{s3Bucket, s3KeyPrefix}
                   } mime path =
    do { notice verbosity $ printf "uploading %s -> s3://%s/%s..."
                path (fromBucket s3Bucket) (Text.unpack key)
       ; upToDate <- isUpToDate ctx path s3Bucket key
       ; unless (dryRun || upToDate) $ putObject ctx path mime s3Bucket key
       ; notice verbosity $ if upToDate then "(up-to-date).\n" else "done.\n"
       }
    where key = Text.pack $ s3KeyPrefix </> takeFileName path
