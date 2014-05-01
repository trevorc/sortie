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
    ( getCredentials
    , hasKey
    , putFile
    )
where

import Blaze.ByteString.Builder  (fromByteString)
import Control.Applicative       ((<$>), (<*>), pure)
import Control.Monad             (mplus, unless, void)
import Crypto.Hash.MD5           (hash)
import Data.ByteString           (hGetContents)
import Data.Conduit              (($=))
import Data.Conduit.Binary       (sourceFile)
import Data.Functor              ((<$))
import Data.IORef                (newIORef)
import Data.Maybe                (isJust)
import Data.Text                 (Text)
import Data.Text.Encoding        (decodeUtf8)
import Network.HTTP.Conduit      (requestBodySource)
import System.FilePath           ((</>), takeFileName)
import System.IO                 (IOMode(ReadMode), hFileSize, withBinaryFile)
import Text.Printf               (printf)

import Aws ( Configuration(..), Credentials, NormalQuery
           , LogLevel(Warning), TimeInfo(Timestamp)
           , HeaderException(..)
           , defaultLog, simpleAws )
import qualified Aws
import qualified Aws.S3 as S3
import qualified Control.Exception as E
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text (filter, pack, unpack)
import qualified Data.ByteString.Char8 as ByteString (pack)
import qualified Data.Conduit.List as CL (map)

import Sortie.Context            (Context(..))
import Sortie.Project
    ( fromAwsToken
    , Bucket(Bucket), fromBucket
    , Project(..) )
import Sortie.Utils              (MimeType(..), die, info, notice)

getCredentials :: Context -> IO (Maybe Credentials)
getCredentials Context{project = Project{awsAccessKeyId, awsSecretAccessKey}} =
    do ref <- newIORef []
       envCreds <- Aws.loadCredentialsFromEnv
       return $ (ctxCreds <*> pure ref) `mplus` envCreds
    where ctxCreds = Aws.Credentials <$> fmap fromAwsToken awsAccessKeyId
                                     <*> fmap fromAwsToken awsSecretAccessKey

s3Config :: S3.S3Configuration NormalQuery
s3Config = Aws.defServiceConfig

connectToS3 :: Context -> IO Configuration
connectToS3 ctx = getCredentials ctx >>=
                  maybe (die "aws keys not found") (return . configure)
    where configure credentials = Configuration {
                                    credentials
                                  , logger = defaultLog Warning
                                  , timeInfo = Timestamp}

hasKey :: Context -> Bucket -> Text -> IO Bool
hasKey ctx bucket key = isJust <$> getKeyETag ctx bucket key

getKeyETag :: Context -> Bucket -> Text -> IO (Maybe Text)
getKeyETag ctx@Context{verbosity} (Bucket bucket) key =
    getETag `E.catch` \HeaderException{headerErrorMessage} ->
        Nothing <$ info verbosity (headerErrorMessage ++ "...")
    where getETag = do
            { config <- connectToS3 ctx
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
isUpToDate ctx@Context{verbosity} path s3Bucket key = do
    { md5 <- withBinaryFile path ReadMode $ \h ->
             decodeUtf8 . Base16.encode . hash <$> hGetContents h
    ; info verbosity $ printf "md5 %s..." (Text.unpack md5)
    ; etag <- fmap (Text.filter (/= '"')) <$> getKeyETag ctx s3Bucket key
    ; info verbosity $ printf "etag %s..." $ maybe "(null)" Text.unpack etag
    ; return $ maybe False (== md5) etag
    }

putObject :: Context -> FilePath -> MimeType -> Bucket -> Text -> IO ()
putObject ctx@Context{verbosity} path (MimeType mime) (Bucket bucket) key =
    do { config <- connectToS3 ctx
       ; size <- fromInteger <$> withBinaryFile path ReadMode hFileSize
       ; info verbosity $ printf "size %s..." (show size)
       ; let { requestBody = requestBodySource size $ sourceFile path
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
