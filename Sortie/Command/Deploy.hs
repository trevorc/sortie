{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sortie.Command.Deploy
-- Copyright   :  (c) Bitbase 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  trevor@bitba.se
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Sortie.Command.Deploy
    ( deploy )
where

import Control.Applicative     ((<$>))
import Control.Lens            ((^.))
import Control.Monad           (unless)
import Data.Version            (showVersion)
import Distribution.Version    (Version)
import System.FilePath         ((</>))
import Text.Printf             (printf)

import Sortie.Context          (Context(..))
import Sortie.Leiningen        (artifactFileName)
import Sortie.Project          (Environment, fromBucket)
import Sortie.SourceControl    (listTags, versionToTag)
import Sortie.Project
    ( s3KeyPrefix, s3Bucket, name, host, installScript, user )
import Sortie.Utils
    ( die, getPackageName, elseM, notice
    , withFileContents, writeCommand )
import qualified Sortie.S3 as S3
    ( hasKey )

ssh :: Context -> Environment -> [(String, String)] -> IO ()
ssh Context{verbosity, dryRun, projectDirectory} env envVars =
    do { notice verbosity $ printf "running `%s' on `%s'\n"
                    (unwords args) (env ^. host)
       ; unless dryRun $ withFileContents script $
                writeCommand verbosity "ssh" (env ^. host : args)
       }
    where { args = [ "sudo", "-u", env ^. user, "env" ] ++ envArgs ++
                   [ "sh", "-" ]
          ; envArgs = [ printf "%s=%s" varName (show val)
                      | (varName, val) <- envVars ]
          ; script = projectDirectory </> env ^. installScript
          }

deploy :: Context               -- | Execution context.
       -> Version               -- | Version to deploy.
       -> Environment           -- | Target deployment environment.
       -> IO ()
deploy ctx@Context{project} version env =
    do { elem tagName <$> listTags `elseM` unknownVersion "git"
       ; S3.hasKey bucket s3Key    `elseM` unknownVersion "S3"
       ; ssh ctx env envVars
       }
    where { tagName = versionToTag version
          ; bucket  = project ^. s3Bucket
          ; s3Key   = project ^. s3KeyPrefix </> artifactFileName project

          ; envVars = [ ("SORTIE_PROJECT_NAME",    getPackageName $ project ^. name)
                      , ("SORTIE_PROJECT_VERSION", showVersion version)
                      , ("SORTIE_S3_KEY_PREFIX",   project ^. s3KeyPrefix)
                      , ("SORTIE_ARTIFACT_NAME",   artifactFileName project)
                      , ("SORTIE_S3_BUCKET",       fromBucket bucket)
                      ]

          ; unknownVersion repo
              = die $ printf "unknown version in %s: %s \
                              \(try `sortie release' first)"
                              repo (showVersion version)
          }
