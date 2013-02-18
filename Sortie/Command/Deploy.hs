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

import Control.Arrow           (first)
import Control.Applicative     ((<$>))
import Control.Monad           (unless)
import Data.Version            (showVersion)
import Distribution.Version    (Version)
import System.FilePath         ((</>))
import Text.Printf             (printf)

import Sortie.Context          (Context(..))
import Sortie.Leiningen        (artifactFileName)
import Sortie.Project          (Environment(..), Project(..), fromBucket)
import Sortie.SourceControl    (listTags, versionToTag)
import Sortie.Utils
    ( die, getPackageName, elseM, notice
    , withFileContents, writeCommand )
import qualified Sortie.S3 as S3
    ( hasKey )

ssh :: Context -> Environment -> [(String, String)] -> IO ()
ssh Context{verbosity, dryRun, projectDirectory}
    Environment{host, execUser, installScript}
    envVars = do
      { notice verbosity $ printf "running `%s' on `%s'\n"
                   (unwords args) host
      ; unless dryRun $ withFileContents scriptPath $
               writeCommand verbosity "ssh" (host : args)
      }
    where { args = [ "sudo", "-u", execUser, "env" ] ++ envArgs ++
                   [ "sh", "-s", "-" ] ++ scriptArgs
          ; envArgs = [ printf "%s=%s" varName (show val)
                      | (varName, val) <- envVars ]
          ; (scriptPath, scriptArgs)
              = first (projectDirectory </>) installScript
          }

projectEnvVars :: Project -> [(String, String)]
projectEnvVars project@Project{projectName, version, s3Bucket, s3KeyPrefix} =
    first (varNamePrefix++) <$>
              [ ("PROJECT_NAME",    getPackageName $ projectName)
              , ("PROJECT_VERSION", showVersion version)
              , ("S3_KEY_PREFIX",   s3KeyPrefix)
              , ("ARTIFACT_NAME",   artifactFileName project)
              , ("S3_BUCKET",       fromBucket s3Bucket)
              ]
    where varNamePrefix = "SORTIE_"

deploy :: Context               -- ^ Execution context.
       -> Version               -- ^ Version to deploy.
       -> Environment           -- ^ Target deployment environment.
       -> IO ()
deploy ctx@Context{project = project@Project{s3Bucket, s3KeyPrefix}
                  } version env =
    do { elem tagName <$> listTags `elseM` unknownVersion "git"
       ; S3.hasKey s3Bucket s3Key  `elseM` unknownVersion "S3"
       ; ssh ctx env $ projectEnvVars project {version}
       }
    where { tagName = versionToTag version
          ; s3Key   = s3KeyPrefix </> artifactFileName project
          ; unknownVersion repo
              = die $ printf "unknown version in %s: %s \
                              \(try `sortie release' first)"
                              repo (showVersion version)
          }
