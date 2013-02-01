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
      ; unless dryRun $ withFileContents script $
               writeCommand verbosity "ssh" (host : args)
      }
    where { args = [ "sudo", "-u", execUser, "env" ] ++ envArgs ++
                   [ "sh", "-" ]
          ; envArgs = [ printf "%s=%s" varName (show val)
                      | (varName, val) <- envVars ]
          ; script = projectDirectory </> installScript
          }

deploy :: Context               -- | Execution context.
       -> Version               -- | Version to deploy.
       -> Environment           -- | Target deployment environment.
       -> IO ()
deploy ctx@Context{project =
                       project@Project{projectName, s3Bucket, s3KeyPrefix}
                  } version env =
    do { elem tagName <$> listTags `elseM` unknownVersion "git"
       ; S3.hasKey s3Bucket s3Key  `elseM` unknownVersion "S3"
       ; ssh ctx env envVars
       }
    where { tagName = versionToTag version
          ; s3Key   = s3KeyPrefix </> artifactFileName project

          ; envVars = [ ("SORTIE_PROJECT_NAME",    getPackageName $ projectName)
                      , ("SORTIE_PROJECT_VERSION", showVersion version)
                      , ("SORTIE_S3_KEY_PREFIX",   s3KeyPrefix)
                      , ("SORTIE_ARTIFACT_NAME",   artifactFileName project)
                      , ("SORTIE_S3_BUCKET",       fromBucket s3Bucket)
                      ]

          ; unknownVersion repo
              = die $ printf "unknown version in %s: %s \
                              \(try `sortie release' first)"
                              repo (showVersion version)
          }
