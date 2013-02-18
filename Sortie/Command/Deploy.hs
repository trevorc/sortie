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
import Control.Monad           (unless)
import Distribution.Version    (Version)
import System.FilePath         ((</>))
import Text.Printf             (printf)

import Sortie.Context          (Context(..))
import Sortie.Project          (Environment(..), Project(..))
import Sortie.ProjectUtils
    ( ensureArtifactInS3, ensureTagExists, projectEnvVars )
import Sortie.Utils
    ( notice
    , withFileContents, writeCommand )

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

deploy :: Context               -- ^ Execution context.
       -> Version               -- ^ Version to deploy.
       -> Environment           -- ^ Target deployment environment.
       -> IO ()
deploy ctx version env =
    do { ensureTagExists project'
       ; ensureArtifactInS3 project'
       ; ssh ctx env $ projectEnvVars project'
       }
    where project' = (project ctx) {version}

