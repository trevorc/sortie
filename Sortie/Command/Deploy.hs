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

import Control.Lens            ((^.))
import Control.Applicative     ((<$>))
import Data.Version            (showVersion)
import Distribution.Version    (Version)
import System.FilePath         ((</>))

import Sortie.Context          (Context(..))
import Sortie.Leiningen        (artifactFileName)
import Sortie.Project          (Environment)
import Sortie.SourceControl    (listTags, versionToTag)
import Sortie.Project
    ( s3KeyPrefix, s3Bucket )
import Sortie.Utils
    ( die, elseM )
import qualified Sortie.S3 as S3
    ( hasKey )

deploy :: Context               -- | Execution context.
       -> Version               -- | Version to deploy.
       -> Environment           -- | Target deployment environment.
       -> IO ()
deploy Context{project} version _env =
    do { elem tagName <$> listTags `elseM` unknownVersion
       ; S3.hasKey bucket s3Key    `elseM` unknownVersion
       }
    where { tagName        = versionToTag version
          ; unknownVersion = die $ "unknown version " ++
                             showVersion version ++
                             " (try `sortie release' first)"
          ; bucket         = project ^. s3Bucket
          ; s3Key          = project ^. s3KeyPrefix </>
                             artifactFileName project
          }
