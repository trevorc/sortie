{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sortie.ProjectUtils
-- Copyright   :  (c) Bitbase 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  trevor@bitba.se
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Sortie.ProjectUtils
    ( ensureMatchingProjectName
    , ensureMatchingProjectVersion
    , ensureTagExists
    , ensureArtifactInS3
    , projectEnvVars )
where

import Control.Arrow           (first)
import Control.Applicative     ((<$>))
import Data.Version            (showVersion)
import Data.Text               (pack)
import Distribution.Version    (Version)
import System.FilePath         ((</>))
import Text.Printf             (printf)

import Sortie.Context          (Context(..))
import Sortie.Leiningen        (artifactFileName)
import Sortie.Project          (Project(..), fromBucket, getPackageName)
import qualified Sortie.Leiningen as Lein
    ( getProjectName, getProjectVersion )
import Sortie.SourceControl    (listTags, versionToTag)
import qualified Sortie.S3 as S3
    ( hasKey )
import Sortie.Utils
    ( die, elseM )


unknownVersion :: String -> Version -> IO ()
unknownVersion repo version = die $ printf "unknown version in %s: %s \
                                           \(try `sortie release' first)"
                                           repo (showVersion version)

ensureTagExists :: Project -> IO ()
ensureTagExists Project{version} =
    elem (versionToTag version) <$> listTags
             `elseM` (unknownVersion "git" version)

ensureArtifactInS3 :: Context -> IO ()
ensureArtifactInS3 ctx@Context{project = project@Project{
                                           s3Bucket, s3KeyPrefix, version}} =
    S3.hasKey ctx s3Bucket s3Key `elseM` (unknownVersion "S3" version)
    where s3Key = pack $ s3KeyPrefix </> artifactFileName project


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

ensureMatchingProjectName :: Context -> IO ()
ensureMatchingProjectName Context{project = Project{projectName}, projectDirectory} =
    (== projectName) <$> Lein.getProjectName projectDirectory
          `elseM` die ("lein project name is not " ++ show projectName)

ensureMatchingProjectVersion :: Context -> IO ()
ensureMatchingProjectVersion Context{project = Project{version}, projectDirectory} =
    (== version) <$> Lein.getProjectVersion projectDirectory
          `elseM` die ("lein project version is not " ++ showVersion version)
