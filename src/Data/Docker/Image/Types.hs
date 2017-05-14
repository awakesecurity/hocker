{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Docker.Image.Types
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Data.Docker.Image.Types where

import           Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text                  as T

import           Types
import           Types.ImageTag

-- | Record of all the metadata we need for a docker image; this
-- includes the basics like registry location, image repository name,
-- image name, image tag, a possible alternative image name, and
-- finally the full manifest JSON for the docker image from which a
-- complete image can be constructed (supplying the config JSON and
-- references to all of the layers).
data HockerImageMeta = HockerImageMeta
  { -- | Docker image repo, the first part of a repository+name
    -- separated by a "/"; e.g: library/debian.
    imageRepo      :: RepoNamePart
    -- | Docker image name, the second part of a repository+name
    -- separated by a "/"; e.g: library/debian.
  , imageName      :: ImageNamePart
    -- | Docker image tag
  , imageTag       :: ImageTag

    -- | A docker image manifest JSON blob as usually fetched from a
    -- docker registry.
    --
    -- TODO: switch this to the JSON AST type?
  , manifestJSON   :: C8L.ByteString
    -- | The URI (even if the default public registry) of the docker
    -- registry.
  , dockerRegistry :: RegistryURI
    -- | An alternative name for the docker image provided in the
    -- output Nix `fetchdocker` derivation expressions. Not replacing
    -- @imageName@ but providing a method for declaring up-front a
    -- possibly cleaner or more intuitive name for use within Nix.
  , altImageName   :: Maybe T.Text
  } deriving (Show)
