{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Docker.Image.V1_2.Types
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
--
-- The types in this module are used to describe two specific pieces
-- of JSON within the v1.2 Docker Image spec: @manifest.json@ and
-- @repositories@.
----------------------------------------------------------------------------

module Data.Docker.Image.V1_2.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.HashMap.Strict            as H
import qualified Data.Text                      as T

import           Data.Docker.Image.AesonHelpers
import           Lib

----------------------------------------------------------------------------
--

-- Pretty-printed example of the `manifest.json` file.
{-
    [
      {
        "Config": "3e83c23dba6a16cd936a3dc044df71b26706c5a4c28181bc3ca4a4af9f5f38ee.json",
        "Layers": [
          "10a267c67f423630f3afe5e04bbbc93d578861ddcc54283526222f3ad5e895b9.tar"
        ],
        "RepoTags": [
          "library/debian:jessie"
        ]
      }
]
-}

-- Pretty-printed example of the `repositories` json file.
{-
    {
      "library/debian": {
        "jessie": "10a267c67f423630f3afe5e04bbbc93d578861ddcc54283526222f3ad5e895b9"
      }
    }
-}

-- | A 'Text' representing a layer hash digest sourced from a docker
-- image's config JSON (different from the image's manifest JSON).
type RefLayer = T.Text

-- | A 'String' representing the full repository tag, e.g: @library/debian@.
type RepoTag  = String

-- | Represents a v1.2 Docker Image manifest.
data ImageManifest = ImageManifest
  { -- | 'FilePath' within the image archive of the image's config
    -- JSON
    config   :: FilePath
    -- | List of image repository tags
  , repoTags :: [T.Text]
    -- | List of layers within the image archive named by their hash
    -- digest and with the tar extension appended
  , layers   :: [FilePath]
  } deriving (Show, Eq)

-- | Represents an object of 'ImageRepo's. The repository names are the
-- top-level keys and their value is an object who's keys are the tags
-- of the repository with the hash-value of the layer that tag
-- references.
data ImageRepositories = ImageRepositories [ImageRepo]
  deriving (Show, Eq)

data ImageRepo = ImageRepo
  { -- | Repository tag
    repo :: T.Text
    -- | 'HashMap' of tags to the top-most layer associated with that tag
  , tags :: H.HashMap T.Text T.Text
  } deriving (Show, Eq)

$(deriveJSON stdOpts{ fieldLabelModifier = upperFirst } ''ImageManifest)

instance ToJSON ImageRepositories where
  toJSON (ImageRepositories r) =
    Object . H.unions $ [i | o@(Object i) <- (fmap toJSON r), isObject o]
    where
      isObject (Object _) = True
      isObject _          = False

instance ToJSON ImageRepo where
  toJSON (ImageRepo r t) = object [ r .= toJSON t ]

instance FromJSON ImageRepositories where
  parseJSON (Object v) = ImageRepositories <$> (mapM buildRepo $ H.toList v)
    where
      buildRepo (k,v') = ImageRepo k <$> parseJSON v'
  parseJSON v          = typeMismatch "ImageRepositories" v
