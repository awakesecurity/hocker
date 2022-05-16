{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
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

import qualified Crypto.Hash                    as Hash
import           Data.Aeson
import qualified Data.Aeson.Key                 as Aeson.Key
import qualified Data.Aeson.KeyMap              as Aeson.KeyMap
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteArray                 as BA
import qualified Data.ByteArray.Encoding        as BA
import qualified Data.ByteString.Char8          as C8
import           Data.ByteString.Lazy.Char8     as C8L
import           Data.HashMap.Strict            as H
import qualified Data.List                      as List
import           Data.Text                      (Text)

import           Data.Docker.Image.AesonHelpers
import           Hocker.Types
import           Hocker.Types.ImageTag

-- | Metadata needed for constructing a docker image.
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
  , manifestJSON   :: C8L.ByteString
    -- | The URI (even if the default public registry) of the docker
    -- registry.
  , dockerRegistry :: RegistryURI
    -- | An alternative name for the docker image in the generated nix
    -- build instructions.
  , altImageName   :: Maybe Text
  } deriving (Show)

-- | Parse a 'C8.ByteString' into a 'Hash.SHA256'.
--
-- A digest value, as seen in the docker registry manifest, is the
-- hexadecimal encoding of a hashing function's digest with the
-- hashing function identifier prefixed onto the string. At this time
-- the only prefix used is @sha256:@.
toDigest :: C8.ByteString -> Maybe (Hash.Digest Hash.SHA256)
toDigest = from . C8.break (== ':')
  where
    from ("sha256", r) = either (const Nothing) Hash.digestFromByteString . toBytes $ C8.tail r
    from (_, _)        = Nothing

    toBytes :: C8.ByteString -> Either String BA.Bytes
    toBytes = BA.convertFromBase BA.Base16

-- | Show a hexadecimal encoded 'SHA256' hash digest and prefix
-- @sha256:@ to it.
showSHA :: Hash.Digest Hash.SHA256 -> String
showSHA = ("sha256:" ++) . show

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

-- | A layer hash digest from a docker image's config JSON. This hash
-- is different from those found in the image's manifest JSON.
type RefLayer = Text

-- | A 'String' representing the full repository tag, e.g: @library/debian@.
type RepoTag  = String

-- | A v1.2 docker image manifest.
data ImageManifest = ImageManifest
  { -- | 'FilePath' within the image archive of the image's config
    -- JSON
    config   :: FilePath
    -- | List of image repository tags
  , repoTags :: [Text]
    -- | List of layers within the image archive named by their hash
    -- digest and with a @.tar@ extension
  , layers   :: [FilePath]
  } deriving (Show, Eq)

-- | A map of 'ImageRepo's. The repository names are the top-level
-- keys and their value is a map who's keys are the tags of the
-- repository with the hash-value of the layer that tag references.
data ImageRepositories = ImageRepositories [ImageRepo]
  deriving (Show, Eq)

data ImageRepo = ImageRepo
  { -- | Repository tag
    repo :: Text
    -- | 'HashMap' of tags to the top-most layer associated with that tag
  , tags :: H.HashMap Text Text
  } deriving (Show, Eq)

$(deriveJSON stdOpts{ fieldLabelModifier = upperFirst } ''ImageManifest)



instance ToJSON ImageRepositories where
  toJSON (ImageRepositories r) =
    Object . List.foldl' Aeson.KeyMap.union mempty $
      [i | o@(Object i) <- (fmap toJSON r), isObject o]
    where
      isObject (Object _) = True
      isObject _          = False

instance ToJSON ImageRepo where
  toJSON (ImageRepo r t) = object [ Aeson.Key.fromText r .= toJSON t ]

instance FromJSON ImageRepositories where
  parseJSON (Object v) =
    ImageRepositories <$> (mapM buildRepo $ Aeson.KeyMap.toList v)
    where
      buildRepo (k,v') = ImageRepo (Aeson.Key.toText k) <$> parseJSON v'
  parseJSON v          = typeMismatch "ImageRepositories" v
