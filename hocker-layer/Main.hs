{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  hocker-layer/Main
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Main where

import qualified Crypto.Hash                  as Hash
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text
import           Options.Generic

import           Hocker.Lib
import           Network.Wreq.Docker.Image    as Docker.Image
import           Network.Wreq.Docker.Registry
import           Hocker.Types
import           Hocker.Types.Hash                   ()
import           Hocker.Types.ImageName
import           Hocker.Types.ImageTag
import           Hocker.Types.URI                    ()

data ProgArgs w = ProgArgs
    { -- | URI for the registry, optional
      registry :: w ::: Maybe RegistryURI
      <?> "URI of registry, defaults to the Docker Hub registry"
    , credentials :: Maybe Credentials
      -- | Filesystem path to write output to
    , out         :: w ::: Maybe FilePath
      <?> "Write content to location"
      -- | Layer sha256 hash digest to fetch from registry
    , imageLayer  :: w ::: Hash.Digest Hash.SHA256
      <?> "Hash digest of layer to fetch"
      -- | Docker image name (includes the repository, e.g: library/debian)
    , imageName   :: ImageName
      -- | Docker image tag
    , imageTag    :: ImageTag
    } deriving (Generic)

instance ParseRecord (ProgArgs Wrapped)
deriving instance Show (ProgArgs Unwrapped)

progSummary :: Data.Text.Text
progSummary = "Fetch a docker image layer from a docker registry without using docker"

main :: IO ()
main = unwrapRecord progSummary >>= \ProgArgs{..} -> do
  let dockerRegistry = fromMaybe defaultRegistry registry

  auth      <- mkAuth dockerRegistry imageName credentials
  layerPath <- Docker.Image.fetchLayer $
    HockerMeta
      { outDir     = Nothing
      , imageLayer = Just imageLayer
      , imageArch  = systemArch
      , ..
      }
  either (Hocker.Lib.exitProgFail . show) Prelude.putStrLn layerPath
