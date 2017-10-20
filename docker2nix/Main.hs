{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS -fno-warn-orphans        #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  docker2nix/Main
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Main where

import           Data.ByteString.Lazy.Char8   as C8L
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import           Options.Generic
import           System.IO                    (hWaitForInput, stdin)

import           Data.Docker.Image.Types
import           Data.Docker.Nix.FetchDocker  as Nix.FetchDocker
import           Hocker.Lib
import           Network.Wreq.Docker.Registry as Docker.Registry
import           Hocker.Types
import           Hocker.Types.ImageName
import           Hocker.Types.ImageTag

-- | Top-level optparse-generic CLI args data type and specification.
data ProgArgs w = ProgArgs
    { -- | URI for the registry, optional
      registry     :: w ::: Maybe RegistryURI
      <?> "URI of registry, defaults to the Docker Hub registry"
      -- | Filepath to a file containing the manifest JSON
    , manifest     :: w ::: Maybe FilePath
      <?> "Fetch image manifest from a path on the filesystem"
      -- | Alternative docker image name made available in the Nix
      -- expression fetchdocker derivation
    , altImageName :: w ::: Maybe Text
      <?> "Alternate image name provided in the `fetcdocker` derivation"
      -- | Docker image name (includes the reponame, e.g: library/debian)
    , name    :: ImageName
      -- | Docker image tag
    , imageTag     :: ImageTag
    } deriving (Generic)


instance ParseRecord (ProgArgs Wrapped)
deriving instance Show (ProgArgs Unwrapped)

progSummary :: Text
progSummary = "Produce a Nix expression given a manifest for a docker image via stdin or via a filepath"

main :: IO ()
main = unwrapRecord progSummary >>= \ProgArgs{..} -> do
  let (imageRepo, imageName) = Hocker.Lib.splitRepository name
      dockerRegistry         = fromMaybe defaultRegistry registry

  manifestJSON <-
    case manifest of
      Just f  -> C8L.readFile f
      Nothing -> do
        let h = stdin
        hWaitForInput h (-1)
        C8L.hGetContents h

  exprs <- Nix.FetchDocker.generate HockerImageMeta{..}
  either (Hocker.Lib.exitProgFail . show) Hocker.Lib.pprintNixExpr exprs
