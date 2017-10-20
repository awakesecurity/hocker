{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  hocker-manifest/Main
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Main where

import           Data.Maybe                   (fromMaybe)
import qualified Data.Text
import           Options.Generic

import           Hocker.Lib
import           Network.Wreq.Docker.Image    as Docker.Image
import           Network.Wreq.Docker.Registry
import           Hocker.Types

progSummary :: Data.Text.Text
progSummary = "Pull a docker image manifest from the registry"

main :: IO ()
main = unwrapRecord progSummary >>= \Options{..} -> do
  let dockerRegistry =  fromMaybe defaultRegistry registry

  auth     <- mkAuth dockerRegistry imageName credentials
  manifest <- Docker.Image.fetchImageManifest $
    HockerMeta
      { outDir     = Nothing
      , imageLayer = Nothing
      , ..
      }
  either (Hocker.Lib.exitProgFail . show) (Hocker.Lib.writeOrPrint out) manifest
