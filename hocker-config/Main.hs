{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  hocker-config/Main
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
progSummary = "Fetch a docker image config JSON from the registry"

main :: IO ()
main = unwrapRecord progSummary >>= \Options{..} -> do
  let dockerRegistry = fromMaybe defaultRegistry registry
      imageArch      = fromMaybe systemArch arch

  auth   <- mkAuth dockerRegistry imageName credentials
  config <- Docker.Image.fetchConfig $
    HockerMeta
      { outDir     = Nothing
      , imageLayer = Nothing
      , ..
      }
  either (Hocker.Lib.exitProgFail . show) (Hocker.Lib.writeOrPrint out) config
