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

import           Data.Maybe                      (fromMaybe)
import qualified Data.Text
import           Options.Generic

import           Lib
import           Network.Wreq.Docker.Image.V1_2  as Docker.Image
import           Network.Wreq.Docker.Registry.V2
import           Types

progSummary :: Data.Text.Text
progSummary = "Fetch a docker image config JSON from the registry"

main :: IO ()
main = unwrapRecord progSummary >>= \Options{..} -> do
  let dockerRegistry = fromMaybe defaultRegistry registry

  auth   <- mkAuth dockerRegistry imageName credentials
  config <- Docker.Image.fetchConfig $
    HockerMeta
      { outDir     = Nothing
      , imageLayer = Nothing
      , ..
      }
  either (Lib.exitProgFail . show) (Lib.writeOrPrint out) config
