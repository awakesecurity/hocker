{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  hocker-fetch/Main
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Main where

import           Data.Maybe                   (fromMaybe)
import qualified Data.Text
import           Data.Text.IO                 as TIO
import           Options.Generic
import           System.IO.Temp               as Tmp

import           Hocker.Lib
import           Network.Wreq.Docker.Image    as Docker.Image
import           Network.Wreq.Docker.Registry
import           Hocker.Types

progSummary :: Data.Text.Text
progSummary = "Fetch a docker image from a docker registry without using docker"

main :: IO ()
main = unwrapRecord progSummary >>= \Options{..} -> do
  let dockerRegistry = fromMaybe defaultRegistry registry
      imageArch      = fromMaybe systemArch arch

  auth <- mkAuth dockerRegistry imageName credentials
  img  <- withSystemTempDirectory "hocker-image-XXXXXX" $ \d ->
    Docker.Image.fetchImage $
      HockerMeta
        { outDir     = Just d
        , imageLayer = Nothing
        , ..
        }
  either (Hocker.Lib.exitProgFail . show) TIO.putStrLn img
