{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Wreq.Docker.Image
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Network.Wreq.Docker.Image where


import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8    as C8L
import           Data.Coerce
import           Data.Either
import           Data.HashSet                  as Set
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           NeatInterpolation
import qualified Network.Wreq                  as Wreq
import           System.FilePath.Posix         as File
import           System.Terminal.Concurrent

import           Data.Docker.Image.Types
import           Hocker.Lib

import           Network.Wreq.Docker.Image.Lib as Docker.Image
import           Network.Wreq.Docker.Registry  as Docker.Registry
import           Hocker.Types
import           Hocker.Types.Exceptions
import           Hocker.Types.ImageName

-- | Fetch an image from the docker registery, assembling the
-- artifacts into a Docker V1.2 Image.
fetchImage :: HockerMeta -> IO (Either HockerException Text)
fetchImage =
  runHocker $ ask >>= \HockerMeta{..} -> do
    imageOutDir  <- Hocker.Lib.requirePath outDir
    manifest     <- fetchManifest >>= checkResponseIntegrity'
    configDigest <- getConfigDigest $ manifest ^. Wreq.responseBody

    -- TODO: use Managed

    -- Fetch and write the configuration json file for the image
    let configFileHash = Hocker.Lib.stripHashId . Text.pack $ showSHA configDigest
    imageConfig     <- fetchImageConfig configDigest
    imageConfigFile <- writeRespBody
                         (File.joinPath [imageOutDir, Text.unpack configFileHash] `addExtension` "json")
                         configFileHash
                         imageConfig

    let refLayers        = pluckRefLayersFrom $ imageConfig ^. Wreq.responseBody
        refLayers'       = fmap Hocker.Lib.stripHashId refLayers
        refLayerSet      = Set.fromList refLayers'
        manifestLayers   = pluckLayersFrom $ manifest ^. Wreq.responseBody
        (_, strippedReg) = Text.breakOnEnd "//" . Text.pack . show $ dockerRegistry
        repoTags         = (Text.unpack strippedReg) </> (coerce imageName)

    -- Concurrently fetch layers and write to disk with a limit of three
    -- threads
    layers <- mapPool 3 Docker.Image.fetchLayer $ Prelude.zip refLayers' manifestLayers

    let writtenLayerSet = Set.fromList . fmap (Text.pack . takeBaseName) $ rights layers
        refLayerSetTxt  = Text.pack (show refLayerSet)
        wrtLayerSetTxt  = Text.pack (show writtenLayerSet)
        dffLayerSetTxt  = Text.pack (show $ Set.difference refLayerSet writtenLayerSet)

    when (writtenLayerSet /= refLayerSet) $
      throwError . hockerException $ Text.unpack
        ([text|
Written layers do not match the reference layers!

Reference layers: ${refLayerSetTxt}
Written   layers: ${wrtLayerSetTxt}

Difference: ${dffLayerSetTxt}
|])

    createImageRepository repoTags refLayers'
    createImageManifest   repoTags imageConfigFile refLayers'

    archivePath <- createImageTar

    return (Text.pack archivePath)

-- | Fetch a layer using its digest key from the docker registery.
fetchLayer :: HockerMeta -> IO (Either HockerException FilePath)
fetchLayer =
  runHocker $ ask >>= \HockerMeta{..} -> do
    layerOut    <- Hocker.Lib.requirePath out
    layerDigest <- Text.pack . show <$> maybe
      (throwError $ hockerException
        "a layer digest is expected!")
      return
      imageLayer

    let shortRef = Text.take 7 layerDigest

    writeC <- liftIO $ getConcurrentOutputter
    liftIO . writeC . Text.unpack $ "Downloading layer: " <> shortRef

    fetchedImageLayer <- Docker.Registry.fetchLayer ("sha256:" <> layerDigest)
    layerPath         <- writeRespBody layerOut layerDigest fetchedImageLayer

    liftIO . writeC $ Text.unpack ("=> wrote " <> shortRef)

    return layerPath

-- | Fetch the configuration JSON file of the specified image from the
-- docker registry.
fetchConfig :: HockerMeta -> IO (Either HockerException C8L.ByteString)
fetchConfig =
  runHocker $ ask >>= \HockerMeta{..} -> do
    configDigest <-
      fetchManifest
        >>= getConfigDigest . view Wreq.responseBody

    fetchImageConfig configDigest
      >>= return . view Wreq.responseBody

-- | Fetch the docker registry manifest JSON file for the specified
-- image from the docker registry..
fetchImageManifest :: HockerMeta -> IO (Either HockerException C8L.ByteString)
fetchImageManifest = runHocker (fetchManifest >>= return . view Wreq.responseBody)
