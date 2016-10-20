{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Wreq.Docker.Image.V1_2
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  AllRightsReserved
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Network.Wreq.Docker.Image.V1_2 where


import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8      as C8L
import           Data.Coerce
import           Data.Either
import           Data.HashSet                    as Set
import           Data.Monoid
import qualified Data.Text                       as T
import           NeatInterpolation
import qualified Network.Wreq                    as Wreq
import           System.FilePath.Posix           as File
import           System.Terminal.Concurrent

import           Data.Docker.Image.V1.Types
import           Lib

import           Network.Wreq.Docker.Image.Lib   as Docker.Image
import           Network.Wreq.Docker.Registry.V2 as Docker.Registry
import           Types
import           Types.Exceptions
import           Types.ImageName

-- | Fetches an image from the specified (or default) V2 Docker
-- Registery and assembles the artifacts into a compatible Docker V1.2
-- Image.
fetchAndAssemble :: HockerMeta -> IO (Either HockerException T.Text)
fetchAndAssemble = runHocker doFetchImage

-- | Fetches a layer by its digest key from the specified (or default)
-- V2 Docker Registery.
fetchLayer :: HockerMeta -> IO (Either HockerException FilePath)
fetchLayer = runHocker doFetchLayer

-- | Fetches the config file of the specified image from the specified
-- (or default) V2 Docker Registry and returns it.
fetchConfig :: HockerMeta -> IO (Either HockerException C8L.ByteString)
fetchConfig = runHocker doFetchConfig

-- | Fetches the manifest file of the specified image from the
-- specified (or default) V2 Docker Registry and returns it.
fetchImageManifest :: HockerMeta -> IO (Either HockerException C8L.ByteString)
fetchImageManifest = runHocker doFetch
  where
    doFetch = fetchManifest >>= return . view Wreq.responseBody

-- | Executes the monadic logic for fetching the docker image config
-- JSON within the ReaderT monad.
doFetchConfig :: Hocker C8L.ByteString
doFetchConfig = ask >>= \HockerMeta{..} -> do
  configDigest <-
    fetchManifest
      >>= checkResponseIntegrity'
      >>= getConfigDigest . view Wreq.responseBody

  fetchImageConfig configDigest
    >>= return . view Wreq.responseBody

-- | Executes the monadic logic for fetching and saving a layer tar
-- archive.
doFetchLayer :: Hocker FilePath
doFetchLayer = ask >>= \HockerMeta{..} -> do
  layerOut <- Lib.requireOutPath out

  layerDigest <- T.pack . show <$> maybe
    (throwError $ hockerException
      "a layer digest is expected!")
    return
    imageLayer

  let shortRef = T.take 7 layerDigest

  writeC <- liftIO $ getConcurrentOutputter
  liftIO . writeC . T.unpack $ "Downloading layer: " <> shortRef

  fetchedImageLayer <- checkResponseIntegrity' =<< Docker.Registry.fetchLayer ("sha256:" <> layerDigest)
  layerPath  <- writeRespBody layerOut layerDigest fetchedImageLayer

  liftIO . writeC $ T.unpack ("=> wrote " <> shortRef)

  return layerPath

-- | Executes the monadic logic for fetching, transforming, and
-- assembling a docker container image.
doFetchImage :: Hocker T.Text
doFetchImage = ask >>= \HockerMeta{..} -> do
  imageOutDir  <- Lib.requireOutPath outDir

  manifest     <- fetchManifest >>= checkResponseIntegrity'
  configDigest <- getConfigDigest $ manifest ^. Wreq.responseBody

  -- TODO: ALL of the below steps that handle saving things to the
  -- disk should probably be wrapped in a bracket function responsible
  -- for cleaning up any partially written data if there's a
  -- failure... Or perhaps instad of bracketing in here, we bracket
  -- around the @runExceptT@?

  -- Fetch and write the configuration json file for the image
  let configFileHash = Lib.stripHashId . T.pack $ showSHA configDigest
  imageConfig     <- fetchImageConfig configDigest
  imageConfigFile <- writeRespBody
                       (File.joinPath [imageOutDir, T.unpack configFileHash] `addExtension` "json")
                       configFileHash
                       imageConfig

  let refLayers        = pluckRefLayersFrom $ imageConfig ^. Wreq.responseBody
      refLayers'       = fmap Lib.stripHashId refLayers
      refLayerSet      = Set.fromList refLayers'
      manifestLayers   = pluckLayersFrom $ manifest ^. Wreq.responseBody
      (_, strippedReg) = T.breakOnEnd "//" . T.pack . show $ dockerRegistry
      repoTags         = (T.unpack strippedReg) </> (coerce imageName)

  -- Concurrently fetch layers and write to disk with a limit of three
  -- threads
  layers <- mapPool 3 Docker.Image.fetchLayer $ Prelude.zip refLayers' manifestLayers

  let writtenLayerSet = Set.fromList . fmap (T.pack . takeBaseName) $ rights layers
      refLayerSetTxt  = T.pack (show refLayerSet)
      wrtLayerSetTxt  = T.pack (show writtenLayerSet)
      dffLayerSetTxt  = T.pack (show $ Set.difference refLayerSet writtenLayerSet)

  when (writtenLayerSet /= refLayerSet) $
    throwError . hockerException $ T.unpack
      ([text|
        Written layers do not match the reference layers!

        Reference layers: ${refLayerSetTxt}
        Written   layers: ${wrtLayerSetTxt}

        Difference: ${dffLayerSetTxt}
      |])

  createImageRepository repoTags refLayers'
  createImageManifest   repoTags imageConfigFile refLayers'

  archivePath <- createImageTar

  return $ T.pack archivePath
