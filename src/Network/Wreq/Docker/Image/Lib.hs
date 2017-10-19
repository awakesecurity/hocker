{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Wreq.Docker.Image.Lib
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Network.Wreq.Docker.Image.Lib where

import qualified Codec.Archive.Tar                 as Tar
import qualified Codec.Compression.GZip            as GZip
import qualified Control.Concurrent.PooledIO.Final as Pool
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8        as C8L
import           Data.Coerce
import qualified Data.HashMap.Strict               as HashMap
import           Data.Monoid
import qualified Data.Text                         as Text
import qualified Network.Wreq                      as Wreq
import qualified System.Directory                  as Directory
import           System.FilePath.Posix             as File
import           System.Terminal.Concurrent

import           Data.Docker.Image.Types
import           Hocker.Lib
import           Network.Wreq.Docker.Registry      as Docker.Registry
import           Types
import           Types.Exceptions
import           Types.ImageTag

-- | Like @mapM@ but concurrently apply a function to the elements of
-- the @Traversable@, limiting the maximum number of worker threads by
-- _n_.
mapPool :: Traversable t
        => Int                                         -- ^ Number of pooled worker threads
        -> ((String -> IO ()) -> a -> Hocker FilePath) -- ^ Processing function
        -> t a                                         -- ^ A Traversable container
        -> Hocker (t (Either HockerException FilePath))
mapPool n f l = do
  env    <- ask
  writeC <- liftIO getConcurrentOutputter
  let f' v = (runHocker (f writeC v) env)

  -- TODO: because I'm re-wrapping the function traversing the
  -- traversable, I need to extract the Left's from the result and
  -- propagate an error up with @throwError@ from this function.
  --
  -- TODO: refactor this such that the previous TODO is unnecessary.
  liftIO . Pool.runLimited n $ traverse (Pool.fork . f') l

-- | Like @mapPool@ but with the arguments flipped.
forPool :: Traversable t
        => Int                                         -- ^ Number of pooled worker threads
        -> t a                                         -- ^ A Traversable container
        -> ((String -> IO ()) -> a -> Hocker FilePath) -- ^ Processing function
        -> Hocker (t (Either HockerException FilePath))
forPool n = flip $ mapPool n

-- | Download, verify, decompress, and write a docker container image
-- layer to the filesystem.
fetchLayer :: (String -> IO ()) -- ^ Concurrent terminal output function
           -> (RefLayer, Layer) -- ^ A tuple of the reference layer hash digest from the image's config JSON and hash digest from the image's manifest JSON
           -> Hocker FilePath
fetchLayer writeC layer@(refl, (stripHashId -> layer')) = ask >>= \HockerMeta{..} -> do
  liftIO . writeC . Text.unpack $ "Downloading layer: " <> (Text.take 7 layer')

  fetchedImageLayer <- checkResponseIntegrity' =<< (Docker.Registry.fetchLayer $ snd layer)

  let decompressed = fetchedImageLayer & Wreq.responseBody %~ GZip.decompress
      shortRef     = Text.take 7 refl

  imageOutDir <- Lib.requirePath outDir

  liftIO $ writeC " => decompressed "

  let layerOutPath = File.joinPath [imageOutDir, Text.unpack refl] `addExtension` "tar"
  layerPath <- writeRespBody layerOutPath refl decompressed

  liftIO . writeC $ Text.unpack ("=> wrote " <> shortRef)

  return layerPath

-- | Generate a @manifest.json@ file.
createImageManifest :: RepoTag    -- ^ e.g: registry.mydomain.net:5001/reponame/imagename
                    -> FilePath   -- ^ Path of image config file for manifest
                    -> [RefLayer] -- ^ Layer hash digests sourced from the image's config JSON
                    -> Hocker ()
createImageManifest repoTag imageConfigFile refls = ask >>= \HockerMeta{..} -> do
  let imageManifest = [
        ImageManifest
          (takeBaseName imageConfigFile `addExtension` "json")
          [Text.pack (repoTag ++ ":" ++ coerce imageTag)]
          (fmap ((`addExtension` "tar") . Text.unpack) refls) ]
  imageOutDir <- Lib.requirePath outDir
  liftIO $ C8L.writeFile
    (imageOutDir </> "manifest" `addExtension` "json")
    (Lib.encodeCanonical imageManifest)

-- | Generate a @repositories@ json file.
--
-- NB: it is JSON but Docker doesn't want it a @.json@ extension
-- unlike its sibling the @manifest.json@ file.
createImageRepository :: RepoTag    -- ^ e.g: registry.mydomain.net:5001/reponame/imagename
                      -> [RefLayer] -- ^ Layer hash digests sourced from the image's configuration JSON
                      -> Hocker ()
createImageRepository repoTag refls = ask >>= \HockerMeta{..} -> do
  let repositories  =
        ImageRepo
          (Text.pack repoTag)

          -- Create a singleton map from a tag and the "latest" layer;
          -- Aeson will correctly encode this as an object with a key
          -- (the tag) and value (the layer within the archive named
          -- by its hash digest)
          (HashMap.singleton
           (Text.pack $ coerce imageTag)
           ((Prelude.last refls) <> ".tar"))
  imageOutDir <- Lib.requirePath outDir
  liftIO $ C8L.writeFile
    (imageOutDir </> "repositories")
    (Lib.encodeCanonical repositories)

-- | Tar and gzip the output dir into the final docker image archive
-- and remove the output dir.
createImageTar :: Hocker FilePath
createImageTar = ask >>= \HockerMeta{..} -> do
  imageOutDir <- Lib.requirePath outDir
  archivePath <- Lib.requirePath out

  entries <- liftIO $ Directory.getDirectoryContents imageOutDir

  -- TODO: remove once we have a newer `directory`
  let entriesToPack = [e | e <- entries, e /= ".", e /= ".."]

  liftIO $ Tar.create archivePath imageOutDir entriesToPack

  -- Cleanup after ourselves
  liftIO $ Directory.removeDirectoryRecursive imageOutDir

  return $ archivePath
