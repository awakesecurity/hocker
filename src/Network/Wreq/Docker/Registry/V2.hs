{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Wreq.Docker.Registry.V2
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
--
-- Convenience functions for interacting with an instance of Docker
-- Distribution (Docker Registry V2). I've kept the module naming
-- consistent with the docker registry terms since that appears to be
-- what everyone uses colloquially even though the formal name for the
-- software is "docker distribution".
----------------------------------------------------------------------------

module Network.Wreq.Docker.Registry.V2 where

import           Control.Lens
import qualified Control.Monad.Except       as Except
import           Control.Monad.Reader
import           Data.Monoid
import qualified Crypto.Hash                as Hash
import           Data.Aeson.Lens
import           Data.ByteString.Lazy.Char8 as C8L
import qualified Data.ByteString.Char8      as C8
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           URI.ByteString
import           NeatInterpolation
import qualified Data.Text                  as Text
import qualified Network.Wreq               as Wreq
import           System.Directory

import           Data.Docker.Image.V1.Types
import           Lib
import           Types
import           Types.Exceptions
import           Types.ImageName
import           Types.ImageTag

-- | Default docker hub registry.
defaultRegistry :: URIRef Absolute
defaultRegistry = URI
  { uriScheme = Scheme "https"
  , uriAuthority = Just $ Authority
    { authorityUserInfo = Nothing
    , authorityHost     = Host "registry-1.docker.io"
    , authorityPort     = Nothing
    }
  , uriPath = "/v2/"
  , uriQuery = Query []
  , uriFragment = Nothing
  }

mkAuth :: RegistryURI
       -> ImageName
       -> Maybe Credentials
       -> IO (Maybe Wreq.Auth)
mkAuth reg (ImageName img) credentials =
  case credentials of
    Just (BearerToken token)
      -> pure (Just $ Wreq.oauth2Bearer (encodeUtf8 token))
    Just (Basic username password)
      -> pure (Just $ Wreq.basicAuth (encodeUtf8 username) (encodeUtf8 password))
    Nothing | reg /= defaultRegistry
              -> pure Nothing
            | otherwise
              -> getHubToken >>= pure . mkHubBearer
  where
    getHubToken     = Wreq.get ("https://auth.docker.io/token?service=registry.docker.io&scope=repository:"<>img<>":pull")
    mkHubBearer rsp = (Wreq.oauth2Bearer . encodeUtf8) <$> (rsp ^? Wreq.responseBody . key "token" . _String)

-- | Retrieve a list of layer hash digests from an image's manifest
-- JSON.
--
-- TODO: pluck out the layer's size and digest into a tuple.
pluckLayersFrom :: Manifest -> [Layer]
pluckLayersFrom = toListOf (key "layers" . values . key "digest" . _String)

-- | Retrieve a list of layer hash digests from an image's config
-- JSON.
--
-- This is subtly different from @pluckLayersFrom@ because both list
-- hash digests for the image's layers but the manifest's layer hash
-- digests are keys into the registry's blob storage referencing the
-- *compressed* layer archive. The config JSON's layer hash digests
-- reference the uncompressed layer tar archives within the image.
pluckRefLayersFrom :: ImageConfigJSON -> [Layer]
pluckRefLayersFrom = toListOf (key "rootfs" . key "diff_ids" . values . _String)

-----------------------------------------------------------------------------
-- Top-level docker-registry V2 REST interface functions

-- | Request a V2 registry manifest for the specified docker image.
fetchManifest :: Hocker RspBS
fetchManifest = ask >>= \HockerMeta{..} ->
  liftIO $ Wreq.getWith (opts auth & accept) (mkURL imageName imageTag dockerRegistry)
  where
    mkURL (ImageName n) (ImageTag t) r = C8.unpack (serializeURIRef' $ Lib.joinURIPath [n, "manifests", t] r)
    accept = Wreq.header "Accept" .~
      [ "application/vnd.docker.distribution.manifest.v2+json"
      , "application/vnd.docker.distribution.manifest.list.v2+json"
      ]

-- | Retrieve the config json of an image by its hash digest (found in
-- the V2 manifest for an image given by a name and tag).
fetchImageConfig :: (Hash.Digest Hash.SHA256) -> Hocker RspBS
fetchImageConfig (showSHA -> digest) = ask >>= \HockerMeta{..} ->
  liftIO $ Wreq.getWith (opts auth) (mkURL imageName dockerRegistry)
  where
    mkURL (ImageName n) r = C8.unpack (serializeURIRef' $ Lib.joinURIPath [n, "blobs", digest] r)

-- | Retrieve a compressed layer blob by its hash digest.
-- 
-- TODO: take advantage of registry's support for the Range header so
-- we can stream downloads.
fetchLayer :: Layer -> Hocker RspBS
fetchLayer layer = ask >>= \HockerMeta{..} ->
  liftIO $ Wreq.getWith (opts auth) (mkURL layer imageName dockerRegistry)
  where
    mkURL
      (Text.unpack -> digest)
      (ImageName name)
      registry
      = C8.unpack (serializeURIRef' $ joinURIPath [name, "blobs", digest] registry)

-- | Write a @Wreq@ response body to the specified @FilePath@,
-- checking the integrity of the file with its sha256 hash digest.
--
-- The second argument, the @StrippedDigest@, must be a hash digest
-- stripped of the "sha256:" hash algorithm identifier prefix.
writeRespBody :: FilePath       -- ^ Filesystem path to write the content to
              -> StrippedDigest -- ^ Hash digest, stripped of its hash algorithm identifier prefix
              -> RspBS         -- ^ Wreq lazy bytestring response object
              -> Hocker FilePath
writeRespBody out digest resp = do
  liftIO . C8L.writeFile out $ resp ^. Wreq.responseBody

  -- Now, verify the file; we assume the sha256 function since that is
  -- used everywhere
  verified <- liftIO $ checkFileIntegrity out digest
  either (Except.throwError . hockerException) return verified

-- | Write a response to the filesystem without a request hash
-- digest. Attempt to fetch the value of the `ETag` header to verify
-- the integrity of the content received.
--
-- The Docker docs do *not* recommended this method for verification
-- because the ETag and Docker-Content-Digest headers may change
-- between the time you issue a request with a digest and when you
-- receive a response back!
writeRespBody' :: FilePath  -- ^ Filesystem path to write the content to
               -> RspBS    -- ^ Wreq lazy bytestring response object
               -> Hocker FilePath
writeRespBody' out r = writeRespBody out etagHash r
  where
    etagHash = decodeUtf8 $ r ^. Wreq.responseHeader "ETag"

-- | Compute a sha256 hash digest of the response body and compare it
-- against the supplied hash digest.
checkResponseIntegrity :: (Except.MonadError HockerException m)
                       => RspBS         -- ^ Wreq lazy bytestring response object
                       -> StrippedDigest -- ^ Hash digest, stripped of its hash algorithm identifier prefix
                       -> m RspBS
checkResponseIntegrity r d = do
  let contentHash = show . Lib.sha256 $ r ^. Wreq.responseBody
      digestHash  = Text.unpack d
  if | contentHash == digestHash -> pure r
     | otherwise ->
         let chTxt = Text.pack contentHash
             dgTxt = Text.pack digestHash
         in Except.throwError
          (hockerException
           (Text.unpack [text|
              Response content hash is $chTxt
              and it does not match the addressable content hash
              $dgTxt
            |]))

-- | Compute a sha256 hash digest of the response body and compare it
-- against the @Docker-Content-Digest@ header from the response.
--
-- The Docker docs do *not* recommended this method for verification
-- because the Docker-Content-Digest header may change between the
-- time you issue a request with a digest and when you receive a
-- response back!
--
-- NB: some registries do not send a @Docker-Content-Digest@ header,
-- I'm not sure yet what the cause for this is but this function's
-- behavior lacking that information is to ignore the hash check.
checkResponseIntegrity' :: (Except.MonadError HockerException m)
                        => RspBS    -- ^ Wreq lazy bytestring response object
                        -> m RspBS
checkResponseIntegrity' rsp =
  case decodeUtf8 (rsp ^. Wreq.responseHeader "Docker-Content-Digest") of
    -- Since some registries may send back no Docker-Content-Digest
    -- header, or an empty one, if it is empty then ignore it
    ""     -> pure rsp
    digest -> checkResponseIntegrity rsp (Lib.stripHashId digest)

-- | Compute a sha256 hash digest for a file and compare that hash to
-- the supplied hash digest.
checkFileIntegrity :: FilePath       -- ^ Filesystem path of file to verify
                   -> StrippedDigest -- ^ Hash digest, stripped of its hash algorithm identifier prefix
                   -> IO (Either String FilePath)
checkFileIntegrity fp digest =
  Except.runExceptT $ do
    exists <- liftIO (doesFileExist fp)
    when (not exists) $
      fail (fp <> " does not exist")

    fileHash <- liftIO (return . show . Lib.sha256 =<< C8L.readFile fp)

    when (Text.unpack digest /= fileHash) $
      let fhTxt = Text.pack fileHash
          fpTxt = Text.pack fp
      in fail $ Text.unpack
        ([text|
           The sha256 hash for $fpTxt: $fhTxt
           Does not match the expected digest: $digest
          |])

    return fp
