{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Wreq.Docker.Registry
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

module Network.Wreq.Docker.Registry where

import qualified Control.Exception          as Exception
import           Control.Lens
import qualified Control.Monad.Except       as Except
import           Control.Monad.Reader
import           Control.Monad              (when)
import qualified Crypto.Hash                as Hash
import           Data.Aeson.Lens
import           Data.ByteString.Lazy.Char8 as C8L
import qualified Data.ByteString.Char8      as C8
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           URI.ByteString
import           NeatInterpolation
import           Data.Bifunctor
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import qualified Network.Wreq               as Wreq
import qualified Turtle
import           System.Directory
import qualified System.IO

import           Data.Docker.Image.Types
import           Hocker.Lib
import           Hocker.Types
import           Hocker.Types.Exceptions
import           Hocker.Types.ImageName
import           Hocker.Types.ImageTag

-- | Default docker hub registry (@https://registry-1.docker.io/v2/@).
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

-- | Given 'Credentials', produce a 'Wreq.Auth'.
--
-- If 'Credentials' is either 'BearerToken' or 'Basic' then produce a
-- 'Wreq.Auth' value for that type of credential.
--
-- If @Nothing@ is provided _and_ the provided 'RegistryURI' matches
-- the default registry, make a request to
-- @https://auth.docker.io/token@ for a temporary pull-only bearer
-- token, assuming the request we want to make is to the public docker
-- hub and without any other credentials.
--
-- Otherwise, return 'Nothing' so that an unauthenticated request can
-- be made.
mkAuth :: RegistryURI       -- ^ Docker registry
       -> ImageName         -- ^ Docker image name
       -> Maybe Credentials -- ^ Docker registry authentication credentials
       -> IO (Maybe Wreq.Auth)
mkAuth reg iname@(ImageName img) credentials =
  case credentials of
    Just (BearerToken token)
      -> pure (Just $ Wreq.oauth2Bearer (encodeUtf8 token))
    Just (Basic username password)
      -> pure (Just $ Wreq.basicAuth (encodeUtf8 username) (encodeUtf8 password))
    Just (CredentialsFile path)
      -> parseCredentialsFile path >>= mkAuth reg iname . Just
    Nothing | reg /= defaultRegistry
              -> pure Nothing
            | otherwise
              -> getHubToken >>= pure . mkHubBearer
  where
    getHubToken     = Wreq.get ("https://auth.docker.io/token?service=registry.docker.io&scope=repository:"<>img<>":pull")
    mkHubBearer rsp = (Wreq.oauth2Bearer . encodeUtf8) <$> (rsp ^? Wreq.responseBody . key "token" . _String)

parseCredentialsFile :: FilePath -> IO Credentials
parseCredentialsFile path = do
  let parse = do
        kvs <- Text.readFile path
          <&> fmap (second (Text.drop 1) . Text.break (== '=')) . Text.lines
        pure $ case kvs of
          [(k,v)] | k == "BEARER_TOKEN", v /= "" -> Just $ BearerToken v
          [(k1,v1),(k2,v2)] | k1 == "USERNAME", v1 /= "", k2 == "PASSWORD", v2 /= "" -> Just $ Basic v1 v2
          [(k1,v1),(k2,v2)] | k2 == "USERNAME", v2 /= "", k1 == "PASSWORD", v1 /= "" -> Just $ Basic v2 v1
          _ -> Nothing

  creds <- parse `Exception.catch` \(e :: Exception.IOException) -> do
    System.IO.hPutStrLn System.IO.stderr "error: while trying to read credentials file..."
    Exception.throwIO e
  case creds of
    Just val -> pure val
    Nothing -> do
      System.IO.hPutStrLn System.IO.stderr "error: could not parse credentials file"
      Turtle.exit (Turtle.ExitFailure 1)


-- | Retrieve a list of layer hash digests from a docker registry
-- image manifest JSON.
--
-- TODO: pluck out the layer's size and digest into a tuple.
pluckLayersFrom :: Manifest -> [Layer]
pluckLayersFrom = toListOf (key "layers" . values . key "digest" . _String)

-- | Retrieve a list of layer hash digests from an image's
-- configuration JSON.
--
-- This is subtly different from 'pluckLayersFrom' because both list
-- hash digests for the image's layers but the manifest's layer hash
-- digests are keys into the registry's blob storage referencing
-- _compressed_ layer archives. The configuration JSON's layer hash
-- digests reference the uncompressed layer tar archives within the
-- image.
pluckRefLayersFrom :: ImageConfigJSON -> [Layer]
pluckRefLayersFrom = toListOf (key "rootfs" . key "diff_ids" . values . _String)

-----------------------------------------------------------------------------
-- Top-level docker-registry V2 REST interface functions

-- | Request a V2 registry manifest for the specified docker image.
fetchManifest :: Hocker RspBS
fetchManifest = ask >>= \HockerMeta{..} ->
  liftIO $ Wreq.getWith (opts auth & accept) (mkURL imageName imageTag dockerRegistry)
  where
    mkURL (ImageName n) (ImageTag t) r = C8.unpack (serializeURIRef' $ Hocker.Lib.joinURIPath [n, "manifests", t] r)
    accept = Wreq.header "Accept" .~
      [ "application/vnd.docker.distribution.manifest.v2+json" ]

-- | Retrieve the configuratino JSON of an image by its hash digest
-- (found in the V2 manifest for an image given by a name and a tag).
fetchImageConfig :: (Hash.Digest Hash.SHA256) -> Hocker RspBS
fetchImageConfig (showSHA -> digest) = ask >>= \HockerMeta{..} ->
  liftIO $ Wreq.getWith (opts auth) (mkURL imageName dockerRegistry)
  where
    mkURL (ImageName n) r = C8.unpack (serializeURIRef' $ Hocker.Lib.joinURIPath [n, "blobs", digest] r)

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

-- | Write a 'Wreq.responseBody' to the specified 'FilePath', checking
-- the integrity of the file with its sha256 hash digest.
--
-- The second argument, the 'StrippedDigest', must be a hash digest
-- stripped of the @sha256:@ algorithm identifier prefix.
writeRespBody :: FilePath       -- ^ Filesystem path to write the content to
              -> StrippedDigest -- ^ Hash digest, stripped of its algorithm identifier prefix
              -> RspBS          -- ^ Wreq lazy bytestring response object
              -> Hocker FilePath
writeRespBody out digest resp = do
  liftIO . C8L.writeFile out $ resp ^. Wreq.responseBody
  verified <- liftIO (checkFileIntegrity out digest)
  either (Except.throwError . hockerException) return verified

-- | Write a response to the filesystem without a request hash
-- digest. Attempt to fetch the value of the @ETag@ header to verify
-- the integrity of the content received.
--
-- The Docker docs do _not_ recommended this method for verification
-- because the @ETag@ and @Docker-Content-Digest@ headers may change
-- between the time you issue a request with a digest and when you
-- receive a response back!
--
-- We do it anyway and leave this warning.
writeRespBody' :: FilePath  -- ^ Filesystem path to write the content to
               -> RspBS     -- ^ Wreq lazy bytestring response object
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
  let contentHash = show . Hocker.Lib.sha256 $ r ^. Wreq.responseBody
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
    digest -> checkResponseIntegrity rsp (Hocker.Lib.stripHashId digest)

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

    fileHash <- liftIO (return . show . Hocker.Lib.sha256 =<< C8L.readFile fp)

    when (Text.unpack digest /= fileHash) $
      let fhTxt = Text.pack fileHash
          fpTxt = Text.pack fp
      in fail $ Text.unpack
        ([text|
The sha256 hash for $fpTxt: $fhTxt
Does not match the expected digest: $digest
|])
    return fp
