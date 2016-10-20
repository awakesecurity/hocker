{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Docker.Image.V1.Types
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  AllRightsReserved
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Data.Docker.Image.V1.Types where

import qualified Crypto.Hash             as Hash
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteArray          as BA
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Char8   as C8
import qualified Data.Text               as T
import           Data.Text.Encoding      (encodeUtf8)

-- | Attempt to parse a @C8.ByteString@ into a @Hash.Digest
-- Hash.SHA256@.
--
-- A @Digest@ in Docker Golang-code parlance is the string hexadecimal
-- representation of a hashing function's digest with the hashing
-- function identifier prefixed onto the string. Right now they only
-- use SHA256 everywhere and also don't really do anything to
-- parameterize it.
--
-- There is a custom Show instance for this newtype to output a string
-- representation of the digest prefixed by its hashing function
-- identifier.
toDigest :: C8.ByteString -> Maybe (Hash.Digest Hash.SHA256)
toDigest = from . C8.break (== ':')
  where
    from ("sha256", r) = either (const Nothing) Hash.digestFromByteString . toBytes $ C8.tail r
    from (_, _)        = Nothing

    toBytes :: C8.ByteString -> Either String BA.Bytes
    toBytes = BA.convertFromBase BA.Base16

-- | A special kind of SHA256 hash digest identifying a layer by its
-- *content*. This value is a hash of an empty, canonicalized JSON
-- string with a "layer_id" (which is actually the layer's @ChainID@)
-- and possibly a parent ID (which is the previous-layer-in-sequence
-- @ContentID@).
newtype ContentID = ContentID (Hash.Digest Hash.SHA256)
  deriving (Eq)

-- | A special kind of SHA256 digest identifying a specific sequence
-- of layers.
--
-- <https://github.com/docker/docker/blob/6e8a2cd29113896acfc3f97a43dd27f751f2f519/layer/layer.go#L60 layer.ChainID>
newtype ChainID = ChainID (Hash.Digest Hash.SHA256)
  deriving (Eq)

-- | A special kind of a SHA256 digest identifying a layer by the
-- sha256 sum of the uncompressed layer tarball. "Diff" in this
-- context refers to the root filesystem contents of the tarball
-- identified by @DiffID@ representing the difference from the
-- previous layer.
--
-- <https://github.com/docker/docker/blob/6e8a2cd29113896acfc3f97a43dd27f751f2f519/layer/layer.go#L68 layer.DiffID>
newtype DiffID = DiffID (Hash.Digest Hash.SHA256)
  deriving (Eq)

-- | Show a hexadecimal encoded SHA256 hash digest and prefix
-- "sha256:" to it.
showSHA :: Hash.Digest Hash.SHA256 -> String
showSHA = ("sha256:" ++) . show

instance Show ContentID where
  show (ContentID d) = showSHA d
instance Show ChainID where
  show (ChainID d)   = showSHA d
instance Show DiffID where
  show (DiffID d)    = showSHA d

instance ToJSON ContentID where
  toJSON v = String . T.pack $ show v
instance ToJSON ChainID where
  toJSON v = String . T.pack $ show v
instance ToJSON DiffID where
  toJSON v = String . T.pack $ show v

instance FromJSON ContentID where
  parseJSON o@(String v) =
    case toDigest $ encodeUtf8 v of
      Just v' -> return $ ContentID v'
      Nothing -> typeMismatch "SHA256 Digest" o
  parseJSON inv = typeMismatch "SHA256 Digest" inv
instance FromJSON ChainID where
  parseJSON o@(String v) =
    case toDigest $ encodeUtf8 v of
      Just v' -> return $ ChainID v'
      Nothing -> typeMismatch "SHA256 Digest" o
  parseJSON inv = typeMismatch "SHA256 Digest" inv
instance FromJSON DiffID where
  parseJSON o@(String v) =
    case toDigest $ encodeUtf8 v of
      Just v' -> return $ DiffID v'
      Nothing -> typeMismatch "SHA256 Digest" o
  parseJSON inv = typeMismatch "SHA256 Digest" inv
