{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS -fno-warn-orphans  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Types
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Types where

import           Control.Applicative
import           Control.Monad.Error.Class
import qualified Control.Monad.Except       as Except
import           Control.Monad.IO.Class
import qualified Control.Monad.Reader       as Reader
import           Control.Monad.Reader.Class
import qualified Crypto.Hash                as Hash
import qualified Data.ByteString.Lazy
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Network.Wreq               as Wreq
import           Network.Wreq.ErrorHandling
import qualified Options.Applicative        as Options
import           Options.Generic
import           URI.ByteString

import           Types.Exceptions
import           Types.Hash                 ()
import           Types.ImageName
import           Types.ImageTag
import           Types.URI                  ()

-- | Docker registry URI.
type RegistryURI     = (URIRef Absolute)

-- | Docker registry username.
type Username        = Text

-- | Docker registry user password.
type Password        = Text

-- | Docker image layer sha256 hash digest.
type Layer           = Text

-- | SHA256 hash digest with the hash algorithm identifier prefix,
-- stripped
type StrippedDigest  = Text

-- | Docker image manifest JSON.
type Manifest        = Data.ByteString.Lazy.ByteString

-- | Docker image config JSON.
type ImageConfigJSON = Data.ByteString.Lazy.ByteString

-- | Wreq response type parameterized by the lazy bytestring type.
type RspBS           = Wreq.Response Data.ByteString.Lazy.ByteString

-- | A file extension.
type Extension       = String

-- | RepoName is the part before the forward slash in a docker image
-- name, e.g: @library@ in @library/debian@
type RepoNamePart    = Text

-- | ImageName is the part after the forward slash in a docker image
-- name, e.g: @library@ in @library/debian@
type ImageNamePart   = Text

-- | Docker image config JSON file's sha256 hash digest in Nix's
-- base32 encoding.
--
-- NB: it's very important to realize there's a significant difference
-- between Nix's base32 encoding and the standard base32 encoding!
-- (i.e, they're not compatible).
type ConfigDigest    = Base32Digest

-- | Generic top-level optparse-generic CLI args data type and
-- specification.
--
-- NOTE: `hocker-layer` does not use this data type because it
-- requires an additional layer sha256 hash digest argument.
data Options w = Options
    { -- | URI for the registry, optional
      registry    :: w ::: Maybe RegistryURI
      <?> "URI of registry, defaults to the Docker Hub registry"
    , credentials :: Maybe Credentials
      -- | Filesystem path to write output to
    , out         :: w ::: Maybe FilePath
      <?> "Write content to location"
      -- | Docker image name (includes the reponame, e.g: library/debian)
    , imageName   :: ImageName
      -- | Docker image tag
    , imageTag    :: ImageTag
    } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

-- | Hocker 'ExceptT' and 'ReaderT' transformer stack threading a
-- 'HockerMeta' data type.
newtype Hocker a = Hocker { unHocker :: Reader.ReaderT HockerMeta (Except.ExceptT HockerException IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader HockerMeta
    , MonadError HockerException
    )

runHocker :: Hocker a -> HockerMeta -> IO (Either HockerException a)
runHocker (unHocker -> d) = Except.runExceptT . interceptHttpExc . Reader.runReaderT d

-- | Red wagon record carrying around the environment as we fetch,
-- transform, and assemble docker image artifacts.
data HockerMeta = HockerMeta
    { dockerRegistry :: RegistryURI
    , auth           :: Maybe Wreq.Auth
    , imageName      :: ImageName
    , imageTag       :: ImageTag
    , out            :: Maybe FilePath
    , outDir         :: Maybe FilePath
    , imageLayer     :: Maybe (Hash.Digest Hash.SHA256)
    } deriving (Show)

-- | Newtype base32 encoding of a hash digest.
--
-- Please note, this base32 encoding is unique to Nix and not
-- compatible with other base32 encodings.
newtype Base32Digest = Base32Digest Text
  deriving (Show, Read, Eq)

-- | Newtype base16 encoding of a hash digest.
--
-- This encoding has no known idiosyncracies specific to Nix, it
-- should be compatible with other tools and library's expectations.
newtype Base16Digest = Base16Digest Text
  deriving (Show, Read, Eq)

data Credentials = Basic Username Password | BearerToken Text
  deriving (Show)

instance ParseField Credentials where
  parseField _ _ = (Basic <$> parseUsername <*> parsePassword) <|> (BearerToken <$> parseToken)
    where
      parseUsername = Text.pack <$>
        (Options.option Options.str $
         (  Options.metavar "BASIC USERNAME"
         <> Options.long    "username"
         <> Options.short   'u'
         <> Options.help    "Username part of a basic auth credential"
         )
        )
      parsePassword = Text.pack <$>
        (Options.option Options.str $
         (  Options.metavar "BASIC PASSWORD"
         <> Options.long    "password"
         <> Options.short   'p'
         <> Options.help    "Password part of a basic auth credential"
         )
        )
      parseToken = Text.pack <$>
        (Options.option Options.str $
         (  Options.metavar "BEARER TOKEN"
         <> Options.long    "token"
         <> Options.short   't'
         <> Options.help    "Bearer token retrieved from a call to `docker login` (mutually exclusive to --username and --password)"
         )
        )

instance ParseFields Credentials
instance ParseRecord Credentials where
  parseRecord = fmap Options.Generic.getOnly parseRecord
