{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Hocker.Lib
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Hocker.Lib where

import           Control.Exception            (throwIO)
import           Control.Lens
import qualified Control.Monad.Except         as Except
import           Control.Monad.IO.Class       (MonadIO (..))
import qualified Crypto.Hash                  as Hash
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty     as AP
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8        as C8
import           Data.ByteString.Lazy.Char8   as C8L
import           Data.Coerce
import           Data.Semigroup               ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Prettyprint.Doc    (LayoutOptions(..),
                                               PageWidth(..), SimpleDocStream)
import qualified Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text
import           Data.Text.Encoding           (encodeUtf8)
import qualified Network.Wreq                 as Wreq
import           Nix.Expr                     (NExpr)
import           Nix.Pretty
import           System.Directory             (findExecutable)
import           System.Environment           (getProgName)
import           System.Exit                  as Exit
import           System.FilePath.Posix        as File
import qualified System.IO
import           URI.ByteString

import           Data.Docker.Image.Types
import           Hocker.Types
import           Hocker.Types.Exceptions
import           Hocker.Types.ImageName
import           Hocker.Types.ImageTag

-- | Throw a 'userError', exiting the program with the supplied
-- message.
die :: MonadIO io => Text -> io a
die = liftIO . throwIO . userError . Text.unpack

-- | Print an error message to stderr and return a non-zero exit code,
-- the message is prefixed with the name of the program.
exitProgFail :: String -> IO a
exitProgFail msg = do
  name <- getProgName
  Exit.die $ name ++ ": " ++ msg

-- | Print the bytestring to stdout if the first argument is
-- @Nothing@, otherwise write the bytestring to the provided
-- filesystem path and print the path to stdout.
writeOrPrint :: Maybe FilePath -> C8L.ByteString -> IO ()
writeOrPrint filepath content = maybe (C8L.putStrLn content) writeContent filepath
  where
     writeContent p = C8L.writeFile p content >> Prelude.putStrLn p

-- | Combine an image name and a base path producing an output path.
mkOutImage :: ImageName -- ^ Docker image name
           -> FilePath  -- ^ Base path to write to
           -> FilePath
mkOutImage n o = o </> (takeBaseName $ coerce n)

-- | Combine an image name, an image tag, and a base path producing an
-- output path with a @-config.json@ suffix.
mkOutConfig :: ImageName -- ^ Docker image name
            -> ImageTag  -- ^ Docker image tag
            -> FilePath  -- ^ Base path to write to
            -> FilePath
mkOutConfig n t o = o </> Prelude.concat
  [ (takeBaseName $ coerce n)
  , "_", coerce t
  ,  "-config.json"
  ]

-- | Combine an image name, an image tag, and a base path producing an
-- output path with a @-manifest.json@ suffix.
mkOutManifest :: ImageName -- ^ Docker image name
              -> ImageTag  -- ^ Docker image tag
              -> FilePath  -- ^ Base path to write to
              -> FilePath
mkOutManifest n t o = o </> Prelude.concat
  [ (takeBaseName $ coerce n)
  , "_", coerce t
  ,  "-manifest.json"
  ]

-- | Join a list of strings and the path part of a 'RegistryURI' to
-- produce a new 'RegistryURI' with a path root of @/v2@.
joinURIPath :: [String]    -- ^ Extra path segments to add
            -> RegistryURI -- ^ Base URI to add path segments to
            -> RegistryURI
joinURIPath pts uri@URI{..} = uri { uriPath = joinedParts }
  where
    joinedParts = C8.pack $ File.joinPath ("/":"v2":(C8.unpack uriPath):pts)

-- | Given a 'Wreq.Auth' produce a 'Wreq.Options'.
opts :: Maybe Wreq.Auth -> Wreq.Options
opts bAuth = Wreq.defaults & Wreq.auth .~ bAuth

-- | Hash a 'Data.ByteString.Lazy.Char8' using the 'Hash.SHA256'
-- algorithm.
sha256 :: C8L.ByteString -> Hash.Digest Hash.SHA256
sha256 = Hash.hashlazy

-- | Strip the @sha256:@ identifier prefix from a hash digest.
stripHashId :: Text -> Text
stripHashId = snd . Text.breakOnEnd ":"

-- | Encode, following Docker's canonical JSON rules, any 'ToJSON'
-- data type.
--
-- The canonicalization rules enable consistent hashing of encoded
-- JSON, a process relied upon heavily by docker for content
-- addressability and unique identification of resources within a
-- docker registry. Notably, an image's config JSON file and layers.
--
-- NB: <http://54.71.194.30:4016/registry/spec/json Docker's canonical JSON spec>
-- intentionally *does not* follow the <http://wiki.laptop.org/go/Canonical_JSON OLPC>'s
-- Canonical JSON format even though it was inspired by it.
encodeCanonical :: Data.Aeson.ToJSON a => a -> C8L.ByteString
encodeCanonical = AP.encodePretty' conf
  where
    conf = AP.defConfig { AP.confIndent = AP.Spaces 0, AP.confCompare = compare }

-- | Throw an error if the first argument is @Nothing@, otherwise
-- return the @FilePath@ unwrapped.
requirePath :: (Except.MonadError HockerException m)
               => Maybe FilePath
               -> m (FilePath)
requirePath = maybe pathError pure
  where
    pathError =
      Except.throwError
       (hockerException "To fetch and assemble a docker image, '--out=<path>' must be supplied")

-- | Pluck out the digest value for the config JSON given a docker
-- registry image manifest. Attempting to parse and return the digest
-- value as a 'Hash.SHA256', otherwise throw an error.
getConfigDigest :: (Except.MonadError HockerException m)
                => C8L.ByteString
                -> m (Hash.Digest Hash.SHA256)
getConfigDigest (view (key "config" . key "digest" . _String) -> digest) =
  maybe badDigest return parsedDigest
  where
    parsedDigest = toDigest $ encodeUtf8 digest
    badDigest    = Except.throwError $ hockerException "Failed parsing the config hash digest"



-- | Split a docker image's name on the forward slash separator so we
-- get the distinct repo name and image name.
splitRepository :: ImageName -> (RepoNamePart, ImageNamePart)
splitRepository (ImageName (Text.pack -> n)) = over _2 Text.tail $ Text.break (=='/') n

-- | Given a nix expression AST, produce a pretty printer document.
renderNixExpr :: NExpr -> SimpleDocStream ann
renderNixExpr =
    Data.Text.Prettyprint.Doc.layoutSmart layoutOptions . prettyNix
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 120 0.4 }

-- | Print a nix expression AST using the 'renderNixExpr' pretty
-- printing renderer.
pprintNixExpr :: NExpr -> IO ()
pprintNixExpr expr =
    Data.Text.Prettyprint.Doc.Render.Text.renderIO System.IO.stdout stream
  where
    stream = renderNixExpr expr

-- | Given an executable's name, try to find it in the PATH.
findExec :: (MonadIO m, Except.MonadError HockerException m)
         => String
         -> m Prelude.FilePath
findExec execname = (liftIO $ findExecutable execname) >>= \case
  Just v  -> return v
  Nothing -> Except.throwError $
               HockerException
                 ("cannot find executable `" <> execname <> "'")
                 Nothing
                 Nothing
