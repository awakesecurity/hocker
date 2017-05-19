{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Lib
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  AllRightsReserved
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Lib where

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
import           Data.Char
import           Data.Coerce
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Text.Encoding           (encodeUtf8)
import qualified Network.Wreq                 as Wreq
import           Nix.Expr                     (NExpr)
import           Nix.Pretty
import           System.Directory             (findExecutable)
import           System.Environment           (getProgName)
import           System.Exit                  as Exit
import           System.FilePath.Posix        as File
import           System.IO                    (stdout)
import           Text.PrettyPrint.ANSI.Leijen as Text.PrettyPrint (SimpleDoc,
                                                                   displayIO,
                                                                   renderPretty)
import           URI.ByteString

import           Data.Docker.Image.V1.Types

import           Types
import           Types.Exceptions
import           Types.ImageName
import           Types.ImageTag

-- | Throw a @userError@, exiting the program with the supplied
-- message.
die :: MonadIO io => T.Text -> io a
die = liftIO . throwIO . userError . T.unpack

-- | Print an error message to stderr and return a non-zero exit code,
-- the message is prefixed with the name of the program.
exitProgFail :: String -> IO a
exitProgFail msg = do
  name <- getProgName
  Exit.die $ name ++ ": " ++ msg

-- | Writes a bytestring to the provided filesystem path if it
-- @isJust@ and prints the path it wrote to the screen, otherwise
-- print the entire contents to the screen.
writeOrPrint :: Maybe FilePath -> C8L.ByteString -> IO ()
writeOrPrint o r = case o of
    Just p' -> C8L.writeFile p' r >> Prelude.putStrLn p'
    Nothing -> C8L.putStrLn r

-- | Make a path given a base path and a docker container name.
mkOutImage :: ImageName -> FilePath -> FilePath
mkOutImage n o = o </> (takeBaseName $ coerce n)

-- | Make a path given a base path, a docker container name, and a
-- docker container tag appending "-config.json" to the basename.
mkOutConfig :: ImageName -> ImageTag -> FilePath -> FilePath
mkOutConfig n t o = o </> Prelude.concat
  [ (takeBaseName $ coerce n)
  , "_", coerce t
  ,  "-config.json"
  ]

-- | Make a path given a base path, a docker container name, and a
-- docker container tag appending "-manifest.json" to the basename.
mkOutManifest :: ImageName -> ImageTag -> FilePath -> FilePath
mkOutManifest n t o = o </> Prelude.concat
  [ (takeBaseName $ coerce n)
  , "_", coerce t
  ,  "-manifest.json"
  ]

-- | Safely join a list of strings and a Network.URI record together
-- using @joinPath@.
joinURIPath :: [String] -> RegistryURI -> RegistryURI
joinURIPath pts uri@URI{..} = uri { uriPath = joinedParts }
  where
    joinedParts = C8.pack $ File.joinPath ("/":"v2":(C8.unpack uriPath):pts)

-- | Produce an @Wreq.Options@ using @Network.Wreq.defaults@ and an @Auth@.
opts :: Maybe Wreq.Auth -> Wreq.Options
opts bAuth = Wreq.defaults & Wreq.auth .~ bAuth

-- | Hash a @Data.ByteString.Lazy.Char8@ using the SHA256 algorithm.
sha256 :: C8L.ByteString -> Hash.Digest Hash.SHA256
sha256 = Hash.hashlazy

-- | Strip the hash algorithm identifier prefix from the beginning of
-- a hash digest string; e.g: "sha256:<digest>" becomes "<digest>".
stripHashId :: T.Text -> T.Text
stripHashId = snd . T.breakOnEnd ":"

-- | Encode, following Docker's canonical JSON rules, any @ToJSON@
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
    -- NB: the spec requires keys to be in lexically sorted order and
    -- it appears that the Ord instance of @Text@ behaves the same way
    -- the Ord instance for @String@ does: it sorts lexically.
    conf = AP.defConfig { AP.confIndent = AP.Spaces 0, AP.confCompare = compare }

-- | Throw an error if `Maybe FilePath` is `Nothing`, otherwise return
-- the @FilePath@ unwrapped.
requireOutPath :: (Except.MonadError HockerException m)
               => Maybe FilePath
               -> m (FilePath)
requireOutPath = maybe outPathError return
  where
    outPathError = Except.throwError $
      hockerException "To fetch and assemble a docker image, `--out=<path>` must be supplied"

-- | Pluck the digest value for the config JSON given a docker
-- registry image manifest. Attempting to parse and return the digest
-- value as a `Digest SHA256`, otherwise throwing an error.
getConfigDigest :: (Except.MonadError HockerException m)
                => C8L.ByteString
                -> m (Hash.Digest Hash.SHA256)
getConfigDigest (view (key "config" . key "digest" . _String) -> digest) =
  maybe badDigest return parsedDigest
  where
    parsedDigest = toDigest $ encodeUtf8 digest
    badDigest    = Except.throwError $ hockerException "Failed parsing the config hash digest"

-- | @upperFirst@ uppercases the first letter of the string.
upperFirst :: String -> String
upperFirst []    = []
upperFirst (h:t) = toUpper h : t

-- | Split a docker image's name on the forward slash separator so we
-- get the distinct repo name and image name.
splitImgName :: ImageName -> (RepoNamePart, ImageNamePart)
splitImgName (ImageName (T.pack -> n)) = over _2 T.tail $ T.break (=='/') n

-- | Pretty print a Nix expression and return a
-- @Text.PrettyPrint.SimpleDoc@, this can in turn be displayed to the
-- screen using @Text.PrettyPrint.displayIO@ or transformed into a
-- string using @Text.PrettyPrint.displayS@.
renderNixExpr :: NExpr -> Text.PrettyPrint.SimpleDoc
renderNixExpr = renderPretty 0.4 120 . prettyNix

-- | Pretty print a Nix expression AST and print to stdout.
pprintNixExpr :: NExpr -> IO ()
pprintNixExpr = displayIO stdout . renderNixExpr

-- | Given an executable's name, try to find it in the current
-- process's PATH context.
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
