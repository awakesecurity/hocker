{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Docker.Nix.FetchDocker
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Data.Docker.Nix.FetchDocker where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except            as Except
import           Data.Aeson.Lens
import qualified Data.Bifunctor                  as Bifunctor
import           Data.Coerce
import           Data.Fix
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8')
import           Data.Text.Encoding.Error
import           Nix.Expr
import           URI.ByteString

import           Data.Docker.Image.Types
import           Data.Docker.Nix.Lib             as Nix.Lib
import           Lib
import           Network.Wreq.Docker.Registry.V2 (pluckLayersFrom)
import           Types
import           Types.Exceptions
import           Types.ImageTag

{- Example output of the pretty-printed, generated Nix expression AST.
{
  config.docker.images.debian = pkgs.fetchdocker {
    name = "debian";
    registry = "https://registry-1.docker.io/v2/";
    repository = "library";
    imageName = "debian";
    tag = "latest";
    imageConfig = pkgs.fetchDockerConfig {
      inherit registry repository imageName tag;
      sha256 = "1viqbygsz9547jy830f2lk2hcrxjf7gl9h1xda9ws5kap8yw50ry";
    };
    imageLayers = let
      layer0 = pkgs.fetchDockerLayer {
        inherit registry repository imageName tag;
        layerDigest = "10a267c67f423630f3afe5e04bbbc93d578861ddcc54283526222f3ad5e895b9";
        sha256 = "1fcmx3aklbr24qsjhm6cvmhqhmrxr6xlpq75mzrk0dj2gz36g8hh";
      };
      in [ layer0 ];
  };
}
-}

-- | @fetchdocker@ derivation name.
constFetchdocker :: T.Text
constFetchdocker       = "fetchdocker"

-- | @fetchDockerConfig@ derivation name.
constFetchDockerConfig :: T.Text
constFetchDockerConfig = "fetchDockerConfig"

-- | @fetchDockerLayer@ derivation name.
constFetchDockerLayer :: T.Text
constFetchDockerLayer  = "fetchDockerLayer"

-- | Generate a Nix expression AST from a @HockerImageMeta@
-- record. This function crucially checks that the supplied manifest
-- JSON contains a key in the top-level object describing what version
-- of the manifest we have.
generate :: HockerImageMeta -> IO (Either HockerException NExpr)
generate dim@HockerImageMeta{..} = runExceptT $
  case (manifestJSON ^? key "schemaVersion" . _Integer) of
    Just 2  -> do
      nixhash      <- Lib.findExec "nix-hash"
      configDigest <- Nix.Lib.toBase32Nix nixhash . Base16Digest $ pluckedConfigDigest
      layerDigests <- forM pluckedLayerDigests $ \d16 ->
        (Base16Digest d16,) <$> (Nix.Lib.toBase32Nix nixhash $ Base16Digest d16)

      ExceptT (pure $ generateFetchDockerExpr dim configDigest layerDigests)
    Just v  ->
      throwError $ HockerException ("Expected: 2 but got: " <> (show v)) Nothing Nothing
    Nothing ->
      throwError $ HockerException "No key 'schemaVersion' in JSON object" Nothing Nothing

  where
    -- 'stripHashId' is necessary because digests in the manifest are
    -- prefixed by the hash algorithm used to generate them
    pluckedConfigDigest = Lib.stripHashId $ manifestJSON ^. key "config" . key "digest" . _String
    pluckedLayerDigests = Lib.stripHashId <$> pluckLayersFrom manifestJSON



{-| Generate a top-level Nix Expression AST from a 'HockerImageMeta'
record, a config digest, and a list of layer digests.

The generated AST, pretty-printed, may look similar to the following:
@
{
  config.docker.images.debian = pkgs.fetchdocker {
    name = "debian";
    registry = "https://registry-1.docker.io/v2/";
    repository = "library";
    imageName = "debian";
    tag = "latest";
    imageConfig = pkgs.fetchDockerConfig {
      inherit registry repository imageName tag;
      sha256 = "1viqbygsz9547jy830f2lk2hcrxjf7gl9h1xda9ws5kap8yw50ry";
    };
    imageLayers = let
      layer0 = pkgs.fetchDockerLayer {
        inherit registry repository imageName tag;
        layerDigest = "10a267c67f423630f3afe5e04bbbc93d578861ddcc54283526222f3ad5e895b9";
        sha256 = "1fcmx3aklbr24qsjhm6cvmhqhmrxr6xlpq75mzrk0dj2gz36g8hh";
      };
      in [ layer0 ];
  };
}
@
-}
generateFetchDockerExpr :: HockerImageMeta -> ConfigDigest -> [(Base16Digest, Base32Digest)] -> Either HockerException NExpr
generateFetchDockerExpr dim@HockerImageMeta{..} configDigest layerDigests = do
  let fetchconfig = mkFetchDockerConfig commonInherits configDigest
      fetchlayers =
        mkLets
         (mkFetchDockerLayers commonInherits layerDigests)
         (mkList $ fmap genLayerId [0..(Prelude.length layerDigests)-1])

  fetchDockerExpr <- mkFetchDocker dim fetchconfig fetchlayers

  pure (Fix $ NSet [ dockerImgExpr fetchDockerExpr ])

  where
    dockerImgExpr fDockerExpr = NamedVar imgSelector fDockerExpr
    genLayerId i = mkSym . T.pack $ "layer" <> show i
    imgSelector  =
        [ StaticKey "config"
        , StaticKey "docker"
        , StaticKey "images"
        , StaticKey imageName
        ]
    commonInherits = inherit
        [ StaticKey "registry"
        , StaticKey "repository"
        , StaticKey "imageName"
        , StaticKey "tag"
        ]

-- | Generate a @pkgs.fetchdocker { ... }@ function call and argument
-- attribute set. Please see 'generateNixExprs' documentation for an
-- example of full output.
mkFetchDocker :: HockerImageMeta -> NExpr -> NExpr -> Either HockerException NExpr
mkFetchDocker HockerImageMeta{..} fetchconfig fetchlayers = do
  registry <- Bifunctor.first mkHockerException serializedRegistry
  pure
    (mkApp (mkPkgsAttrSelector constFetchdocker)
     (attrsE
      [ ("name",        mkStr $ fromMaybe imageName altImageName)
      , ("registry",    mkStr registry)
      , ("repository",  mkStr imageRepo)
      , ("imageName",   mkStr imageName)
      , ("tag",         mkStr (T.pack $ coerce imageTag))
      , ("imageConfig", fetchconfig)
      , ("imageLayers", fetchlayers)
      ]))
  where
    serializedRegistry = decodeUtf8' (serializeURIRef' dockerRegistry)
    mkHockerException (DecodeError err char) =
      HockerException (err <> " " <> (show char)) Nothing Nothing
    mkHockerException err =
      HockerException (show err) Nothing Nothing


-- | Generate a @pkgs.fetchDockerConfig { ... }@ function call and
-- argument attrset. This function takes an argument for a list of
-- static keys to inherit from the parent attribute set; it helps
-- reduce the noise in the output expression.
mkFetchDockerConfig :: Binding NExpr -> Base32Digest -> NExpr
mkFetchDockerConfig inherits (Base32Digest digest) =
    mkApp (mkPkgsAttrSelector constFetchDockerConfig)
          (Fix $ NSet [ inherits, "sha256" $= (mkStr digest) ])

-- | Generate a list of Nix expression ASTs representing
-- @pkgs.fetchDockerLayer { ... }@ function calls. This function takes
-- an argument for a list of static keys to inherit from the parent
-- attribute set; it helps reduce the noise in the output expression.
--
-- NB: the hash digest tuple in the second argument is the base16
-- encoded hash digest plucked from the image's manifest JSON and a
-- @nix-hash@ base32 encoded copy.
--
-- This is necessary because fixed output derivations require a
-- pre-computed hash (which we have, thanks to the manifest) and the
-- hash must be base32 encoded using @nix-hash@'s own base32
-- encoding. The base16 encoded hash digest is needed intact in order
-- for the @pkgs.fetchDockerLayer@ builder script (which calls the
-- @hocker-layer@ utility) to download the layer from a docker
-- registry.
mkFetchDockerLayers :: Binding NExpr -> [(Base16Digest, Base32Digest)] -> [Binding NExpr]
mkFetchDockerLayers inherits layerDigests =
  fmap mkFetchLayer $ Prelude.zip [0..(Prelude.length layerDigests)] layerDigests
  where
    mkLayerId i = T.pack $ "layer" <> show i
    mkFetchLayer (i, ((Base16Digest d16), (Base32Digest d32))) =
      (mkLayerId i) $= mkApp (mkPkgsAttrSelector constFetchDockerLayer)
                             (Fix $ NSet
                                [ inherits
                                , "layerDigest" $= (mkStr d16) -- Required in order to perform a registry request
                                , "sha256"      $= (mkStr d32) -- Required by Nix for fixed output derivations
                                ])

-- | Generate a selector for an attribute within the @pkgs@ set; i.e
-- @pkgs.fetchDockerLayer@.
mkPkgsAttrSelector :: T.Text -> NExpr
mkPkgsAttrSelector k = Fix $ NSelect (mkSym "pkgs") [StaticKey k] Nothing
