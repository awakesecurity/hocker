{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Docker.Image.V1.Layer
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
--
-- Many of these functions are named after their equivalent functions
-- in the docker Golang source code.
--
-- <https://github.com/docker/docker/blob/6e8a2cd29113896acfc3f97a43dd27f751f2f519/layer/layer.go layer.go>
----------------------------------------------------------------------------

module Data.Docker.Image.V1.Layer where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as CL8
import           Data.Coerce
import           Data.Docker.Image.V1.Types
import           Data.Foldable
import qualified Data.HashMap.Strict        as H
import           Data.Monoid
import           Data.Sequence              as Seq
import           Data.Sequence.Lens
import qualified Data.Text                  as T

import           Lib

type Parent = ChainID
type TopLayerJSON = Data.Aeson.Object

-- | Produce a @ChainID@ using a sequence of layer @DiffIDs@.
--
-- <https://github.com/docker/docker/blob/6e8a2cd29113896acfc3f97a43dd27f751f2f519/layer/layer.go#L239 layer.CreateChainID>
createChainID :: Seq DiffID    -- ^ A sequence of layer @DiffID@s, (usually) fetched from the image's config JSON.
              -> Maybe ChainID
createChainID = createChainIDFromParent Nothing

-- | Produce a @ChainID@ given the @ChainID@ of a parent layer and a
-- sequence of layer @DiffIDs@.
--
-- <https://github.com/docker/docker/blob/6e8a2cd29113896acfc3f97a43dd27f751f2f519/layer/layer.go#L243 layer.createChainIDFromParent>
createChainIDFromParent :: Maybe Parent -- ^ Previous (parent) @ChainID@ in the sequence used to produce the next @ChainID@.
                        -> Seq DiffID   -- ^ A sequence of layer @DiffID@s, (usually) fetched from the image's config JSON.
                        -> Maybe ChainID
createChainIDFromParent parent (Seq.viewl -> EmptyL)    = parent
createChainIDFromParent parent (Seq.viewl -> h :< rest) =
  createChainIDFromParent (maybe root layer parent) rest
  where
    root  = Just $ coerce h
    layer = Just . flip chainDigest h

createChainIDFromParent parent _ = parent

-- | Produce a @ChainID@ given a parent @ChainID@ and a layer
-- @DiffID@.
chainDigest :: Parent -- ^ Parent @ChainID@ used to produce a child @ChainID@.
            -> DiffID -- ^ Layer @DiffID@.
            -> ChainID
chainDigest (show -> c) (show -> d) = ChainID .
    Lib.sha256 . CL8.pack $ concat [c, " ", d]

-- | Produce a sequence of @ChainID@s from a sequence of layer
-- @DiffID@s.
--
-- <https://github.com/docker/docker/blob/b826bebda0cff2cc2d3083b954c810d2889eefe5/image/tarexport/save.go#L242 save.saveImage>
chainIDSequence :: Seq DiffID
                -> Seq (Maybe ChainID)
chainIDSequence diffIDSeq = mapWithIndex chainIDSlice diffIDSeq
  where
    chainIDSlice (succ -> i) _ =
      createChainID $ seqOf (slicedTo i) diffIDSeq

-- | Produce a sequence of unwrapped Just's from a sequence of
-- Maybe's.
squishMaybe :: MonadPlus m => m (Maybe a) -> m a
squishMaybe = join . fmap adapt
  where
    adapt Nothing  = mzero
    adapt (Just x) = return x

-- | Produce layer content ID hashes given an empty JSON config with
-- the layer's @ChainID@ injected as the value of the `layer_id` key
-- and, if not the base layer, the previous @ContentID@ injected as
-- the value of the `parent` key.
--
-- The JSON that is encoded *must* be in the canonical format
-- specified by Docker, please see @Lib.encodeCanonical@ for a
-- convenience function to encode an @Aeson.Value@ satisfying those
-- rules.
contentIDSequence :: Seq ChainID  -- ^ A sequence of @ChainID@s, please see @chainIDSequence@.
                  -> TopLayerJSON -- ^ Config JSON paired with the top-most layer of the image.
                  -> Seq ContentID
contentIDSequence cids fj = foldl' (contentIDFold fj $ Seq.length cids) Seq.empty cids

-- | A folding function given to @foldl'@. This function computes the
-- @ContentID@'s for each layer using the last computed @ContentID@ as
-- the parent @ContentID@ for each iteration.
--
-- The first two arguments are closed over before being fed to
-- @foldl'@ producing a partial function that satisfies @foldl'@'s
-- first argument type signature.
contentIDFold :: TopLayerJSON  -- ^ Config JSON to be hashed with the top-most layer of the image.
              -> Int           -- ^ Length of the @ChainID@ sequence being folded over.
              -> Seq ContentID -- ^ The sequence of @ContentID@s accumulated.
              -> ChainID       -- ^ The @ChainID@ for producing a @ContentID@.
              -> Seq ContentID
contentIDFold _ _ acc@(Seq.viewr -> EmptyR) chainid =
  acc |> hashContent Nothing chainid emptyLayerJSON
contentIDFold topLayerJSON ln acc@(Seq.viewr -> _ :> parent) chainid =
  acc |> hashedContentID
  where
    -- Check to see if we're at the end of the sequence we're folding
    -- over, if so then hash the content using the top-layer config
    -- JSON instead of the empty JSON
    hashedContentID =
      if ln == (succ $ Seq.length acc)
      then hashContent (Just parent) chainid topLayerJSON
      else hashContent (Just parent) chainid emptyLayerJSON

contentIDFold _ _ acc chainid =
  acc |> hashContent Nothing chainid emptyLayerJSON

-- | Produce a @ContentID@, given a parent and a @ChainID@, builds the
-- empty JSON object with those two values and encodes it following
-- the canonical JSON rules.
hashContent :: Maybe ContentID    -- ^ Parent @ContentID@ for injection into the hashing JSON.
            -> ChainID            -- ^ @ChainID@ to be hashed with the hashing JSON.
            -> Data.Aeson.Object  -- ^ Aeson AST to be canonically encoded; this can be either the ephemeral JSON or the config JSON.
            -> ContentID
hashContent p c jsn = mk $ ephemeralHashableLayerJSON p c jsn
  where
    mk = ContentID . Lib.sha256 . Lib.encodeCanonical

-- | @emptyLayerJSON@ produces "empty" JSON for use in layer content
-- hashing.
--
-- The Aeson instances for @ContentID@, @DiffID@, and @ChainID@ will
-- correctly output a hex serialization of the SHA256 digest and
-- prefix it with "sha256:", which is necessary to correctly hash the
-- layer config in the same way that Docker's Golang code does it.
--
-- NB: I've manually assembled this in the "canonical order" it needs
-- to be in, in order to correctly hash the JSON string. There is also
-- a custom Aeson pretty printing function that serializes ADTs into
-- the canonical form and should make this function moot once an
-- appropriate ADT is in place.
--
-- TODO: codify this as an ADT to get rid of this manual construction
-- and make things clearer. For now, the manually constructed one is
-- fine (to get things working).
emptyLayerJSON :: Data.Aeson.Object
emptyLayerJSON = H.fromList
  [ "container_config" .= object
      [ "Hostname"      .= ("" :: String)
      , "Domainname"    .= ("" :: String) -- NB: this one isn't cased like the others :(
      , "User"          .= ("" :: String)
      , "AttachStdin"   .= False
      , "AttachStdout"  .= False
      , "AttachStderr"  .= False
      , "Tty"           .= False
      , "OpenStdin"     .= False
      , "StdinOnce"     .= False
      , "Env"           .= (Nothing :: Maybe String)
      , "Cmd"           .= (Nothing :: Maybe String)
      , "Image"         .= ("" :: String)

      -- This is a object with significant keys and empty values
      -- (don't ask me why)
      , "Volumes"       .= (Nothing :: Maybe Data.Aeson.Value)
      , "WorkingDir"    .= ("" :: String)
      , "Entrypoint"    .= (Nothing :: Maybe String)
      , "OnBuild"       .= (Nothing :: Maybe String)
      , "Labels"        .= (Nothing :: Maybe [String])
      ]

   -- This is the "canonical" empty timestamp
   , "created"  .= emptyTimeStamp
   ]

-- | Produce an "empty" JSON object given a parent and a
-- @ChainID@. This is used internally to produce the @ContentID@ hash
-- for a given layer.
ephemeralHashableLayerJSON :: Maybe ContentID   -- ^ Parent @ContentID@, if Nothing, will not be included in the Aeson AST.
                           -> ChainID           -- ^ @ChainID@ of the layer we're producing the @ContentID@ for.
                           -> Data.Aeson.Object -- ^ Aeson AST we want to inject the parent @ContentID@ and layer @ChainID@ into.
                           -> Data.Aeson.Value
ephemeralHashableLayerJSON parent layerid layerJSON =
  Object $ layerJSON `H.union` H.fromList
  ([ "layer_id" .= layerid ] <> (maybeSingletonParent parent))

-- | Produce a layer JSON object given a parent, a @ContentID@, and an
-- Aeson Value Object. This function is different from
-- @ephemeralHashableLayerJSON@ in that its output is (later on)
-- written to the filesystem alongside the `layer.tar` file within the
-- directory named after the @ContentID@ hash.
permanentLayerJSON :: Maybe ContentID
                   -> ContentID
                   -> Data.Aeson.Object
                   -> Data.Aeson.Value
permanentLayerJSON parent layerContentId layerJSON =
  Object $ layerJSON `H.union` H.fromList
    ([ "id" .= (mkPermHash layerContentId) ] <> maybeSingletonParent (mkPermHash <$> parent))
  where
    mkPermHash = Lib.stripHashId . T.pack . show

-- TODO: this should be parsed into an ADT, transformed algebraically
-- into what it should be, then re-encoded; instead of performing
-- Map-based operations on the AST. This was the quicker option though
-- for now; need to get something working first.
imageConfig2LayerConfig :: Data.Aeson.Object
                        -> Data.Aeson.Object
imageConfig2LayerConfig = H.filterWithKey keyWhitelist
  where
    keyWhitelist k _ = k `elem`
      [ "container"
      , "container_config"
      , "docker_version"
      , "config"
      , "architecture"
      , "os"
      ]

-- | Produce mempty if the parent is Nothing; if the parent is @Just
-- ContentID@ then it returns a singleton list with the expected
-- @Data.Aeson.Pair@ construction for the empty layer JSON.
--
-- The input argument is parameterized because the permanent JSON
-- config objects store hashes with the "sha256:" prefix stripped, but
-- the ephemeral JSON objects used to produce the Content ID hashes
-- want the "sha256:" prefix to be present!
maybeSingletonParent :: ToJSON a
                     => Maybe a
                     -> [(T.Text, Data.Aeson.Value)]
maybeSingletonParent = maybe mempty (singletonList . ("parent" .=))
  where
    -- Alternatively - singleton v = [v]
    singletonList = (: [])

-- | Produce the string "0001-01-01T00:00:00Z".
emptyTimeStamp :: String
emptyTimeStamp = "0001-01-01T00:00:00Z"
