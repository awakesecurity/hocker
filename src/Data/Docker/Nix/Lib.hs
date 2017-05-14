{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleContexts     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Docker.Nix.Lib
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Data.Docker.Nix.Lib where

import           Control.Foldl                   as Foldl
import           Turtle
import           Control.Monad.Except as Except
import qualified Data.Text                       as T

import           Types
import           Types.Exceptions

-- | Convert a @Base16Digest@ to a @Base32Digest@ using the supplied
-- `nix-hash` utility.
--
-- NB: Nix implements its own custom base32 encoding function for
-- hashes that is not compatible with other more standard and native
-- implementations in Haskell. I opted to call out to `nix-hash`
-- instead of re-implementing their algorithm here in Haskell because
-- it's non-standard and may change, creating a maintenance headache
-- and "surprise" behavior for users.
toBase32Nix :: (MonadIO m, Except.MonadError HockerException m)
            => Prelude.FilePath -- ^ Path to the `nix-hash` executable, see @Lib.findExec@.
            -> Base16Digest     -- ^ @Base16@ hash digest to @Base32@ encode.
            -> m Base32Digest
toBase32Nix nixhash (Base16Digest d16) =
  Turtle.fold convertDigest Foldl.head >>= \case
    Nothing     -> throwError $ HockerException "nothing was returned by `nix-hash', not even an error" Nothing Nothing
    Just result ->
      either
       (throwError . hockerExc . T.unpack . lineToText)
       (return . Base32Digest . lineToText)
       result
  where
    hockerExc m   = HockerException m Nothing Nothing
    convertDigest =
      inprocWithErr
        (T.pack nixhash)
        [ "--type"
        , "sha256"
        , "--to-base32"
        , d16
        ]
        Turtle.empty
