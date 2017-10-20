{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Hocker.Types.Hash
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Hocker.Types.Hash where

import qualified Crypto.Hash             as Hash
import qualified Data.ByteArray          as BA
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Char8   as C8
import           Data.Monoid
import qualified Data.Text
import qualified Options.Applicative     as Options
import           Options.Generic

toBytes :: C8.ByteString -> Either String BA.Bytes
toBytes = BA.convertFromBase BA.Base16

readSHA256 :: C8.ByteString -> Maybe (Hash.Digest Hash.SHA256)
readSHA256 = either (const Nothing) Hash.digestFromByteString . toBytes

instance ParseField (Hash.Digest Hash.SHA256) where
  parseField h _ _ =
      (Options.option (Options.maybeReader (readSHA256 . C8.pack)) $
       ( Options.metavar "SHA256"
       <> Options.short 'l'
       <> Options.long "layer"
       <> maybe mempty (Options.help . Data.Text.unpack) h
       )
      )

instance ParseFields (Hash.Digest Hash.SHA256) where
instance ParseRecord (Hash.Digest Hash.SHA256) where
  parseRecord = fmap getOnly parseRecord
