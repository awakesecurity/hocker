{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS -fno-warn-orphans  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Types.URI
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Types.URI where

import           Control.Lens
import qualified Data.ByteString.Char8       as C8
import           Data.Monoid
import qualified Data.Text                   as Text
import qualified Options.Applicative         as Options
import           Options.Applicative.Builder
import           Options.Generic
import           URI.ByteString

-- | Parse a URI value.
uriReader :: ReadM (URIRef Absolute)
uriReader = Options.eitherReader parseURIArg
  where
    parseURIArg (parseURI strictURIParserOptions . C8.pack -> parsedURI) =
      over _Left show parsedURI

instance ParseField (URIRef Absolute) where
  parseField h n s =
      (Options.option uriReader $
       ( Options.metavar "URI"
       <> foldMap (Options.long  . Text.unpack) n
       <> foldMap Options.short s
       <> foldMap (Options.help  . Text.unpack) h
       )
      )

instance ParseFields (URIRef Absolute) where
instance ParseRecord (URIRef Absolute) where
  parseRecord = fmap getOnly parseRecord
