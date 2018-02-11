-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Docker.Image.AesonHelpers
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Data.Docker.Image.AesonHelpers where

import           Data.Aeson.Types

-- | Produce a default option record with @omitNothingFields@ set to
-- True by default.
stdOpts :: Options
stdOpts = defaultOptions{ omitNothingFields = True }
