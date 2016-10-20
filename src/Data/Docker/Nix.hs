{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Docker.Nix
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  AllRightsReserved
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
--
-- This module only re-exports Nix modules providing Docker-specific
-- functionality as it pertains to Nix.
----------------------------------------------------------------------------

module Data.Docker.Nix
( -- * Generating `fetchdocker` Nix Derivation Expressions
  module Data.Docker.Nix.FetchDocker
) where

import           Data.Docker.Nix.FetchDocker
