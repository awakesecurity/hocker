{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Types.ImageName
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Types.ImageName where

import           Control.DeepSeq
import           Data.Monoid
import qualified Options.Applicative as Options
import           Options.Generic

newtype ImageName = ImageName { unImageName :: String }
  deriving (Generic, Show)

instance ParseField ImageName where
  parseField _ _ _ =
    ImageName <$>
      (Options.argument Options.str $
       ( Options.metavar "IMAGE-NAME"
      <> Options.help "Docker image name, e.g: 'debian' in debian:jessie"
       )
      )

instance ParseFields ImageName where
instance ParseRecord ImageName where
  parseRecord = fmap getOnly parseRecord

instance NFData ImageName
