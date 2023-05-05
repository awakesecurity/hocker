{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Hocker.Types.ImageTag
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Hocker.Types.ImageTag where

import           Control.DeepSeq
import qualified Options.Applicative as Options
import           Options.Generic

newtype ImageTag = ImageTag { unImageTag :: String }
  deriving (Generic, Show)

instance ParseField ImageTag where
  readField = ImageTag <$> Options.str
  parseField _help _long _short _value =
    ImageTag <$>
      (Options.argument Options.str $
       ( Options.metavar "IMAGE-TAG"
      <> Options.help "Docker image tag identifier, e.g: 'jessie' in debian:jessie"
       )
      )

instance ParseFields ImageTag where
instance ParseRecord ImageTag where
  parseRecord = fmap getOnly parseRecord

instance NFData ImageTag
