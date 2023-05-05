{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Hocker.Types.Exceptions
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Hocker.Types.Exceptions where

import           Control.DeepSeq
import           Control.Exception
import           GHC.Generics

data HockerException = HockerException
  { baseMsg  :: String
  , expected :: Maybe String
  , received :: Maybe String
  } deriving (Read, Generic, NFData)

instance Exception HockerException
instance Show HockerException where
  show (HockerException m e r) = m <> (ext $ e <> r)
    where
      ext (Just v) = "; " <> v
      ext Nothing  = mempty

hockerException :: String -> HockerException
hockerException m = HockerException m Nothing Nothing
