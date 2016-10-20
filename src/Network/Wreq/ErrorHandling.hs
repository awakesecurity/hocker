{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Wreq.ErrorHandling
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  AllRightsReserved
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Network.Wreq.ErrorHandling where

import           Control.Exception.Lifted  as Lifted
import           Control.Lens
import           Control.Monad.Except
import           Data.ByteString.Char8     as C8
import           Data.Monoid
import           Network.HTTP.Client
import           Network.HTTP.Types.Status

#if !MIN_VERSION_http_client(0,5,0)
import           Data.HashMap.Lazy         as H
#endif

import           Types.Exceptions

interceptHttpExc :: ExceptT HockerException IO a
                 -> ExceptT HockerException IO a
interceptHttpExc a = Lifted.try a >>= except . over _Left prettify
  where
    except (Left  e) = throwError e
    except (Right v) = return v

prettify :: HttpException -> HockerException
#if MIN_VERSION_http_client(0,5,0)
prettify
  (HttpExceptionRequest _
   (StatusCodeException
    (responseStatus -> (Status code msg)) body))
  = HockerException
      (show code <> " " <> C8.unpack msg)
      (Just $ C8.unpack body)
      Nothing
#else
prettify
  (StatusCodeException (Status code msg) (H.fromList -> e) _)
  = HockerException
      ((show code) <> " " <> C8.unpack msg)
      (C8.unpack <$> H.lookup "X-Response-Body-Start" e)
      Nothing
#endif

prettify e = HockerException (show e) Nothing Nothing
