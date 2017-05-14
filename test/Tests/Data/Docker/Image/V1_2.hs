{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Docker.Image.V1_2
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Tests.Data.Docker.Image.V1_2 where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8   as C8L
import           Data.Docker.Image.V1_2.Types
import           Data.HashMap.Strict          as H
import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib

-----------------------------------------------------------------------------
--
unitTests = testGroup "V1.2 Image Tests"
  [ testCase "ImageManifest golden encoding"      testImageManifestGoldenEncoding
  , testCase "ImageManifest two-way encoding"     testImageManifestTwoWayEncoding
  , testCase "ImageRepositories golden encoding"  testImageRepositoriesGoldenEncoding
  , testCase "ImageRepositories two-way encoding" testImageRepositoriesTwoWayEncoding
  ]

-----------------------------------------------------------------------------
-- TESTS

testImageManifestGoldenEncoding =
  let goldenStr = "[{\"Config\":\"3e83c23dba6a16cd936a3dc044df71b26706c5a4c28181bc3ca4a4af9f5f38ee.json\",\"Layers\":[\"10a267c67f423630f3afe5e04bbbc93d578861ddcc54283526222f3ad5e895b9/layer.tar\"],\"RepoTags\":[\"library/debian:jessie\"]}]"
      imgManifest = [ImageManifest
                      "3e83c23dba6a16cd936a3dc044df71b26706c5a4c28181bc3ca4a4af9f5f38ee.json"
                      [ "library/debian:jessie" ]
                      [ "10a267c67f423630f3afe5e04bbbc93d578861ddcc54283526222f3ad5e895b9/layer.tar" ]
                    ]
  in (Lib.encodeCanonical imgManifest) @?= (C8L.pack goldenStr)

testImageManifestTwoWayEncoding =
  let imgManifest = [ImageManifest
                      "3e83c23dba6a16cd936a3dc044df71b26706c5a4c28181bc3ca4a4af9f5f38ee.json"
                      [ "library/debian:jessie" ]
                      [ "10a267c67f423630f3afe5e04bbbc93d578861ddcc54283526222f3ad5e895b9/layer.tar" ]
                    ]
      encoded     = Lib.encodeCanonical imgManifest
  in decode encoded @?= (Just imgManifest)

testImageRepositoriesGoldenEncoding =
  let goldenStr   = "{\"library/debian\":{\"jessie\":\"10a267c67f423630f3afe5e04bbbc93d578861ddcc54283526222f3ad5e895b9\"}}"
      imgRepos    = ImageRepositories
                     [ImageRepo
                        "library/debian"
                        (H.singleton
                           "jessie"
                           "10a267c67f423630f3afe5e04bbbc93d578861ddcc54283526222f3ad5e895b9")]

  in (Lib.encodeCanonical imgRepos) @?= (C8L.pack goldenStr)

testImageRepositoriesTwoWayEncoding =
  let imgRepos = ImageRepositories
                  [ImageRepo
                     "library/debian"
                     (H.singleton
                        "jessie"
                        "10a267c67f423630f3afe5e04bbbc93d578861ddcc54283526222f3ad5e895b9")]
      encoded  = Lib.encodeCanonical imgRepos
  in decode encoded @?= (Just imgRepos)
