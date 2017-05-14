{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Docker.Image.V1
-- Copyright   :  (C) 2016 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Tests.Data.Docker.Image.V1 where

import qualified Crypto.Hash                as Hash
import qualified Data.ByteString.Char8      as C8
import           Data.Docker.Image.V1.Layer
import           Data.Docker.Image.V1.Types
import           Data.Maybe
import           Data.Sequence              as Seq

import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib

unitTests = testGroup "V1 Image Tests"
  [ testCase "Digest (De)Serialization"    testDigest
  , testCase "Handle bad digest"           testBadDigest1
  , testCase "Handle bad digest"           testBadDigest2
  , testCase "Digest == ChainID"           testChainID
  , testCase "Digest == DiffID"            testDiffID
  , testCase "ChainID sequence generation" testChainIDGeneration
  ]

mkHash = Lib.sha256 "somestring"

-- DiffID sequence from a real Docker Image.
diffIds :: Seq DiffID
diffIds = fromList $ fmap (DiffID . fromJust . toDigest)
  [ "sha256:f96222d75c5563900bc4dd852179b720a0885de8f7a0619ba0ac76e92542bbc8"
  , "sha256:149636c850120e59e6bb79f2fc23ed423030afc73841c221906a147d61da11a9"
  , "sha256:33c3a104206aed2ae947e03c48cc011af0a3e5b87e7ba8e7cbc298273a638921"
  , "sha256:2681a05b8f8288a384dbddf0b899ec9d2bea3ee265f1678230d0bdac6dc13da1"
  , "sha256:dcfda398b984bb5a55e1932079b6cc4823e433bd6c962f9667eaf28b0f1fe7e0"
  , "sha256:2a182bf72d68b9c7cb76be0f9dcadd047088ae6f8cb85e7ac9661f68537acccd"
  , "sha256:647af69f55fd5fdc27db7b6aa51945aec53b0b03d17095e79b4c69c6432195c7"
  , "sha256:c7ef4827bb9592e9788c1cc49e3db4e265c12f49c9b1f6c9bb99551eb5189020"
  , "sha256:f9361c1f9b1eb2d93709546fe6ad48786cea55c03c4e52d3f1cdb341e0d398da"
  ]

-- Pre-computed golden result produced by a valid Python
-- implementation of the ChainID sequence generation logic.
preComputedChainIds :: Seq ChainID
preComputedChainIds = fromList $ fmap (ChainID . fromJust . toDigest)
  [ "sha256:f96222d75c5563900bc4dd852179b720a0885de8f7a0619ba0ac76e92542bbc8"
  , "sha256:5e6f832cd2df18460af48ed117c5b63bc2189971c9346e6d952376b5a8ba74ff"
  , "sha256:19947c09eddb9dab0d1b938660cd72ea4bb8f0f24c604cf9e1d9b14772d7bd6d"
  , "sha256:b0fbea1a99ec834d59e524733f1be81f1dce325dbe9df58bba5dec7014b386c8"
  , "sha256:262faf2cc4db81d3bcb526099b7dc33069b24f4028a9a23d46edca2493077ce0"
  , "sha256:ac07dba5e07787c2a10edc3f8d8052f38cb5bec6767520bbab4289cb55b3a3f4"
  , "sha256:c781557b490e1e8ff2132af386abe2a9c2d3cb66df06ee2cbd489d869432328a"
  , "sha256:ff275e52e374819094e8035459820bf8e5fc42f287f603b445a8aee7aba2b689"
  , "sha256:ffd859ffb35598eeec1283f3ccb3633f2798c042343425f635d616633cf63c2b"
  ]

testDigest =
  let digest    = mkHash
      digestStr = showSHA digest
  in toDigest (C8.pack digestStr) @?= (Just digest)

testBadDigest1 = toDigest "ffd859ffb35598eeec1283f3ccb3633f2798c042343425f635d616633cf63c2b" @?= Nothing
testBadDigest2 = toDigest "ffd859ffb35598eeec1283f3corrupt?" @?= Nothing

testChainID =
  let digest = mkHash
  in (show $ ChainID digest) @?= showSHA digest

testDiffID =
  let digest = mkHash
  in (show $ DiffID digest) @?= showSHA digest

testChainIDGeneration =
  let chainIDs = squishMaybe $ chainIDSequence diffIds
  in chainIDs @?= preComputedChainIds
