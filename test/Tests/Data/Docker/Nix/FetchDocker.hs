{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Docker.Nix.FetchDocker
-- Copyright   :  (C) 2017 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Tests.Data.Docker.Nix.FetchDocker where

import           Control.Exception            as CE
import           Control.Monad.Except         as Except
import           Data.ByteString              as BS
import           Data.ByteString.Lazy.Char8   as C8L
import           Data.Either                  (either)
import qualified Data.Text                    as Text
import qualified Prettyprinter.Render.String
import           Data.Word8                   as W8
import           Network.URI

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.Golden.Advanced   (goldenTest)
import           Test.Tasty.HUnit
import           Text.Printf                  (printf)

import           Data.Docker.Image.Types
import           Data.Docker.Nix.FetchDocker  as Nix.FetchDocker
import           Data.Docker.Nix.Lib          as Nix.Lib
import           Hocker.Lib
import           Network.Wreq.Docker.Registry as Docker.Registry
import           Hocker.Types
import           Hocker.Types.ImageTag

-- | Compare a given string against the golden file contents,
-- ignoring differences in contiguous nonempty spans of whitespace,
-- and the presence or absence of whitespace before or after a comma.
goldenVsStringCanonicalize
  :: TestName -- ^ test name
  -> FilePath -- ^ path to golden file
  -> IO C8L.ByteString -- ^ action that returns string to compare
  -> TestTree -- ^ the test verifies that the returned string equals the
              -- golden file contents when ignoring differences in whitespace
goldenVsStringCanonicalize name ref act =
  goldenTest
    name
    (BS.readFile ref)
    (C8L.toStrict <$> act)
    cmp
    upd
  where
  cmp x y = cmpCanonicalize msg x y
    where
    msg = printf "Test output was different from '%s'. It was: %s" ref (show y)
  upd = BS.writeFile ref

cmpCanonicalize ::
  String -> BS.ByteString -> BS.ByteString -> IO (Maybe String)
cmpCanonicalize e x y =
    return $ if canonicalize x == canonicalize y then Nothing else Just e
  where
    canonicalize = BS.pack . BS.foldr op []
    op x acc
      | W8.isSpace x, y : _ <- acc, y == W8._space = acc
      | W8.isSpace x, y : _ <- acc, y == W8._comma = acc
      | W8.isSpace x = W8._space : acc
      | x == W8._comma, y : ys <- acc, y == W8._space = W8._comma : ys
      | otherwise = x : acc

tests = testGroup "FetchDocker Nix Generation Tests"
  [ goldenVsStringCanonicalize
      "Golden vs. Generated `fetchDocker' Nix Expression"
      "test/data/golden-debian_jessie.nix"
      generateFetchDockerNix
  , testCase "Base16 Digest to Base32 Digest" testBase16toBase32
  ]

testBase16toBase32 :: Assertion
testBase16toBase32 = do
  let b16     = Base16Digest "5c90d4a2d1a8dfffd05ff2dd659923f0ca2d843b5e45d030e17abbcd06a11b5b"
      b32     = Base32Digest "0nqvl43cvfvsw4qd0iay7f22vjph4fcnbpgjbz8gzpx8s6id942w"

  res <- Except.runExceptT (Nix.Lib.toBase32Nix b16)

  either
    (assertFailure . show)
    (assertEqual "" b32)
    res

generateFetchDockerNix :: IO C8L.ByteString
generateFetchDockerNix = do
  manifest      <- C8L.readFile "test/data/manifest-debian_jessie.json"
  nixExpression <- Nix.FetchDocker.generate
    HockerImageMeta
      { imageRepo      = "library"
      , imageName      = "debian"
      , imageTag       = ImageTag "jessie"
      , manifestJSON   = manifest
      , dockerRegistry = defaultRegistry
      , altImageName   = Nothing
      }

  let display = Prettyprinter.Render.String.renderString

  either
    (Hocker.Lib.die . Text.pack . show)
    (return . C8L.pack . display . Hocker.Lib.renderNixExpr)
    nixExpression
