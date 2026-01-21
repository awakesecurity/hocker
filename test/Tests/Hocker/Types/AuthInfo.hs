{-# LANGUAGE OverloadedStrings #-}

module Tests.Hocker.Types.AuthInfo (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.ByteString.Char8 as C8
import           Data.Either           (isLeft)
import           URI.ByteString        (Absolute, URIRef, parseURI, strictURIParserOptions)
import           Hocker.Types.AuthInfo (AuthInfo(..), parseWWWAuthHeader)

mkURI :: C8.ByteString -> URIRef Absolute
mkURI bs = case parseURI strictURIParserOptions bs of
  Right u -> u
  Left  e -> error $ "Test configuration error, invalid URI: " ++ show e

tests :: TestTree
tests = testGroup "Hocker.Types.AuthInfo Parsing"
  [ testGroup "Valid Headers"
      [ testCase "Internal Corporate Header" $ do
          let h = "Bearer realm=\"https://auth.internal.example.com/v2/token\",service=\"auth.internal.example.com\",scope=\"repository:project/image:pull,push\""
          let expected = AuthInfo 
                { realm   = mkURI "https://auth.internal.example.com/v2/token"
                , service = "auth.internal.example.com"
                , scope   = "repository:project/image:pull,push"
                }
          parseWWWAuthHeader h @?= Right expected

      , testCase "Docker Hub Header" $ do
          let h = "Bearer realm=\"https://auth.docker.io/token\",service=\"registry.docker.io\",scope=\"repository:library/node:pull\""
          let expected = AuthInfo 
                { realm   = mkURI "https://auth.docker.io/token"
                , service = "registry.docker.io"
                , scope   = "repository:library/node:pull"
                }
          parseWWWAuthHeader h @?= Right expected

      , testCase "Parameter Permutation & Whitespace" $ do
          -- RFC 7235: Order of params shouldn't matter; OWS (Optional White Space) allowed
          let h = "Bearer scope=\"read\",  service=my-svc,  realm=\"https://example.com/\""
          let expected = AuthInfo 
                { realm   = mkURI "https://example.com/"
                , service = "my-svc"
                , scope   = "read"
                }
          parseWWWAuthHeader h @?= Right expected

      , testCase "Ignore extra args" $ do
          let h = "Bearer scope=\"read\", foo=baz, service=my-svc,  realm=\"https://example.com/\", "
          let expected = AuthInfo 
                { realm   = mkURI "https://example.com/"
                , service = "my-svc"
                , scope   = "read"
                }
          parseWWWAuthHeader h @?= Right expected
      
      , testCase "Ignore extra challenges" $ do
          let h = "Bearer scope=\"read\", foo=baz, service=my-svc,  realm=\"https://example.com/\",,,,,,,,,, Bearer scope=\"read\", foo=baz, service=other-svc,  realm=\"https://example.com/\", "
          let expected = AuthInfo 
                { realm   = mkURI "https://example.com/"
                , service = "my-svc"
                , scope   = "read"
                }
          parseWWWAuthHeader h @?= Right expected

      , testCase "Ignore extra non-bearer challenges" $ do
          let h = "Newauth realm=\"apps\", type=1, title=\"Login to \\\"apps\\\"\", Basic realm=\"simple\",Bearer scope=\"read\", foo=baz, service=my-svc,  realm=\"https://example.com/\",Basic realm=\"foo\", charset=\"UTF-8\""
          let expected = AuthInfo 
                { realm   = mkURI "https://example.com/"
                , service = "my-svc"
                , scope   = "read"
                }
          parseWWWAuthHeader h @?= Right expected
      ]

  , testGroup "Invalid Headers (missing required fields)"
      [ testCase "Missing Scope" $ do
          let h = "Bearer realm=\"https://example.com/\", service=\"svc\""
          assertBool "Should fail: missing scope" (isLeft $ parseWWWAuthHeader h)

      , testCase "Missing Realm" $ do
          let h = "Bearer service=\"s\", scope=\"*\""
          assertBool "Should fail: missing realm" (isLeft $ parseWWWAuthHeader h)

      , testCase "Non-Absolute URI in Realm" $ do
          let h = "Bearer realm=\"/v2/token\", service=\"s\", scope=\"*\""
          assertBool "Should fail: relative realm" (isLeft $ parseWWWAuthHeader h)

      , testCase "Malformed Quoted String" $ do
          let h = "Bearer realm=\"https://example.com/\", service=\"svc, scope=\"unclosed\""
          assertBool "Should fail: unclosed quotes" (isLeft $ parseWWWAuthHeader h)
      ]

  , testGroup "Invalid Headers (garbage data)"
      [ testCase "Missing Scope" $ do
          let h = "Basic realm=\"WallyWorld\""
          assertBool "Should fail: only Bearer challenge is supported" (isLeft $ parseWWWAuthHeader h)

      , testCase "Missing Realm" $ do
          let h = "Bearer service=\"s\", scope=\"*\""
          assertBool "Should fail: relative realm" (isLeft $ parseWWWAuthHeader h)

      , testCase "Non-Absolute URI in Realm" $ do
          let h = "Bearer realm=\"/v2/token\", service=\"s\", scope=\"*\""
          assertBool "Should fail: relative realm" (isLeft $ parseWWWAuthHeader h)

      , testCase "Malformed Quoted String" $ do
          let h = "Bearer realm=\"https://example.com/\", service=\"svc, scope=\"unclosed\""
          assertBool "Should fail: unclosed quotes" (isLeft $ parseWWWAuthHeader h)
      ]
  ]
