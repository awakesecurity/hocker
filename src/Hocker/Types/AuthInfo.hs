{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Hocker.Types.AuthInfo
-- Copyright   :  (C) 2026 Awake Networks
-- License     :  Apache-2.0
-- Maintainer  :  Awake Networks <opensource@awakenetworks.com>
-- Stability   :  stable
----------------------------------------------------------------------------

module Hocker.Types.AuthInfo (
  AuthInfo(..),
  parseWWWAuthHeader
) where

import           Control.Applicative          ((<|>))
import           Data.Bifunctor               (first)
import qualified Data.ByteString.Char8        as C8
import qualified Data.CaseInsensitive         as CI
import           Data.Char                    (isAlphaNum, ord)
import           Data.Maybe                   (listToMaybe, mapMaybe)
import           URI.ByteString               (Absolute, URIRef, parseURI, strictURIParserOptions)
import           Text.Read                    (readEither)
import           Text.ParserCombinators.ReadP

import           Hocker.Types.Exceptions


data AuthInfo = AuthInfo
  { realm   :: URIRef Absolute
  , service :: C8.ByteString
  , scope   :: C8.ByteString
  }

newtype AuthScheme = AuthScheme (CI.CI String)
  deriving (Show, Eq)

data AuthParams = AuthParamsB64 String | AuthParamsArr [(CI.CI String, String)]
  deriving (Show, Eq)

data Challenge = Challenge
  { scheme :: AuthScheme
  , params :: AuthParams
  } deriving (Show, Eq)

isWhitespace :: Char -> Bool
isWhitespace ' '  = True
isWhitespace '\t' = True
isWhitespace _    = False

-- OWS            = *( SP / HTAB )
--                ; optional whitespace
-- BWS            = OWS
--                ; "bad" whitespace
ows :: ReadP String
ows = munch isWhitespace

-- tchar          = "!" / "#" / "$" / "%" / "&" / "'" / "*"
--                / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
--                / DIGIT / ALPHA
--                ; any VCHAR, except delimiters
tchar :: ReadP Char
tchar = satisfy isTchar
  where
    isTchar c = isAlphaNum c || c `elem` ("!#$%&'*+-.^_`|~" :: String)

-- token          = 1*tchar
token :: ReadP String
token = many1 tchar

-- token68        = 1*( ALPHA / DIGIT / "-" / "." / "_" / "~" / "+" / "/" ) *"="
token68 :: ReadP String
token68 = do
  let validChar c = isAlphaNum c || c `elem` ("-._~+/" :: String)
  part1 <- many1 $ satisfy validChar
  part2 <- many $ satisfy (== '=')
  return (part1 <> part2)

-- quoted-string  = DQUOTE *( qdtext / quoted-pair ) DQUOTE
quotedString :: ReadP String
quotedString = do
  _      <- char '"'
  str    <- many (qdtext <|> quotedPair)
  _      <- char '"'
  return str

-- obs-text       = %x80-FF
-- qdtext         = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text
qdtext :: ReadP Char
qdtext = satisfy isQdtext
  where
    isQdtext c = 
      let code = ord c 
      in isWhitespace c || code == 0x21 
         || (code >= 0x23 && code <= 0x5B) 
         || (code >= 0x5D && code <= 0x7E) 
         || (code >= 0x80 && code <= 0xFF)

-- quoted-pair    = "\" ( HTAB / SP / VCHAR / obs-text )
quotedPair :: ReadP Char
quotedPair = do
  _ <- char '\\'
  satisfy (\c -> let code = ord c in isWhitespace c || (code >= 0x21 && code <= 0x7E) || (code >= 0x80 && code <= 0xFF))

authScheme :: ReadP AuthScheme
authScheme = (AuthScheme . CI.mk) <$> token

-- auth-param = token BWS "=" BWS ( token / quoted-string )
authParam :: ReadP (CI.CI String, String)
authParam = do
  key <- token
  _   <- ows
  _   <- char '='
  _   <- ows
  val <- token <|> quotedString
  return (CI.mk key, val)

-- Not part of RFC, extracted for readability
-- 1*SP ( token68 / [ ( "," / auth-param ) *( OWS "," [ OWS auth-param ] ) ] )
authParams :: ReadP AuthParams
authParams = do
  _ <- munch1 (== ' ')

  let authToken = AuthParamsB64 <$> token68
  let authParams' = do
        _   <- many $ (char ',' >> ows)
        res <- sepBy authParam (ows >> char ',' >> (munch (\c -> isWhitespace c || c == ',')))
        _   <- many $ (ows >> char ',')
        pure $ AuthParamsArr res

  authToken <|> authParams'

-- challenge = auth-scheme [ 1*SP ( token68 / [ ( "," / auth-param ) *( OWS "," [ OWS auth-param ] ) ] ) ]
challenge :: ReadP Challenge
challenge = do
  s <- authScheme
  ps <- authParams <|> (pure $ AuthParamsArr [])
  return $ Challenge s ps

-- WWW-Authenticate = *( "," OWS ) challenge *( OWS "," [ OWS challenge ] )
challenges :: ReadP [Challenge]
challenges = do
  -- The header can start with an "empty" challenge
  _ <- many $ (char ',' >> ows)
  let sepByAtLeastComma = (ows >> char ',' >> (munch (\c -> isWhitespace c || c == ',')))
  cs <- sepBy1 challenge sepByAtLeastComma
  -- It can also end with "empty" challenges
  _ <- many $ (char ',' >> ows)
  return cs

newtype WWWAuthHeader = WWWAuthHeader [Challenge]

instance Read WWWAuthHeader where
  readsPrec _ = readP_to_S $ WWWAuthHeader <$> (challenges <* eof)

parseWWWAuthHeader :: C8.ByteString -> Either HockerException AuthInfo
parseWWWAuthHeader headerValue = do
  WWWAuthHeader parsedChallenges <- first hockerException $ readEither $ C8.unpack headerValue

  maybe notFoundErr Right $ listToMaybe $ mapMaybe transform parsedChallenges
        where
          notFoundErr = Left $ hockerException "Unable to extract AuthInfo from WWW-Authentication header"
          transform c
            | scheme c /= AuthScheme (CI.mk "bearer") = Nothing
            | otherwise = case params c of
                AuthParamsArr ps -> do
                  rawRealm <- C8.pack <$> lookup (CI.mk "realm") ps
                  service  <- C8.pack <$> lookup (CI.mk "service") ps
                  scope    <- C8.pack <$> lookup (CI.mk "scope") ps

                  realm <- either (\_ -> Nothing) Just $ parseURI strictURIParserOptions rawRealm
                  pure AuthInfo{ realm, service, scope }
                
                AuthParamsB64 _ -> Nothing -- Not supported for now
