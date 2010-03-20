{-# LANGUAGE OverloadedStrings #-}

{-|
  Parser for URI
-}

module Network.Web.URI (
    URI, uriScheme, uriAuthority, uriPath, uriQuery, uriFragment
  , URIAuth, uriUserInfo, uriRegName, uriPort
  , parseURI
  , uriHostName, toURLwoPort
  , isAbsoluteURI, unEscapeString
  ) where

import qualified Data.ByteString.Char8 as S
import Data.Char

{-|
  Abstract data type for URI
-}
data URI = URI {
    uriScheme    :: S.ByteString
  , uriAuthority :: Maybe URIAuth
  , uriPath      :: S.ByteString
  , uriQuery     :: S.ByteString
  , uriFragment  :: S.ByteString
} deriving Show

{-|
  Abstract data type for URI Authority
-}
data URIAuth = URIAuth {
    uriUserInfo :: S.ByteString
  , uriRegName  :: S.ByteString
  , uriPort     :: S.ByteString
} deriving Show

----------------------------------------------------------------

{-|
  Parsing URI.
-}
parseURI :: S.ByteString -> Maybe URI
parseURI url = Just URI {
    uriScheme = "http:"
  , uriAuthority = Just URIAuth {
        uriUserInfo = ""
      , uriRegName = host
      , uriPort = port
      }
  , uriPath = path
  , uriQuery = query
  , uriFragment = ""
  }
  where
    (auth,pathQuery) = parseURL url
    (path,query) = parsePathQuery pathQuery
    (host,port) = parseAuthority auth

parseURL :: S.ByteString -> (S.ByteString,S.ByteString)
parseURL reqUri = let (hostServ,path) = S.break (=='/') $ S.drop 7 reqUri
                  in (hostServ, checkPath path)
  where
    checkPath ""   = "/"
    checkPath path = path

parsePathQuery :: S.ByteString -> (S.ByteString,S.ByteString)
parsePathQuery = S.break (=='?')

parseAuthority :: S.ByteString -> (S.ByteString,S.ByteString)
parseAuthority hostServ
  | serv == "" = (host, "")
  | otherwise  = (host, S.tail serv)
  where
    (host,serv) = S.break (==':') hostServ

----------------------------------------------------------------

{-|
  Getting a hostname from 'URI'.
-}
uriHostName :: URI -> S.ByteString
uriHostName uri = maybe "" uriRegName $ uriAuthority uri

{-|
  Making a URL string from 'URI' without port.
-}
toURLwoPort :: URI -> S.ByteString
toURLwoPort uri = uriScheme uri +++ "//" +++ uriHostName uri +++ uriPath uri +++ uriQuery uri
  where
    (+++) = S.append

----------------------------------------------------------------

{-|
  Checking whether or not URI starts with \"http://\".
-}
isAbsoluteURI :: S.ByteString -> Bool
isAbsoluteURI url = "http://" `S.isPrefixOf` url

{-|
  Decoding the %XX encoding.
-}
unEscapeString :: S.ByteString -> S.ByteString
unEscapeString "" = ""
unEscapeString bs
  | S.head bs == '%' && S.length bs >= 3
    && isHexDigit c1 && isHexDigit c2    = dc <:> unEscapeString cs
  where
    [_,c1,c2] = S.unpack $ S.take 3 bs
    cs = S.drop 3 bs
    dc = chr $ digitToInt c1 * 16 + digitToInt c2
    (<:>) = S.cons

unEscapeString bs = c <:> unEscapeString cs
  where
    c = S.head bs
    cs = S.tail bs
    (<:>) = S.cons
