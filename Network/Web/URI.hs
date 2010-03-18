module Network.Web.URI (
    URI, uriScheme, uriAuthority, uriPath, uriQuery, uriFragment
  , URIAuth, uriUserInfo, uriRegName, uriPort
  , uriHostName, toURLwoPort, parseURI, isAbsoluteURI
  , unEscapeString
  ) where

import Data.Char
import Data.List
import Network.BSD

data URI = URI {
    uriScheme :: String
  , uriAuthority :: Maybe URIAuth
  , uriPath :: String
  , uriQuery :: String
  , uriFragment :: String
} deriving Show

data URIAuth = URIAuth {
    uriUserInfo :: String
  , uriRegName :: String
  , uriPort :: String
} deriving Show

{-|
  Getting a hostname from 'URI'.
-}
uriHostName :: URI -> String
uriHostName uri = maybe "" uriRegName $ uriAuthority uri

{-|
  Making a URL string from 'URI' without port.
-}
toURLwoPort :: URI -> String
toURLwoPort uri = uriScheme uri ++ "//" ++ uriHostName uri ++ uriPath uri ++ uriQuery uri

{-|
  Parsing URI.
-}
parseURI :: String -> Maybe URI
parseURI url = Just URI {
    uriScheme = "http"
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

parseURL :: String -> (HostName,FilePath)
parseURL reqUri = let (hostServ,path) = break (=='/') $ drop 7 reqUri
                  in (hostServ, checkPath path)
  where
    checkPath ""   = "/"
    checkPath path = path

parsePathQuery :: FilePath -> (FilePath,FilePath)
parsePathQuery = break (=='?')

parseAuthority :: HostName -> (HostName, ServiceName)
parseAuthority hostServ
  | serv == "" = (host, "")
  | otherwise  = (host, tail serv)
  where
    (host,serv) = break (==':') hostServ

isAbsoluteURI :: String -> Bool
isAbsoluteURI url = "http://" `isPrefixOf` url

unEscapeString :: String -> String
unEscapeString [] = ""
unEscapeString ('%':c1:c2:cs)
  | isHexDigit c1 && isHexDigit c2 = dc : unEscapeString cs
  where
    dc = chr $ digitToInt c1 * 16 + digitToInt c2
unEscapeString (c:cs) = c : unEscapeString cs
