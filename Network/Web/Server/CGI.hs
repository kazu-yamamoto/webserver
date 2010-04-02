{-# LANGUAGE OverloadedStrings #-}

module Network.Web.Server.CGI (tryGetCGI) where

import Control.Applicative
import Control.Concurrent
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import Network.TCPInfo
import Network.Web.HTTP
import Network.Web.Server.Params
import Network.Web.URI
import System.IO
import System.Posix.IO
import System.Process
import System.Timeout

gatewayInterface :: String
gatewayInterface = "CGI/1.1"

----------------------------------------------------------------

tryGetCGI :: BasicConfig -> Request -> CGI -> IO (Maybe Response)
tryGetCGI cnf req cgi = processCGI `catch` const internalError
  where
    processCGI = do
      (mrhdl0,mhb)  <- maybeCreateHandle
      (rhdl1,whdl1) <- createHandle
      let envVars = makeEnv cnf req cgi
      forkIO $ execCGI (progPath cgi) envVars mrhdl0 (Just whdl1) mhb
      mrsp <- timeout (10 * 1000000) $ processCGIoutput rhdl1
      maybe internalError (return . Just) mrsp
    maybeCreateHandle = case reqBody req of
      Nothing -> return (Nothing,Nothing)
      Just body -> do
        (rhdl0,whdl0) <- createHandle
        return (Just rhdl0, Just (whdl0,body))
    internalError = return $ Just responseInternalServerError

createHandle :: IO (Handle,Handle)
createHandle = do
  (rfd,wfd) <- createPipe
  rhdl <- fdToHandle rfd
  whdl <- fdToHandle wfd
  return (rhdl,whdl)

type ENVVARS = [(String,String)]

execCGI :: FilePath -> ENVVARS -> Maybe Handle -> Maybe Handle -> Maybe (Handle,L.ByteString) -> IO ()
execCGI prog envVars sti sto mhb = do
  runProcess prog [] Nothing (Just envVars) sti sto Nothing
  case mhb of
    Nothing -> return ()
    Just (whdl,body) -> do
      L.hPut whdl body
      hClose whdl

makeEnv :: BasicConfig -> Request -> CGI -> ENVVARS
makeEnv cnf req cgi = addLength . addType . addCookie $ baseEnv
  where
    baseEnv = [("GATEWAY_INTERFACE", gatewayInterface)
              ,("SCRIPT_NAME",       scriptName cgi)
              ,("REQUEST_METHOD",    show (reqMethod req))
              ,("SERVER_NAME",       S.unpack . uriHostName . reqURI $ req)
              ,("SERVER_PORT",       myPort (tcpInfo cnf))
              ,("REMOTE_ADDR",       peerAddr (tcpInfo cnf))
              ,("SERVER_PROTOCOL",   show (reqVersion req))
              ,("SERVER_SOFTWARE",   S.unpack . serverName $ cnf)
              ,("PATH_INFO",         unEscapeString . pathInfo $ cgi)
              ,("QUERY_STRING",      unEscapeString . tail . queryString $ cgi)]
    addLength = add "CONTENT_LENGTH" (lookupField FkContentLength req)
    addType   = add "CONTENT_TYPE" (lookupField FkContentType req)
    addCookie = add "HTTP_COOKIE" (lookupField FkCookie req)
    add _   Nothing    envs = envs
    add key (Just val) envs = (key,S.unpack val) : envs

processCGIoutput :: Handle -> IO Response
processCGIoutput rhdl = do
  flds <- receiveFields rhdl -- xxx CT: [and Status:] in order
  case lookupField' FkContentType flds of
    Nothing -> return responseInternalServerError
    Just _  -> do
      let st = maybe OK id (lookupField' FkStatus flds >>= toStatus)
      responseAny st flds <$> L.hGetContents rhdl

----------------------------------------------------------------

responseAny :: Status -> Fields -> L.ByteString -> Response
responseAny st flds val = makeResponse3 st (Just val) Nothing flds'
  where
    flds' = case lookupField' FkSetCookie2 flds of
      Nothing -> flds
      Just _  -> insertField' FkCacheControl "no-cache=\"set-cookie2\"" flds

responseInternalServerError :: Response
responseInternalServerError = makeResponse InternalServerError []
