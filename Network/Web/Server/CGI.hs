{-# LANGUAGE OverloadedStrings #-}

module Network.Web.Server.CGI (tryGetCGI) where

import Control.Applicative
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import Network.TCPInfo
import Network.Web.HTTP
import Network.Web.Server.Params
import Network.Web.URI
import System.IO
import System.Process
import System.Timeout

gatewayInterface :: String
gatewayInterface = "CGI/1.1"

----------------------------------------------------------------

tryGetCGI :: BasicConfig -> Request -> CGI -> IO (Maybe Response)
tryGetCGI cnf req cgi = processCGI `catch` const internalError
  where
    processCGI = do
      let envVars = makeEnv cnf req cgi
          pro = CreateProcess {
              cmdspec = RawCommand (progPath cgi) []
            , cwd = Nothing
            , env = Just envVars
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = True
            }
      (Just whdl,Just rhdl,_,_) <- createProcess pro
      maybe (return ()) (L.hPut whdl) (reqBody req)
      hClose whdl
      mrsp <- timeout (10 * 1000000) $ processCGIoutput rhdl
      maybe internalError (return . Just) mrsp
    internalError = return $ Just responseInternalServerError

type ENVVARS = [(String,String)]

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
              ,("QUERY_STRING",      unEscapeString . safeTail . queryString $ cgi)]
    addLength = add "CONTENT_LENGTH" (lookupField FkContentLength req)
    addType   = add "CONTENT_TYPE" (lookupField FkContentType req)
    addCookie = add "HTTP_COOKIE" (lookupField FkCookie req)
    add _   Nothing    envs = envs
    add key (Just val) envs = (key,S.unpack val) : envs
    safeTail "" = ""
    safeTail xs = tail xs

processCGIoutput :: Handle -> IO Response
processCGIoutput rhdl = do
  flds <- receiveFields rhdl -- xxx CT: [and Status:] in order
  case lookupField' FkContentType flds of
    Nothing -> return responseInternalServerError
    Just _  -> do
      let st = fromMaybe OK (lookupField' FkStatus flds >>= toStatus)
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
