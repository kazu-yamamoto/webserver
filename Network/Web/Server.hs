{-|
  HTTP server library.
-}
module Network.Web.Server (connection, WebServer, WebConfig(..)) where

import Control.Exception
import Control.Applicative
import qualified Data.ByteString.Char8 as S
import Data.Maybe
import Data.Time
import IO
import Network.Web.HTTP hiding (receive,respond)
import qualified Network.Web.HTTP as HTTP (receive,respond)
import Network.Web.Date
import System.Timeout

----------------------------------------------------------------

{-|
  The type for HTTP server.
-}
type WebServer = Maybe Request -> IO Response

{-|
  The configuration for 'connection'.
-}
data WebConfig = WebConfig {
    -- | A hook to be called when an HTTP connection is closed.
    closedHook :: String -> IO ()
    -- | A hook to be called when access succeeds.
  , accessHook :: String -> IO ()
    -- | A hook to be called when an access error occurs.
  , errorHook :: String -> IO ()
    -- | A hook to be called when a fatal error occurs.
  , fatalErrorHook :: String -> IO ()
    -- | A time to unblock receiving an HTTP request in seconds.
  , connectionTimer :: Int
}

----------------------------------------------------------------

{-|
  A function to run an 'WebServer'. 'Handle' should be mode by
  converting an accepted socket.
  Keep-alive / termination of HTTP 1.0 and HTTP 1.1 is correctly handled.
  So, 'WebServer' need not to handle the Connection: header in response.
  The Date: header is automatically added in response.
-}
connection :: Handle -> WebServer -> WebConfig -> IO ()
connection hdl srv cnf = session hdl srv cnf `catches` serverError
  where
    serverError =
      [Handler (\e -> closedHook cnf (show (e::ServerException))),
       Handler (\e -> errorHook  cnf (show (e::SomeException)))]

----------------------------------------------------------------

session :: Handle -> WebServer -> WebConfig -> IO ()
session hdl svr cnf = do
    mreq <- recvRequest hdl cnf
    rsp <- runServer svr mreq
    persist <- sendResponse hdl cnf rsp mreq
    case persist of
      Close -> closedHook cnf $ "Connection is closed"
      Keep  -> session hdl svr cnf
      _     -> return () -- never reached
  where
    runServer server mreq = do
        date <- utcToDate <$> getCurrentTime
        addDate date <$> server mreq
    addDate date rsp  = insertField FkDate date rsp

----------------------------------------------------------------

recvRequest :: Handle -> WebConfig -> IO (Maybe Request)
recvRequest hdl cnf = do
    mmreq <- timeout tm (HTTP.receive hdl)
    case mmreq of
      Nothing   -> throw TimeOut
      Just mreq -> return mreq
 where
   microseconds = 1000000
   tm = connectionTimer cnf * microseconds

----------------------------------------------------------------

sendResponse :: Handle -> WebConfig -> Response -> Maybe Request -> IO Persist
sendResponse hdl cnf rsp (Just req) = do
    let ver = reqVersion req
        cnct = lookupField FkConnection req
        status = rspStatus rsp
        persist = if badStatus status
                  then Close
                  else checkPersist ver cnct rsp
        hook = accessHook cnf
    sendResponse' hdl ver persist rsp hook
sendResponse hdl cnf rsp Nothing = do
    let hook = errorHook cnf
    sendResponse' hdl HTTP10 Close rsp hook

sendResponse' :: Handle -> Version -> Persist -> Response -> (String -> IO ()) -> IO Persist
sendResponse' hdl ver persist rsp hook = do
    HTTP.respond hdl ver persist rsp
    hook $ rspLogMsg rsp ++ " [" ++ show (rspStatus rsp) ++ "]"
    return persist

----------------------------------------------------------------

-- CGI and HTTP/1.0 -> Close

checkPersist :: Version -> Maybe S.ByteString -> Response -> Persist
checkPersist HTTP11 Nothing     _   = Keep
checkPersist HTTP11 (Just cnct) _
    | toPersist cnct == Close       = Close
    | otherwise                     = Keep
checkPersist HTTP10 Nothing     _   = Close
checkPersist HTTP10 (Just cnct) rsp
    | toPersist cnct == Keep        = if isJust (rspBody rsp) &&
                                    isNothing (rspLength rsp)
                                 then Close
                                 else Keep
    | otherwise                     = Close
