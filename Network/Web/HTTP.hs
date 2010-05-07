{-# LANGUAGE OverloadedStrings #-}
{-|
  HTTP library for HTTP server.
-}
module Network.Web.HTTP (receive, respond,
                         Request,
                         reqMethod, reqURI, reqVersion, reqFields,
                         reqBody, reqLength,
                         Response,
                         rspStatus, rspFields, rspBody, rspLength, rspLogMsg,
                         makeResponse, makeResponse2, makeResponse3,
                         Comm, Fields,
                         lookupField, lookupField',
                         insertField, insertField',
                         receiveFields,
                         module Network.Web.Params) where

import Control.Applicative
import Control.Exception (try, throw)
import Control.Monad
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import IO hiding (try)
import Network.Web.Params
import Network.Web.URI
import System.IO.Error hiding (try)
import Text.Printf

----------------------------------------------------------------

{-|
  Abstract data type of HTTP request.
-}
data Request = Request {
  -- | Request method
    reqMethod  :: Method
  -- | URI parsed from absolute URL or relative URL with the Host: field
  , reqURI     :: URI
  -- | HTTP version
  , reqVersion :: Version
  -- | Key-values of request header
  , reqFields  :: Fields
  -- | Entity body if exists
  , reqBody    :: Maybe L.ByteString
  -- | Length of entity body from Content-Length:
  , reqLength  :: Integer
} deriving Show

{-|
  Abstract data type of HTTP response.
-}
data Response = Response {
  -- | Response status
    rspStatus  :: Status
  , rspFields  :: Fields
  , rspBody    :: Maybe L.ByteString -- Nothing -> No entity / No CL:
                                     -- Just bs
                                     -- Just bs.empty
  , rspLength  :: Maybe Integer      -- Nothing -> chunked or close
                                     -- Just x  -> CL: x
  , rspLogMsg  :: String
}

{-|
  A class to abstract 'Request' and 'Response'.
-}
class Comm a where
   getFields :: a -> Fields
   setFields :: a -> Fields -> a

instance Comm Request where
   getFields = reqFields
   setFields req hdrs = req { reqFields = hdrs }

instance Comm Response where
   getFields = rspFields
   setFields rsp hdrs = rsp { rspFields = hdrs }

----------------------------------------------------------------

{-|
  Default Request.
-}
defaultRequest :: Request
defaultRequest = Request {
    reqMethod  = GET
  , reqURI     = undefined
  , reqVersion = HTTP11
  , reqFields  = emptyFields
  , reqBody    = Nothing
  , reqLength  = 0
}

{-|
  Default Response.
-}
defaultResponse :: Response
defaultResponse = Response {
    rspStatus   = OK
  , rspFields   = emptyFields
  , rspBody     = Nothing
  , rspLength   = Nothing
  , rspLogMsg   = ""
}

----------------------------------------------------------------

{-|
  A function to make 'Response'.
-}
makeResponse :: Status -> [(FieldKey,FieldValue)] -> Response
makeResponse st kvs = defaultResponse { rspStatus = st
                                      , rspBody   = Just body
                                      , rspLength = Just len
                                      , rspFields = flds
                                      }
  where
    (<+>) = L.append
    body = "<html><body>" <+> L.pack (show st) <+> "</body></html>\n"
    len = fromIntegral $ L.length body
    flds = toFields kvs

{-|
  A function to make 'Response'.
-}
makeResponse2 :: Status -> Maybe L.ByteString -> Maybe Integer -> [(FieldKey,FieldValue)] -> Response
makeResponse2 st mval mlen kvs = defaultResponse { rspStatus = st
                                                 , rspBody   = mval
                                                 , rspLength = mlen
                                                 , rspFields = flds
                                                 }
  where
    flds = toFields kvs

{-|
  A function to make 'Response'.
-}
makeResponse3 :: Status -> Maybe L.ByteString -> Maybe Integer -> Fields -> Response
makeResponse3 st mval mlen flds = defaultResponse { rspStatus = st
                                                  , rspBody   = mval
                                                  , rspLength = mlen
                                                  , rspFields = flds'
                                                  }
  where
    flds' = copyFields flds

----------------------------------------------------------------

{-|
  Receiving HTTP request from 'Handle'.
  If request is broken, 'Nothing' is returned.
-}
receive :: Handle -> IO (Maybe Request)
receive hdl = do
    mreq <- try $ receiveRequest hdl
    case mreq of
      Left  e   -> if isEOFError e ||
                      show (ioeGetErrorType e) == "resource vanished"
                   then throw TerminatedByClient
                   else return Nothing
      Right req -> return $ Just req

receiveRequest :: Handle -> IO Request
receiveRequest hdl = do
    (method,url,version) <- receiveRequestLine hdl
    let req0 = defaultRequest
        req1 = req0 { reqMethod = method, reqVersion = version }
    flds <- receiveFields hdl
    uri <- toURI url flds
    let req2 = req1 { reqURI = uri, reqFields = flds }
    receiveBody hdl flds req2

isEOH :: S.ByteString -> Bool
isEOH l = S.null l || l == "\r"

receiveRequestLine :: Handle -> IO (Method,S.ByteString,Version)
receiveRequestLine hdl = parseRequestLine <$> skipNullLines
  where
    skipNullLines = do
      l <- S.hGetLine hdl
      if isEOH l
         then skipNullLines
         else return l
{-|
  Parsing HTTP header from 'Handle'.
  This function is useful to parse CGI output.
-}
receiveFields :: Handle -> IO Fields
receiveFields hdl = toFields . map parseField <$> getHeaderLines
  where
    getHeaderLines = do
      l <- S.hGetLine hdl
      if isEOH l
         then return []
         else (l:) <$> getHeaderLines

toURI :: S.ByteString -> Fields -> IO URI
toURI url fields = maybe (fail "toURI") return $ toURI' url fields

toURI' :: S.ByteString -> Fields -> Maybe URI
toURI' url fields
  | isAbsoluteURI url = parseURI url
  | otherwise         = lookupField' FkHost fields >>= \host ->
                        parseURI $ "http://" +++ host +++ url
  where
    (+++) = S.append

receiveBody :: Handle -> Fields -> Request -> IO Request
receiveBody hdl flds req =
    case lookupField' FkContentLength flds of
      Nothing -> return req
      Just bs  -> do
        let Just (len,_) = S.readInteger bs -- xxx
        body <- L.hGet hdl (fromIntegral len)
        return req { reqBody = Just body, reqLength = len }

----------------------------------------------------------------

{-|
  Sending HTTP response to 'Handle'.
  If 'Keep' is specified, the HTTP connection
  will be kept. If 'Close' is specified, the connection will be closed.
  'Version' should be copied from 'Request'.
-}

respond :: Handle -> Version -> Persist -> Response -> IO ()
respond hdl ver persist rsp = do
    sendStatusLine hdl ver rsp
    sendResponseFields hdl ver persist rsp
    S.hPutStr hdl crlf
    sendResponseBody hdl ver rsp
    hFlush hdl

sendStatusLine :: Handle -> Version -> Response -> IO ()
sendStatusLine hdl ver rsp = do
    S.hPutStr hdl $ fromVersion ver
    S.hPutStr hdl spc
    S.hPutStr hdl $ fromStatus (rspStatus rsp) -- including reason-phrase
    S.hPutStr hdl crlf

sendResponseFields :: Handle -> Version -> Persist -> Response -> IO ()
sendResponseFields hdl ver persist rsp = do
    putFields
    putContentLength
    putTransferEncoding
    putConnection
 where
    putFields = S.hPutStr hdl . S.concat . map composeField . fromFields $ getFields rsp
    putContentLength =
      case rspBody rsp >> rspLength rsp of
        Just len -> S.hPutStr hdl $ composeField (FkContentLength, S.pack (show len))
        Nothing -> return ()
    putTransferEncoding =
      when (ver == HTTP11 && isJust (rspBody rsp) && isNothing (rspLength rsp)) $
          S.hPutStr hdl $ composeField (FkTransferEncoding, "chunked")
    putConnection = S.hPutStr hdl $ composeField (FkConnection, fromPersist persist)

sendResponseBody :: Handle -> Version -> Response -> IO ()
sendResponseBody hdl ver rsp =
  case rspBody rsp of
    Just body -> case rspLength rsp of
      Just _  -> L.hPut hdl body
      Nothing -> if ver == HTTP10
                 then L.hPut hdl body
                 else sendChunk hdl body
    Nothing   -> return ()

sendChunk :: Handle -> L.ByteString -> IO ()
sendChunk hdl body = do
    let (fcnk,rest) = L.splitAt chunkSize body
    if L.null rest
      then do
        putChunk fcnk $ toHex (L.length fcnk)
        putLastChunk
        S.hPutStr hdl crlf
      else do
        putChunk fcnk defSize
        sendChunk hdl rest
  where
    chunkSize = 1024 * 4
    defSize = toHex chunkSize
    toHex = S.pack . printf "%X"
    putChunk cnk siz = do
        S.hPutStr hdl siz
        S.hPutStr hdl crlf
        L.hPut hdl cnk
        S.hPutStr hdl crlf
    putLastChunk = do
        S.hPutStr hdl "0"
        S.hPutStr hdl crlf

----------------------------------------------------------------

{-|
  Abstract data type for Key-values of HTTP header.
-}
newtype Fields = Fields (M.Map FieldKey FieldValue) deriving Show

emptyFields :: Fields
emptyFields = Fields M.empty

{-|
  Looking up the HTTP field value.
-}
lookupField :: Comm a => FieldKey -> a -> Maybe FieldValue
lookupField key comm  = lookupField' key (getFields comm)

{-|
  Looking up the HTTP field value.
-}
lookupField' :: FieldKey -> Fields -> Maybe FieldValue
lookupField' key (Fields fields) = maybe Nothing (Just . trim) mvalue
  where
    mvalue = M.lookup key fields

{-|
  Inserting the HTTP field.
-}
insertField :: Comm a => FieldKey -> FieldValue -> a -> a
insertField key val comm = setFields comm fields
  where
    fields = insertField' key val $ getFields comm

{-|
  Inserting the HTTP field.
-}
insertField' :: FieldKey -> FieldValue -> Fields -> Fields
insertField' key val (Fields fields) = Fields (M.insert key val fields)

toFields :: [(FieldKey,FieldValue)] -> Fields
toFields kvs = Fields (M.fromList kvs)

fromFields :: Fields -> [(FieldKey,FieldValue)]
fromFields (Fields fields) = M.toList fields

copyFields :: Fields -> Fields
copyFields = toFields . map (\(x,y) -> (x, trim y)) . fromFields

----------------------------------------------------------------

composeField :: (FieldKey,FieldValue) -> S.ByteString
composeField (k,v) = fromFieldKey k +++ ": " +++ v +++ crlf
  where
    (+++) = S.append

parseField :: S.ByteString -> (FieldKey,FieldValue)
parseField l = let kv = S.break (==':') (chomp l)
               in toKeyValue kv
  where
    toKeyValue (k,"") = (toFieldKey k, "")
    toKeyValue (k,bs) = (toFieldKey k, S.tail bs) -- bs is trimmed by lookupField

parseRequestLine :: S.ByteString -> (Method,S.ByteString,Version)
parseRequestLine l = let (m,l') = S.break (==' ') (chomp l)
                         (u,v') = S.break (==' ') (chop l')
                         v = trim v'
                     in (toMethod m, u, toVersion v)

----------------------------------------------------------------

chop :: S.ByteString -> S.ByteString
chop = S.dropWhile isSpace

chomp :: S.ByteString -> S.ByteString
chomp = fst . S.break (=='\r')

trim :: S.ByteString -> S.ByteString
trim = S.reverse . chop . S.reverse . chop

----------------------------------------------------------------

crlf :: S.ByteString
crlf = "\r\n"

spc :: S.ByteString
spc = " "
