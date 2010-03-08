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
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS hiding (ByteString)
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import Data.Maybe
import IO hiding (try)
import Network.URI
import Network.Web.Params
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
  , reqBody    :: Maybe ByteString
  -- | Length of entity body from Content-Length:
  , reqLength  :: Integer
}

{-|
  Abstract data type of HTTP response.
-}
data Response = Response {
  -- | Response status
    rspStatus  :: Status
  , rspFields  :: Fields
  , rspBody    :: Maybe ByteString -- Nothing -> No entity / No CL:
                                   -- Just bs
                                   -- Just bs.empty
  , rspLength  :: Maybe Integer    -- Nothing -> chunked or close
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
    cs = "<html><body>" ++ show st ++ "</body></html>\n"
    len = fromIntegral $ length cs
    body = LBS.pack cs
    flds = toFields kvs

{-|
  A function to make 'Response'.
-}
makeResponse2 :: Status -> Maybe ByteString -> Maybe Integer -> [(FieldKey,FieldValue)] -> Response
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
makeResponse3 :: Status -> Maybe ByteString -> Maybe Integer -> Fields -> Response
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
      Left  e   -> if isEOFError e
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

isEOH :: String -> Bool
isEOH l = null l || l == "\r"

receiveRequestLine :: Handle -> IO (Method,String,Version)
receiveRequestLine hdl = parseRequestLine <$> skipNullLines
  where
    skipNullLines = do
      l <- hGetLine hdl
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
      l <- hGetLine hdl
      if isEOH l
         then return []
         else (l:) <$> getHeaderLines

toURI :: String -> Fields -> IO URI
toURI url fields = maybe (fail "toURI") return $ toURI' url fields

toURI' :: String -> Fields -> Maybe URI
toURI' url fields
  | isAbsoluteURI url = parseURI url
  | otherwise         = lookupField' FkHost fields >>= \host ->
                        parseURI $ "http://" ++ host ++ url

receiveBody :: Handle -> Fields -> Request -> IO Request
receiveBody hdl flds req =
    case lookupField' FkContentLength flds of
      Nothing -> return req
      Just cs  -> do
        let len = read cs
        body <- LBS.hGet hdl (fromIntegral len)
        return req { reqBody = Just body, reqLength = len }

----------------------------------------------------------------

{-|
  Sending HTTP response to 'Handle'.
  If 'Keep' is specified, the HTTP connection
  will be kept. If 'Close' is specified, the connection will be closed.
  'Version' should be copied from 'Request'.
-}

respond :: Handle -> Version -> Persist -> Response -> IO ()
respond h ver persist rsp = do
    sendStatusLine h ver rsp
    sendResponseFields h ver persist rsp
    hPutStr h crlf
    sendResponseBody h ver rsp
    hFlush h

sendStatusLine :: Handle -> Version -> Response -> IO ()
sendStatusLine h ver rsp = do
    hPutStr h $ show ver
    hPutStr h spc
    hPutStr h $ show (rspStatus rsp) -- including reason-phrase
    hPutStr h crlf

sendResponseFields :: Handle -> Version -> Persist -> Response -> IO ()
sendResponseFields h ver persist rsp = do
    putFields
    putContentLength
    putTransferEncoding
    putConnection
 where
    putFields = hPutStr h . concatMap composeField . fromFields $ getFields rsp
    putContentLength =
      case rspBody rsp >> rspLength rsp of
        Just len -> hPutStr h $ composeField (FkContentLength, show len)
        Nothing -> return ()
    putTransferEncoding =
      if ver == HTTP11 && isJust (rspBody rsp) && isNothing (rspLength rsp)
      then hPutStr h $ composeField (FkTransferEncoding, "chunked")
      else return ()
    putConnection = hPutStr h $ composeField (FkConnection, show persist)

sendResponseBody :: Handle -> Version -> Response -> IO ()
sendResponseBody h ver rsp =
  case rspBody rsp of
    Just body -> case rspLength rsp of
      Just _  -> LBS.hPut h body
      Nothing -> if ver == HTTP10
                 then LBS.hPut h body
                 else sendChunk h body
    Nothing   -> return ()

sendChunk :: Handle -> ByteString -> IO ()
sendChunk h body = do
    let (fcnk,rest) = LBS.splitAt chunkSize body
    if LBS.null rest
      then do
        putChunk fcnk $ toHex (LBS.length fcnk)
        putLastChunk
      else do
        putChunk fcnk defSize
        sendChunk h rest
  where
    chunkSize = 1024 * 4
    defSize = toHex chunkSize
    toHex = printf "%X"
    putChunk cnk siz = do
        hPutStr h siz
        hPutStr h crlf
        LBS.hPut h cnk
        hPutStr h crlf
    putLastChunk = do
        hPutStr h "0"
        hPutStr h crlf
        hPutStr h crlf

----------------------------------------------------------------

{-|
  Abstract data type for Key-values of HTTP header.
-}
newtype Fields = Fields (Map FieldKey FieldValue) deriving Show

emptyFields :: Fields
emptyFields = Fields Map.empty

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
    mvalue = Map.lookup key fields

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
insertField' key val (Fields fields) = Fields (Map.insert key val fields)

toFields :: [(FieldKey,FieldValue)] -> Fields
toFields kvs = Fields (Map.fromList kvs)

fromFields :: Fields -> [(FieldKey,FieldValue)]
fromFields (Fields fields) = Map.toList fields

copyFields :: Fields -> Fields
copyFields = toFields . map (\(x,y) -> (x, trim y)) . fromFields

----------------------------------------------------------------

composeField :: (FieldKey,FieldValue) -> String
composeField (k,v) = fromFieldKey k ++ ": " ++ v ++ crlf

parseField :: String -> (FieldKey,FieldValue)
parseField l = let kv = break (==':') (chomp l)
               in toKeyValue kv
  where
    toKeyValue (k,"")  = (toFieldKey k, "")
    toKeyValue (k,_:v) = (toFieldKey k, v) -- v is trimmed by lookupField

parseRequestLine :: String -> (Method,String,Version)
parseRequestLine l = let (m,l') = break (==' ') (chomp l)
                         (u,v') = break (==' ') (chop l')
                         v = trim v'
                     in (read m, u, read v)

----------------------------------------------------------------

chop :: String -> String
chop = dropWhile isSpace

chomp :: String -> String
chomp = fst . break (=='\r')

trim :: String -> String
trim = reverse . chop . reverse . chop

----------------------------------------------------------------

crlf :: String
crlf = "\r\n"

spc :: String
spc = " "
