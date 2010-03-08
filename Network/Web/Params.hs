{-# LANGUAGE DeriveDataTypeable #-}

{-|
  Parameters of HTTP.
-}
module Network.Web.Params (Method(..), Version(..), Status(..),
                           toStatus, badStatus,
                           Persist(..), ServerException(..),
                           FieldKey(..), FieldValue,
                           toFieldKey, fromFieldKey,
                           CT, textHtml, selectContentType) where

import Control.Exception
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import Data.Typeable

----------------------------------------------------------------

{-|
  Methods of HTTP.
-}
data Method = GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT
            | UnknownMethod deriving (Show,Eq,Enum,Bounded)

methodAlist :: [(String,Method)]
methodAlist = let methods = [minBound..maxBound]
              in zip (map show methods) methods

readMethod :: String -> Method
readMethod s = maybe UnknownMethod id $ lookup s methodAlist

instance Read Method where
    readsPrec _ s = [(readMethod s,"")]

----------------------------------------------------------------

{-|
  Versions of HTTP.
-}
data Version = HTTP10 | HTTP11 deriving Eq

instance Show Version where
   show HTTP10    = "HTTP/1.0"
   show HTTP11    = "HTTP/1.1"

readVersion :: String -> Version
readVersion "HTTP/1.1" = HTTP11
readVersion _          = HTTP10

instance Read Version where
    readsPrec _ s = [(readVersion s,"")]

----------------------------------------------------------------

{-|
  Status of HTTP.
-}

data Status = Continue | SwitchingProtocols
            -- 2xx
            | OK | Created | Accepted | NonAuthoritativeInformation
            | NoContent | ResetContent | PartialContent Integer Integer
            -- 3xx
            | MultipleChoices | MovedPermanently | Found | SeeOther
            | NotModified | UseProxy | TemporaryRedirect
            -- 4xx
            | BadRequest | Unauthorized | PaymentRequired | Forbidden
            | NotFound | MethodNotAllowed | NotAcceptable
            | ProxyAuthenticationRequired | RequestTimeout | Conflict
            | Gone | LengthRequired | PreconditionFailed
            | RequestEntityTooLarge | RequestURITooLarge
            | UnsupportedMediaType | RequestedRangeNotSatisfiable
            | ExpectationFailed
            -- 5xx
            | InternalServerError | NotImplemented | BadGateway
            | ServiceUnavailable | GatewayTimeout | HTTPVersionNotSupported

instance Show Status where
    show Continue = "100 Continue"
    show SwitchingProtocols = "101 Switching Protocols"
    show OK = "200 OK"
    show Created = "201 Created"
    show Accepted = "202 Accepted"
    show NonAuthoritativeInformation = "203 Non-Authoritative Information"
    show NoContent = "204 No Content"
    show ResetContent = "205 Reset Content"
    show (PartialContent _ _) = "206 Partial Content"
    show MultipleChoices = "300 Multiple Choices"
    show MovedPermanently = "301 Moved Permanently"
    show Found = "302 Found"
    show SeeOther = "303 See Other"
    show NotModified = "304 Not Modified"
    show UseProxy = "305 Use Proxy"
    show TemporaryRedirect = "307 Temporary Redirect"
    show BadRequest = "400 Bad Request"
    show Unauthorized = "401 Unauthorized"
    show PaymentRequired = "402 Payment Required"
    show Forbidden = "403 Forbidden"
    show NotFound = "404 Not Found"
    show MethodNotAllowed = "405 Method Not Allowed"
    show NotAcceptable = "406 Not Acceptable"
    show ProxyAuthenticationRequired = "407 Proxy Authentication Required"
    show RequestTimeout = "408 RequestTimeout"
    show Conflict = "409 Conflict"
    show Gone = "410 Gone"
    show LengthRequired = "411 Length Required"
    show PreconditionFailed = "412 Precondition Failed"
    show RequestEntityTooLarge = "413 Request Entity Too Large"
    show RequestURITooLarge = "414 Request-URI Too Large"
    show UnsupportedMediaType = "415 Unsupported Media Type"
    show RequestedRangeNotSatisfiable = "416 Requested Range Not Satisfiable"
    show ExpectationFailed = "417 Expectation Failed"
    show InternalServerError = "500 Internal Server Error"
    show NotImplemented = "501 Not Implemented"
    show BadGateway = "502 Bad Gateway"
    show ServiceUnavailable = "503 Service Unavailable"
    show GatewayTimeout = "504 Gateway Time-out"
    show HTTPVersionNotSupported = "505 HTTP Version Not Supported"

{-|
  Converting numeric status to 'Status'.
-}
toStatus :: String -> Maybe Status
toStatus "200" = Just OK
toStatus "302" = Just Found
toStatus "400" = Just BadRequest
toStatus "501" = Just NotImplemented
toStatus _     = Nothing

{-|
  Returning 'True' for 4xx and 5xx.
-}
badStatus :: Status -> Bool
badStatus status = n == '4' || n == '5'
  where
    n:_ = show status

----------------------------------------------------------------

{-|
  Field key of HTTP header.
-}
data FieldKey = FkAcceptLanguage
              | FkCacheControl
              | FkConnection
              | FkContentLength
              | FkContentType
              | FkCookie
              | FkDate
              | FkHost
              | FkIfModifiedSince
              | FkIfRange
              | FkIfUnmodifiedSince
              | FkLastModified
              | FkLocation
              | FkRange
              | FkServer
              | FkSetCookie2
              | FkStatus
              | FkTransferEncoding
              | FkOther String
              deriving (Eq,Show,Ord)

fieldKeyList :: [FieldKey]
fieldKeyList = [ FkAcceptLanguage
               , FkCacheControl
               , FkConnection
               , FkContentLength
               , FkContentType
               , FkCookie
               , FkDate
               , FkHost
               , FkIfModifiedSince
               , FkIfRange
               , FkIfUnmodifiedSince
               , FkLastModified
               , FkLocation
               , FkRange
               , FkServer
               , FkSetCookie2
               , FkStatus
               , FkTransferEncoding ]

fieldStringList :: [String]
fieldStringList = [ "Accept-Language"
                  , "Cache-Control"
                  , "Connection"
                  , "Content-Length"
                  , "Content-Type"
                  , "Cookie"
                  , "Date"
                  , "Host"
                  , "If-Modified-Since"
                  , "If-Range"
                  , "If-Unmodified-Since"
                  , "Last-Modified"
                  , "Location"
                  , "Range"
                  , "Server"
                  , "Set-Cookie2"
                  , "Status"
                  , "Transfer-Encoding" ]

{-|
  Field value of HTTP header.
-}
type FieldValue = String

stringFieldKey :: Map FieldValue FieldKey
stringFieldKey = Map.fromList (zip fieldStringList fieldKeyList)

fieldKeyString :: Map FieldKey FieldValue
fieldKeyString = Map.fromList (zip fieldKeyList fieldStringList)

{-|
  Converting field key to 'FieldKey'.
-}
toFieldKey :: String -> FieldKey
toFieldKey str = maybe (FkOther cstr) id $ Map.lookup cstr stringFieldKey
  where
    cstr = capitalize str

{-|
  Converting 'FieldKey' to field key.
-}
fromFieldKey :: FieldKey -> String
fromFieldKey (FkOther cstr) = cstr
fromFieldKey key = maybe err id $ Map.lookup key fieldKeyString
  where
    err = error "fromFieldKey"

capitalize :: String -> String
capitalize s = toup s
    where
  toup [] = []
  toup (x:xs)
    | isLetter x = toUpper x : stay xs
    | otherwise  =         x : toup xs
  stay [] = []
  stay (x:xs)
    | isLetter x =         x : stay xs
    | otherwise  =         x : toup xs

----------------------------------------------------------------

{-|
  The type for Content-Type.
-}

type CT = String

{-|
  Selecting a value of Content-Type from a file suffix.
-}
selectContentType :: String -> CT
selectContentType ""  = textPlain
selectContentType ext = maybe appOct id (lookup lext contentTypeDB)
  where
    lext = map toLower ext

{-|
  The value for text/html.
-}
textHtml :: CT
textHtml = "text/html"

textPlain :: CT
textPlain = "text/plain"

appOct :: CT
appOct = "application/octet-stream"

contentTypeDB :: [(FilePath,CT)]
contentTypeDB = [ (".html", textHtml)
                , (".txt",  textPlain)
                , (".css",  "text/css")
                , (".js",   "application/javascript")
                , (".jpg",  "image/jpeg")
                , (".png",  "image/png")
                , (".gif",  "image/gif")
                , (".pdf",  "application/pdf")
                , (".zip",  "application/zip")
                , (".gz",   appOct)
                , (".ico",  "image/x-icon")
                ]

----------------------------------------------------------------

-- | The type for persist connection or not
data Persist = Close | Keep | PerUnknown deriving Eq

instance Show Persist where
   show Close      = "close"
   show Keep       = "keep-alive"
   show PerUnknown = "unknown"

instance Read Persist where
    readsPrec _ s = [(readPersist s,"")]

readPersist :: String -> Persist
readPersist cs = readPersist' (downcase cs)
  where
    downcase = map toLower
    readPersist' "close"      = Close
    readPersist' "keep-alive" = Keep
    readPersist' _            = PerUnknown

----------------------------------------------------------------

-- | Exceptions for Web server
data ServerException
  = TimeOut
  | TerminatedByClient
  deriving (Eq, Ord, Typeable)

instance Exception ServerException

instance Show ServerException where
  showsPrec _ TimeOut            = showString "Connection time out"
  showsPrec _ TerminatedByClient = showString "Connection is terminated by client"
