{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-|
  Parameters of HTTP.
-}
module Network.Web.Params (
    Method(..), toMethod
  , Version(..), toVersion, fromVersion
  , Status(..), toStatus, fromStatus, badStatus
  , Persist(..), toPersist, fromPersist
  , ServerException(..)
  , FieldKey(..), FieldValue
  , toFieldKey, fromFieldKey
  , CT, textHtml, selectContentType
  ) where

import Control.Exception
import qualified Data.ByteString.Char8 as S
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable

----------------------------------------------------------------

{-|
  Methods of HTTP.
-}
data Method = GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT
            | UnknownMethod deriving (Show,Eq,Enum,Bounded)

methodAlist :: [(S.ByteString,Method)]
methodAlist = let methods = [minBound..maxBound]
              in zip (map (S.pack . show) methods) methods

toMethod :: S.ByteString -> Method
toMethod s = fromMaybe UnknownMethod $ lookup s methodAlist

----------------------------------------------------------------

{-|
  Versions of HTTP.
-}
data Version = HTTP10 | HTTP11 deriving (Eq,Show)

fromVersion :: Version -> S.ByteString
fromVersion HTTP10    = "HTTP/1.0"
fromVersion HTTP11    = "HTTP/1.1"

toVersion :: S.ByteString -> Version
toVersion "HTTP/1.1" = HTTP11
toVersion _          = HTTP10

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
            deriving Show

fromStatus :: Status -> S.ByteString
fromStatus Continue = "100 Continue"
fromStatus SwitchingProtocols = "101 Switching Protocols"
fromStatus OK = "200 OK"
fromStatus Created = "201 Created"
fromStatus Accepted = "202 Accepted"
fromStatus NonAuthoritativeInformation = "203 Non-Authoritative Information"
fromStatus NoContent = "204 No Content"
fromStatus ResetContent = "205 Reset Content"
fromStatus (PartialContent _ _) = "206 Partial Content"
fromStatus MultipleChoices = "300 Multiple Choices"
fromStatus MovedPermanently = "301 Moved Permanently"
fromStatus Found = "302 Found"
fromStatus SeeOther = "303 See Other"
fromStatus NotModified = "304 Not Modified"
fromStatus UseProxy = "305 Use Proxy"
fromStatus TemporaryRedirect = "307 Temporary Redirect"
fromStatus BadRequest = "400 Bad Request"
fromStatus Unauthorized = "401 Unauthorized"
fromStatus PaymentRequired = "402 Payment Required"
fromStatus Forbidden = "403 Forbidden"
fromStatus NotFound = "404 Not Found"
fromStatus MethodNotAllowed = "405 Method Not Allowed"
fromStatus NotAcceptable = "406 Not Acceptable"
fromStatus ProxyAuthenticationRequired = "407 Proxy Authentication Required"
fromStatus RequestTimeout = "408 RequestTimeout"
fromStatus Conflict = "409 Conflict"
fromStatus Gone = "410 Gone"
fromStatus LengthRequired = "411 Length Required"
fromStatus PreconditionFailed = "412 Precondition Failed"
fromStatus RequestEntityTooLarge = "413 Request Entity Too Large"
fromStatus RequestURITooLarge = "414 Request-URI Too Large"
fromStatus UnsupportedMediaType = "415 Unsupported Media Type"
fromStatus RequestedRangeNotSatisfiable = "416 Requested Range Not Satisfiable"
fromStatus ExpectationFailed = "417 Expectation Failed"
fromStatus InternalServerError = "500 Internal Server Error"
fromStatus NotImplemented = "501 Not Implemented"
fromStatus BadGateway = "502 Bad Gateway"
fromStatus ServiceUnavailable = "503 Service Unavailable"
fromStatus GatewayTimeout = "504 Gateway Time-out"
fromStatus HTTPVersionNotSupported = "505 HTTP Version Not Supported"

{-|
  Converting numeric status to 'Status'.
-}
toStatus :: S.ByteString -> Maybe Status
toStatus "200" = Just OK
toStatus "302" = Just Found
toStatus "400" = Just BadRequest
toStatus "501" = Just NotImplemented
toStatus _     = Nothing

{-|
  Returning 'True' for 4xx and 5xx.
-}
badStatus :: Status -> Bool
badStatus status = n `elem` "45"
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
              | FkOther S.ByteString
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

fieldStringList :: [S.ByteString]
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
type FieldValue = S.ByteString

stringFieldKey :: M.Map FieldValue FieldKey
stringFieldKey = M.fromList (zip fieldStringList fieldKeyList)

fieldKeyString :: M.Map FieldKey FieldValue
fieldKeyString = M.fromList (zip fieldKeyList fieldStringList)

{-|
  Converting field key to 'FieldKey'.
-}
toFieldKey :: S.ByteString -> FieldKey
toFieldKey str = fromMaybe (FkOther cstr) $ M.lookup cstr stringFieldKey
  where
    cstr = capitalize str

{-|
  Converting 'FieldKey' to field key.
-}
fromFieldKey :: FieldKey -> S.ByteString
fromFieldKey (FkOther cstr) = cstr
fromFieldKey key = fromMaybe err $ M.lookup key fieldKeyString
  where
    err = error "fromFieldKey"

(<:>) :: Char -> S.ByteString -> S.ByteString
(<:>) = S.cons

capitalize :: S.ByteString -> S.ByteString
capitalize s = toup s
    where
  toup "" = ""
  toup bs
    | isLetter x = toUpper x <:> stay xs
    | otherwise  =         x <:> toup xs
    where
      x  = S.head bs
      xs = S.tail bs
  stay "" = ""
  stay bs
    | isLetter x =         x <:> stay xs
    | otherwise  =         x <:> toup xs
    where
      x  = S.head bs
      xs = S.tail bs

----------------------------------------------------------------

{-|
  The type for Content-Type.
-}

type CT = S.ByteString

{-|
  Selecting a value of Content-Type from a file suffix.
-}
selectContentType :: String -> CT
selectContentType ""  = textPlain
selectContentType ext = fromMaybe appOct $ lookup lext contentTypeDB
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
                , (".en",   textHtml) -- xxx
                , (".ja",   textHtml) -- xxx
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
                , (".svg",  "image/svg+xml")
                ]

----------------------------------------------------------------

-- | The type for persist connection or not
data Persist = Close | Keep | PerUnknown deriving (Eq,Show)

fromPersist :: Persist -> S.ByteString
fromPersist Close      = "close"
fromPersist Keep       = "keep-alive"
fromPersist PerUnknown = "unknown"

toPersist :: S.ByteString -> Persist
toPersist cs = readPersist' (downcase cs)
  where
    downcase = S.map toLower
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
