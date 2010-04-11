module Network.Web.Server.Params where

import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Time
import Network.TCPInfo
import Network.Web.URI

{-|
  The configuration for the basic web server.
-}
data BasicConfig = BasicConfig {
   -- | A mapper from 'URI' to 'Path'.
   mapper :: URI -> Path
   -- | Resource obtaining function. The second argument is
   --   (offset of the resource, and length from the offset).
 , obtain :: FilePath -> Maybe (Integer,Integer) -> IO L.ByteString
   -- | A function to return the size of the resource and
   --   its modification time if exists.
 , info   :: FilePath -> IO (Maybe (Integer, UTCTime))
   -- | A server name specified the Server: field.
 , serverName :: S.ByteString
   -- | 'TCPInfo' for passing CGI. (See c10k library.)
 , tcpInfo :: TCPInfo
}

{-|
  Control information of how to handle 'URI'.
-}
data Path =
    -- | 'URI' cannot be converted into any resources.
    None
    -- | 'URI' is converted into a resource (typically a file).
  | File FilePath
    -- | 'URI' is converted into CGI.
  | PathCGI CGI
  deriving (Eq,Show)

{-|
  Internal information of CGI converted from 'URI'.
-}
data CGI = CGI {
    -- | A porgram path to be executed.
    progPath    :: FilePath
    -- | A script name.
  , scriptName  :: String
    -- | A path information.
  , pathInfo    :: String
    -- | A query string.
  , queryString :: String
  } deriving (Eq,Show)
