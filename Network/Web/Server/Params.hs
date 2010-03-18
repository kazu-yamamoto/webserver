module Network.Web.Server.Params where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Time
import Network.Web.URI
import Network.Web.Utils

{-|
  The configuration for the basic web server.
-}
data BasicConfig = BasicConfig {
   -- | A mapper from 'URI' to 'Path'.
   mapper :: URI -> Path
   -- | Resource obtaining function. The second argument is
   --   (offset of the resource, and length from the offset).
 , obtain :: FilePath -> Maybe (Integer,Integer) -> IO ByteString
   -- | A function to return the size of the resource and
   --   its modification time if exists.
 , info   :: FilePath -> IO (Maybe (Integer, UTCTime))
   -- | A server name specified the Server: field.
 , serverName :: String
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
  | CGI FilePath URLParameter ScriptName deriving (Eq,Show)

{-|
  A type for URL parameter.
-}
type URLParameter = String

{-|
  A type for script name.
-}
type ScriptName = String
