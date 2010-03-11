{-|
  Utility functions.
-}
module Network.Web.Utils where

import Data.List
import Network.Socket
import Network.URI
import System.IO

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

----------------------------------------------------------------

{-|
  TCP connection information.
-}
data TCPInfo = TCPInfo {
  -- | Local IP address
    myAddr :: HostName
  -- | Local port number
  , myPort :: ServiceName
  -- | Remote IP address
  , peerAddr :: HostName
  -- | Remote port number
  , peerPort :: ServiceName
  } deriving (Eq,Show)

{-|
  Getting TCP connection information.
-}
getTCPInfo :: Socket -> IO TCPInfo
getTCPInfo sock = do
    (Just vMyAddr,   Just vMyPort)   <- getSocketName sock >>= getInfo
    (Just vPeerAddr, Just vPeerPort) <- getPeerName sock >>= getInfo
    return TCPInfo { myAddr = strip vMyAddr
                   , myPort = vMyPort
                   , peerAddr = strip vPeerAddr
                   , peerPort = vPeerPort}
  where
    getInfo = getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True
    strip x
      | "::ffff:" `isPrefixOf` x = drop 7 x
      | otherwise                = x
