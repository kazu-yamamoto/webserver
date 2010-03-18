{-|
  Utility functions.
-}
module Network.Web.Utils where

import Data.List
import Network.Socket
import System.IO

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
