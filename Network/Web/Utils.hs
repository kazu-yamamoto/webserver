{-|
  Utility functions.
-}
module Network.Web.Utils where

import Network.URI

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
