module Network.Wai.Responses where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Types
import Network.Wai

resp404 :: Request -> Response
resp404 req = responseLBS status404 [] html
  where html = L8.concat
             [L8.pack
              "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
              \<HTML><HEAD>\n\
              \<TITLE>404 Not Found</TITLE>\n\
              \</HEAD><BODY>\n\
              \<H1>Not Found</H1>\n\
              \<P>The requested URL "
             , L8.pack . S8.unpack $ rawPathInfo req
             , L8.pack " was not found on this server.</P>\n\
                       \</BODY></HTML>\n"]

