{- |
Frank is a Sinatra-inspired DSL (see <http://www.sinatrarb.com>) for creating
routes. It is composable with all 'ToApplication' types, but is designed to be used
with 'Network.Wai.Controller's. Each verb ('get', 'post', 'put', etc') takes a
URL pattern of the form \"\/dir\/:paramname\/dir\" (see 'routePattern' for
details) and a 'ToApplication':

@
  main :: IO ()
  main = runSettings defaultSettings $ mkRouter $ do
    get \"\/\" $ do
      req <- request
      return $ okHtml $ fromString $
        \"Welcome Home \" ++ (show $ serverName req)
    get \"\/user\/:id\" $ do
      userId \<- queryParam \"id\" >>= fromMaybe \"\"
      return $ ok \"text/json\" $ fromString $
        \"{\\\"myid\\\": \" ++ (show userId) ++ \"}\"
    put \"\/user\/:id\" $ do
      ...
@

-}
module Web.Frank
  ( get
  , post
  , put
  , delete
  , options
  ) where

import Network.HTTP.Types
import Web.Simple.Router
import qualified Data.ByteString as S

-- | Helper method
frankMethod :: ToApplication a => StdMethod -> S.ByteString -> a -> Controller ()
frankMethod method pattern = routeMethod method . routePattern pattern . routeTop . routeApp

-- | Matches the GET method on the given URL pattern
get :: ToApplication r => S.ByteString -> r -> Controller ()
get = frankMethod GET

-- | Matches the POST method on the given URL pattern
post :: ToApplication r => S.ByteString -> r -> Controller ()
post = frankMethod POST

-- | Matches the PUT method on the given URL pattern
put :: ToApplication r => S.ByteString -> r -> Controller ()
put = frankMethod PUT

-- | Matches the DELETE method on the given URL pattern
delete :: ToApplication r => S.ByteString -> r -> Controller ()
delete = frankMethod DELETE

-- | Matches the OPTIONS method on the given URL pattern
options :: ToApplication r => S.ByteString -> r -> Controller ()
options = frankMethod OPTIONS

