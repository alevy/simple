{- |
Frank is a Sinatra-inspired DSL (see <http://www.sinatrarb.com>) for creating
routes. It is composable with all 'ToApplication' types, but is designed to be used
with 'Network.Wai.Controller's. Each verb ('get', 'post', 'put', etc') takes a
URL pattern of the form \"\/dir\/:paramname\/dir\" (see 'routePattern' for
details) and a 'ToApplication':

@
  main :: IO ()
  main = run 3000 $ controllerApp () $ do
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
import Web.Simple.Controller
import qualified Data.ByteString as S

-- | Helper method
frankMethod :: StdMethod -> S.ByteString -> Controller r a -> Controller r ()
frankMethod method pattern = routeMethod method . routePattern pattern . routeTop

-- | Matches the GET method on the given URL pattern
get :: S.ByteString -> Controller r a -> Controller r ()
get = frankMethod GET

-- | Matches the POST method on the given URL pattern
post :: S.ByteString -> Controller r a -> Controller r ()
post = frankMethod POST

-- | Matches the PUT method on the given URL pattern
put :: S.ByteString -> Controller r a -> Controller r ()
put = frankMethod PUT

-- | Matches the DELETE method on the given URL pattern
delete :: S.ByteString -> Controller r a -> Controller r ()
delete = frankMethod DELETE

-- | Matches the OPTIONS method on the given URL pattern
options :: S.ByteString -> Controller r a -> Controller r ()
options = frankMethod OPTIONS
