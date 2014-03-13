{-# LANGUAGE Trustworthy #-}
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
import Web.Simple.Controller.Trans
import qualified Data.ByteString as S

-- | Helper method
frankMethod :: Monad m
            => StdMethod -> S.ByteString -> ControllerT m r a
            -> ControllerT m r ()
frankMethod method pattern = routeMethod method . routePattern pattern . routeTop

-- | Matches the GET method on the given URL pattern
get :: Monad m => S.ByteString -> ControllerT m r a -> ControllerT m r ()
get = frankMethod GET

-- | Matches the POST method on the given URL pattern
post :: Monad m => S.ByteString -> ControllerT m r a -> ControllerT m r ()
post = frankMethod POST

-- | Matches the PUT method on the given URL pattern
put :: Monad m => S.ByteString -> ControllerT m r a -> ControllerT m r ()
put = frankMethod PUT

-- | Matches the DELETE method on the given URL pattern
delete :: Monad m => S.ByteString -> ControllerT m r a -> ControllerT m r ()
delete = frankMethod DELETE

-- | Matches the OPTIONS method on the given URL pattern
options :: Monad m => S.ByteString -> ControllerT m r a -> ControllerT m r ()
options = frankMethod OPTIONS
