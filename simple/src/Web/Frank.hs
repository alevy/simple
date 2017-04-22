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
      respond $ okHtml $ fromString $
        \"Welcome Home \" ++ (show $ serverName req)
    get \"\/user\/:id\" $ do
      userId \<- queryParam \"id\" >>= fromMaybe \"\"
      respond $ ok \"text/json\" $ fromString $
        \"{\\\"myid\\\": \" ++ (show userId) ++ \"}\"
    put \"\/user\/:id\" $ do
      ...
@

-}
module Web.Frank
  ( get
  , post
  , put
  , patch
  , delete
  , options
  ) where

import Network.HTTP.Types
import Web.Simple.Controller.Trans
import Data.Text (Text)

-- | Helper method
frankMethod :: Monad m
            => StdMethod -> Text -> ControllerT s m a
            -> ControllerT s m ()
frankMethod method pattern = routeMethod method . routePattern pattern . routeTop

-- | Matches the GET method on the given URL pattern
get :: Monad m => Text -> ControllerT s m a -> ControllerT s m ()
get = frankMethod GET

-- | Matches the POST method on the given URL pattern
post :: Monad m => Text -> ControllerT s m a -> ControllerT s m ()
post = frankMethod POST

-- | Matches the PUT method on the given URL pattern
put :: Monad m => Text -> ControllerT s m a -> ControllerT s m ()
put = frankMethod PUT

-- | Matches the PATCH method on the given URL pattern
patch :: Monad m => Text -> ControllerT s m a -> ControllerT s m ()
patch = frankMethod PATCH

-- | Matches the DELETE method on the given URL pattern
delete :: Monad m => Text -> ControllerT s m a -> ControllerT s m ()
delete = frankMethod DELETE

-- | Matches the OPTIONS method on the given URL pattern
options :: Monad m => Text -> ControllerT s m a -> ControllerT s m ()
options = frankMethod OPTIONS
