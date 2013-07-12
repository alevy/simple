{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Web.REST
  ( REST(..), RESTController, rest, routeREST
  , index, show, create, update, delete
  , edit, new
  ) where

import Prelude hiding (show)

import Control.Monad.Trans.State
import Control.Monad.Identity
import Web.Simple.Responses
import Web.Simple.Controller
import Network.HTTP.Types

data REST r = REST
  { restIndex   :: Controller r ()
  , restShow    :: Controller r ()
  , restCreate  :: Controller r ()
  , restUpdate  :: Controller r ()
  , restDelete  :: Controller r ()
  , restEdit    :: Controller r ()
  , restNew     :: Controller r ()
  }

defaultREST :: REST r
defaultREST = REST
  { restIndex   = respond $ notFound
  , restShow    = respond $ notFound
  , restCreate  = respond $ notFound
  , restUpdate  = respond $ notFound
  , restDelete  = respond $ notFound
  , restEdit    = respond $ notFound
  , restNew     = respond $ notFound
  }

type RESTControllerM r a = StateT (REST r) Identity a

rest :: RESTControllerM r a -> REST r
rest rcontroller = snd . runIdentity $ runStateT rcontroller defaultREST

routeREST :: REST r -> Controller r ()
routeREST rst = do
  routeMethod GET $ do
    routeTop $ restIndex rst
    routeName "new" $ restNew rst
    routeVar "id" $ do
      routeTop $ restShow rst
      routeName "edit" $ restEdit rst

  routeMethod POST $ routeTop $ restCreate rst

  routeMethod DELETE $ routeVar "id" $ restDelete rst

  routeMethod PUT $ routeVar "id" $ restUpdate rst

type RESTController r = RESTControllerM r ()

index :: Controller r a -> RESTController r
index route = modify $ \controller ->
  controller { restIndex = void route }

create :: Controller r a -> RESTController r
create route = modify $ \controller ->
  controller { restCreate = void route }

edit :: Controller r a -> RESTController r
edit route = modify $ \controller ->
  controller { restEdit = void route }

new :: Controller r a -> RESTController r
new route = modify $ \controller ->
  controller { restNew = void route }

show :: Controller r a -> RESTController r
show route = modify $ \controller ->
  controller { restShow = void route }

update :: Controller r a -> RESTController r
update route = modify $ \controller ->
  controller { restUpdate = void route }

delete :: Controller r a -> RESTController r
delete route = modify $ \controller ->
  controller { restDelete = void route }

