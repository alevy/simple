{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Web.REST
  ( REST(..), RESTController, rest
  , index, show, create, update, delete
  , edit, new
  ) where

import Prelude hiding (show)

import Control.Monad.Trans.State
import Control.Monad.Identity
import Web.Simple.Responses
import Web.Simple.Router
import Network.HTTP.Types

data REST = REST
  { restIndex   :: Route ()
  , restShow    :: Route ()
  , restCreate  :: Route ()
  , restUpdate  :: Route ()
  , restDelete  :: Route ()
  , restEdit    :: Route ()
  , restNew     :: Route ()
  }

defaultREST :: REST
defaultREST = REST
  { restIndex   = routeAll $ notFound
  , restShow    = routeAll $ notFound
  , restCreate  = routeAll $ notFound
  , restUpdate  = routeAll $ notFound
  , restDelete  = routeAll $ notFound
  , restEdit    = routeAll $ notFound
  , restNew     = routeAll $ notFound
  }

instance Routeable REST where
  runRoute controller = runRoute $ do
    routeMethod GET $ do
      routeTop $ restIndex controller
      routeName "new" $ restNew controller
      routeVar "id" $ do
        routeTop $ restShow controller
        routeName "edit" $ restEdit controller

    routeMethod POST $ routeTop $ restCreate controller

    routeMethod DELETE $ routeVar "id" $ restDelete controller

    routeMethod PUT $ routeVar "id" $ restUpdate controller

type RESTControllerM a = StateT REST Identity a

instance Routeable (RESTControllerM a) where
  runRoute controller = rt
    where rt req = do
            let (_, st) = runIdentity $ runStateT controller defaultREST
            runRoute st req

rest :: RESTControllerM a -> REST
rest controller = snd . runIdentity $ runStateT controller defaultREST

type RESTController = RESTControllerM ()

index :: Routeable r => r -> RESTController
index route = modify $ \controller ->
  controller { restIndex = routeAll route }

create :: Routeable r => r -> RESTController
create route = modify $ \controller ->
  controller { restCreate = routeAll route }

edit :: Routeable r => r -> RESTController
edit route = modify $ \controller ->
  controller { restEdit = routeAll route }

new :: Routeable r => r -> RESTController
new route = modify $ \controller ->
  controller { restNew = routeAll route }

show :: Routeable r => r -> RESTController
show route = modify $ \controller ->
  controller { restShow = routeAll route }

update :: Routeable r => r -> RESTController
update route = modify $ \controller ->
  controller { restUpdate = routeAll route }

delete :: Routeable r => r -> RESTController
delete route = modify $ \controller ->
  controller { restDelete = routeAll route }

