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
import Web.Simple.Controller
import Network.HTTP.Types
import Network.Wai (Application)

data REST = REST
  { restIndex   :: Application
  , restShow    :: Application
  , restCreate  :: Application
  , restUpdate  :: Application
  , restDelete  :: Application
  , restEdit    :: Application
  , restNew     :: Application
  }

defaultREST :: REST
defaultREST = REST
  { restIndex   = toApp $ notFound
  , restShow    = toApp $ notFound
  , restCreate  = toApp $ notFound
  , restUpdate  = toApp $ notFound
  , restDelete  = toApp $ notFound
  , restEdit    = toApp $ notFound
  , restNew     = toApp $ notFound
  }

type RESTControllerM a = StateT REST Identity a

{- instance Routeable (RESTControllerM a) where
  runRoute controller = rt
    where rt req = do
            let (_, st) = runIdentity $ runStateT controller defaultREST
            runRoute st req
-}

rest :: RESTControllerM a -> REST
rest rcontroller = snd . runIdentity $ runStateT rcontroller defaultREST

instance ToApplication REST where
  toApp rst = controllerApp $ do
    routeMethod GET $ do
      routeTop . routeApp $ restIndex rst
      routeName "new" . routeApp $ restNew rst
      routeVar "id" $ do
        routeTop . routeApp $ restShow rst
        routeName "edit" . routeApp $ restEdit rst

    routeMethod POST $ routeTop . routeApp $ restCreate rst

    routeMethod DELETE $ routeVar "id" . routeApp $ restDelete rst

    routeMethod PUT $ routeVar "id" . routeApp $ restUpdate rst

instance ToApplication (RESTControllerM a) where
  toApp = toApp . rest

type RESTController = RESTControllerM ()

index :: ToApplication r => r -> RESTController
index route = modify $ \controller ->
  controller { restIndex = toApp route }

create :: ToApplication r => r -> RESTController
create route = modify $ \controller ->
  controller { restCreate = toApp route }

edit :: ToApplication r => r -> RESTController
edit route = modify $ \controller ->
  controller { restEdit = toApp route }

new :: ToApplication r => r -> RESTController
new route = modify $ \controller ->
  controller { restNew = toApp route }

show :: ToApplication r => r -> RESTController
show route = modify $ \controller ->
  controller { restShow = toApp route }

update :: ToApplication r => r -> RESTController
update route = modify $ \controller ->
  controller { restUpdate = toApp route }

delete :: ToApplication r => r -> RESTController
delete route = modify $ \controller ->
  controller { restDelete = toApp route }

