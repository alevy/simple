{-# LANGUAGE Safe, FlexibleInstances, OverloadedStrings #-}
{- |

REST is a DSL for creating routes using RESTful HTTP verbs.
See <http://en.wikipedia.org/wiki/Representational_state_transfer>

-}
module Web.REST
  ( REST(..), RESTController, rest, routeREST
  , index, show, create, update, delete
  , edit, new
  ) where

import Prelude hiding (show)

import Control.Monad.Trans.State
import Data.Functor.Identity
import Web.Simple.Responses
import Web.Simple.Controller.Trans
import Network.HTTP.Types

-- | Type used to encode a REST controller.
data REST m s = REST
  { restIndex   :: ControllerT s m ()
  , restShow    :: ControllerT s m ()
  , restCreate  :: ControllerT s m ()
  , restUpdate  :: ControllerT s m ()
  , restDelete  :: ControllerT s m ()
  , restEdit    :: ControllerT s m ()
  , restNew     :: ControllerT s m ()
  }

-- | Default state, returns @404@ for all verbs.
defaultREST :: Monad m => REST m s
defaultREST = REST
  { restIndex   = respond $ notFound
  , restShow    = respond $ notFound
  , restCreate  = respond $ notFound
  , restUpdate  = respond $ notFound
  , restDelete  = respond $ notFound
  , restEdit    = respond $ notFound
  , restNew     = respond $ notFound
  }

-- | Monad used to encode a REST controller incrementally.
type RESTControllerM m r a = StateT (REST m r) Identity a

rest :: Monad m => RESTControllerM m r a -> REST m r
rest rcontroller = snd . runIdentity $ runStateT rcontroller defaultREST

routeREST :: Monad m => REST m s -> ControllerT s m ()
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

type RESTController m r = RESTControllerM m r ()

-- | GET \/
index :: ControllerT s m () -> RESTController m s
index route = modify $ \controller ->
  controller { restIndex = route }

-- | POST \/
create :: ControllerT s m () -> RESTController m s
create route = modify $ \controller ->
  controller { restCreate = route }

-- | GET \/:id\/edit
edit :: ControllerT s m () -> RESTController m s
edit route = modify $ \controller ->
  controller { restEdit = route }

-- | GET \/new
new :: ControllerT s m () -> RESTController m s
new route = modify $ \controller ->
  controller { restNew = route }

-- | GET \/:id
show :: ControllerT s m () -> RESTController m s
show route = modify $ \controller ->
  controller { restShow = route }

-- | PUT \/:id
update :: ControllerT s m () -> RESTController m s
update route = modify $ \controller ->
  controller { restUpdate = route }

-- | DELETE \/:id
delete :: ControllerT s m () -> RESTController m s
delete route = modify $ \controller ->
  controller { restDelete = route }

