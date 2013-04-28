{- |


\Simple\ is based on WAI - an standard interface for communicating between web
servers (like warp) and web applications. You can use \Simple\ completely
independently (and of course, use any WAI server to run it). Alternatively, you
can embed existing existing WAI applications inside an app built with \Simple\,
and embed an app built with simple in another WAI app.

All the components in \Simple\ are designed to be small and simple
enough to understand, replaceable, and work as well independantly as they do
together.

-}
module Web.Simple (
    module Web.Simple.Router
  , module Web.Simple.Controller
  , module Web.Simple.Responses
  -- * Overview
  -- $Overview

  -- * Tutorial
  -- $Tutorial
  ) where

import Web.Simple.Router
import Web.Simple.Responses
import Web.Simple.Controller

{- $Overview
 #overview#

WAI applications are functions of type 'Network.Wai.Application' - give a
client 'Network.Wai.Request' they return a 'Network.Wai.Response' to return to
the client (i.e. an HTTP status code, headers, body etc\'). A \Simple\
application is composed of a set of 'Routeable's -- a typeclass similar to an
'Network.Wai.Application' except it returns a 'Maybe' 'Network.Wai.Response'.

The simplest instance of 'Routeable' is a 'Network.Wai.Application' itself.
This is a 'Routeable' that always succeeds. A 'Controller' is an 'Application',
but it internalizes the 'Network.Wai.Request' argument in a
'Control.Monad.Trans.ReaderT' and provides some convenience methods for
accessing properties of the request (e.g. parsing form data). More
interestingly, 'Route's can decide whether to respond to the 'Network.Wai.Request' dynamically, based on
the contents of the 'Network.Wai.Request' or any external input (e.g. time of
day, a database query etc\'). For example, 'routeHost' falls-through to it\'s
second argument (another 'Routeable') if the \"Host\" header in the client\'s
'Network.Wai.Request' matches the first argument:

@
  routeHost \"hackage.haskell.org\" myHackageApp
@

There are other 'Route's for matching based on the request path, the HTTP
method, and it\'s easy to write other 'Route's. 'Route' is also an instance of
'Monad' and 'Data.Monoid.Monoid' so they can be chained together to route
requests in a single application to different controllers. If the first 'Route'
fails, the next is tried until there are no more 'Route's. Thus, a \Simple\ app
might look something like this:

@
  mkRouter $ do
    routeTop $ do
      ... handle home page ...
    routeName \"posts\" $ do
      routeMethod GET $
        ... get all posts ...
      routeMethod POST $
        ... create new post ...
@

where 'mkRouter' generates an 'Network.Wai.Application' from a 'Routeable'
returning a 404 (not found) response if all routes fail.

It\'s convenient to specialize sets of these 'Route's for some common patters.
This package includes the 'Web.Frank' module which provide an API to create
applications similar to the Sinatra framework for Ruby, and the 'Web.REST'
module to create RESTful applications similar to Ruby on Rails. The example
above could be rewritten using 'Web.Frank' as such:

@
  mkRouter $ do
    get \"/\" $ do
      ... display home page ...
    get \"/posts\" $ do
      ... get all posts ...
    post \"/posts\" $ do
      ... create new post ...
@

-}

{- $Tutorial
#tutorial#

-}
