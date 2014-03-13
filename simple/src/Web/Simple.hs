{-# LANGUAGE Trustworthy #-}
{- |


/Simple/ is based on WAI - an standard interface for communicating between web
servers (like warp) and web applications. You can use /Simple/ completely
independently (and of course, use any WAI server to run it). Alternatively, you
can embed existing existing WAI applications inside an app built with /Simple/,
and embed an app built with simple in another WAI app.

All the components in /Simple/ are designed to be small and simple
enough to understand, replaceable, and work as well independantly as they do
together.

-}
module Web.Simple (
    module Web.Simple.Responses
  , module Web.Simple.Controller
  , module Web.Simple.Static
  , module Network.Wai
  -- * Overview
  -- $Overview

  -- * Tutorial
  -- $Tutorial

  -- ** Controllers
  -- $Controllers

  -- ** Routing
  -- $Routing
  ) where

import Network.Wai
import Web.Simple.Responses
import Web.Simple.Controller
import Web.Simple.Static

{- $Overview
 #overview#

WAI applications are functions of type 'Network.Wai.Application' - given a
client 'Network.Wai.Request' they return a 'Network.Wai.Response' to return to
the client (i.e. an HTTP status code, headers, body etc\'). A /Simple/
application 'Controller' -- a wrapper around WAI\'s 'Network.Wai.Application'
either returns a monadic value, or a 'Network.Wai.Response'. This allows
'Controller's to be chained together to create arbitrary complex routes. If a
'Controller' \"matches\" a route (e.g., based on the HTTP path, hostname,
cookies etc), it can 'respond' which shortcircuits the remaining execution and
immediately send the response back to the client. If none, of the 'Controller's
match, an HTTP 404 (NOT FOUND) response will be returned.

For example, this is a trivial \Simple\ app that notices whether the incoming
request was for the hostname \"hackage.haskell.org\" or \"www.haskell.org\":

@
  routeHost \"hackage.haskell.org\" $ do
    respond $ okHtml \"Welcome to Hackage\"

  routeHost \"www.haskell.org\" $ do
    respond $ okHtml \"You\'ve reached the Haskell Language home page\"
@

'routeHost' is a combinator that matches the a request based on the \"Host\"
header and defers to the passed in 'Controller' or returns '()'. There are
other built-in combinators for matching based on the request path, the HTTP
method, and it\'s easy to write your own combinators. You can chain such
combinators together monadically or using 'mappend' (since 'Controller' is an
instance of 'Monoid'). A typical /Simple/ app looks something like this:

@
  controllerApp () $ do
    routeTop $ do
      ... handle home page ...
    routeName \"posts\" $ do
      routeMethod GET $
        ... get all posts ...
      routeMethod POST $
        ... create new post ...
@

where 'controllerApp' generates an 'Network.Wai.Application' from a 'Controller'
returning a 404 (not found) response if all routes fail.

This package also includes the "Web.Frank" module which provide an API to create
applications similar to the Sinatra framework for Ruby, and the "Web.REST"
module to create RESTful applications similar to Ruby on Rails. Neither of
these modules is \"special\", in the sense that they are merely implemented in
terms of 'Controller's. The example above could be rewritten using "Web.Frank"
as such:

@
  controllerApp () $ do
    get \"/\" $ do
      ... display home page ...
    get \"/posts\" $ do
      ... get all posts ...
    post \"/posts\" $ do
      ... create new post ...
@

\Simple\ is broken down into the following modules:

@
  Web
  |-- "Web.Simple" - Re-exports most common modules
  |   |-- "Web.Simple.Controller" - Base monad and built-in routing combinators
  |   |-- "Web.Simple.Responses" - Common HTTP responses
  |   |-- "Web.Simple.Auth" - 'Controller's for authentication
  |   |-- "Web.Simple.Cache" - in memory and filesystem cache utilities
  |-- "Web.Frank" - Sinatra style 'Route's
  +-- "Web.REST" - Monad for creating RESTful controllers
@

-}

{- $Tutorial
#tutorial#

/Simple/ comes with a utility called \smpl\ which automates some common tasks
like creating a new application, running migrations and launching a development
server. To create a new /Simple/ app in a directory called \"example_app\", run:

@
  $ smpl create example_app
@

This will create a directory called \"example_app\" containing a /.cabal/ file
and and a single Haskell source file, \"Main.hs\":

@
\{\-\# LANGUAGE OverloadedStrings #\-\}

module Main where

import Web.Simple
import Network.Wai.Handler.Warp
import System.Posix.Env

app :: (Application -> IO ()) -> IO ()
app runner = runner $ do
  -- TODO: App initialization code here
  controllerApp () $ do
    respond $ okHtml \"Hello World\"

main :: IO ()
main = do
  port <- read \`fmap\` getEnvDefault \"PORT\" \"3000\"
  app (run port)
@

The `app` function is the entry point to your application. The argument is a
function that knows how to run a `Network.Wai.Application` -- for example,
warp's run method. `mkRouter` transforms a `Routeable` into an
`Network.Wai.Application`. The boilerplate is just a `Response` with the body
\"Hello World\" (and content-type \"text/html\"). To run a development server
on port 3000:

@
  $ cd example_app
  $ smpl
@

Pointing your browser to <http://localhost:3000> should display
\"Hello World\"!
-}

{- $Controllers
#controllers#

What is this 'controllerApp' business? The basic type in /Simple/ is a
'Controller' which contains both a 'Request' and app specific state.
'controllerApp' takes an initial application state (/unit/ in the example above)
and transforms a 'Controller' into a WAI 'Application' so it can be run by a
server like warp.

A 'Controller' is a 'Monad' that can perform actions in 'IO' (using 'liftIO'),
access the underlying 'request' or application state (via 'controllerState').
Finally, a 'Controller' can 'respond' to a request. 'respond' short-circuits
the rest of the computation and returns the 'Response' to the client.
'controllerApp' transforms a 'Controller' into a WAI application by running the
'Controller'. If the 'Controller' does not call 'respond', 'controllerApp'
defaults to responding to the client with a 404 not found. For example:

@
controllerApp () $ do
  liftIO $ putStrLn \"Responding to request\"
  respond $ okHtml \"Hello World\"
  liftIO $ putStrLn \"This message is never actually printed\"
@

When run, this code will always print the first message
(\"Responding to request\") and respond with a 200 page containing \"Hello
World\", but never print the second message. Short-circuiting the computation
in this way allows us to respond in different ways based on the request:

@
controllerApp () $ do
  path \<- rawPathInfo \<$> request
  when (path == \"/timeofday\") $ do
    timeStr \<- liftIO $ S8.pack . show \<$> getClockTime
    respond $ okHtml timeStr
  when (path == \"/whoami\") $
    user \<- liftIO $ S8.pack \<$> getLoginName
    respond $ okHtml user
@

This controller will respond with the current time if the path \"/timeofday\"
is requested, and the user running the server if the path \"/whoami\" is
requested. If neither of those paths match, it will respond with a 404
(NOT FOUND).

 -}

{- $Routing
#routing#

An app that does the same thing for every request is not very useful (well, it
might be, but if it is, even /Simple/ is not simple enough for you). We want to
build applications that do perform different actions based on properties of the
client\'s request - e.g., the path requests, GET or POST requests, the \"Host\"
header, etc\'. /Simple/\'s 'Controller's are flexible to accomplish this.
'Controller's encapsulate a function from a 'Request' to 'Either' a 'Response'
or some monadic value.

For example, let\'s extend the example using the 'Monad' syntax:

@
controllerApp () $ do
  routeTop $ do
    routeHost \"localhost\" $ respond $ okHtml \"Hello, localhost!\"
    routeHost \"test.lvh.me\" $ respond $ okHtml \"Hello, test.lvh.me!\"
  routeName \"advice\" $ okHtml \"Be excellent to each other!\"
@

Now, the app will respond differently depending on whether the client is
requesting the host name \"localhost\" or \"test.lvh.me\", or if the requested
path is \"\/advice\" rather than \"\/\". Take it for a spin in the browser (make
sure `smpl` is still running):

  * <http://localhost:3000>

  * <http://test.lvh.me:3000>

  * <http://localhost:3000/advice>

In this example, 'routeTop' matches if the 'Network.Wai.Request's
'Network.Wai.pathInfo' is empty, which means the requested path is \"\/\" (as
in this case), or the rest of the path has been consumed by previous 'Route's.
'routeName' matches if the next component in the path (specifically the 'head'
of 'Network.Wai.pathInfo') matches the argument (and if so, removes it). Check
out "Web.Simple.Router" for more complete documentation of these and other
'Route's.

For many apps it will be convenient to use even higher level routing APIs. The
modules "Web.Frank" and "Web.Sinatra" provide Sinatra-like and RESTful APIs,
respectively. Both modules are implement purely in terms of 'Route's and you
can easily implement your own patterns as well.

-}

