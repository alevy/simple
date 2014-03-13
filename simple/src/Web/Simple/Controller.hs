{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

{- | 'Controller' provides a convenient syntax for writting 'Application'
  code as a Monadic action with access to an HTTP request as well as app
  specific data (e.g. a database connection pool, app configuration etc.)
  This module also defines some
  helper functions that leverage this feature. For example, 'redirectBack'
  reads the underlying request to extract the referer and returns a redirect
  response:

  @
    myController = do
      ...
      if badLogin then
        redirectBack
        else
          ...
  @
-}
module Web.Simple.Controller
  (
  -- * Example
  -- $Example
  -- * Controller Monad
    Controller, T.ControllerT(..), ControllerState
  , controllerApp, controllerState, putState
  , request, localRequest, respond
  , requestHeader
  -- * Common Routes
  , routeHost, routeTop, routeMethod, routeAccept
  , routePattern, routeName, routeVar
  -- * Inspecting query
  , T.Parseable
  , queryParam, queryParam', queryParams
  , readQueryParam, readQueryParam', readQueryParams
  , parseForm
  -- * Redirection via referrer
  , redirectBack
  , redirectBackOr
  -- * Exception handling
  , T.ControllerException
  , module Control.Exception.Peel
  -- * Integrating other WAI components
  , fromApp
  -- * Low-level utilities
  , body
  , hoistEither, ask, local, pass
  ) where

import           Control.Exception.Peel
import           Control.Monad.IO.Class
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Parse
import           Web.Simple.Controller.Trans
                  (ControllerT, ControllerState)
import qualified Web.Simple.Controller.Trans as T
import           Web.Simple.Responses


-- | The Controller Monad is both a State-like monad which, when run, computes
-- either a 'Response' or a result. Within the Controller Monad, the remainder
-- of the computation can be short-circuited by 'respond'ing with a 'Response'.
type Controller = ControllerT IO

hoistEither :: Either Response a -> Controller r a
hoistEither = T.hoistEither

ask :: Controller r (r, Request)
ask = T.ask

-- | Extract the request
request :: Controller r Request
request = T.request

local :: ((r, Request) -> (r, Request)) -> Controller r a -> Controller r a
local = T.local

-- | Modify the request for the given computation
localRequest :: (Request -> Request) -> Controller r a -> Controller r a
localRequest = T.localRequest

-- | Extract the application-specific state
controllerState :: Controller r r
controllerState = T.controllerState

putState :: r -> Controller r ()
putState = T.putState

-- | Convert the controller into an 'Application'
controllerApp :: r -> Controller r a -> Application
controllerApp = T.controllerApp

-- | Decline to handle the request
--
-- @pass >> c === c@
-- @c >> pass === c@
pass :: Controller r ()
pass = T.pass

-- | Provide a response
--
-- @respond r >>= f === respond r@
respond :: Response -> Controller r a
respond = T.respond


-- | Lift an application to a controller
fromApp :: Application -> Controller r ()
fromApp = T.fromApp

-- | Matches on the hostname from the 'Request'. The route only succeeds on
-- exact matches.
routeHost :: S.ByteString -> Controller r a -> Controller r ()
routeHost = T.routeHost

-- | Matches if the path is empty.
--
-- Note that this route checks that 'pathInfo'
-- is empty, so it works as expected in nested contexts that have
-- popped components from the 'pathInfo' list.
routeTop :: Controller r a -> Controller r ()
routeTop = T.routeTop

-- | Matches on the HTTP request method (e.g. 'GET', 'POST', 'PUT')
routeMethod :: StdMethod -> Controller r a -> Controller r ()
routeMethod = T.routeMethod

-- | Matches if the request's Content-Type exactly matches the given string
routeAccept :: S8.ByteString -> Controller r a -> Controller r ()
routeAccept = T.routeAccept

-- | Routes the given URL pattern. Patterns can include
-- directories as well as variable patterns (prefixed with @:@) to be added
-- to 'queryString' (see 'routeVar')
--
--  * \/posts\/:id
--
--  * \/posts\/:id\/new
--
--  * \/:date\/posts\/:category\/new
--
routePattern :: S.ByteString -> Controller r a -> Controller r ()
routePattern = T.routePattern

-- | Matches if the first directory in the path matches the given 'ByteString'
routeName :: S.ByteString -> Controller r a -> Controller r ()
routeName = T.routeName

-- | Always matches if there is at least one directory in 'pathInfo' but and
-- adds a parameter to 'queryString' where the key is the first parameter and
-- the value is the directory consumed from the path.
routeVar :: S.ByteString -> Controller r a -> Controller r ()
routeVar = T.routeVar

--
-- query parameters
--

-- | Looks up the parameter name in the request's query string and returns the
-- @Parseable@ value or 'Nothing'.
--
-- For example, for a request with query string: \"?foo=bar&baz=7\",
-- @queryParam \"foo\"@
-- would return @Just "bar"@, but
-- @queryParam \"zap\"@
-- would return @Nothing@.
queryParam :: T.Parseable a
           => S8.ByteString -- ^ Parameter name
           -> Controller r (Maybe a)
queryParam = T.queryParam

-- | Like 'queryParam', but throws an exception if the parameter is not present.
queryParam' :: T.Parseable a
            => S.ByteString -> Controller r a
queryParam' = T.queryParam'

-- | Selects all values with the given parameter name
queryParams :: T.Parseable a
            => S.ByteString -> Controller r [a]
queryParams = T.queryParams

-- | Like 'queryParam', but further processes the parameter value with @read@.
-- If that conversion fails, an exception is thrown.
readQueryParam :: Read a
               => S8.ByteString -- ^ Parameter name
               -> Controller r (Maybe a)
readQueryParam = T.readQueryParam

-- | Like 'readQueryParam', but throws an exception if the parameter is not present.
readQueryParam' :: Read a
                => S8.ByteString -- ^ Parameter name
                -> Controller r a
readQueryParam' = T.readQueryParam'

-- | Like 'queryParams', but further processes the parameter values with @read@.
-- If any read-conversion fails, an exception is thrown.
readQueryParams :: Read a
                => S8.ByteString -- ^ Parameter name
                -> Controller r [a]
readQueryParams = T.readQueryParams

-- | Parses a HTML form from the request body. It returns a list of 'Param's as
-- well as a list of 'File's, which are pairs mapping the name of a /file/ form
-- field to a 'FileInfo' pointing to a temporary file with the contents of the
-- upload.
--
-- @
--   myControllerT = do
--     (prms, files) <- parseForm
--     let mPicFile = lookup \"profile_pic\" files
--     case mPicFile of
--       Just (picFile) -> do
--         sourceFile (fileContent picFile) $$
--           sinkFile (\"images/\" ++ (fileName picFile))
--         respond $ redirectTo \"/\"
--       Nothing -> redirectBack
-- @
parseForm :: Controller r ([Param], [(S.ByteString, FileInfo L.ByteString)])
parseForm = do
  req <- request
  liftIO $ parseRequestBody lbsBackEnd req

-- | Reads and returns the body of the HTTP request.
body :: Controller r L8.ByteString
body = do
  req <- request
  liftIO $ L8.fromChunks `fmap` (requestBody req $$ CL.consume)

-- | Returns the value of the given request header or 'Nothing' if it is not
-- present in the HTTP request.
requestHeader :: HeaderName -> Controller r (Maybe S8.ByteString)
requestHeader name = request >>= return . lookup name . requestHeaders

-- | Redirect back to the referer. If the referer header is not present
-- redirect to root (i.e., @\/@).
redirectBack :: Controller r ()
redirectBack = redirectBackOr (redirectTo "/")

-- | Redirect back to the referer. If the referer header is not present
-- fallback on the given 'Response'.
redirectBackOr :: Response -- ^ Fallback response
               -> Controller r ()
redirectBackOr def = do
  mrefr <- requestHeader "referer"
  case mrefr of
    Just refr -> respond $ redirectTo refr
    Nothing   -> respond def

{- $Example
 #example#

The most basic 'Routeable' types are 'Application' and 'Response'. Reaching
either of these types marks a termination in the routing lookup. This module
exposes a monadic type 'Route' which makes it easy to create routing logic
in a DSL-like fashion.

'Route's are concatenated using the '>>' operator (or using do-notation).
In the end, any 'Routeable', including a 'Route' is converted to an
'Application' and passed to the server using 'mkRoute':

@

  mainAction :: Controller () ()
  mainAction = ...

  signinForm :: Controller () ()
  signinForm req = ...

  login :: Controller () ()
  login = ...

  updateProfile :: Controller () ()
  updateProfile = ...

  main :: IO ()
  main = run 3000 $ controllerApp () $ do
    routeTop mainAction
    routeName \"sessions\" $ do
      routeMethod GET signinForm
      routeMethod POST login
    routeMethod PUT $ routePattern \"users/:id\" updateProfile
    routeAll $ responseLBS status404 [] \"Are you in the right place?\"
@

-}
