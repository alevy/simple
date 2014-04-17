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
    Controller, T.ControllerT(..)
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
  , hoistEither
  ) where

import           Control.Exception.Peel
import           Control.Monad.IO.Class
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Text (Text)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Parse
import           Web.Simple.Controller.Trans
                  (ControllerT)
import qualified Web.Simple.Controller.Trans as T
import           Web.Simple.Responses


-- | The Controller Monad is both a State-like monad which, when run, computes
-- either a 'Response' or a result. Within the Controller Monad, the remainder
-- of the computation can be short-circuited by 'respond'ing with a 'Response'.
type Controller s = ControllerT s IO

hoistEither :: Either Response a -> Controller s a
hoistEither = T.hoistEither

-- | Extract the request
request :: Controller s Request
request = T.request

-- | Modify the request for the given computation
localRequest :: (Request -> Request) -> Controller s a -> Controller s a
localRequest = T.localRequest

-- | Extract the application-specific state
controllerState :: Controller s s
controllerState = T.controllerState

putState :: s -> Controller s ()
putState = T.putState

-- | Convert the controller into an 'Application'
controllerApp :: s -> Controller s a -> Application
controllerApp = T.controllerApp

-- | Provide a response
--
-- @respond r >>= f === respond r@
respond :: Response -> Controller s a
respond = T.respond


-- | Lift an application to a controller
fromApp :: Application -> Controller s ()
fromApp = T.fromApp

-- | Matches on the hostname from the 'Request'. The route only succeeds on
-- exact matches.
routeHost :: S.ByteString -> Controller s a -> Controller s ()
routeHost = T.routeHost

-- | Matches if the path is empty.
--
-- Note that this route checks that 'pathInfo'
-- is empty, so it works as expected in nested contexts that have
-- popped components from the 'pathInfo' list.
routeTop :: Controller s a -> Controller s ()
routeTop = T.routeTop

-- | Matches on the HTTP request method (e.g. 'GET', 'POST', 'PUT')
routeMethod :: StdMethod -> Controller s a -> Controller s ()
routeMethod = T.routeMethod

-- | Matches if the request's Content-Type exactly matches the given string
routeAccept :: S8.ByteString -> Controller s a -> Controller s ()
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
routePattern :: Text -> Controller s a -> Controller s ()
routePattern = T.routePattern

-- | Matches if the first directory in the path matches the given 'ByteString'
routeName :: Text -> Controller s a -> Controller s ()
routeName = T.routeName

-- | Always matches if there is at least one directory in 'pathInfo' but and
-- adds a parameter to 'queryString' where the key is the first parameter and
-- the value is the directory consumed from the path.
routeVar :: Text -> Controller s a -> Controller s ()
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
           -> Controller s (Maybe a)
queryParam = T.queryParam

-- | Like 'queryParam', but throws an exception if the parameter is not present.
queryParam' :: T.Parseable a
            => S.ByteString -> Controller s a
queryParam' = T.queryParam'

-- | Selects all values with the given parameter name
queryParams :: T.Parseable a
            => S.ByteString -> Controller s [a]
queryParams = T.queryParams

-- | Like 'queryParam', but further processes the parameter value with @read@.
-- If that conversion fails, an exception is thrown.
readQueryParam :: Read a
               => S8.ByteString -- ^ Parameter name
               -> Controller s (Maybe a)
readQueryParam = T.readQueryParam

-- | Like 'readQueryParam', but throws an exception if the parameter is not present.
readQueryParam' :: Read a
                => S8.ByteString -- ^ Parameter name
                -> Controller s a
readQueryParam' = T.readQueryParam'

-- | Like 'queryParams', but further processes the parameter values with @read@.
-- If any read-conversion fails, an exception is thrown.
readQueryParams :: Read a
                => S8.ByteString -- ^ Parameter name
                -> Controller s [a]
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
parseForm :: Controller s ([Param], [(S.ByteString, FileInfo L.ByteString)])
parseForm = do
  req <- request
  liftIO $ parseRequestBody lbsBackEnd req

-- | Reads and returns the body of the HTTP request.
body :: Controller s L8.ByteString
body = do
  req <- request
  liftIO $ L8.fromChunks `fmap` (requestBody req $$ CL.consume)

-- | Returns the value of the given request header or 'Nothing' if it is not
-- present in the HTTP request.
requestHeader :: HeaderName -> Controller s (Maybe S8.ByteString)
requestHeader name = request >>= return . lookup name . requestHeaders

-- | Redirect back to the referer. If the referer header is not present
-- redirect to root (i.e., @\/@).
redirectBack :: Controller s a
redirectBack = redirectBackOr (redirectTo "/")

-- | Redirect back to the referer. If the referer header is not present
-- fallback on the given 'Response'.
redirectBackOr :: Response -- ^ Fallback response
               -> Controller s a
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
