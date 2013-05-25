{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |

Conceptually, a route is function that, given an HTTP request, may return
an action (something that would return a response for the client if run).
Routes can be concatenated--where each route is evaluated until one
matches--and nested. Routes are expressed through the 'Routeable' type class.
'runRoute' transforms an instance of 'Routeable' to a function from 'Request'
to a monadic action (in the 'ResourceT' monad) that returns a
'Maybe' 'Response'. The return type was chosen to be monadic so routing
decisions can depend on side-effects (e.g. a random number or counter for A/B
testing, IP geolocation lookup etc').

-}

module Web.Simple.Router
  (
  -- * Example
  -- $Example
    ToApplication(..)
  -- * Controller Monad
  , Controller(..), ensure, request, respond, pass, local
  -- * Common Routes
  , routeApp, routeHost, routeTop, routeMethod
  , routePattern, routeName, routeVar
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans.Either
import Control.Monad.Reader
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Monoid
import Data.Conduit
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Web.Simple.Responses

{- |
'Routeable' types can be converted into a route function using 'runRoute'.
If the route is matched it returns a 'Response', otherwise 'Nothing'.

In general, 'Routeable's are data-dependant (on the 'Request'), but don't have
to be. For example, 'Application' is an instance of 'Routeable' that always
returns a 'Response':

@
  instance Routeable Application where
    runRoute app req = app req >>= return . Just
@

-}
class ToApplication r where
  toApp :: r -> Application

instance ToApplication Application where
  toApp = id

instance ToApplication Response where
  toApp = const . return

{- |
The 'Route' type is a basic instance of 'Routeable' that simply holds the
routing function and an arbitrary additional data parameter. The power is
derived from the instances of 'Monad' and 'Monoid', which allow the
simple construction of complex routing rules using either lists ('Monoid') or
do-notation. Moreover, because of it's simple type, any 'Routeable' can be used
as a 'Route' (using 'routeAll' or by applying it to 'runRoute'), making it
possible to leverage the monadic or monoid syntax for any 'Routeable'.

Commonly, route functions that construct a 'Route' only inspect the 'Request'
and other parameters. For example, 'routeHost' looks at the hostname:

@
  routeHost :: Routeable r => S.ByteString -> r -> Controller ()
  routeHost host route = Controller func ()
    where func req = if host == serverName req
                       then runRoute route req
                       else return Nothing
@

However, because the result of a route is in the
'ResourceT' monad, routes have all the power of an 'Application' and can make
state-dependant decisions. For example, it is trivial to implement a route that
succeeds for every other request (perhaps for A/B testing):

@
  routeEveryOther :: (Routeable r1, Routeable r2)
                  => TVar Int -> r1 -> r2 -> Controller ()
  routeEveryOther counter r1 r2 = Controller func ()
    where func req = do
            i <- liftIO . atomically $ do
                    i' <- readTVar counter
                    writeTVar counter (i' + 1)
                    return i'
            if i `mod` 2 == 0
              then runRoute r1 req
              else runRoute r2 req
@

-}

newtype Controller a = Controller (EitherT Response (ReaderT Request (ResourceT IO)) a)
                    deriving ( Monad, MonadIO, MonadReader Request
                             , Functor, Applicative)

ensure :: Controller a -> Controller b -> Controller b
ensure finalize act = do
  req <- request
  ea <- Controller $ lift . lift $ runRoute req act
  finalize
  Controller $ hoistEither ea

pass :: Controller ()
pass = Controller $ right ()

respond :: Response -> Controller a
respond = Controller . left

runRoute :: Request -> Controller a -> ResourceT IO (Either Response a)
runRoute req (Controller router) = runReaderT (runEitherT router) req

request :: Controller Request
request = ask

instance Monoid (Controller ()) where
  mempty = return ()
  mappend m1 m2 = m1 >> m2

instance ToApplication (Controller a) where
  toApp r = \req -> do
    eres <- runRoute req r
    case eres of
      Left resp -> return resp
      Right _ -> return notFound

-- | A route that always matches (useful for converting a 'Routeable' into a
-- 'Route').
routeApp :: ToApplication a => a -> Controller b
routeApp app = do
  req <- request 
  resp <- Controller $ lift . lift $ (toApp app) req
  respond resp

-- | Matches on the hostname from the 'Request'. The route only successeds on
-- exact matches.
routeHost :: S.ByteString -> Controller () -> Controller ()
routeHost host next = do
  req <- request
  if host == serverName req then
    next
    else pass

-- | Matches if the path is empty. Note that this route checks that 'pathInfo'
-- is empty, so it works as expected when nested under namespaces or other
-- routes that pop the 'pathInfo' list.
routeTop :: Controller () -> Controller ()
routeTop next = do
  req <- request
  if null (pathInfo req)  || (T.length . head $ pathInfo req) == 0
    then next
    else pass

-- | Matches on the HTTP request method (e.g. 'GET', 'POST', 'PUT')
routeMethod :: StdMethod -> Controller () -> Controller ()
routeMethod method next = do
  req <- request
  if renderStdMethod method == requestMethod req then
    next
    else pass

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
routePattern :: S.ByteString -> Controller () -> Controller ()
routePattern pattern route =
  let patternParts = map T.unpack $ decodePathSegments pattern
  in foldr mkRoute (route >> return ()) patternParts
  where mkRoute (':':varName) = routeVar (S8.pack varName)
        mkRoute varName = routeName (S8.pack varName)

-- | Matches if the first directory in the path matches the given 'ByteString'
routeName :: S.ByteString -> Controller () -> Controller ()
routeName name next = do
  req <- request
  if (length $ pathInfo req) > 0 && S8.unpack name == (T.unpack . head . pathInfo) req
    then local popHdr next
    else pass
  where popHdr req = req { pathInfo = (tail . pathInfo $ req) }

-- | Always matches if there is at least one directory in 'pathInfo' but and
-- adds a parameter to 'queryString' where the key is the first parameter and
-- the value is the directory consumed from the path.
routeVar :: S.ByteString -> Controller () -> Controller ()
routeVar varName next = do
  req <- request
  if (length $ pathInfo req) > 0 then
    local popHdr next
    else pass
  where popHdr req = req {
              pathInfo = (tail . pathInfo $ req)
            , queryString = (varName, Just (varVal req)):(queryString req)}
        varVal req = S8.pack . T.unpack . head . pathInfo $ req

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

  mainAction :: Application
  mainAction req = ...

  signinForm :: Application
  signinForm req = ...

  login :: Application
  login req = ...

  updateProfile :: Application
  updateProfile req = ...

  main :: IO ()
  main = runSettings defaultSettings $ mkRoute $ do
    routeTop mainAction
    routeName \"sessions\" $ do
      routeMethod GET signinForm
      routeMethod POST login
    routeMethod PUT $ routePattern \"users/:id\" updateProfile
    routeAll $ responseLBS status404 [] \"Are you in the right place?\"
@

-}

