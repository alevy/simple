{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

{- | 'ControllerT' provides a convenient syntax for writting 'Application'
  code as a Monadic action with access to an HTTP request as well as app
  specific data (e.g. a database connection pool, app configuration etc.)
  This module also defines some
  helper functions that leverage this feature. For example, 'redirectBack'
  reads the underlying request to extract the referer and returns a redirect
  response:

  @
    myControllerT = do
      ...
      if badLogin then
        redirectBack
        else
          ...
  @
-}
module Web.Simple.Controller.Trans where

import           Control.Monad.IO.Class
import           Control.Monad.IO.Peel
import           Control.Applicative
import           Control.Exception.Peel
import           Control.Monad hiding (guard)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.List (find)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable
import           Network.HTTP.Types
import           Network.Wai
import           Web.Simple.Responses


type ControllerState r = (r, Request)

-- | The ControllerT Monad is both a State-like monad which, when run, computes
-- either a 'Response' or a result. Within the ControllerT Monad, the remainder
-- of the computation can be short-circuited by 'respond'ing with a 'Response'.
newtype ControllerT m r a = ControllerT
  { runController :: ControllerState r ->
                      m (Either Response a, ControllerState r) }

instance (Monad m, Functor m) => Functor (ControllerT m r) where
  fmap f (ControllerT act) = ControllerT $ \st0 -> do
    (eaf, st) <- act st0
    case eaf of
      Left resp -> return (Left resp, st)
      Right result -> return (Right $ f result, st)

instance (Monad m, Applicative m) => Applicative (ControllerT m r) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ControllerT m r) where
  return a = ControllerT $ \st -> return $ (Right a, st)
  (ControllerT act) >>= fn = ControllerT $ \st0 -> do
    (eres, st) <- act st0
    case eres of
      Left resp -> return (Left resp, st)
      Right result -> do
        let (ControllerT fres) = fn result
        fres st

instance MonadIO m => MonadIO (ControllerT m r) where
  liftIO = liftC . liftIO

instance MonadPeelIO (ControllerT IO r) where
  peelIO = do
    r <- controllerState
    req <- request
    return $ \ctrl -> do
      res <- fst `fmap` runController ctrl (r, req)
      return $ hoistEither res

liftC :: Monad m => m a -> ControllerT m r a
liftC act = ControllerT $ \st -> act >>= \r -> return (Right r, st)

hoistEither :: Monad m => Either Response a -> ControllerT m r a
hoistEither eith = ControllerT $ \st -> return (eith, st)

ask :: Monad m => ControllerT m r (r, Request)
ask = ControllerT $ \rd -> return (Right rd, rd)

-- | Extract the request
request :: Monad m => ControllerT m r Request
request = liftM snd ask

local :: Monad m
      => ((r, Request) -> (r, Request)) -> ControllerT m r a -> ControllerT m r a
local f (ControllerT act) = ControllerT $ \st@(_, r) -> do
  (eres, (req, _)) <- act (f st)
  return (eres, (req, r))

-- | Modify the request for the given computation
localRequest :: Monad m
             => (Request -> Request) -> ControllerT m r a -> ControllerT m r a
localRequest f = local (\(r,req) -> (r, f req))

-- | Extract the application-specific state
controllerState :: Monad m => ControllerT m r r
controllerState = liftM fst ask

putState :: Monad m => r -> ControllerT m r ()
putState r = ControllerT $ \(_, req) -> return (Right (), (r, req))

-- | Convert the controller into an 'Application'
controllerApp :: Monad m => r -> ControllerT m r a -> SimpleApplication m
controllerApp r ctrl req =
  runController ctrl (r, req) >>=
    either return (const $ return notFound) . fst

-- | Decline to handle the request
--
-- @pass >> c === c@
-- @c >> pass === c@
pass :: Monad m => ControllerT m r ()
pass = ControllerT $ \st -> return (Right (), st)

-- | Provide a response
--
-- @respond r >>= f === respond r@
respond :: Monad m => Response -> ControllerT m r a
respond resp = hoistEither $ Left resp


-- | Lift an application to a controller
fromApp :: Monad m => (Request -> m Response) -> ControllerT m r ()
fromApp app = do
  req <- request
  resp <- liftC $ app req
  respond resp

-- | Matches on the hostname from the 'Request'. The route only succeeds on
-- exact matches.
routeHost :: Monad m => S.ByteString -> ControllerT m r a -> ControllerT m r ()
routeHost host = guardReq $ \req ->
  host == (fromMaybe "" $ requestHeaderHost req)

-- | Matches if the path is empty.
--
-- Note that this route checks that 'pathInfo'
-- is empty, so it works as expected in nested contexts that have
-- popped components from the 'pathInfo' list.
routeTop :: Monad m => ControllerT m r a -> ControllerT m r ()
routeTop = guardReq $ \req -> null (pathInfo req) ||
                              (T.length . head $ pathInfo req) == 0

-- | Matches on the HTTP request method (e.g. 'GET', 'POST', 'PUT')
routeMethod :: Monad m => StdMethod -> ControllerT m r a -> ControllerT m r ()
routeMethod method = guardReq $ (renderStdMethod method ==) . requestMethod

-- | Matches if the request's Content-Type exactly matches the given string
routeAccept :: Monad m => S8.ByteString -> ControllerT m r a -> ControllerT m r ()
routeAccept contentType = guardReq (isJust . find matching . requestHeaders)
 where matching hdr = fst hdr == hAccept && snd hdr == contentType

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
routePattern :: Monad m
             => S.ByteString -> ControllerT m r a -> ControllerT m r ()
routePattern pattern route =
  let patternParts = map T.unpack $ decodePathSegments pattern
  in foldr mkRoute (route >> return ()) patternParts
  where mkRoute (':':varName) = routeVar (S8.pack varName)
        mkRoute name = routeName (S8.pack name)

-- | Matches if the first directory in the path matches the given 'ByteString'
routeName :: Monad m => S.ByteString -> ControllerT m r a -> ControllerT m r ()
routeName name next = do
  req <- request
  if (length $ pathInfo req) > 0 && S8.unpack name == (T.unpack . head . pathInfo) req
    then localRequest popHdr next >> return ()
    else pass
  where popHdr req = req { pathInfo = (tail . pathInfo $ req) }

-- | Always matches if there is at least one directory in 'pathInfo' but and
-- adds a parameter to 'queryString' where the key is the first parameter and
-- the value is the directory consumed from the path.
routeVar :: Monad m => S.ByteString -> ControllerT m r a -> ControllerT m r ()
routeVar varName next = do
  req <- request
  case pathInfo req of
    [] -> pass
    x:_ | T.null x -> pass
        | otherwise -> localRequest popHdr next >> return ()
  where popHdr req = req {
              pathInfo = (tail . pathInfo $ req)
            , queryString = (varName, Just (varVal req)):(queryString req)}
        varVal req = S8.pack . T.unpack . head . pathInfo $ req

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
queryParam :: (Monad m, Parseable a)
           => S8.ByteString -- ^ Parameter name
           -> ControllerT m r (Maybe a)
queryParam varName = do
  qr <- liftM queryString request
  return $ case lookup varName qr of
    Just p -> Just $ parse $ fromMaybe S.empty p
    _ -> Nothing

-- | Like 'queryParam', but throws an exception if the parameter is not present.
queryParam' :: (Monad m, Parseable a)
            => S.ByteString -> ControllerT m r a
queryParam' varName =
  queryParam varName >>= maybe (err $ "no parameter " ++ show varName) return

-- | Selects all values with the given parameter name
queryParams :: (Monad m, Parseable a)
            => S.ByteString -> ControllerT m r [a]
queryParams varName = request >>= return .
                                  map (parse . fromMaybe S.empty . snd) .
                                  filter ((== varName) . fst) .
                                  queryString

-- | The class of types into which query parameters may be converted
class Parseable a where
  parse :: S8.ByteString -> a

instance Parseable S8.ByteString where
  parse = id
instance Parseable String where
  parse = S8.unpack
instance Parseable Text where
  parse = T.decodeUtf8

-- | Like 'queryParam', but further processes the parameter value with @read@.
-- If that conversion fails, an exception is thrown.
readQueryParam :: (Monad m, Read a)
               => S8.ByteString -- ^ Parameter name
               -> ControllerT m r (Maybe a)
readQueryParam varName =
  queryParam varName >>= maybe (return Nothing) (liftM Just . readParamValue varName)

-- | Like 'readQueryParam', but throws an exception if the parameter is not present.
readQueryParam' :: (Monad m, Read a)
                => S8.ByteString -- ^ Parameter name
                -> ControllerT m r a
readQueryParam' varName =
  queryParam' varName >>= readParamValue varName

-- | Like 'queryParams', but further processes the parameter values with @read@.
-- If any read-conversion fails, an exception is thrown.
readQueryParams :: (Monad m, Read a)
                => S8.ByteString -- ^ Parameter name
                -> ControllerT m r [a]
readQueryParams varName =
  queryParams varName >>= mapM (readParamValue varName)

readParamValue :: (Monad m, Read a)
               => S8.ByteString -> Text -> ControllerT m r a
readParamValue varName =
  maybe (err $ "cannot read parameter: " ++ show varName) return .
    readMay . T.unpack
  where readMay s = case [x | (x,rst) <- reads s, ("", "") <- lex rst] of
                      [x] -> Just x
                      _ -> Nothing

-- | Returns the value of the given request header or 'Nothing' if it is not
-- present in the HTTP request.
requestHeader :: Monad m => HeaderName -> ControllerT m r (Maybe S8.ByteString)
requestHeader name = request >>= return . lookup name . requestHeaders

-- | Redirect back to the referer. If the referer header is not present
-- redirect to root (i.e., @\/@).
redirectBack :: Monad m => ControllerT m r ()
redirectBack = redirectBackOr (redirectTo "/")

-- | Redirect back to the referer. If the referer header is not present
-- fallback on the given 'Response'.
redirectBackOr :: Monad m
               => Response -- ^ Fallback response
               -> ControllerT m r ()
redirectBackOr def = do
  mrefr <- requestHeader "referer"
  case mrefr of
    Just refr -> respond $ redirectTo refr
    Nothing   -> respond def

-- | Like 'Application', but with 'm' as the underlying monad
type SimpleApplication m = Request -> m Response

-- | Like 'Application', but with 'm' as the underlying monad
type SimpleMiddleware m = SimpleApplication m -> SimpleApplication m

-- guard

guard :: Monad m => Bool -> ControllerT m r a -> ControllerT m r ()
guard b c = if b then c >> return () else pass

guardM :: Monad m
       => ControllerT m r Bool -> ControllerT m r a -> ControllerT m r ()
guardM b c = b >>= flip guard c

guardReq :: Monad m
         => (Request -> Bool) -> ControllerT m r a -> ControllerT m r ()
guardReq f = guardM (liftM f request)

data ControllerException = ControllerException String
  deriving (Typeable)

instance Show ControllerException where
  show (ControllerException msg) = "ControllerT: " ++ msg

instance Exception ControllerException

err :: String -> ControllerT m r a
err = throw . ControllerException

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

  mainAction :: ControllerT () ()
  mainAction = ...

  signinForm :: ControllerT () ()
  signinForm req = ...

  login :: ControllerT () ()
  login = ...

  updateProfile :: ControllerT () ()
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
