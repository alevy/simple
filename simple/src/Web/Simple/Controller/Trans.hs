{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

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

import           Control.Exception
import           Control.Monad hiding (guard)
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Applicative
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

-- | The ControllerT Monad is both a State-like monad which, when run, computes
-- either a 'Response' or a result. Within the ControllerT Monad, the remainder
-- of the computation can be short-circuited by 'respond'ing with a 'Response'.
newtype ControllerT s m a = ControllerT
  { runController :: s -> Request ->
                      m (Either Response a, s) }

instance Functor m => Functor (ControllerT s m) where
  fmap f (ControllerT act) = ControllerT $ \st0 req ->
    go `fmap` act st0 req
    where go (eaf, st) = case eaf of
                              Left resp -> (Left resp, st)
                              Right result -> (Right $ f result, st)

instance (Monad m, Functor m) => Applicative (ControllerT s m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ControllerT s m) where
  return a = ControllerT $ \st _ -> return $ (Right a, st)
  (ControllerT act) >>= fn = ControllerT $ \st0 req -> do
    (eres, st) <- act st0 req
    case eres of
      Left resp -> return (Left resp, st)
      Right result -> do
        let (ControllerT fres) = fn result
        fres st req

instance (Functor m, Monad m) => Alternative (ControllerT s m) where
  empty = respond notFound
  (<|>) = (>>)

instance Monad m => MonadPlus (ControllerT s m) where
  mzero = respond notFound
  mplus = flip (>>)

instance MonadTrans (ControllerT s) where
  lift act = ControllerT $ \st _ -> act >>= \r -> return (Right r, st)

instance Monad m => MonadState s (ControllerT s m) where
  get = ControllerT $ \s _ -> return (Right s, s)
  put s = ControllerT $ \_ _ -> return (Right (), s)

instance Monad m => MonadReader Request (ControllerT s m) where
  ask = ControllerT $ \st req -> return (Right req, st)
  local f (ControllerT act) = ControllerT $ \st req -> act st (f req)

instance MonadIO m => MonadIO (ControllerT s m) where
  liftIO = lift . liftIO

instance (Applicative m, Monad m, MonadBase m m) => MonadBase m (ControllerT s m) where
  liftBase = liftBaseDefault

instance MonadBaseControl m m => MonadBaseControl m (ControllerT s m) where
  newtype StM (ControllerT s m) a = StCtrl (Either Response a, s)
  liftBaseWith fn = ControllerT $ \st req -> do
    res <- fn $ \act -> liftM StCtrl $ runController act st req
    return (Right res, st)
  restoreM (StCtrl (a, s)) = ControllerT $ \_ _ -> return (a, s)

hoistEither :: Monad m => Either Response a -> ControllerT s m a
hoistEither eith = ControllerT $ \st _ -> return (eith, st)

-- | Extract the request
request :: Monad m => ControllerT s m Request
request = ask

-- | Modify the request for the given computation
localRequest :: Monad m
             => (Request -> Request) -> ControllerT s m a -> ControllerT s m a
localRequest = local

-- | Extract the application-specific state
controllerState :: Monad m => ControllerT s m s
controllerState = get

putState :: Monad m => s -> ControllerT s m ()
putState = put

-- | Convert the controller into an 'Application'
controllerApp :: Monad m => s -> ControllerT s m a -> SimpleApplication m
controllerApp s ctrl req =
  runController ctrl s req >>=
    either return (const $ return notFound) . fst

-- | Provide a response
--
-- @respond r >>= f === respond r@
respond :: Monad m => Response -> ControllerT s m a
respond resp = hoistEither $ Left resp


-- | Lift an application to a controller
fromApp :: Monad m => (Request -> m Response) -> ControllerT s m ()
fromApp app = do
  req <- request
  resp <- lift $ app req
  respond resp

-- | Matches on the hostname from the 'Request'. The route only succeeds on
-- exact matches.
routeHost :: Monad m => S.ByteString -> ControllerT s m a -> ControllerT s m ()
routeHost host = guardReq $ \req ->
  Just host == requestHeaderHost req

-- | Matches if the path is empty.
--
-- Note that this route checks that 'pathInfo'
-- is empty, so it works as expected in nested contexts that have
-- popped components from the 'pathInfo' list.
routeTop :: Monad m => ControllerT s m a -> ControllerT s m ()
routeTop = guardReq $ \req -> null (pathInfo req) ||
                              (T.length . head $ pathInfo req) == 0

-- | Matches on the HTTP request method (e.g. 'GET', 'POST', 'PUT')
routeMethod :: Monad m => StdMethod -> ControllerT s m a -> ControllerT s m ()
routeMethod method = guardReq $ (renderStdMethod method ==) . requestMethod

-- | Matches if the request's Content-Type exactly matches the given string
routeAccept :: Monad m => S8.ByteString -> ControllerT s m a -> ControllerT s m ()
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
             => Text -> ControllerT s m a -> ControllerT s m ()
routePattern pattern route =
  let patternParts = decodePathSegments (T.encodeUtf8 pattern)
  in foldr mkRoute (route >> return ()) patternParts
  where mkRoute name = case T.uncons name of
                            Just (':', varName) -> routeVar varName
                            _ -> routeName name

-- | Matches if the first directory in the path matches the given 'ByteString'
routeName :: Monad m => Text -> ControllerT s m a -> ControllerT s m ()
routeName name next = do
  req <- request
  if (length $ pathInfo req) > 0 && name == (head . pathInfo) req
    then localRequest popHdr next >> return ()
    else return ()
  where popHdr req = req { pathInfo = (tail . pathInfo $ req) }

-- | Always matches if there is at least one directory in 'pathInfo' but and
-- adds a parameter to 'queryString' where the key is the first parameter and
-- the value is the directory consumed from the path.
routeVar :: Monad m => Text -> ControllerT s m a -> ControllerT s m ()
routeVar varName next = do
  req <- request
  case pathInfo req of
    [] -> return ()
    x:_ | T.null x -> return ()
        | otherwise -> localRequest popHdr next >> return ()
  where popHdr req = req {
              pathInfo = (tail . pathInfo $ req)
            , queryString = (T.encodeUtf8 varName, Just (varVal req)):(queryString req)}
        varVal req = T.encodeUtf8 . head . pathInfo $ req

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
           -> ControllerT s m (Maybe a)
queryParam varName = do
  qr <- liftM queryString request
  return $ case lookup varName qr of
    Just p -> Just $ parse $ fromMaybe S.empty p
    _ -> Nothing

-- | Like 'queryParam', but throws an exception if the parameter is not present.
queryParam' :: (Monad m, Parseable a)
            => S.ByteString -> ControllerT s m a
queryParam' varName =
  queryParam varName >>= maybe (err $ "no parameter " ++ show varName) return

-- | Selects all values with the given parameter name
queryParams :: (Monad m, Parseable a)
            => S.ByteString -> ControllerT s m [a]
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
               -> ControllerT s m (Maybe a)
readQueryParam varName =
  queryParam varName >>= maybe (return Nothing) (liftM Just . readParamValue varName)

-- | Like 'readQueryParam', but throws an exception if the parameter is not present.
readQueryParam' :: (Monad m, Read a)
                => S8.ByteString -- ^ Parameter name
                -> ControllerT s m a
readQueryParam' varName =
  queryParam' varName >>= readParamValue varName

-- | Like 'queryParams', but further processes the parameter values with @read@.
-- If any read-conversion fails, an exception is thrown.
readQueryParams :: (Monad m, Read a)
                => S8.ByteString -- ^ Parameter name
                -> ControllerT s m [a]
readQueryParams varName =
  queryParams varName >>= mapM (readParamValue varName)

readParamValue :: (Monad m, Read a)
               => S8.ByteString -> Text -> ControllerT s m a
readParamValue varName =
  maybe (err $ "cannot read parameter: " ++ show varName) return .
    readMay . T.unpack
  where readMay s = case [x | (x,rst) <- reads s, ("", "") <- lex rst] of
                      [x] -> Just x
                      _ -> Nothing

-- | Returns the value of the given request header or 'Nothing' if it is not
-- present in the HTTP request.
requestHeader :: Monad m => HeaderName -> ControllerT s m (Maybe S8.ByteString)
requestHeader name = request >>= return . lookup name . requestHeaders

-- | Redirect back to the referer. If the referer header is not present
-- redirect to root (i.e., @\/@).
redirectBack :: Monad m => ControllerT s m ()
redirectBack = redirectBackOr (redirectTo "/")

-- | Redirect back to the referer. If the referer header is not present
-- fallback on the given 'Response'.
redirectBackOr :: Monad m
               => Response -- ^ Fallback response
               -> ControllerT s m ()
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

guard :: Monad m => Bool -> ControllerT s m a -> ControllerT s m ()
guard b c = if b then c >> return () else return ()

guardM :: Monad m
       => ControllerT s m Bool -> ControllerT s m a -> ControllerT s m ()
guardM b c = b >>= flip guard c

guardReq :: Monad m
         => (Request -> Bool) -> ControllerT s m a -> ControllerT s m ()
guardReq f = guardM (liftM f request)

data ControllerException = ControllerException String
  deriving (Typeable)

instance Show ControllerException where
  show (ControllerException msg) = "ControllerT: " ++ msg

instance Exception ControllerException

err :: String -> ControllerT s m a
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
