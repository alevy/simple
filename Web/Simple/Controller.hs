{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

{- | 'Controller' provides a convenient syntax for writting 'Application'
  code as a Monadic action with access to an HTTP request, rather than a
  function that takes the request as an argument. This module also defines some
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
    Controller(..)
  , fromApp
  , controllerApp
  , appState
  , runControllerIO
  , request, respond, localRequest
  -- * Common Routes
  , routeHost, routeTop, routeMethod
  , routePattern, routeName, routeVar
  -- * Inspecting query
  , Parseable
  , queryParam, queryParam', queryParams
  , readQueryParam, readQueryParam', readQueryParams
  , parseForm
  -- * Redirection via referrer
  , redirectBack
  , redirectBackOr
  -- * Exception handling
  , ControllerException
  , module Control.Exception.Peel
  -- * Integrating other WAI components
  , ToApplication(..)
  -- * Low-level utilities
  , body
  -- , guard, guardM, guardReq
  ) where

import           Control.Applicative
import           Control.Exception.Peel
import           Control.Monad.IO.Peel
import           Control.Monad.Reader hiding (guard)
import           Control.Monad.Trans.Either
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Parse
import           Web.Simple.Responses

-- | A controller that embeds an application-supplied value, which may
-- be extracted with 'controllerRValue'.
newtype Controller r a =
  Controller (EitherT Response (ReaderT (r,Request) (ResourceT IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (r,Request))

instance MonadPeelIO (Controller r) where
  peelIO = do
    r <- appState
    req <- request
    return $ \ctrl -> do
      res <- runControllerIO ctrl r req
      return $ Controller $ hoistEither res

-- | Extract the request
request :: Controller r Request
request = liftM snd ask

-- | Modify the request for the given computation
localRequest :: (Request -> Request) -> Controller r a -> Controller r a
localRequest f = local (\(r,req) -> (r, f req))

-- | Extract the application-specific state
appState :: Controller r r 
appState = liftM fst ask

-- | Convert the controller into an 'Application'
controllerApp :: r -> Controller r a -> Application
controllerApp r ctrl req =
  runController ctrl r req >>=
    either return (const $ return notFound) 

runController :: Controller r a -> r -> Request -> ResourceT IO (Either Response a)
runController (Controller m) r req = runReaderT (runEitherT m) (r,req)

-- | Run a 'Controller' in the @IO@ monad
runControllerIO :: Controller r a -> r -> Request -> IO (Either Response a)
runControllerIO ctrl r = runResourceT . runController ctrl r

-- | Decline to handle the request
--
-- @pass >> c === c@
-- @c >> pass === c@
pass :: Controller r ()
pass = Controller $ right ()

-- | Provide a response
--
-- @respond r >>= f === respond r@
respond :: Response -> Controller r a
respond = Controller . left


-- | Lift an application to a controller
fromApp :: ToApplication a => a -> Controller r ()
fromApp app = do
  req <- request 
  resp <- liftIO $ runResourceT $ (toApp app) req
  respond resp

-- | Matches on the hostname from the 'Request'. The route only succeeds on
-- exact matches.
routeHost :: S.ByteString -> Controller r a -> Controller r ()
routeHost host = guardReq $ (host ==) . serverName

-- | Matches if the path is empty.
--
-- Note that this route checks that 'pathInfo'
-- is empty, so it works as expected in nested contexts that have
-- popped components from the 'pathInfo' list.
routeTop :: Controller r a -> Controller r ()
routeTop = guardReq $ \req -> null (pathInfo req) ||
                              (T.length . head $ pathInfo req) == 0

-- | Matches on the HTTP request method (e.g. 'GET', 'POST', 'PUT')
routeMethod :: StdMethod -> Controller r a -> Controller r ()
routeMethod method = guardReq $ (renderStdMethod method ==) . requestMethod

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
routePattern pattern route =
  let patternParts = map T.unpack $ decodePathSegments pattern
  in foldr mkRoute (route >> return ()) patternParts
  where mkRoute (':':varName) = routeVar (S8.pack varName)
        mkRoute varName = routeName (S8.pack varName)

-- | Matches if the first directory in the path matches the given 'ByteString'
routeName :: S.ByteString -> Controller r a -> Controller r ()
routeName name next = do
  req <- request
  if (length $ pathInfo req) > 0 && S8.unpack name == (T.unpack . head . pathInfo) req
    then localRequest popHdr next >> return ()
    else pass
  where popHdr req = req { pathInfo = (tail . pathInfo $ req) }

-- | Always matches if there is at least one directory in 'pathInfo' but and
-- adds a parameter to 'queryString' where the key is the first parameter and
-- the value is the directory consumed from the path.
routeVar :: S.ByteString -> Controller r a -> Controller r ()
routeVar varName next = do
  req <- request
  if (length $ pathInfo req) > 0
    then localRequest popHdr next >> return ()
    else pass
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
queryParam :: Parseable a
           => S8.ByteString -- ^ Parameter name
           -> Controller r (Maybe a)
queryParam varName = do
  qr <- liftM queryString request
  return $ case lookup varName qr of
    Just p -> Just $ parse $ fromMaybe S.empty p
    _ -> Nothing

-- | Like 'queryParam', but throws an exception if the parameter is not present.
queryParam' :: Parseable a
            => S.ByteString -> Controller r a
queryParam' varName =
  queryParam varName >>= maybe (err $ "no parameter " ++ show varName) return

-- | Selects all values with the given parameter name
queryParams :: Parseable a
            => S.ByteString -> Controller r [a]
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
readQueryParam :: Read a
               => S8.ByteString -- ^ Parameter name
               -> Controller r (Maybe a)
readQueryParam varName =
  queryParam varName >>= maybe (return Nothing) (liftM Just . readParamValue varName)

-- | Like 'readQueryParam', but throws an exception if the parameter is not present.
readQueryParam' :: Read a
                => S8.ByteString -- ^ Parameter name
                -> Controller r a
readQueryParam' varName =
  queryParam' varName >>= readParamValue varName

-- | Like 'queryParams', but further processes the parameter values with @read@.
-- If any read-conversion fails, an exception is thrown.
readQueryParams ::  Read a
                => S8.ByteString -- ^ Parameter name
                -> Controller r [a]
readQueryParams varName =
  queryParams varName >>= mapM (readParamValue varName)

readParamValue :: Read a => S8.ByteString -> Text -> Controller r a
readParamValue varName =
  maybe (err $ "cannot read parameter: " ++ show varName) return .
    readMay . T.unpack
  where readMay s = case [x | (x,rst) <- reads s, ("", "") <- lex rst] of
                      [x] -> Just x
                      _ -> Nothing

-- | Parses a HTML form from the request body. It returns a list of 'Param's as
-- well as a list of 'File's, which are pairs mapping the name of a /file/ form
-- field to a 'FileInfo' pointing to a temporary file with the contents of the
-- upload.
--
-- @
--   myController = do
--     (prms, files) <- parseForm
--     let mPicFile = lookup \"profile_pic\" files
--     case mPicFile of
--       Just (picFile) -> do
--         sourceFile (fileContent picFile) $$
--           sinkFile (\"images/\" ++ (fileName picFile))
--         respond $ redirectTo \"/\"
--       Nothing -> redirectBack
-- @
parseForm :: Controller r ([Param], [(S.ByteString, FileInfo FilePath)])
parseForm = request >>= liftIO . runResourceT . parseRequestBody tempFileBackEnd

-- | Reads and returns the body of the HTTP request.
body :: Controller r L8.ByteString
body = do
  bd <- liftM requestBody request
  liftIO $ runResourceT $ bd $$ (CL.consume >>= return . L8.fromChunks)

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
    Just refr -> respond $ redirectTo $ S8.unpack refr
    Nothing   -> respond def

-- guard

guard :: Bool -> Controller r a -> Controller r ()
guard b c = if b then c >> return () else pass

guardM :: Controller r Bool -> Controller r a -> Controller r ()
guardM b c = b >>= flip guard c

guardReq :: (Request -> Bool) -> Controller r a -> Controller r ()
guardReq f = guardM (liftM f request)

-- | The class of types that can be converted to an 'Application'
class ToApplication r where
  toApp :: r -> Application

instance ToApplication Application where
  toApp = id

instance ToApplication Response where
  toApp = const . return

data ControllerException = ControllerException String
  deriving (Typeable)

instance Show ControllerException where
  show (ControllerException msg) = "Controller: " ++ msg

instance Exception ControllerException

err :: String -> Controller r a
err = throwIO . ControllerException

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
