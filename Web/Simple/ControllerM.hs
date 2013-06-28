{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A class of web controllers, and operations on them.
 -
 - Simple instances include 'Controller' and 'ControllerR'.
-}


module Web.Simple.ControllerM
  (
  -- * ControllerM Monads
    ControllerM(..)
  , fromApp
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
  -- * Integrating other WAI components
  , ToApplication(..)
  -- * Low-level utilities
  , body
  -- , guard, guardM, guardReq
  ) where

import           Control.Monad.Reader hiding (guard)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Parse
import           Web.Simple.Responses

-- | The class of monads with "Controller" behavior
class (MonadIO m) => ControllerM m where
  -- | Extract the request
  request :: m Request

  -- | Modify the request for the given computation
  localRequest :: (Request -> Request) -> m a -> m a

  -- | Decline to handle the request
  --
  -- Must obey:
  -- @pass >> c === c@
  -- @c >> pass === c@
  pass :: m ()

  -- | Provide a response
  --
  -- Must obey: @respond r >>= f === respond r@
  respond :: Response -> m a

-- | Lift an application to a controller
fromApp :: (ControllerM m, ToApplication a) => a -> m ()
fromApp app = do
  req <- request 
  resp <- liftIO $ runResourceT $ (toApp app) req
  respond resp

-- | Matches on the hostname from the 'Request'. The route only succeeds on
-- exact matches.
routeHost :: (ControllerM m, S.ByteString) => m a -> m ()
routeHost host = guardReq $ (host ==) . serverName

-- | Matches if the path is empty.
--
-- Note that this route checks that 'pathInfo'
-- is empty, so it works as expected in nested contexts that have
-- popped components from the 'pathInfo' list.
routeTop :: (ControllerM m) => m a -> m ()
routeTop = guardReq $ \req -> null (pathInfo req) ||
                              (T.length . head $ pathInfo req) == 0

-- | Matches on the HTTP request method (e.g. 'GET', 'POST', 'PUT')
routeMethod :: (ControllerM m) => StdMethod -> m a -> m ()
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
routePattern :: (ControllerM m) => S.ByteString -> m a -> m ()
routePattern pattern route =
  let patternParts = map T.unpack $ decodePathSegments pattern
  in foldr mkRoute (route >> return ()) patternParts
  where mkRoute (':':varName) = routeVar (S8.pack varName)
        mkRoute varName = routeName (S8.pack varName)

-- | Matches if the first directory in the path matches the given 'ByteString'
routeName :: (ControllerM m) => S.ByteString -> m a -> m ()
routeName name next = do
  req <- request
  if (length $ pathInfo req) > 0 && S8.unpack name == (T.unpack . head . pathInfo) req
    then localRequest popHdr next >> return ()
    else pass
  where popHdr req = req { pathInfo = (tail . pathInfo $ req) }

-- | Always matches if there is at least one directory in 'pathInfo' but and
-- adds a parameter to 'queryString' where the key is the first parameter and
-- the value is the directory consumed from the path.
routeVar :: (ControllerM m) => S.ByteString -> m a -> m ()
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
queryParam :: (ControllerM m, Parseable a)
           => S8.ByteString -- ^ Parameter name
           -> m (Maybe a)
queryParam varName = do
  qr <- liftM queryString request
  return $ case lookup varName qr of
    Just p -> Just $ parse $ fromMaybe S.empty p
    _ -> Nothing

-- | Like 'queryParam', but throws an exception if the parameter is not present.
queryParam' :: (ControllerM m, Parseable a)
            => S.ByteString -> m a
queryParam' varName =
  queryParam varName >>= maybe (fail $ "no parameter " ++ show varName) return

-- | Selects all values with the given parameter name
queryParams :: (ControllerM m, Parseable a)
            => S.ByteString -> m [a]
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
readQueryParam :: (ControllerM m, Read a)
               => S8.ByteString -- ^ Parameter name
               -> m (Maybe a)
readQueryParam varName =
  queryParam varName >>= maybe (return Nothing) (liftM Just . readParamValue varName)

-- | Like 'readQueryParam', but throws an exception if the parameter is not present.
readQueryParam' :: (ControllerM m, Read a)
                => S8.ByteString -- ^ Parameter name
                -> m a
readQueryParam' varName =
  queryParam' varName >>= readParamValue varName

-- | Like 'queryParams', but further processes the parameter values with @read@.
-- If any read-conversion fails, an exception is thrown.
readQueryParams :: (ControllerM m, Read a)
                => S8.ByteString -- ^ Parameter name
                -> m [a]
readQueryParams varName =
  queryParams varName >>= mapM (readParamValue varName)

readParamValue :: (ControllerM m, Read a) => S8.ByteString -> Text -> m a
readParamValue varName =
  maybe (fail $ "cannot read parameter: " ++ show varName) return .
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
parseForm :: (ControllerM m) => m ([Param], [(S.ByteString, FileInfo FilePath)])
parseForm = request >>= liftIO . runResourceT . parseRequestBody tempFileBackEnd

-- | Reads and returns the body of the HTTP request.
body :: (ControllerM m) => m L8.ByteString
body = do
  bd <- liftM requestBody request
  liftIO $ runResourceT $ bd $$ (CL.consume >>= return . L8.fromChunks)

-- | Returns the value of the given request header or 'Nothing' if it is not
-- present in the HTTP request.
requestHeader :: (ControllerM m) => HeaderName -> m (Maybe S8.ByteString)
requestHeader name = request >>= return . lookup name . requestHeaders

-- | Redirect back to the referer. If the referer header is not present
-- redirect to root (i.e., @\/@).
redirectBack :: (ControllerM m) => m ()
redirectBack = redirectBackOr (redirectTo "/")

-- | Redirect back to the referer. If the referer header is not present
-- fallback on the given 'Response'.
redirectBackOr :: (ControllerM m)
               => Response -- ^ Fallback response
               -> m ()
redirectBackOr def = do
  mrefr <- requestHeader "referer"
  case mrefr of
    Just refr -> respond $ redirectTo $ S8.unpack refr
    Nothing   -> respond def

-- guard

guard :: (ControllerM m) => Bool -> m a -> m ()
guard b c = if b then c >> return () else pass

guardM :: (ControllerM m) => m Bool -> m a -> m ()
guardM b c = b >>= flip guard c

guardReq :: (ControllerM m) => (Request -> Bool) -> m a -> m ()
guardReq f = guardM (liftM f request)

-- | The class of types that can be converted to an 'Application'
class ToApplication r where
  toApp :: r -> Application

instance ToApplication Application where
  toApp = id

instance ToApplication Response where
  toApp = const . return

{-
class ToController a m where
  toCtrl :: (ControllerM m) => a -> m ()

instance ToController Application m where
  toCtrl = fromApp

instance ToController Response where
  toCtrl = respond

instance (ControllerM m) => ToController (m a) where
  toCtrl c = c >> return ()
-}
