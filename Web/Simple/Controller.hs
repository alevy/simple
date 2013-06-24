{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, OverlappingInstances, UndecidableInstances #-}

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
  ( Controller
  -- * Utility functions
  , redirectBack
  , redirectBackOr
  , Parseable
  , queryParam, queryParam', queryParams
  , readQueryParam, readQueryParam', readQueryParams
  , parseForm
  , respond
  -- * Low level functions
  , request
  , body
  ) where

import Control.Monad.Trans.Class
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Parse
import Data.Text (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import Web.Simple.Responses
import Web.Simple.Router

-- | Redirect back to the referer. If the referer header is not present
-- redirect to root (i.e., @\/@).
redirectBack :: Controller a
redirectBack = redirectBackOr (redirectTo "/")

-- | Redirect back to the referer. If the referer header is not present
-- fallback on the given 'Response'.
redirectBackOr :: Response -- ^ Fallback 'Response'
               -> Controller a
redirectBackOr def = do
  mrefr <- requestHeader "referer"
  case mrefr of
    Just refr -> respond $ redirectTo $ S8.unpack refr
    Nothing   -> respond def

-- | Looks up the parameter name in the request's query string and returns the
-- @Parseable@ value or 'Nothing'.
--
-- For example, for a request with query string: \"?foo=bar&baz=7\",
-- @queryParam \"foo\"@
-- would return @Just "bar"@, but
-- @queryParam \"zap\"@
-- would return @Nothing@.
queryParam :: Parseable a => S8.ByteString -- ^ Parameter name
           -> Controller (Maybe a)
queryParam varName = do
  qr <- fmap queryString request
  return $ case lookup varName qr of
    Just p -> Just $ parse $ fromMaybe S.empty p
    _ -> Nothing

-- | Like 'queryParam', but throws an exception if the parameter is not present.
queryParam' :: Parseable a => S.ByteString -> Controller a
queryParam' varName =
  queryParam varName >>= maybe (fail $ "no parameter " ++ show varName) return

-- | Selects all values with the given parameter name
queryParams :: Parseable a => S.ByteString -> Controller [a]
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
  parse = Text.decodeUtf8

-- | Like 'queryParam', but further processes the parameter value with @read@.
-- If that conversion fails, an exception is thrown.
readQueryParam :: Read a
               => S8.ByteString -- ^ Parameter name
               -> Controller (Maybe a)
readQueryParam varName =
  queryParam varName >>= maybe (return Nothing) (fmap Just . readParamValue varName)

-- | Like 'readQueryParam', but throws an exception if the parameter is not present.
readQueryParam' :: Read a
                => S8.ByteString -- ^ Parameter name
                -> Controller a
readQueryParam' varName =
  queryParam' varName >>= readParamValue varName

-- | Like 'queryParams', but further processes the parameter values with @read@.
-- If any read-conversion fails, an exception is thrown.
readQueryParams :: Read a
                => S8.ByteString -- ^ Parameter name
                -> Controller [a]
readQueryParams varName =
  queryParams varName >>= mapM (readParamValue varName)

readParamValue :: (Read a, Monad m)
               => S8.ByteString -> Text -> m a
readParamValue varName =
  maybe (fail $ "cannot read parameter: " ++ show varName) return .
    readMay . Text.unpack
  where readMay s = case [x | (x,rst) <- reads s, ("", "") <- lex rst] of
                      [x] -> Just x
                      _ -> Nothing

-- | Returns the value of the given request header or 'Nothing' if it is not
-- present in the HTTP request.
requestHeader :: HeaderName -> Controller (Maybe S8.ByteString)
requestHeader name = do
  req <- request
  return $ lookup name $ requestHeaders req

-- | Reads and returns the body of the HTTP request.
body :: Controller L8.ByteString
body = do
  bd <- fmap requestBody request
  Controller $ lift . lift $ bd $$ (CL.consume >>= return . L8.fromChunks)

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
parseForm :: Controller ([Param], [(S.ByteString, FileInfo FilePath)])
parseForm = do
  request >>= Controller . lift . lift . (parseRequestBody tempFileBackEnd)

