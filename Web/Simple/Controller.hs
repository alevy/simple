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
  , queryParam
  , parseForm
  , respond
  -- * Low level functions
  , request
  , body
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit
import Data.Conduit.List as CL
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Parse
import Web.Simple.Responses
import Web.Simple.Router

type Controller = Route

-- | Redirect back to the referer. If the referer header is not present
-- redirect to root (i.e., @\/@).
redirectBack :: Controller Response
redirectBack = redirectBackOr (redirectTo "/")

-- | Redirect back to the referer. If the referer header is not present
-- fallback on the given 'Response'.
redirectBackOr :: Response -- ^ Fallback 'Response'
               -> Controller Response
redirectBackOr def = do
  mrefr <- requestHeader "referer"
  case mrefr of
    Just refr -> respond $ redirectTo $ S8.unpack refr
    Nothing   -> respond def

-- | Looks up the parameter name in the request's query string and returns the
-- value as a 'S8.ByteString' or 'Nothing'.
--
-- For example, for a request with query string: \"?foo=bar&baz=7\",
-- @
--   queryParam \"foo\"
-- @
--
-- would return /Just "bar"/, but
--
-- @
--   queryParam \"zap\"
-- @
--
-- would return /Nothing/
queryParam :: Parseable a => S8.ByteString -- ^ Parameter name
           -> Controller (Maybe a)
queryParam varName = do
  qr <- fmap queryString request
  case lookup varName qr of
    Just p -> return $ fmap parse p
    _ -> return Nothing

class Parseable a where
  parse :: S8.ByteString -> a

instance Parseable S8.ByteString where
  parse = id
instance Parseable String where
  parse = S8.unpack
instance Read a => Parseable a where
  parse = read . S8.unpack


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
  Route $ lift . lift $ bd $$ (CL.consume >>= return . L8.fromChunks)

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
  request >>= Route . lift . lift . (parseRequestBody tempFileBackEnd)

