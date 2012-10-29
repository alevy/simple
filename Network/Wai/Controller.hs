{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

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

module Network.Wai.Controller
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
import Network.Wai.Responses
import Network.Wai.Router

data ControllerState = ControllerState { csRequest :: Request }

-- | A 'Controller' is a 'Reader' monad that contains the HTTP request in its
-- environment. A 'Controller' is 'Routeable' simply by running the 'Reader'.
type Controller = ReaderT ControllerState (ResourceT IO)

instance Routeable (Controller Response) where
  runRoute controller req = fmap Just $
    runReaderT controller $ ControllerState req

-- | Reads the underlying 'Request'
request :: Controller Request
request = fmap csRequest ask

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
    Just refr -> return $ redirectTo $ S8.unpack refr
    Nothing   -> return def

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
queryParam :: S8.ByteString -- ^ Parameter name
           -> Controller (Maybe S8.ByteString)
queryParam varName = do
  qr <- fmap queryString request
  case lookup varName qr of
    Just n -> return n
    _ -> return Nothing

-- | An alias for 'return' that's helps the the compiler type a code block as a
-- 'Controller'. For example, when using the 'Network.Wai.Frank' routing DSL to
-- define a simple route that justs returns a 'Response', 'respond' can be used
-- to avoid explicit typing of the argument:
--
-- @
--   get \"/\" $ do
--     someSideEffect
--     respond $ okHtml \"Hello World\"
-- @
--
-- instead of:
--
-- @
--   get \"/\" $ (do
--     someSideEffect
--     return $ okHtml \"Hello World\") :: Controller Response
-- @
respond :: Routeable r => r -> Controller r
respond = return

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
  lift $ bd $$ (CL.consume >>= return . L8.fromChunks)

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
  request >>= lift . (parseRequestBody tempFileBackEnd)

