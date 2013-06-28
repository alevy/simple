{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
  {-# LANGUAGE TypeSynonymInstances #-}
  {-# LANGUAGE FlexibleInstances #-}
  {-# LANGUAGE OverloadedStrings, OverlappingInstances, UndecidableInstances #-}
-}

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
  --  $Example
    module Web.Simple.ControllerM
  , ask, local
  , Controller
  , controllerApp
  , withApplicationValue
  , lookupControllerValue
  , parseForm
  -- * Low level functions
  , body
  ) where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Vault as Vault
import           Network.Wai
import           Network.Wai.Parse
import Web.Simple.ControllerM
import Web.Simple.Responses

newtype Controller a =
  Controller (EitherT Response (ReaderT Request (ResourceT IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Request)

instance ControllerM Controller where
  pass = Controller $ right ()
  respond = Controller . left

instance ToApplication (Controller a) where
  toApp = controllerApp

controllerApp :: Controller a -> Application
controllerApp ctrl req =
  runController ctrl req >>=
    either return (const $ return notFound) 

runController :: Controller a -> Request -> ResourceT IO (Either Response a)
runController (Controller m) req = runReaderT (runEitherT m) req

{-
runControllerIO :: Request -> Controller a -> IO (Either Response a)
runControllerIO ctrl = runResourceT . runController ctrl
-}

{-
ensure :: Controller a -> Controller b -> Controller b
ensure finalize act = do
  req <- request
  ea <- Controller $ lift . lift $ runController act req
  finalize
  Controller $ hoistEither ea
-}

{-
instance Monoid (Controller ()) where
  mempty = return ()
  mappend m1 m2 = m1 >> m2
-}

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

-- | Place an arbitrary value in an @Application@, to be extricated by 'lookupApplicationValue'
withApplicationValue :: Vault.Key a -> a -> Middleware
withApplicationValue key x app req =
  app $ req { vault = Vault.insert key x (vault req) }

-- | Extract an arbitrary value from an application
lookupApplicationValue :: Vault.Key a -> Request -> Maybe a
lookupApplicationValue key req = Vault.lookup key $ vault req

lookupControllerValue :: Vault.Key a -> Controller (Maybe a)
lookupControllerValue key = request >>= return . lookupApplicationValue key

-- | Reads and returns the body of the HTTP request.
body :: Controller L8.ByteString
body = do
  bd <- fmap requestBody request
  Controller $ lift . lift $ bd $$ (CL.consume >>= return . L8.fromChunks)

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
