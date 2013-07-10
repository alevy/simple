{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    module Web.Simple.ControllerM
  -- * Controller
  , Controller(..)
  , controllerApp
  -- * ControllerR
  , ControllerR(..)
  , controllerRApp
  , controllerRValue
  , runControllerRIO
  ) where

import           Control.Applicative
import           Control.Exception.Peel
import           Control.Monad.IO.Peel
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Conduit
import           Network.Wai
import           Web.Simple.ControllerM
import           Web.Simple.Responses

-- | A basic Controller
newtype Controller a =
  Controller (EitherT Response (ReaderT Request (ResourceT IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Request)

instance ControllerM Controller where
  request = ask
  localRequest = local
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

-- | A controller that embeds an application-supplied value, which may
-- be extracted with 'controllerRValue'.
newtype ControllerR r a =
  ControllerR (EitherT Response (ReaderT (r,Request) (ResourceT IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (r,Request))

instance ControllerM (ControllerR r) where
  request = liftM snd ask
  localRequest f = local (\(r,req) -> (r, f req))
  pass = ControllerR $ right ()
  respond = ControllerR . left

instance MonadPeelIO (ControllerR r) where
  peelIO = do
    r <- controllerRValue
    req <- request
    return $ \ctrl -> do
      res <- runControllerRIO ctrl r req
      return $ ControllerR $ hoistEither res

-- | Extract the application-specific value
controllerRValue :: ControllerR r r 
controllerRValue = liftM fst ask

-- | Convert the controller into an 'Application'
controllerRApp :: ControllerR r a -> r -> Application
controllerRApp ctrl r req =
  runControllerR ctrl r req >>=
    either return (const $ return notFound) 

runControllerR :: ControllerR r a -> r -> Request -> ResourceT IO (Either Response a)
runControllerR (ControllerR m) r req = runReaderT (runEitherT m) (r,req)

-- | Run a 'ControllerR' in the @IO@ monad
runControllerRIO :: ControllerR r a -> r -> Request -> IO (Either Response a)
runControllerRIO ctrl r = runResourceT . runControllerR ctrl r

{-
ensure :: Controller a -> Controller b -> Controller b
ensure finalize act = do
  req <- request
  ea <- Controller $ lift . lift $ runController act req
  finalize
  Controller $ hoistEither ea
-}

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
