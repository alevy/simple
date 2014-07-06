{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Typeable

data Request = Request
data Response = Response

data Rejected = Fallthrough | HttpError String
              deriving (Show, Typeable)

newtype ControllerT s m a =
  ControllerT { unControllerT ::
     Request -> s
     -> (Either Rejected Response -> m (Either Rejected Response))
     -> (a -> s -> m (Either Rejected Response))
     -> m (Either Rejected Response)
     }

runControllerT :: Monad m => (ControllerT s m ()) -> s -> Request -> m (Either Rejected Response)
runControllerT (ControllerT ca) s req = ca req s return (\_ _ -> return $ Left Fallthrough)

wrap :: (m (Either Rejected Response) -> m (Either Rejected Response))
        -> ControllerT s m a -> ControllerT s m a
wrap f (ControllerT ca) = ControllerT $ \req s kf ks ->
  f $ ca req s kf ks

instance Monad (ControllerT s m) where
  return a = ControllerT $ \req s _ ks -> ks a s
  (ControllerT ca) >>= f = ControllerT $ \req s kf ks ->
    ca req s kf (\a s' -> unControllerT (f a) req s' kf ks)
  fail _ = ControllerT $ \_ _ kf _ -> kf (Left Fallthrough)

instance MonadPlus (ControllerT s m) where
  mzero = fail "mzero"
  mplus (ControllerT ca) (ControllerT cb) =
    ControllerT $ \req s kf ks ->
    let dodone (Left Fallthrough) = cb req s kf ks
        dodone notfalthrough = kf notfalthrough
    in ca req s dodone ks

instance Functor (ControllerT s m) where
  fmap = liftM

instance Applicative (ControllerT s m) where
  pure = return
  (<*>) = ap

instance Alternative (ControllerT s m) where
  empty = mzero
  (<|>) = mplus
