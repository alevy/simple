module Web.Simple.Controller.Exception where

import qualified Control.Exception as E
import           Control.Monad.Trans.Control
import           Web.Simple.Controller

onException :: Controller s a -> Controller s b -> Controller s a
onException act handler = control $ \runInM -> do
  runInM act `E.onException` runInM handler

finally :: Controller s a -> Controller s b -> Controller s a
finally act next = control $ \runInM -> E.mask $ \restore -> do
  r <- restore (runInM act) `E.onException` (runInM next)
  _ <- runInM next
  return r

bracket :: Controller s a -> (a -> Controller s b)
        -> (a -> Controller s c) -> Controller s c
bracket aquire release act = control $ \runInM -> E.mask $ \restore -> do
  let release' a = runInM $ restoreM a >>= release
  a <- runInM aquire
  r <- (restore $ runInM $ restoreM a >>= act) `E.onException` release' a
  _ <- release' a
  return r

handle :: E.Exception e => (e -> Controller s a) -> Controller s a
       -> Controller s a
handle handler act = control $ \runInM -> do
  E.handle (runInM . handler) $ runInM act

