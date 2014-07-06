module Web.Simple.Controller.Exception where

import qualified Control.Exception as E
import           Control.Monad.Trans.Control
import           Web.Simple.Controller

onException :: Controller s a -> Controller s b -> Controller s a
onException act handler = control $ \runInM -> do
  runInM act `E.onException` runInM handler

finally :: Controller s a -> Controller s b -> Controller s a
finally act handler = do
  res <- (act `onException` handler)
  handler
  return res

bracket :: Controller s a -> (a -> Controller s b)
        -> (a -> Controller s c) -> Controller s c
bracket aquire release act = do
  a <- aquire
  act a `finally` release a

handle :: E.Exception e => (e -> Controller s a) -> Controller s a
       -> Controller s a
handle handler act = control $ \runInM -> do
  E.handle (runInM . handler) $ runInM act

