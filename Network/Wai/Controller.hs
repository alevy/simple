{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Network.Wai.Controller
  ( Controller
  , request
  , queryParam
  ) where

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as S8
import Data.Conduit
import Network.Wai
import Network.Wai.Router

data ControllerState = ControllerState { csRequest :: Request }

type Controller = ReaderT ControllerState (ResourceT IO)

instance Routeable (Controller Response) where
  runRoute controller req = fmap Just $
    runReaderT controller $ ControllerState req

request :: Controller Request
request = fmap csRequest ask

queryParam :: S8.ByteString -> Controller (Maybe S8.ByteString)
queryParam varName = do
  qr <- fmap queryString request
  case lookup varName qr of
    Just n -> return n
    _ -> return Nothing

