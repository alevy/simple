{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Network.Wai.Controller
  ( Controller
  , request
  , body
  , queryParam
  , parseForm
  , respond
  ) where

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit
import Data.Conduit.List as CL
import Network.Wai
import Network.Wai.Parse
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

respond :: Routeable r => r -> Controller r
respond = return

body :: Controller L8.ByteString
body = do
  bd <- fmap requestBody request
  lift $ bd $$ (CL.consume >>= return . L8.fromChunks)

parseForm :: Controller ([Param], [File FilePath])
parseForm = do
  request >>= lift . (parseRequestBody tempFileBackEnd)
