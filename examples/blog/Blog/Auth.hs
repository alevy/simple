{-# LANGUAGE OverloadedStrings #-}
module Blog.Auth where

import Prelude hiding (div)

import Blaze.ByteString.Builder (toByteString)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Maybe
import Network.HTTP.Types
import Network.HTTP.Conduit (withManager)
import Network.Wai
import Text.Blaze.Html5 (form, input, (!), h2, div)
import Text.Blaze.Html5.Attributes
  (type_, method, action, name, placeholder, value, class_)
import Web.Frank
import Web.Simple
import Web.Authenticate.OpenId

import Blog.Common

handleOpenId :: (T.Text -> Controller a ()) -> Controller a ()
handleOpenId loginHandler = do
  get "auth/finalize" $ do
    prms <- (map (\(k,(Just v)) -> (decodeUtf8 k, decodeUtf8 v)))
              <$> queryString <$> request
    oidr <- liftIO $ withManager $ authenticateClaimed prms
    case identifier <$> oirClaimed oidr of
      Just openid -> do
        loginHandler openid
      _ -> respond forbidden
  get "auth/login" $ do
    claimedId <- queryParam' "openid_identifier"
    let completePage = "http://localhost:5000/auth/finalize"
    fu <- liftIO $ withManager $ getForwardUrl claimedId
                    completePage Nothing []
    respond $ redirectTo $ encodeUtf8 fu

handleLogin :: T.Text -> Controller AppSettings ()
handleLogin openid = do
  ret <- fromMaybe "/" `fmap` sessionLookup "return_to"
  sessionDelete "return_to"
  sessionInsert "user" $ encodeUtf8 openid
  respond $ redirectTo ret

logout :: Controller AppSettings ()
logout = do
  sessionDelete "user"
  respond $ redirectTo "/"

requiresAdmin :: S8.ByteString
              -> Controller AppSettings b -> Controller AppSettings b
requiresAdmin loginUrl cnt = do
  muser <- sessionLookup "user"
  if isJust muser then
    cnt
    else do
      req <- request
      sessionInsert "return_to" $ rawPathInfo req
      respond $ redirectTo loginUrl

loginPage :: Controller AppSettings ()
loginPage = respondTemplate $ do
  div ! class_ "login_form" $ do
    h2 "Login with OpenId"
    form ! action "/auth/login" ! method "GET" $ do
      input ! type_ "text" ! name "openid_identifier"
            ! placeholder "http://"
      input ! type_ "submit" ! value "Login"

