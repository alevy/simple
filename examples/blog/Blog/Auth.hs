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
import Web.Cookie
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
      Just openid -> loginHandler openid
      _ -> respond forbidden
  get "auth/login" $ do
    claimedId <- queryParam' "openid_identifier"
    let completePage = "http://localhost:5000/auth/finalize"
    fu <- liftIO $ withManager $ getForwardUrl claimedId
                    completePage Nothing []
    respond $ redirectTo $ encodeUtf8 fu

handleLogin :: T.Text -> Controller a ()
handleLogin openid = do
  cookies <- (maybe [] parseCookies) <$> requestHeader "Cookie"
  let ret = fromMaybe "/" $ lookup "return_to" cookies
      userCookie = def
                     { setCookieName = "user"
                     , setCookiePath = Just "/"
                     , setCookieValue = encodeUtf8 openid }
      delRetCookie = def
                       { setCookieName = "return_to"
                       , setCookiePath = Just "/"
                       , setCookieValue = ""
                       , setCookieMaxAge = Just 0}
  respond $ addCookie userCookie $ addCookie delRetCookie $ redirectTo ret

logout :: Controller a ()
logout = do
    let cookie =  def
                  { setCookieName = "user"
                  , setCookiePath = Just "/"
                  , setCookieValue = ""
                  , setCookieMaxAge = Just 0}
    respond $ addCookie cookie $ redirectTo "/"

addCookie :: SetCookie -> Response -> Response
addCookie cookie resp =
  let (stat, hdrs, src) = responseSource resp
      hd = ("Set-Cookie", toByteString . renderSetCookie $ cookie)
  in ResponseSource stat (hd:hdrs) src

requiresAdmin :: S8.ByteString -> Controller a b -> Controller a b
requiresAdmin loginUrl cnt = do
  cookies <- (maybe [] parseCookies) <$> requestHeader "Cookie"
  if (isJust $ lookup "user" cookies) then
    cnt
    else do
      req <- request
      let cookie = def
                     { setCookieName = "return_to"
                     , setCookiePath = Just "/"
                     , setCookieValue = rawPathInfo req }
      respond $
        addCookie cookie $
        redirectTo loginUrl

loginPage :: Controller AppSettings ()
loginPage = respondTemplate $ do
  div ! class_ "login_form" $ do
    h2 "Login with OpenId"
    form ! action "/auth/login" ! method "GET" $ do
      input ! type_ "text" ! name "openid_identifier"
            ! placeholder "http://"
      input ! type_ "submit" ! value "Login"

