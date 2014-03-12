{-# LANGUAGE OverloadedStrings #-}

-- | Provides HTTP Basic Authentication.
module Web.Simple.Auth
  ( AuthRouter
  , basicAuthRoute, basicAuth, authRewriteReq
  ) where

import Control.Monad
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import Data.Maybe
import Network.HTTP.Types
import Network.Wai
import Web.Simple.Responses
import Web.Simple.Controller

-- | An 'AuthRouter' authenticates a 'Request' and, if successful, forwards the
-- 'Request' to the 'Routeable'.
type AuthRouter m r a = (Request -> S8.ByteString
                                 -> S8.ByteString
                                 -> Controller m r (Maybe Request))
                    -> Controller m r a
                    -> Controller m r a

-- | An 'AuthRouter' that uses HTTP basic authentication to authenticate a request
-- in a particular realm.
basicAuthRoute :: Monad m => String -> AuthRouter m r a
basicAuthRoute realm testAuth next = do
  req <- request
  let authStr = fromMaybe "" $ lookup hAuthorization (requestHeaders req)
  when (S8.take 5 authStr /= "Basic") requireAuth

  case fmap (S8.split ':') $ decode $ S8.drop 6 authStr of
    Right (user:pwd:[]) -> do
      mfin <- testAuth req user pwd
      maybe requireAuth (\finReq -> localRequest (const finReq) next) mfin
    _ -> requireAuth
  where requireAuth = respond $ requireBasicAuth realm

-- | Wraps an 'AuthRouter' to take a simpler authentication function (that just
-- just takes a username and password, and returns 'True' or 'False'). It also
-- adds an \"X-User\" header to the 'Request' with the authenticated user\'s
-- name (the first argument to the authentication function).
authRewriteReq :: Monad m
               => AuthRouter m r a
               -> (S8.ByteString -> S8.ByteString -> Controller m r Bool)
               -> Controller m r a
               -> Controller m r a
authRewriteReq authRouter testAuth rt =
  authRouter (\req user pwd -> do
    success <- testAuth user pwd
    if success then
      return $ Just $ transReq req user
      else return Nothing) rt
  where transReq req user = req
          { requestHeaders = ("X-User", user):(requestHeaders req)}

-- | A 'Route' that uses HTTP basic authentication to authenticate a request for a realm
-- with the given username ans password. The request is rewritten with an 'X-User' header
-- containing the authenticated username before being passed to the next 'Route'.
basicAuth :: Monad m
          => String
          -- ^ Realm
          -> S8.ByteString
          -- ^ Username
          -> S8.ByteString
          -- ^ Password
          -> Controller m r a -> Controller m r a
basicAuth realm user pwd = authRewriteReq (basicAuthRoute realm)
  (\u p -> return $ u == user && p == pwd)

