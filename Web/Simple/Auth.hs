{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Web.Simple.Auth where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Types
import Network.Wai
import Web.Simple.Responses
import Web.Simple.Router

-- | An 'AuthRouter' authenticates a 'Request' and, if successful, forwards the
-- 'Request' to the 'Routeable'.
type AuthRouter r = Routeable r
                  => (Request -> S8.ByteString
                              -> S8.ByteString
                              -> IO (Maybe Request))
                  -> r
                  -> Route ()

-- | An 'AuthRouter' that uses HTTP basic authentication to authenticate a request
-- in a particular realm.
basicAuthRoute :: String -> AuthRouter r
basicAuthRoute realm testAuth rt = Route (\req -> do
  didAuthenticate <-
    case lookup hAuthorization (requestHeaders req) of
      Nothing -> return Nothing
      Just authStr
              | S8.take 5 authStr /= "Basic" -> return Nothing
              | otherwise -> do
                    let up = fmap (S8.split ':') $ decode $ S8.drop 6 authStr
                    case up of
                      Right (user:pass:[]) -> liftIO $ testAuth req user pass
                      _ -> return Nothing
  case didAuthenticate of
    Nothing -> return $ Just $ requireBasicAuth realm
    Just finReq -> runRoute rt finReq
  ) ()

-- | Wraps an 'AuthRouter' to take a simpler authentication function (that just
-- just takes a username and password, and returns 'True' or 'False'). It also
-- adds an \"X-User\" header to the 'Request' with the authenticated user\'s
-- name (the first argument to the authentication function).
authRewriteReq :: Routeable r
                    => AuthRouter r
                    -> (S8.ByteString -> S8.ByteString -> IO Bool)
                    -> r
                    -> Route ()
authRewriteReq authRouter testAuth rt =
  authRouter (\req user pwd -> do
    success <- testAuth user pwd
    if success then
      return $ Just $ transReq req user
      else return Nothing) rt
  where transReq req user = req { requestHeaders = ("X-User", user):(requestHeaders req)}

-- | A 'Route' that uses HTTP basic authentication to authenticate a request for a realm
-- with the given username ans password. The request is rewritten with an 'X-User' header
-- containing the authenticated username before being passed to the next 'Route'.
basicAuth :: Routeable r
          => String
          -- ^ Realm
          -> S8.ByteString
          -- ^ Username
          -> S8.ByteString
          -- ^ Password
          -> r -> Route ()
basicAuth realm user pass = authRewriteReq (basicAuthRoute realm)
  (\u p -> return $ u == user && p == pass)

