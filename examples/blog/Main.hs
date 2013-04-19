{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as S8 (pack)
import Web.Simple
import Web.Simple.Auth
import Web.REST (restIndex)
import System.Environment
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.MethodOverridePost

import Blog.Controllers.PostsController

app runner = do
  env <- getEnvironment
  let adminUser = maybe "admin" S8.pack $ lookup "ADMIN_USERNAME" env
  let adminPassword = maybe "password" S8.pack $ lookup "ADMIN_PASSWORD" env

  runner $ methodOverridePost . (staticPolicy $ addBase "static") $
    mkRouter $ do
      routeName "posts" postsController
      routeName "admin" $
        basicAuth "Simple Blog Admin" adminUser adminPassword
          postsAdminController
      routeTop $ restIndex postsController

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
  putStrLn $ "Starting server on port " ++ (show port)
  app (run port)

