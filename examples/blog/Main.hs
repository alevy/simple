{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as S8 (pack)
import Database.PostgreSQL.Devel
import Database.PostgreSQL.Migrate
import Web.Simple
import Web.Simple.Auth
import Web.Simple.Cache
import Web.REST (restIndex, rest, routeREST)
import System.Environment
import System.INotify
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.MethodOverridePost
import Network.Wai.Middleware.RequestLogger
import System.FilePath
import System.IO

import Blog.Auth
import Blog.Common
import Blog.Controllers.CommentsController
import Blog.Controllers.PostsController

app runner = withINotify $ \inotify -> do
  env <- getEnvironment
  let adminUser = maybe "admin" S8.pack $ lookup "ADMIN_USERNAME" env
  let adminPassword = maybe "password" S8.pack $ lookup "ADMIN_PASSWORD" env

  let requireAuth = basicAuth "Simple Blog Admin" adminUser adminPassword
  
  settings <- newAppSettings inotify

  runner $ methodOverridePost $
    controllerApp settings $ do
      handleOpenId handleLogin      
      routeName "login" loginPage
      routePattern "logout" logout

      routePattern "admin" $ requiresAdmin "/login" $ do
        routeName "posts" $ do
          routePattern ":post_id/comments" $ commentsAdminController
          routeREST $ postsAdminController
        routeTop $ respond $ redirectTo "/admin/posts/"
      routeName "posts" $ do
        routePattern ":post_id/comments" $ commentsController
        routeREST $ postsController
      routeTop $ restIndex $ postsController
      fromApp $ staticPolicy (addBase "static") $ const $ return notFound

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
  let dev = maybe False (== "development") $ lookup "ENV" env
  let logger = if dev then logStdoutDev else logStdout
  when dev $ void $ do
    putStrLn "Starting dev database..."
    initLocalDB "db/development"
    startLocalDB "db/development"
    setLocalDB "db/development"
    initializeDb
    runMigrationsForDir stdout defaultMigrationsDir
  putStrLn $ "Starting server on port " ++ (show port)
  app (run port . logger)

