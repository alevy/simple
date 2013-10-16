{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Database.PostgreSQL.Devel
import Database.PostgreSQL.Migrate
import Web.Simple
import Web.Simple.Session
import Web.REST (restIndex, routeREST)
import System.Environment
import System.INotify
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.MethodOverridePost
import Network.Wai.Middleware.RequestLogger
import System.IO

import Blog.Auth
import Blog.Common
import Blog.Controllers.CommentsController
import Blog.Controllers.PostsController

app :: (Application -> IO ()) -> IO ()
app runner = withINotify $ \inotify -> do
  settings <- newAppSettings inotify

  runner $ methodOverridePost $
    controllerApp settings $ withSession $ do
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

