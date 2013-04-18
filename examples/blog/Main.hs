{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Simple
import System.Environment
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp

import Blog.Controllers.PostsController

app runner = do
  runner $ mkRouter $ do
    routeName "posts" postsController
    routeTop $ redirectTo "/posts"
    routeAll $ staticApp $ defaultFileServerSettings "static"

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
  putStrLn $ "Starting server on port " ++ (show port)
  app (run port)

