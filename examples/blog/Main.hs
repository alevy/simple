{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Simple
import System.Environment
import Network.Wai.Handler.Warp

import Blog.Controllers.PostsController

app runner = do
  runner $ mkRouter $ do
    routeName "posts" postsController
    routeAll $ okHtml "Hello World"

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
  putStrLn $ "Starting server on port " ++ (show port)
  app (run port)

