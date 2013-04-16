{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Simple
import System.Environment

import Blog.Controllers.PostsController

app runner = do
  runner $ mkRouter $ do
    routeName "posts" postsController
    routeAll $ okHtml "Hello World"

