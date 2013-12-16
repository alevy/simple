module Main where

import System.Environment
import Network.Wai.Handler.Warp

import Application

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
  app (run port)

