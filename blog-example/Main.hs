module Main where

import System.Environment
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

import Application

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
  app (run port . logStdout)

