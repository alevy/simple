{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.String
import Network.Wai.Handler.Warp
import Network.Wai
import Network.Wai.Router
import Network.Wai.Responses
import Network.Wai.Frank
import Network.Wai.Controller

main :: IO ()
main = runSettings defaultSettings $ mkRouter $ do
  get "/" $ do
    req <- request
    return $ okHtml $ fromString $ "Welcome Home " ++ (show $ serverName req)

