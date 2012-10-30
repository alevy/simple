{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.String
import Network.Wai.Handler.Warp
import Network.Wai
import Web.Simple
import Web.Frank

main :: IO ()
main = runSettings defaultSettings $ mkRouter $ do
  get "/" $ do
    req <- request
    return $ okHtml $ fromString $ "Welcome Home " ++ (show $ serverName req)

