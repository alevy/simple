{-# LANGUAGE OverloadedStrings #-}
module Frank where

import Data.String
import Network.Wai.Handler.Warp
import Network.Wai
import Web.Simple
import Web.Simple.Auth
import Web.Frank

app :: Application
app = mkRouter $ do
  get "secret" $ basicAuth "Test App" "jdoe" "mypass" $ do
    respond $ okHtml $ "This page is very very secret..."
  get "/" $ do
    req <- request
    return $ okHtml $ fromString $ "Welcome to your Home " ++ (show $ serverName req)

runApp :: (Application -> IO ()) -> IO ()
runApp f = f app

main :: IO ()
main = runSettings defaultSettings app

