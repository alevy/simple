{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Peel
import Control.Monad.Trans
import Test.Hspec
import Test.Hspec.HUnit
import Network.Wai
import Web.Simple.Controller.Trans
import Web.Simple.Responses

main :: IO ()
main = hspec $ do
  describe "ControllerT#routeName" $ do
    it "matches route when name is correct" $ do
      let ctrl =  do
            routeName "hello" $ respond $ okHtml ""
            lift $ expectationFailure "Path should have matched"
      controllerApp () ctrl $ defaultRequest { pathInfo = ["hello"] }
      return ()
    it "doesn't match route when name is incorrect" $ do
      let ctrl =  do
            routeName "yello" $
              lift $ expectationFailure "Path should have matched"
      controllerApp () ctrl $ defaultRequest { pathInfo = ["hello"] }
      return ()
    it "doesn't match route when path is empty" $ do
      let ctrl =  do
            routeName "yello" $
              lift $ expectationFailure "Path should have matched"
      controllerApp () ctrl $ defaultRequest { pathInfo = [] }
      return ()
    it "pops one directory from pathInfo when inside block" $ do
      let ctrl =  do
            routeName "hello" $ do
              pi <- pathInfo `fmap` request
              lift $ pi `shouldBe` ["world"]
            pi <- pathInfo `fmap` request
            lift $ pi `shouldBe` ["hello", "world"]
      controllerApp () ctrl $
        defaultRequest { pathInfo = ["hello", "world"] }
      return ()
  describe "ControllerT#routeVar" $ do
    it "matches route if pathInfo not empty" $ do
      let ctrl =  do
            routeVar "hello" $ respond $ okHtml ""
            lift $ expectationFailure "Path should have matched"
      controllerApp () ctrl $ defaultRequest { pathInfo = ["blarg"] }
      return ()
    it "doesn't match route when path is empty" $ do
      let ctrl =  do
            routeVar "yello" $
              lift $ expectationFailure "Path should have matched"
      controllerApp () ctrl $ defaultRequest { pathInfo = [] }
      return ()
    it "queues value of first path directory in query param" $ do
      let ctrl =  do
            routeVar "foo" $ do
              qs <- queryParam "foo"
              lift $ qs `shouldBe` Just ("hello" :: String)
      controllerApp () ctrl $
        defaultRequest { pathInfo = ["hello", "world"] }
      return ()
    it "pops one directory from pathInfo when inside block" $ do
      let ctrl =  do
            routeVar "foo" $ do
              pi <- pathInfo `fmap` request
              lift $ pi `shouldBe` ["world"]
            pi <- pathInfo `fmap` request
            lift $ pi `shouldBe` ["hello", "world"]
      controllerApp () ctrl $
        defaultRequest { pathInfo = ["hello", "world"] }
      return ()
  describe "ControllerT#routeTop" $ do
    it "matches when path is empty" $ do
      let ctrl =  do
            routeTop $ respond $ okHtml "Yey!"
            lift $ expectationFailure "Top should have matched"
      controllerApp () ctrl $
        defaultRequest
      return ()
    it "fails when path is not empty" $ do
      let ctrl =  do
            routeTop $ lift $ expectationFailure "Top should not have matched"
      controllerApp () ctrl $
        defaultRequest { pathInfo = ["blah"] }
      return ()
  describe "ControllerT#routeHost" $ do
    it "matches when host header is the same" $ do
      let ctrl =  do
            routeHost "www.example.com" $ respond $ okHtml "Yey!"
            lift $ expectationFailure "Host should have matched"
      controllerApp () ctrl $
        defaultRequest { requestHeaderHost = Just "www.example.com" }
      return ()
    it "fails when host header is not the same" $ do
      let ctrl =  do
            routeHost "www.example2.com" $ do
              lift $ expectationFailure "Host should not have matched"
      controllerApp () ctrl $
        defaultRequest { requestHeaderHost = Just "www.example.com" }
      return ()
    it "fails when host header is not present" $ do
      let ctrl =  do
            routeHost "www.example.com" $ do
              lift $ expectationFailure "Host should not have matched"
      controllerApp () ctrl $
        defaultRequest { requestHeaderHost = Nothing }
      return ()

  describe "MonadPeelIO instance" $ do
    it "Preserves state changes in inner block" $ do
      let expected = 1234
          ctrl = do
                  k <- peelIO
                  join $ liftIO $ k $ do
                    putState expected
      s <- snd `fmap` runController ctrl 0 defaultRequest
      s `shouldBe` expected

