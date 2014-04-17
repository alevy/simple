{-# LANGUAGE OverloadedStrings #-}
module Main where

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

