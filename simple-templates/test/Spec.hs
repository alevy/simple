{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import qualified Web.Simple.Templates.ParserSpec

main :: IO ()
main = hspec $ do
  Web.Simple.Templates.ParserSpec.spec

