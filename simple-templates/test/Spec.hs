{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import qualified Web.Simple.Templates.LanguageSpec

main :: IO ()
main = hspec $ do
  Web.Simple.Templates.LanguageSpec.spec

