{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import qualified Web.Simple.Templates.LanguageSpec
import qualified Web.Simple.Templates.ParserSpec

main :: IO ()
main = hspec $ do
  Web.Simple.Templates.LanguageSpec.spec
  Web.Simple.Templates.ParserSpec.spec

