{-# LANGUAGE OverloadedStrings #-}
module Web.Simple.Templates.LanguageSpec where

import Data.Aeson
import Test.HUnit
import Test.Hspec
import Web.Simple.Templates.Language

spec :: Spec
spec = describe "Web.Simple.Templates.Language" $ do
  describe "valueToText" $ do
    it "renders whole number as decimal" $ do
      let val = Number 4563
      assertEqual "" "4563" (valueToText val)
    it "renders rational number with decimal point" $ do
      let val = Number 3432.5
      assertEqual "" "3432.5" (valueToText val)

