{-# LANGUAGE OverloadedStrings #-}
module Blog.Templates where

import Prelude hiding (head, div, id)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)

defaultTemplate :: Html -> Html
defaultTemplate html = docTypeHtml $ do
  head $ do
    title $ "My Blog"
    link ! rel "stylesheet" ! type_ "text/css" ! href "/css/style.css"
  body $ do
    div ! id "header" $ do
      h1 $ "My Blog"
    div ! id "content" $ do
      html

