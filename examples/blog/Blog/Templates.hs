{-# LANGUAGE OverloadedStrings #-}
module Blog.Templates where

import Prelude hiding (head, div, id)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

adminTemplate :: Html -> Html
adminTemplate html = do
  div ! id "admin" $ do
    header ! class_ "banner" $ do
      h2 "Admin interface"
      ul ! class_ "nav" $ do
        li $ a ! href "/admin/posts" $ "List posts"
        li $ a ! href "/admin/posts/new" $ "New Post"
    html

