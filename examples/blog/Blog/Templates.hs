{-# LANGUAGE OverloadedStrings #-}
module Blog.Templates where

import Prelude hiding (head, div, id)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)

defaultTemplate :: Html -> Html
defaultTemplate html = docTypeHtml $ do
  head $ do
    title $ preEscapedToHtml ("Simple B&#955og" :: String)
    link ! rel "stylesheet" ! type_ "text/css" ! href "/css/style.css"
  body $ do
    header ! id "sidebar" $ do
      a ! href "/" $
        img ! src "/images/haskell-logoc.svg" ! class_ "logo"
      h1 $ a ! href "/" $
        preEscapedToHtml ("Simple B&#955og" :: String)
      h2 $ "the simple framework"
      h3 $ do
        "this blog demostrates the basic capabilities of the simple"
        " web framework."
      ul ! class_ "nav" $ do
        li $ a ! href "/" $ "Home"
        li $ a ! href "/admin" $ "Admin"
    section ! id "river" $ do
      html
    footer $ do
      "Made with the "
      a ! href "http://gitstar.com/alevy/simple" $ "simple"
      " web framework."

adminTemplate :: Html -> Html
adminTemplate html = defaultTemplate $ do
  div ! id "admin" $ do
    header ! class_ "banner" $ do
      h2 "Admin interface"
      ul ! class_ "nav" $ do
        li $ a ! href "/admin/posts" $ "List posts"
        li $ a ! href "/admin/posts/new" $ "New Post"
    html
