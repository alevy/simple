{-# LANGUAGE OverloadedStrings #-}
module Blog.Views.Posts where

import Prelude hiding (id, div, show)
import qualified Prelude as Pre

import Control.Monad
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (form, label)

import qualified Blog.Models.Post as P

index :: [P.Post] -> Html
index posts = do
  forM_ posts $ \post -> do
    let (Just pid) = P.postId post
    let postUrl = "/posts/" ++ (Pre.show pid)
    div ! class_ "post" $ do
      h2 $ a ! href (toValue postUrl) $ toHtml $ P.title post
      p $ toHtml $ P.body post

show :: P.Post -> Html
show post = do
  let (Just pid) = P.postId post
  let editUrl = "/posts/" ++ (Pre.show pid) ++ "/edit"
  div ! class_ "post" $ do
    h2 $ toHtml $ P.title post
    p $ a ! href (toValue editUrl) $ "edit"
    p $ toHtml $ P.body post

new :: Html
new =  do
  form ! action "/posts/" ! method "post" $ do
    p $ do
      label ! for "title" $ "Title:"
      br
      input ! type_ "text" ! name "title" ! id "title"
    p $ do
      label ! for "body" $ "Body:"
      br
      textarea ! name "body" ! id "body "$ ""
    input ! type_ "submit" ! value "Submit"

