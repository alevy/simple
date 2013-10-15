{-# LANGUAGE OverloadedStrings #-}
module Blog.Views.Comments where

import Prelude hiding (id, div, show, span)
import qualified Prelude as Pre

import Control.Monad
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (form, label, cite, span)
import Network.Gravatar

import qualified Blog.Models.Comment as C
import qualified Blog.Models.Post as P

listComments :: P.Post -> [C.Comment] -> Html
listComments post comments = do
  header $
    h2 $ do
      "Comments: "
      em $ toHtml $ P.title post
  div ! class_ "content" $ do
    ol ! id "comments" $ forM_ comments $ \comment -> do
      let delCommentUrl = "/admin/posts/" ++
                          (Pre.show $ P.postId post) ++
                          "/comments/" ++
                          (Pre.show $ C.commentId comment)
      li $ do
        renderComment comment
        form ! action (toValue delCommentUrl) ! method "POST" $ do
          input ! type_ "hidden" ! name "_method" ! value "DELETE"
          p $ input ! type_ "submit" ! value "Delete"

renderComment :: C.Comment -> Html
renderComment comment = do
  div ! class_ "comment" $ do
    div ! class_ "comment-author vcard" $ do
      img ! src (toValue $
                  gravatar (def {gSize = Just $ Size 48}) $ C.email comment)
          ! name (toValue $ C.name comment)
      cite ! class_ "fn" $ toHtml $ C.name comment
      " "
      span ! class_ "says" $ "says:"
    p $ toHtml $ C.comment comment
  hr

