{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PostsController where

import Prelude hiding (id)

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as S8
import Web.Simple
import Web.REST
import qualified Blog.Models.Post as P
import qualified Database.Connection as DB
import Database.PostgreSQL.Models

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (form, label)
import Text.Blaze.Html.Renderer.Utf8

postsController = do
  
  index $ DB.withConnection $ \conn -> do
    posts <- liftIO $ findAll P.posts conn
    respond $ okHtml $ renderHtml $ do
      forM_ posts $ \post -> do
        h1 $ toHtml $ P.title post
        p $ toHtml $ P.body post

  new $ do
    respond $ okHtml $ renderHtml $ do
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

  create $ DB.withConnection $ \conn -> do
    (params, _) <- parseForm
    let mpost = do
          pTitle <- lookup "title" params
          pBody <- lookup "body" params
          return $ P.Post Nothing (fromString $ S8.unpack pTitle)
                                  (fromString $ S8.unpack pBody)
    case mpost of
      Just post -> do
        liftIO $ insert post conn
        respond $ redirectTo "/posts/"
      Nothing -> redirectBack

