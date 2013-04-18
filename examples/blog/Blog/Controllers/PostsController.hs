{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PostsController where

import Prelude hiding (show)

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as S8
import Web.Simple
import Web.REST
import qualified Database.PostgreSQL.Connection as DB
import Database.PostgreSQL.Models

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Blog.Models.Post as P
import qualified Blog.Views.Posts as V
import Blog.Templates

postsController = do
  
  index $ DB.withConnection $ \conn -> do
    posts <- liftIO $ findAll P.posts conn
    respond $ okHtml $ renderHtml $ defaultTemplate $ V.index posts

  show $ DB.withConnection $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- liftIO $ find P.posts pid conn
    respond $ okHtml $ renderHtml $ defaultTemplate $ V.show post

  new $ do
    respond $ okHtml $ renderHtml $ defaultTemplate V.new

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

