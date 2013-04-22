{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PostsController where

import Prelude hiding (show)
import qualified Prelude

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as S8
import Data.Time.LocalTime (getZonedTime)
import Data.Maybe
import Web.Simple
import Web.REST
import qualified Database.PostgreSQL.Connection as DB
import Database.PostgreSQL.Models

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Blog.Models.Post as P
import qualified Blog.Views.Posts as V
import Blog.Templates

import Network.Wai

postsController cache = rest $ do
  
  index $ DB.withConnection $ \conn -> do
    posts <- liftIO $ findAll P.posts conn
    respond $ okHtml $ renderHtml $ defaultTemplate $ V.index posts

  show $ do
    path <- fmap (S8.unpack . rawPathInfo) request
    fmap okHtml $ fetchOr cache path $
      DB.withConnection $ \conn -> do
        (Just pid) <- queryParam "id"
        (Just post) <- liftIO $ find P.posts pid conn
        return $ renderHtml $ defaultTemplate $ V.show post

postsAdminController cache = rest $ do
  index $ DB.withConnection $ \conn -> do
    posts <- liftIO $ findAll P.posts conn
    respond $ okHtml $ renderHtml $ adminTemplate $ V.listPosts posts

  edit $ DB.withConnection $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- liftIO $ find P.posts pid conn
    respond $ okHtml $ renderHtml $ adminTemplate $ V.edit post

  update $ DB.withConnection $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- liftIO $ find P.posts pid conn
    (params, _) <- parseForm
    curTime <- liftIO $ getZonedTime
    let mpost = do
          pTitle <- lookup "title" params
          pBody <- lookup "body" params
          return $ post { P.title = fromString $ S8.unpack pTitle
                        , P.body = fromString $ S8.unpack pBody }
    case mpost of
      Just post -> do
        liftIO $ upsert post conn
        invalidate cache $ P.postUrl post
        respond $ redirectTo $ P.postUrl post
      Nothing -> redirectBack

  new $ do
    respond $ okHtml $ renderHtml $ adminTemplate V.new

  create $ DB.withConnection $ \conn -> do
    (params, _) <- parseForm
    curTime <- liftIO $ getZonedTime
    let mpost = do
          pTitle <- lookup "title" params
          pBody <- lookup "body" params
          return $ P.Post Nothing (fromString $ S8.unpack pTitle)
                                  (fromString $ S8.unpack pBody)
                                  curTime
    case mpost of
      Just post -> do
        liftIO $ insert post conn
        respond $ redirectTo "/posts/"
      Nothing -> redirectBack

  delete $ DB.withConnection $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- liftIO $ find P.posts pid conn
    liftIO $ destroy post conn
    invalidate cache $ P.postUrl post
    respond $ redirectTo "/posts"

