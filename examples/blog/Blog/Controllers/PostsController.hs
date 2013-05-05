{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PostsController where

import Prelude hiding (show)
import qualified Prelude

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as S8
import Data.Time.LocalTime (getZonedTime)
import Data.Maybe
import Web.Simple
import Web.Simple.Cache
import Web.REST
import qualified Database.PostgreSQL.Connection as DB
import Database.PostgreSQL.ORM.Model
import Database.PostgreSQL.ORM.Relationships
import Data.String

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Blog.Models
import qualified Blog.Models.Comment as C
import qualified Blog.Models.Post as P
import qualified Blog.Views.Posts as V
import Blog.Templates

import Network.Wai

postsController = rest $ do
  
  index $ DB.withConnection $ \conn -> do
    posts <- liftIO $ findAll conn
    respond $ okHtml $ renderHtml $ defaultTemplate $ V.index posts

  show $ do
    path <- fmap (S8.unpack . rawPathInfo) request
    DB.withConnection $ \conn -> do
      (Just pid) <- queryParam "id"
      (Just post) <- liftIO $ find conn pid
      comments <- liftIO $ findMany conn post
      return $ okHtml $ renderHtml $ defaultTemplate $ V.show post comments

postsAdminController = rest $ do
  index $ DB.withConnection $ \conn -> do
    posts <- liftIO $ findAll conn
    respond $ okHtml $ renderHtml $ adminTemplate $ V.listPosts posts

  edit $ DB.withConnection $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- liftIO $ find conn pid
    respond $ okHtml $ renderHtml $ adminTemplate $ V.edit post

  update $ DB.withConnection $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- liftIO $ find conn pid
    (params, _) <- parseForm
    curTime <- liftIO $ getZonedTime
    let mpost = do
          pTitle <- lookup "title" params
          pBody <- lookup "body" params
          return $ post { P.title = fromString $ S8.unpack pTitle
                        , P.body = fromString $ S8.unpack pBody }
    case mpost of
      Just post -> do
        liftIO $ save conn post
        respond $ redirectTo $ P.postUrl (P.postId post)
      Nothing -> redirectBack

  new $ do
    respond $ okHtml $ renderHtml $ adminTemplate V.new

  create $ DB.withConnection $ \conn -> do
    (params, _) <- parseForm
    curTime <- liftIO $ getZonedTime
    let mpost = do
          pTitle <- lookup "title" params
          pBody <- lookup "body" params
          return $ P.Post NullKey (fromString $ S8.unpack pTitle)
                                  (fromString $ S8.unpack pBody)
                                  curTime
    case mpost of
      Just post -> do
        liftIO $ save conn post
        respond $ redirectTo "/posts/"
      Nothing -> redirectBack

  delete $ DB.withConnection $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- liftIO $ (find conn pid :: IO (Maybe P.Post))
    liftIO $ destroy conn post
    respond $ redirectTo "/posts"

