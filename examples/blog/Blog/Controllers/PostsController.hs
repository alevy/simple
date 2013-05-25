{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PostsController where

import Prelude hiding (show)
import qualified Prelude

import Common

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as S8
import Data.Time.LocalTime (getZonedTime)
import Data.Maybe
import Data.Monoid
import Web.Simple
import Web.Simple.Cache
import Web.REST
import Database.PostgreSQL.ORM
import Data.String

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Blog.Models
import qualified Blog.Models.Comment as C
import qualified Blog.Models.Post as P
import qualified Blog.Views.Posts as V
import Blog.Templates

import Network.Wai

postsController as = rest $ do
  
  index $ withConnection as $ \conn -> do
    mpage <- queryParam "offset"
    let page = maybe 0 id mpage
    posts <- dbSelect conn $ setLimit 10
                                    $ setOffset (page * 10)
                                    $ modelDBSelect
    respond $ okHtml $ renderHtml $ defaultTemplate $ V.index posts

  show $ do
    path <- fmap (S8.unpack . rawPathInfo) request
    withConnection as $ \conn -> do
      (Just pid) <- queryParam "id"
      (Just post) <- findRow conn pid
      comments <- allComments conn post
      respond $ okHtml $ renderHtml $ defaultTemplate $ V.show post comments

postsAdminController as = rest $ do
  index $ withConnection as $ \conn -> do
    posts <- findAll conn
    respond $ okHtml $ renderHtml $ adminTemplate $ V.listPosts posts

  edit $ withConnection as $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- findRow conn pid
    respond $ okHtml $ renderHtml $ adminTemplate $ V.edit post []

  update $ withConnection as $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- findRow conn pid
    (params, _) <- parseForm
    curTime <- liftIO $ getZonedTime
    let mpost = do
          pTitle <- lookup "title" params
          pBody <- lookup "body" params
          return $ post { P.title = fromString $ S8.unpack pTitle
                        , P.body = fromString $ S8.unpack pBody }
    case mpost of
      Just post -> do
        merrs <- save conn post
        case merrs of
          Left _ -> respond $ redirectTo $ P.postUrl (P.postId post)
          Right errs ->
            respond $ okHtml $ renderHtml $ adminTemplate $ V.edit post errs
      Nothing -> redirectBack

  new $ do
    respond $ okHtml $ renderHtml $ adminTemplate $ V.new []

  create $ withConnection as $ \conn -> do
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
        eerr <- save conn post
        case eerr of
          Left _ -> respond $ redirectTo "/posts/"
          Right errs ->
            respond $ okHtml $ renderHtml $ adminTemplate $ V.new errs
      Nothing -> redirectBack

  delete $ withConnection as $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- findRow conn pid :: Controller (Maybe P.Post)
    destroy conn post
    respond $ redirectTo "/posts"

