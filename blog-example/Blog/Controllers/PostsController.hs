{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PostsController where

import Prelude hiding (show)
import qualified Prelude

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Data.Text.Encoding
import Data.Time.LocalTime (getZonedTime)
import Database.PostgreSQL.ORM
import Web.Simple
import Web.Simple.Templates
import Web.REST

import Blog.Auth
import Blog.Common
import Blog.Models
import Blog.Models.Post

postsController :: REST AppSettings
postsController = rest $ do

  index $ withConnection $ \conn -> do
    mpage <- readQueryParam "offset"
    let page = maybe 0 id mpage
    posts <- liftIO $ dbSelect conn $ setLimit 10
                                    $ setOffset (page * 10)
                                    $ modelDBSelect
    render "posts/index.html" (posts :: [Post])

  show $ do
    withConnection $ \conn -> do
      pid <- readQueryParam' "id"
      (Just post) <- liftIO $ findRow conn pid
      comments <- liftIO $ allComments conn post
      render "posts/show.html" $
        object ["post" .= post, "comments" .= comments]

postsAdminController :: Controller AppSettings ()
postsAdminController = requiresAdmin "/login" $ routeREST $ rest $ do
  index $ withConnection $ \conn -> do
    posts <- liftIO $ dbSelect conn $
      setOrderBy "posted_at desc" $ modelDBSelect
    renderLayout "layouts/admin.html"
      "admin/posts/index.html" (posts :: [Post])

  edit $ withConnection $ \conn -> do
    pid <- readQueryParam' "id"
    (Just post) <- liftIO $
      findRow conn pid :: Controller AppSettings (Maybe Post)
    renderLayout "layouts/admin.html"
      "admin/posts/edit.html" $
        object ["post" .= post]

  update $ withConnection $ \conn -> do
    pid <- readQueryParam' "id"
    (Just post) <- liftIO $ findRow conn pid
    (params, _) <- parseForm
    let mpost = do
          pTitle <- lookup "title" params
          pBody <- lookup "body" params
          return $ post { postTitle = decodeUtf8 pTitle
                        , postBody = decodeUtf8 pBody }
    case mpost of
      Just post0 -> do
        epost <- liftIO $ trySave conn post0
        case epost of
          Left errs -> do
            liftIO $ print epost
            renderLayout "layouts/admin.html"
                                    "admin/posts/edit.html" $
                                    object [ "errors" .= errs, "post" .= post0 ]
          Right p -> respond $ redirectTo $ S8.pack $ postUrl $ postId p
      Nothing -> redirectBack

  new $ renderLayout "layouts/admin.html"
    "admin/posts/new.html" $ Null

  create $ withConnection $ \conn -> do
    (params, _) <- parseForm
    curTime <- liftIO $ getZonedTime
    let mpost = do
          pTitle <- lookup "title" params
          pBody <- lookup "body" params
          return $ Post NullKey (decodeUtf8 pTitle)
                                  (decodeUtf8 pBody)
                                  curTime
    case mpost of
      Just post0 -> do
        epost <- liftIO $ trySave conn post0
        case epost of
          Left errs -> renderLayout "layouts/admin.html"
                                    "admin/posts/new.html" errs
          Right post -> respond $ redirectTo $
            "/posts/" `S8.append` (S8.pack $ Prelude.show $ postId post)
      Nothing -> redirectBack

  delete $ withConnection $ \conn -> do
    pid <- readQueryParam' "id"
    (Just post) <- liftIO $ findRow conn pid
    liftIO $ destroy conn (post :: Post)
    respond $ redirectTo "/admin/posts"

