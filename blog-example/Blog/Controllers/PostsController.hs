{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PostsController where

import Prelude hiding (show)
import qualified Prelude

import Control.Monad
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
import qualified Blog.Models.Post as P

postsController :: REST AppSettings
postsController = rest $ do
  
  index $ withConnection $ \conn -> do
    mpage <- readQueryParam "offset"
    let page = maybe 0 id mpage
    posts <- liftIO $ dbSelect conn $ setLimit 10
                                    $ setOffset (page * 10)
                                    $ modelDBSelect
    render "posts/index.html" (posts :: [P.Post])

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
    posts <- liftIO $ findAll conn :: Controller AppSettings [P.Post]
    renderLayout "templates/admin.html"
      "admin/posts/index.html" posts

  edit $ withConnection $ \conn -> do
    pid <- readQueryParam' "id"
    (Just post) <- liftIO $
      findRow conn pid :: Controller AppSettings (Maybe P.Post)
    renderLayout "templates/admin.html"
      "admin/posts/edit.html" $
        object ["post" .= post]

  update $ withConnection $ \conn -> do
    pid <- readQueryParam' "id"
    (Just post) <- liftIO $ findRow conn pid
    (params, _) <- parseForm
    let mpost = do
          pTitle <- lookup "title" params
          pBody <- lookup "body" params
          return $ post { P.title = decodeUtf8 pTitle
                        , P.body = decodeUtf8 pBody }
    case mpost of
      Just p -> do
        errs <- liftIO $
                  catch (trySave conn p >> return [])
                        (\(ValidationError errs) -> return errs)
        when (not . null $ errs) $
          renderLayout "templates/admin.html"
            "admin/posts/edit.html" $
              object ["post" .= post, "errors" .= errs]
        respond $ redirectTo $ S8.pack $ P.postUrl (P.postId p)
      Nothing -> redirectBack

  new $ renderLayout "templates/admin.html"
    "admin/posts/new.html" $ Null

  create $ withConnection $ \conn -> do
    (params, _) <- parseForm
    curTime <- liftIO $ getZonedTime
    let mpost = do
          pTitle <- lookup "title" params
          pBody <- lookup "body" params
          return $ P.Post NullKey (decodeUtf8 pTitle)
                                  (decodeUtf8 pBody)
                                  curTime
    case mpost of
      Just post -> do
        errs <- liftIO $
                  either id (const []) `fmap` (trySave conn post)
        when (not . null $ errs) $
          renderLayout "templates/admin.html"
            "admin/posts/new.html" errs
        respond $ redirectTo "/posts/"
      Nothing -> redirectBack

  delete $ withConnection $ \conn -> do
    pid <- readQueryParam' "id"
    (Just post) <- liftIO $ findRow conn pid :: Controller AppSettings (Maybe P.Post)
    liftIO $ destroy conn post
    respond $ redirectTo "/posts"

