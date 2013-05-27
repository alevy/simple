{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PostsController where

import Prelude hiding (show)
import qualified Prelude

import Common

import Control.Exception
import Control.Monad
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
    posts <- liftIO $ dbSelect conn $ setLimit 10
                                    $ setOffset (page * 10)
                                    $ modelDBSelect
    respond $ okHtml $ renderHtml $ defaultTemplate $ V.index posts

  show $ do
    withConnection as $ \conn -> do
      (Just pid) <- queryParam "id"
      (Just post) <- liftIO $ findRow conn pid
      comments <- liftIO $ allComments conn post
      respond $ okHtml $ renderHtml $ defaultTemplate $ V.show post comments

postsAdminController as = rest $ do
  index $ withConnection as $ \conn -> do
    posts <- liftIO $ findAll conn
    respond $ okHtml $ renderHtml $ adminTemplate $ V.listPosts posts

  edit $ withConnection as $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- liftIO $ findRow conn pid
    respond $ okHtml $ renderHtml $ adminTemplate $ V.edit post []

  update $ withConnection as $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- liftIO $ findRow conn pid
    (params, _) <- parseForm
    curTime <- liftIO $ getZonedTime
    let mpost = do
          pTitle <- lookup "title" params
          pBody <- lookup "body" params
          return $ post { P.title = fromString $ S8.unpack pTitle
                        , P.body = fromString $ S8.unpack pBody }
    case mpost of
      Just post -> do
        errs <- liftIO $
                  catch (trySave conn post >> return [])
                        (\(ValidationError errs) -> return errs)
        when (not . null $ errs) $
          respond $ okHtml $ renderHtml $ adminTemplate $ V.edit post errs
        respond $ redirectTo $ P.postUrl (P.postId post)
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
        errs <- liftIO $
                  catch (trySave conn post >> return [])
                        (\(ValidationError errs) -> return errs)
        when (not . null $ errs) $
          respond $ okHtml $ renderHtml $ adminTemplate $ V.new errs
        respond $ redirectTo "/posts/"
      Nothing -> redirectBack

  delete $ withConnection as $ \conn -> do
    (Just pid) <- queryParam "id"
    (Just post) <- liftIO $ findRow conn pid :: Controller (Maybe P.Post)
    liftIO $ destroy conn post
    respond $ redirectTo "/posts"

