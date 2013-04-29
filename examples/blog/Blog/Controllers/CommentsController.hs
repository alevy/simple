{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.CommentsController where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import qualified Database.PostgreSQL.Connection as DB
import Database.PostgreSQL.Models
import Web.Simple
import Web.Simple.Cache
import Web.Frank

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Blog.Models.Comment as C
import qualified Blog.Models.Post as P

import Blog.Templates
import Blog.Views.Comments

lookupText :: S8.ByteString -> [(S8.ByteString, S8.ByteString)] -> Maybe T.Text
lookupText k vals = fmap (T.pack . S8.unpack) $ lookup k vals

commentsController cache = do
  post "/" $ do
    (Just pid) <- queryParam "post_id"
    (params, _) <- parseForm
    let mcomment = do
          name <- lookupText "name" params
          email <- lookupText "email" params
          comment <- lookupText "comment" params
          return $ C.Comment Nothing name email comment
    case mcomment of
      Just comment -> DB.withConnection $ \conn -> do
        (Just post) <- liftIO $ find P.posts pid conn
        liftIO $ insertFor post comment conn
        invalidate cache $ P.postUrl pid
    redirectBack

commentsAdminController cache = do
  get "/" $ DB.withConnection $ \conn -> do
    (Just pid) <- queryParam "post_id"
    (Just post) <- liftIO $ find P.posts pid conn
    comments <- liftIO $ childrenOf post C.comments conn
    respond $ okHtml $ renderHtml $ adminTemplate $ listComments post comments
    
  delete ":id" $ DB.withConnection $ \conn -> do
    (Just pid) <- queryParam "post_id"
    (Just cid) <- queryParam "id"
    liftIO $ destroy C.comments cid conn
    invalidate cache $ P.postUrl pid
    redirectBack

