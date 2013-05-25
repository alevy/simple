{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.CommentsController where

import Common

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Database.PostgreSQL.ORM
import Web.Simple
import Web.Simple.Cache
import Web.Frank

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Blog.Models ()
import qualified Blog.Models.Comment as C
import qualified Blog.Models.Post as P

import Blog.Models
import Blog.Templates
import Blog.Views.Comments

lookupText :: S8.ByteString -> [(S8.ByteString, S8.ByteString)] -> Maybe T.Text
lookupText k vals = fmap (T.pack . S8.unpack) $ lookup k vals

commentsController as = do
  post "/" $ do
    (Just pid) <- queryParam "post_id"
    (params, _) <- parseForm
    let mcomment = do
          name <- lookupText "name" params
          email <- lookupText "email" params
          comment <- lookupText "comment" params
          return $ C.Comment NullKey name email comment pid
    case mcomment of
      Just comment -> withConnection as $ \conn -> do
        (Just post) <- liftIO $ findRow conn pid
        liftIO $ save conn comment
    redirectBack

commentsAdminController as = do
  get "/" $ withConnection as $ \conn -> do
    (Just pid) <- queryParam "post_id"
    (Just post) <- liftIO $ findRow conn pid
    comments <- liftIO $ allComments conn post
    respond $ okHtml $ renderHtml $ adminTemplate $ listComments post comments
    
  delete ":id" $ withConnection as $ \conn -> do
    (Just cid) <- queryParam "id"
    (Just comment) <- liftIO $ (findRow conn cid :: IO (Maybe C.Comment))
    liftIO $ destroy conn comment
    redirectBack

