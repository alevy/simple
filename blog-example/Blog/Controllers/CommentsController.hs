{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.CommentsController where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.LocalTime
import Database.PostgreSQL.ORM
import Web.Simple
import Web.Simple.Templates
import Web.Frank

import Blog.Common
import qualified Blog.Models ()
import qualified Blog.Models.Comment as C
import Blog.Models.Post

import Blog.Auth
import Blog.Models

lookupText :: S8.ByteString -> [(S8.ByteString, S8.ByteString)] -> Maybe T.Text
lookupText k vals = fmap (T.pack . S8.unpack) $ lookup k vals

commentsController :: Controller AppSettings ()
commentsController = do
  post "/" $ do
    pid <- readQueryParam' "post_id"

    (params, _) <- parseForm
    curTime <- liftIO $ getZonedTime
    let mcomment = do
          name <- lookupText "name" params
          email <- lookupText "email" params
          comment <- lookupText "comment" params
          return $ C.Comment NullKey name email comment pid curTime
    case mcomment of
      Just comment -> withConnection $ \conn -> do
        mpost <- liftIO $ findRow conn pid
        when (not $ isJust mpost) $ respond notFound
        let myPost = fromJust mpost

        ec <- liftIO $ trySave conn comment
        case ec of
          Right _ -> respond $ redirectTo $
            encodeUtf8 $ "/posts/" <> (postSlug myPost)
          Left errs -> do
            comments <- liftIO $ allComments conn myPost
            render "posts/show.html" $
              object ["comment" .= comment, "errors" .= errs
                     , "post" .= myPost, "comments" .= comments]
      Nothing -> respond $ badRequest
    redirectBack

commentsAdminController :: Controller AppSettings ()
commentsAdminController = requiresAdmin "/login" $ do
  get "/" $ withConnection $ \conn -> do
    pid <- readQueryParam' "post_id"
    (Just p) <- liftIO $ findRow conn pid
    comments <- liftIO $ allComments conn p
    renderLayout "layouts/admin.html"
      "admin/comments/index.html" $
        object ["post" .= p, "comments" .= comments]
    
  delete ":id" $ withConnection $ \conn -> do
    cid <- readQueryParam' "id"
    (Just comment) <- liftIO $ (findRow conn cid :: IO (Maybe C.Comment))
    liftIO $ destroy conn comment
    redirectBack

