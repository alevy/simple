{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PostsController where

import Prelude hiding (show)
import qualified Prelude

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as S8
import Data.Text.Encoding
import Data.Time.LocalTime (getZonedTime)
import Web.Simple
import Web.REST
import Database.PostgreSQL.ORM

import Blog.Common
import Blog.Models
import qualified Blog.Models.Post as P
import qualified Blog.Views.Posts as V

postsController :: REST AppSettings
postsController = rest $ do
  
  index $ withConnection $ \conn -> do
    mpage <- readQueryParam "offset"
    let page = maybe 0 id mpage
    posts <- liftIO $ dbSelect conn $ setLimit 10
                                    $ setOffset (page * 10)
                                    $ modelDBSelect
    respondTemplate $ V.index posts

  show $ do
    withConnection $ \conn -> do
      pid <- readQueryParam' "id"
      (Just post) <- liftIO $ findRow conn pid
      comments <- liftIO $ allComments conn post
      respondTemplate $ V.show post comments

postsAdminController :: REST AppSettings
postsAdminController = rest $ do
  index $ withConnection $ \conn -> do
    posts <- liftIO $ findAll conn
    respondAdminTemplate $ V.listPosts posts

  edit $ withConnection $ \conn -> do
    pid <- readQueryParam' "id"
    (Just post) <- liftIO $ findRow conn pid
    respondTemplate $ V.edit post []

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
          respondAdminTemplate $ V.edit p errs
        respond $ redirectTo $ S8.pack $ P.postUrl (P.postId p)
      Nothing -> redirectBack

  new $ do
    respondAdminTemplate $ V.new []

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
                  catch (trySave conn post >> return [])
                        (\(ValidationError errs) -> return errs)
        when (not . null $ errs) $
          respondAdminTemplate $ V.new errs
        respond $ redirectTo "/posts/"
      Nothing -> redirectBack

  delete $ withConnection $ \conn -> do
    pid <- readQueryParam' "id"
    (Just post) <- liftIO $ findRow conn pid :: Controller AppSettings (Maybe P.Post)
    liftIO $ destroy conn post
    respond $ redirectTo "/posts"

