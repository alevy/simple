{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PostsController where

import Prelude hiding (show)
import qualified Prelude

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

import Blog.Common
import Blog.Models
import qualified Blog.Models.Comment as C
import qualified Blog.Models.Post as P
import qualified Blog.Views.Posts as V

import Network.Wai

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
          respondAdminTemplate $ V.edit post errs
        respond $ redirectTo $ P.postUrl (P.postId post)
      Nothing -> redirectBack

  new $ do
    respondAdminTemplate $ V.new []

  create $ withConnection $ \conn -> do
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
          respondAdminTemplate $ V.new errs
        respond $ redirectTo "/posts/"
      Nothing -> redirectBack

  delete $ withConnection $ \conn -> do
    pid <- readQueryParam' "id"
    (Just post) <- liftIO $ findRow conn pid :: Controller AppSettings (Maybe P.Post)
    liftIO $ destroy conn post
    respond $ redirectTo "/posts"

