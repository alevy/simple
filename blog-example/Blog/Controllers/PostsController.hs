{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PostsController where

import Prelude hiding (show)
import qualified Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Data.List (partition)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
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
    posts <- liftIO $ dbSelect conn $ addWhere_ "published"
                                    $ setLimit 10
                                    $ setOffset (page * 10)
                                    $ setOrderBy "posted_at desc"
                                    $ modelDBSelect
    render "posts/index.html" (posts :: [Post])

  show $ do
    withConnection $ \conn -> do
      slug <- queryParam' "id"
      mpost <- liftIO $ listToMaybe <$>
        (dbSelect conn $
          addWhere "slug = ?" [slug :: S8.ByteString] $
          modelDBSelect)
      case mpost of
        Just post -> do
          comments <- liftIO $ allComments conn post
          render "posts/show.html" $
            object ["post" .= post, "comments" .= comments]
        Nothing -> respond notFound

postsAdminController :: Controller AppSettings ()
postsAdminController = requiresAdmin "/login" $ routeREST $ rest $ do
  index $ withConnection $ \conn -> do
    posts <- liftIO $ dbSelect conn $
      setOrderBy "posted_at desc, published" $ modelDBSelect
    let (published, drafts) = partition postPublished posts
    renderLayout "layouts/admin.html"
      "admin/posts/index.html" $
      object ["published" .= published, "drafts" .= drafts]

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
    curTime <- liftIO $ getZonedTime
    let mpost = do
          pTitle <- (decodeUtf8 <$> lookup "title" params) <|>
                      (pure $ postTitle post)
          pBody <- (decodeUtf8 <$> lookup "body" params) <|>
                    (pure $ postBody post)
          pPublished <- lookup "published" params *> pure True <|>
                          (pure $ postPublished post)
          let pTime =
                if (not $ postPublished post) && pPublished then
                  curTime
                  else postPostedAt post
          return $ post { postTitle = pTitle
                        , postBody = pBody
                        , postPublished = pPublished
                        , postPostedAt = pTime }
    case mpost of
      Just post0 -> do
        epost <- liftIO $ trySave conn post0
        case epost of
          Left errs -> do
            liftIO $ print epost
            renderLayout "layouts/admin.html"
                                    "admin/posts/edit.html" $
                                    object [ "errors" .= errs, "post" .= post0 ]
          Right p -> respond $ redirectTo $
            "/admin/posts/" <> (S8.pack $ Prelude.show $ postId p) <> "/edit"
      Nothing -> redirectBack

  new $ renderLayout "layouts/admin.html"
    "admin/posts/new.html" $ Null

  create $ withConnection $ \conn -> do
    (params, _) <- parseForm
    curTime <- liftIO $ getZonedTime
    let pTitle = decodeUtf8 <$> lookup "title" params
        pBody = decodeUtf8 <$> lookup "body" params
        pSlug = ((T.null `mfilter` (decodeUtf8 <$> lookup "slug" params))
                  <|> fmap slugFromTitle pTitle)
        mpost = Post NullKey <$> pTitle <*> pBody <*> pSlug
                <*> pure False <*> pure curTime
    case mpost of
      Just post0 -> do
        epost <- liftIO $ trySave conn post0
        case epost of
          Left errs -> renderLayout "layouts/admin.html"
                                    "admin/posts/new.html" $ object
                                    [ "errors" .= errs, "post" .= post0 ]
          Right p -> respond $ redirectTo $
            encodeUtf8 $ "/posts/" <> (postSlug p)
      Nothing -> redirectBack

  delete $ withConnection $ \conn -> do
    pid <- readQueryParam' "id"
    (Just post) <- liftIO $ findRow conn pid
    liftIO $ destroy conn (post :: Post)
    respond $ redirectTo "/admin/posts"

