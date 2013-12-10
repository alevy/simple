{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings #-}
module Blog.Models.Comment where

import Data.Aeson
import Data.Text (Text)
import Data.Time.LocalTime
import Database.PostgreSQL.ORM.Model

import GHC.Generics

import Blog.Models.Post

data Comment = Comment { commentId :: DBKey
                       , commentName :: Text
                       , commentEmail :: Text
                       , commentComment :: Text
                       , commentPostId :: DBRef Post
                       , commentCommentedAt :: ZonedTime }
                  deriving (Show, Generic)

instance ToJSON Comment

instance Model Comment where
  modelInfo = underscoreModelInfo "comment"

