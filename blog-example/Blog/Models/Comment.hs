{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings #-}
module Blog.Models.Comment where

import Data.Aeson
import Data.Text (Text)
import Database.PostgreSQL.ORM.Model

import GHC.Generics

import Blog.Models.Post

data Comment = Comment { commentId :: DBKey
                       , name :: Text
                       , email :: Text
                       , comment :: Text
                       , postId :: DBRef Post }
                  deriving (Show, Generic)

instance ToJSON Comment

instance Model Comment where
  modelInfo = defaultModelInfo { modelTable = "comments"
                               , modelColumns = [ "id"
                                                , "name"
                                                , "email"
                                                , "comment"
                                                , "post_id"]}

