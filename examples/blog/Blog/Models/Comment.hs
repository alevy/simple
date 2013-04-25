{-# LANGUAGE TypeFamilies #-}
module Blog.Models.Comment where

import Control.Applicative
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Models

data Comment = Comment { commentId :: Maybe Integer
                       , name :: Text
                       , email :: Text
                       , comment :: Text } deriving (Show, Read)

comments :: TableName Comment
comments = TableName "comments"

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field <*> field

instance PostgreSQLModel Comment where
  type PrimaryKey Comment = Integer
  primaryKey = commentId
  tableName _ = comments
  columns _ = [ Column "name" name
              , Column "email" email
              , Column "comment" comment ]

