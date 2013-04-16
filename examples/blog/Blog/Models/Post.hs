{-# LANGUAGE TypeFamilies #-}
module Blog.Models.Post where

import Control.Applicative
import Data.Text
import Database.PostgreSQL.Models
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

data Post = Post { key :: Maybe Integer
                 , title :: Text
                 , body :: Text } deriving (Show, Read)

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field

instance ToRow Post where
  toRow post = toRow ( title post
                     , body post)

posts :: TableName Post
posts = TableName "posts"

instance PostgreSQLModel Post where
  type PrimaryKey Post = Integer
  primaryKey = key
  tableName _ = posts
  columns _ = ["title", "body"]
  orderBy _ = Just "id desc"
