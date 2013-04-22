{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Blog.Models.Post where

import Control.Applicative
import Data.Maybe
import Data.Text
import Data.Time.LocalTime
import Data.Time.Format
import Database.PostgreSQL.Models
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import System.Locale
import Text.Blaze.Html
import Text.Pandoc

data Post = Post { postId :: Maybe Integer
                 , title :: Text
                 , body :: Text
                 , postedAt :: ZonedTime} deriving (Show, Read)

postedAtStr :: Post -> String
postedAtStr post = formatTime defaultTimeLocale "%B %e, %C%y %R" $ postedAt post

markdownBody :: Post -> Html
markdownBody = (writeHtml def) . (readMarkdown def) . unpack . body

postUrl :: Post -> String
postUrl post = "/posts/" ++ (show $ fromJust $ postId post)

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field

instance ToRow Post where
  toRow post = toRow ( title post
                     , replace "\r" "" $ body post
                     , postedAt post)

posts :: TableName Post
posts = TableName "posts"

instance PostgreSQLModel Post where
  type PrimaryKey Post = Integer
  primaryKey = postId
  tableName _ = posts
  columns _ = ["title", "body", "posted_at"]
  orderBy _ = Just "posted_at desc"

