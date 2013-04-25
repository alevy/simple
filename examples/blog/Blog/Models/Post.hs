{-# LANGUAGE OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
module Blog.Models.Post where

import Control.Applicative
import Data.Maybe
import Data.Text (Text, unpack, replace)
import Data.Time.LocalTime
import Data.Time.Format
import Database.PostgreSQL.Models
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import System.Locale
import Text.Blaze.Html
import Text.Pandoc

import qualified Blog.Models.Comment as C

data Post = Post { postId :: Maybe Integer
                 , title :: Text
                 , body :: Text
                 , postedAt :: ZonedTime} deriving (Show, Read)

postedAtStr :: Post -> String
postedAtStr post = formatTime defaultTimeLocale "%B %e, %C%y %R" $ postedAt post

markdownBody :: Post -> Html
markdownBody = (writeHtml def) . (readMarkdown def) . unpack . body

postUrl :: Integer -> String
postUrl pid = "/posts/" ++ (show pid)

posts :: TableName Post
posts = TableName "posts"

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field

instance PostgreSQLModel Post where
  type PrimaryKey Post = Integer
  primaryKey = postId
  tableName _ = posts
  columns _ = [ Column "title" title
              , Column "body" ((replace "\r" "") . body)
              , Column "posted_at" postedAt]
  orderBy _ = Just "posted_at desc"

instance HasMany Post C.Comment where
  foreignKey _ _ = "post_id"
