{-# LANGUAGE OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Blog.Models.Post where

import Control.Applicative
import Data.Maybe
import Data.Text (Text, unpack, replace)
import Data.Time.LocalTime
import Data.Time.Format
import Database.PostgreSQL.ORM.Model
import Database.PostgreSQL.ORM.Relationships
import System.Locale
import Text.Blaze.Html
import Text.Pandoc

import GHC.Generics

data Post = Post { postId :: DBKey
                 , title :: Text
                 , body :: Text
                 , postedAt :: ZonedTime} deriving (Show, Generic)

postedAtStr :: Post -> String
postedAtStr post = formatTime defaultTimeLocale "%B %e, %C%y %R" $
                    postedAt post

markdownBody :: Post -> Html
markdownBody = (writeHtml def) . (readMarkdown def) . unpack . body

postUrl :: DBKey -> String
postUrl pid = "/posts/" ++ (show pid)

instance Model Post where
  modelInfo = defaultModelInfo { modelTable = "posts"
                               , modelColumns = ["id", "title", "body", "posted_at"]}

