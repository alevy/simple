{-# LANGUAGE OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Blog.Models.Post where

import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Data.Time.LocalTime
import Data.Time.Format
import Database.PostgreSQL.ORM
import System.Locale

import GHC.Generics

data Post = Post { postId :: DBKey
                 , title :: Text
                 , body :: Text
                 , postedAt :: ZonedTime} deriving (Show, Generic)

instance ToJSON Post

postedAtStr :: Post -> String
postedAtStr post = formatTime defaultTimeLocale "%B %e, %C%y %R" $
                    postedAt post

postUrl :: DBKey -> String
postUrl pid = "/posts/" ++ (show pid)

instance Model Post where
  modelInfo = defaultModelInfo { modelTable = "posts"
                               , modelColumns = ["id", "title", "body", "posted_at"]}

  modelValid =
    validateNotEmpty title
      "title" "Title cannot be empty"
    <> validateNotEmpty body
      "body"  "Body cannot be empty"

