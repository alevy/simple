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
                 , postTitle :: Text
                 , postBody :: Text
                 , postPostedAt :: ZonedTime} deriving (Show, Generic)

instance ToJSON Post

postedAtStr :: Post -> String
postedAtStr post = formatTime defaultTimeLocale "%B %e, %C%y %R" $
                    postPostedAt post

postUrl :: DBKey -> String
postUrl pid = "/posts/" ++ (show pid)

instance Model Post where
  modelInfo = underscoreModelInfo "post"

  modelValid =
    validateNotEmpty postTitle
      "title" "Title cannot be empty"
    <> validateNotEmpty postBody
      "body"  "Body cannot be empty"

