{-# LANGUAGE OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Blog.Models.Post where

import Data.Aeson
import Data.Char
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Data.Time.Format
import Database.PostgreSQL.ORM
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import System.Locale

import GHC.Generics

data Post = Post { postId :: DBKey
                 , postTitle :: Text
                 , postSlug :: Text
                 , postBody :: Text
                 , postPublished :: Bool
                 , postPostedAt :: ZonedTime} deriving (Show, Generic)

instance ToJSON Post

postedAtStr :: Post -> String
postedAtStr post = formatTime defaultTimeLocale "%B %e, %C%y %R" $
                    postPostedAt post

validateSlug :: Post -> [InvalidError]
validateSlug post =
  if postSlug post =~ pattern then
    []
    else [InvalidError "slug"
            "Slug must contain only letters, numbers and dashes"]
  where pattern :: Text
        pattern = "^[a-z1-9-]{0,32}$"

instance Model Post where
  modelInfo = underscoreModelInfo "post"

  modelValid =
    validateNotEmpty postTitle
      "title" "Title cannot be empty"
    <> validateNotEmpty postBody
      "body"  "Body cannot be empty"
    <> validateNotEmpty postSlug
      "slug"  "Slug cannot be empty"
    <> validateSlug

slugFromTitle :: Text -> Text
slugFromTitle title = T.take 32 $
  T.map (\c -> if c == ' ' then '-' else toLower c) $
  T.filter (\c -> c == ' ' || isAlphaNum c) title

