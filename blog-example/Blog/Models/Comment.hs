{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings #-}
module Blog.Models.Comment where

import Data.Aeson
import Data.Text (Text)
import Data.Time.LocalTime
import Data.Monoid
import Database.PostgreSQL.ORM
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

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

validateEmail :: Comment -> ValidationError
validateEmail = validate (\comment -> commentEmail comment =~ pattern)
    "email" "You must enter a valid e-mail address"
  where pattern :: Text
        pattern = "^[^@]+[@][^@]+$"

instance Model Comment where
  modelInfo = underscoreModelInfo "comment"

  modelValid =
    validateNotEmpty commentName
      "name" "Name cannot be empty"
    <> validateEmail
    <> validateNotEmpty commentComment
      "comment" "Comment cannot be empty"

