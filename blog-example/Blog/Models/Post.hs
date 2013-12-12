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
import System.Locale

import GHC.Generics

data Post = Post { postId :: DBKey
                 , postTitle :: Text
                 , postStub :: Text
                 , postBody :: Text
                 , postPostedAt :: ZonedTime} deriving (Show, Generic)

instance ToJSON Post

postedAtStr :: Post -> String
postedAtStr post = formatTime defaultTimeLocale "%B %e, %C%y %R" $
                    postPostedAt post

validateOnlyChars :: Model a => (a -> Text) -> [Char]
                  -> Text -> Text -> a -> [InvalidError]
validateOnlyChars accs chrs colName msg model =
  if T.all suchChars (accs model) then
    []
    else [InvalidError colName msg]
  where suchChars c = any (== c) chrs

instance Model Post where
  modelInfo = underscoreModelInfo "post"

  modelValid =
    validateNotEmpty postTitle
      "title" "Title cannot be empty"
    <> validateNotEmpty postBody
      "body"  "Body cannot be empty"
    <> validateNotEmpty postStub
      "stub"  "Stub cannot be empty"

stubFromTitle :: Text -> Text
stubFromTitle title = T.take 32 $
  T.map (\c -> if c == ' ' then '-' else toLower c) $
  T.filter (\c -> c == ' ' || isAlphaNum c) title

