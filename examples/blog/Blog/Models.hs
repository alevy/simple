{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Blog.Models where

import Control.Applicative
import Database.PostgreSQL.Simple
import Database.PostgreSQL.ORM.Association
import Database.PostgreSQL.ORM.DBSelect
import Database.PostgreSQL.ORM.Model

import qualified Blog.Models.Comment as C
import qualified Blog.Models.Post as P


postComments :: Association P.Post C.Comment
postComments = has

allComments :: Connection -> P.Post -> IO [C.Comment]
allComments conn post = map lookupRow <$> query conn
                                                (assocQuery postComments)
                                                (assocParam postComments post)
selectComments :: P.Post -> DBSelect (LookupRow C.Comment)
selectComments post = addWhere "comments.post_id = ?" (Only $ P.postId post) $
                    assocSelect postComments

