{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Blog.Models where

import Control.Monad.IO.Class
import Control.Applicative
import Database.PostgreSQL.Simple
import Database.PostgreSQL.ORM

import qualified Blog.Models.Comment as C
import qualified Blog.Models.Post as P


postComments :: Association P.Post C.Comment
postComments = has

allComments :: Connection -> P.Post -> IO [C.Comment]
allComments = findAssoc postComments


