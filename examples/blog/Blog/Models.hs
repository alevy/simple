{-# LANGUAGE MultiParamTypeClasses #-}
module Blog.Models where

import Database.PostgreSQL.ORM.Relationships

import qualified Blog.Models.Comment as C
import qualified Blog.Models.Post as P

instance HasMany P.Post C.Comment

