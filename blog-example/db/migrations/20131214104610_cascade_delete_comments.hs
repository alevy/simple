{-# LANGUAGE OverloadedStrings, CPP #-}
import Control.Monad
import Data.Monoid
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up c = void $
  execute_ c $
    "alter table comment drop constraint comment_post_id_fkey, " <>
    "add constraint comment_post_id_fkey foreign key (post_id) " <>
    "references post(id) on update cascade on delete cascade"

down :: Connection -> IO ()
down c = void $
  execute_ c $
    "alter table comment drop constraint comment_post_id_fkey, " <>
    "add constraint comment_post_id_fkey foreign key (post_id) " <>
    "references post(id)"

main :: IO ()
main = defaultMain up down

