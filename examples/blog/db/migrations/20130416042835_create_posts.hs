{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $
  create_table "posts" $
    [ column "id" "serial PRIMARY KEY"
    , column "title" "varchar(255)"
    , column "body" "text" ]

down :: Connection -> IO ()
down = migrate $
  drop_table "posts"

main :: IO ()
main = defaultMain up down

