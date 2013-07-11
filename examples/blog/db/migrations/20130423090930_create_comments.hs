{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up = migrate $
  create_table "comments"
    [ column "id" "serial PRIMARY KEY"
    , column "name" "varchar(255) NOT NULL"
    , column "email" "varchar(255) NOT NULL"
    , column "comment" "text NOT NULL"
    , column "post_id" "integer REFERENCES posts(id)"]

down = migrate $
  drop_table "comments"

main :: IO ()
main = defaultMain up down

