{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up = migrate $
  create_table "comment"
    [ column "id" "serial PRIMARY KEY"
    , column "name" "varchar(255) NOT NULL"
    , column "email" "varchar(255) NOT NULL"
    , column "comment" "text NOT NULL"
    , column "post_id" "integer REFERENCES post(id)"]

down = migrate $
  drop_table "comment"

main :: IO ()
main = defaultMain up down

