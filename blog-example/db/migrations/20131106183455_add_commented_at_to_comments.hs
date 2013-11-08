{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up = migrate $
  add_column "comments" "commented_at" "timestamptz NOT NULL DEFAULT now()"

down = migrate $
  drop_column "comments" "commented_at"

main :: IO ()
main = defaultMain up down

