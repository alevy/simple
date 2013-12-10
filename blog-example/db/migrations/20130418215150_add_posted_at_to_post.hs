{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up = migrate $
  add_column "post" "posted_at" "timestamptz NOT NULL DEFAULT now()"

down = migrate $
  drop_column "post" "posted_at"

main :: IO ()
main = defaultMain up down

