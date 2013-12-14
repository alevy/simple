{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $
  add_column "post" "published" "boolean NOT NULL DEFAULT TRUE"

down :: Connection -> IO ()
down = migrate $ do
  drop_column "post" "published"

main :: IO ()
main = defaultMain up down

