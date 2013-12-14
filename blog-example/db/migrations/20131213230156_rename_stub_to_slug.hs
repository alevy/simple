{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $
  rename_column "post" "stub" "slug"

down :: Connection -> IO ()
down = migrate $ do
  rename_column "post" "slug" "stub"

main :: IO ()
main = defaultMain up down

