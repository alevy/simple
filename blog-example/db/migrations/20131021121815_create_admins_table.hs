{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations

up = migrate $
  create_table "admins"
    [ column "openid" "varchar(255) PRIMARY KEY" ]

down = migrate $
  drop_table "admins"

main = defaultMain up down
