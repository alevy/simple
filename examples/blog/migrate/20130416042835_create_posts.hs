import Database.PostgreSQL.Migrations
import Web.Simple.Migrations

up :: Migration
up = dbUp $ do
  create_table "posts" $ do
    column "id" serial [PRIMARY_KEY]
    column "title" string []
    column "body" text []

down :: Migration
down = dbDown $ do
  drop_table "posts"

