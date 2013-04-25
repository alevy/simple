import Database.PostgreSQL.Migrations
import Web.Simple.Migrations

up :: Migration
up = dbUp $ do
  create_table "comments" $ do
    column "id" serial [PRIMARY_KEY]
    column "name" string [NOT_NULL]
    column "email" string [NOT_NULL]
    column "comment" text [NOT_NULL]
    column "post_id" integer [REFERENCES "posts" "id"]

down :: Migration
down = dbDown $ drop_table "comments"

