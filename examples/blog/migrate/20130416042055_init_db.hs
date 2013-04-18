import Database.PostgreSQL.Migrations
import Web.Simple.Migrations

up :: Migration
up = initDb

down :: Migration
down = const . const $ return False

