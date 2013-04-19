import Control.Monad.IO.Class
import Data.Time.Clock
import Data.Time.Format
import Database.PostgreSQL.Migrations
import Web.Simple.Migrations
import System.Locale

up :: Migration
up = dbUp $ do
  add_column "posts" "posted_at" timestamp [NOT_NULL, DEFAULT "now()"]

down :: Migration
down = dbDown $ do
  drop_column "posts" "posted_at"

