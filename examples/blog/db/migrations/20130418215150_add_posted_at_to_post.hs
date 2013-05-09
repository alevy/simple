import Database.PostgreSQL.Migration
import Database.PostgreSQL.Simple

up :: Migration
up conn = do
  execute_ conn $
    add_column "posts" "posted_at timestamp NOT NULL DEFAULT now()"
  return ()

down :: Migration
down conn = do
  execute_ conn $ drop_column "posts" "posted_at"
  return ()

main :: IO ()
main = defaultMain up down

