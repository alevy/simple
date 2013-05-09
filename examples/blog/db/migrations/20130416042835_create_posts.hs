import Database.PostgreSQL.Migration
import Database.PostgreSQL.Simple

up :: Migration
up conn = do
  execute_ conn $
    create_table "posts" $
      [ "id  serial PRIMARY KEY"
      , "title varchar(255)"
      , "body text" ]
  return ()

down :: Migration
down conn = do
  execute_ conn $ drop_table "posts"
  return ()

main :: IO ()
main = defaultMain up down

