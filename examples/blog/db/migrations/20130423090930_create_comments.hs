import Database.PostgreSQL.Migration
import Database.PostgreSQL.Simple

up :: Migration
up conn = do
  execute_ conn $
    create_table "comments"
      [ "id serial PRIMARY KEY"
      , "name varchar(255) NOT NULL"
      , "email varchar(255) NOT NULL"
      , "comment text NOT NULL"
      , "post_id integer REFERENCES posts(id)"]
  return ()

down :: Migration
down conn = do 
  execute_ conn $ drop_table "comments"
  return ()

main :: IO ()
main = defaultMain up down

