{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S8
import Data.Char
import Data.Int
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up conn = flip migrate conn $ do
  add_column "post" "stub" "varchar(32)"
  liftIO $ do
    posts <- query_ conn "select id, title from post"
    forM_ posts $ \(pid, title) -> do
      let stub = S8.take 32 $
            S8.map (\c -> if c == ' ' then '-' else toLower c) $
            S8.filter (\c -> c == ' ' || isAlphaNum c) title
      execute conn "update post set stub = ? where id = ?" (stub, pid :: Int64)
  change_column "post" "stub" "set NOT NULL"
  create_unique_index "post_stub_idx" "post" ["stub"]

down :: Connection -> IO ()
down = migrate $ do
  drop_column "post" "stub"

main :: IO ()
main = defaultMain up down

