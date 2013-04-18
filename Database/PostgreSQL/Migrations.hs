{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.Migrations
  ( runDb, dbUp, dbDown, initDb
  , module Database.PostgreSQL.Sequel
  ) where

import Prelude
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Connection
import Database.PostgreSQL.Sequel
import Database.PostgreSQL.Simple
import Web.Simple.Migrations

runDb :: Sequel a -> IO a
runDb act = withConnection $ \conn -> runSequel act conn

dbUp :: Sequel a -> Migration
dbUp act version name = runDb $ do
  (Only count:_) <-
    sqlQuery "select count(version) from schema_migrations where version = ?"
              [version]
  if (count :: Int) > 0 then
    return False
    else do
          liftIO $ putStrLn $
            "=== Running migration: " ++ name ++ " (" ++ version ++ ")"
          act
          sqlExecute "insert into schema_migrations (version) values(?)"
                     (Only version)
          return True

dbDown :: Sequel a -> Migration
dbDown act version name = runDb $ do
  (Only count:_) <-
    sqlQuery "select count(version) from schema_migrations where version = ?"
              [version]
  if (count :: Int) == 0 then
    return False
    else do
          liftIO $ putStrLn $
            "=== Rolling back: " ++ name ++ " (" ++ version ++ ")"
          act
          sqlExecute "delete from schema_migrations where version = ?"
                     (Only version)
          return True

initDb :: Migration
initDb _ _ = runDb $ do
  (Only count:_) <- sqlQuery "select count(*) from pg_class where relname = ?"
                      (Only ("schema_migrations" :: String))
  if (count :: Int) /= 0 then
    return False
    else do
          liftIO $ putStrLn "Creating table `schema_migrations`"
          create_table "schema_migrations" $
            column "version" string [UNIQUE]
          return True

