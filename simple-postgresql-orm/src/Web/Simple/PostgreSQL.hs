{-# LANGUAGE FlexibleInstances #-}
module Web.Simple.PostgreSQL
  ( module Web.Simple.PostgreSQL
  , module Database.PostgreSQL.ORM
  ) where

import Control.Monad
import qualified Data.ByteString.Char8 as S8
import Data.Pool
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Devel
import Database.PostgreSQL.Migrate
import Database.PostgreSQL.Simple
import GHC.Conc (numCapabilities)
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Web.Simple

type PostgreSQLConn = Pool Connection

class HasPostgreSQL hs where
  postgreSQLConn :: hs -> PostgreSQLConn

instance HasPostgreSQL PostgreSQLConn where
  postgreSQLConn = id

createPostgreSQLConn :: IO PostgreSQLConn
createPostgreSQLConn = do
  env <- getEnvironment
  let dev = maybe False (== "development") $ lookup "ENV" env
  when dev $ void $ do
    cwd <- getCurrentDirectory
    let dbdir = cwd </> "db" </> "development"
    putStrLn "Starting dev database..."
    initLocalDB dbdir
    startLocalDB dbdir
    setLocalDB dbdir
    initializeDb
    runMigrationsForDir stdout defaultMigrationsDir
    putStrLn "Dev database started..."
  let envConnect = maybe S8.empty S8.pack $ lookup "DATABASE_URL" env
  createPool (connectPostgreSQL envConnect) close numCapabilities 2 10

withConnection :: HasPostgreSQL hs
               => (Connection -> Controller hs b) -> Controller hs b
withConnection func = do
  pool <- postgreSQLConn `fmap` controllerState
  withResource pool func

