module Database.Migrations
  ( defaultMain
  , module Database.Sequel
  ) where

import Prelude hiding (catch)
import Control.Exception
import Database.Connection
import Database.Sequel
import Database.PostgreSQL.Simple
import System.Environment

defaultMain :: Sequel () -> Sequel () -> IO ()
defaultMain up down = do
  dbURI <- catch (getEnv "DATABASE_URL")
            (const $ return "postgres://" :: SomeException -> IO String)
  createConnection $ parseDbURL dbURI

  args <- getArgs
  case args of
    "--rollback":_ -> runSequel down >>= withConnection . (flip execute_)
    _ -> runSequel up >>= withConnection . (flip execute_)
  return ()
