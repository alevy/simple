{-# LANGUAGE FlexibleContexts #-}
module Common where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S8
import Data.Pool
import Database.PostgreSQL.Simple
import System.Environment
import Web.Simple.Controller

data AppSettings = AppSettings { appDB :: Pool Connection }

newAppSettings :: IO AppSettings
newAppSettings = do
  createConnectionPool Nothing >>= return . AppSettings

createConnectionPool :: Maybe S8.ByteString -> IO (Pool Connection)
createConnectionPool mconnectStr = do
  createPool (createConnection mconnectStr)
             (\c -> rollback c >> close c)
             1
             (fromInteger 60)
             20

createConnection :: Maybe S8.ByteString -> IO Connection
createConnection mconnectStr = do
  dbUrl <- case mconnectStr of
    Nothing -> do
      env <- getEnvironment
      return $ maybe S8.empty S8.pack $ lookup "DATABASE_URL" env
    Just str -> return str
  connectPostgreSQL dbUrl

withConnection :: AppSettings
               -> (Connection -> Controller b) -> Controller b
withConnection settings func = withResource (appDB settings) $ \conn -> do
  liftIO $ begin conn
  res <- func conn
  liftIO $ commit conn
  return res

