{-# LANGUAGE FlexibleContexts #-}
module Common where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S8
import Database.PostgreSQL.Simple
import System.Environment
import Web.Simple.Controller

data AppSettings = AppSettings { appDB :: MVar Connection }

newAppSettings :: IO AppSettings
newAppSettings = do
  createConnection Nothing >>= newMVar >>= return . AppSettings

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
withConnection settings func = do
  let dbvar = (appDB settings)
  conn <- liftIO $ takeMVar dbvar
  liftIO $ begin conn
  res <- func conn
  liftIO $ commit conn
  return res

