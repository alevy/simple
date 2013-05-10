{-# LANGUAGE FlexibleContexts #-}
module Common where

import Control.Monad.IO.Class
import Data.Pool
import qualified Database.PostgreSQL.Connection as DB
import Database.PostgreSQL.Simple
import Web.Simple.Controller

data AppSettings = AppSettings { appDB :: Pool Connection }

withConnection :: AppSettings -> (Connection -> Controller a) -> Controller a
withConnection = DB.withConnection . appDB

newAppSettings :: IO AppSettings
newAppSettings = do
  DB.createConnectionPool Nothing >>= return . AppSettings

