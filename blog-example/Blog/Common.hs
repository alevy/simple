module Blog.Common
  ( AppSettings, newAppSettings
  , module Web.Simple.PostgreSQL
  ) where

import Control.Applicative
import Web.Simple
import Web.Simple.PostgreSQL
import Web.Simple.Templates
import Web.Simple.Session
import Blog.Helpers

data AppSettings = AppSettings { appDB :: PostgreSQLConn
                               , appSession :: Maybe Session }

newAppSettings :: IO AppSettings
newAppSettings = do
  db <- createPostgreSQLConn
  return $ AppSettings { appDB = db , appSession = Nothing }

instance HasPostgreSQL AppSettings where
  postgreSQLConn = appDB

instance HasSession AppSettings where
  getSession = appSession
  setSession sess = do
    cs <- controllerState
    putState $ cs { appSession = Just sess }

instance HasTemplates AppSettings where
  defaultLayout = Just <$> getTemplate "templates/main.html"
  functionMap = return helperFunctions

