{-# LANGUAGE FlexibleContexts #-}
module Database.Connection where

import Data.Pool
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Database.PostgreSQL.Simple
import Network.URI
import System.IO.Unsafe
import System.Environment

{-# NOINLINE conns #-}
conns :: Pool Connection
conns = unsafePerformIO $ do
  dbUrl <- getEnv "DATABASE_URL"
  let creator = createConnection $ parseDbURL dbUrl
  createPool creator
             (\c -> rollback c >> close c)
             1
             (fromInteger 60)
             20

createConnection :: ConnectInfo -> IO Connection
createConnection config = connect config

parseDbURL :: String -> ConnectInfo
parseDbURL uri = case mdbURI of
    Nothing -> defaultConnectInfo
    Just dbURI | uriScheme dbURI == "postgres:" ->
      let auth = uriAuthority dbURI
      in ConnectInfo { connectPort = toPort auth
                  , connectHost = toHostname auth
                  , connectUser = toUsername auth
                  , connectPassword = toPassword auth
                  , connectDatabase = toDatabase . uriPath $ dbURI}
      | otherwise -> defaultConnectInfo
  where mdbURI = parseURI uri
        toPort (Just (URIAuth _ _ "")) = 5432
        toPort (Just (URIAuth _ _ (':':p))) = read p
        toPort _ = 5432
        toHostname (Just (URIAuth _ h _)) = h
        toHostname _ = "localhost"
        toUsername (Just (URIAuth ua _ _)) = takeWhile (\x -> x /= ':' && x /= '@') ua
        toUsername Nothing = ""
        toPassword (Just (URIAuth ua _ _)) = case dropWhile (/= ':') ua of
                                               (':':password) -> takeWhile (/= '@') password
                                               _ -> ""
        toPassword Nothing = ""
        toDatabase ('/':db) = db
        toDatabase _ = ""

withConnection :: (MonadIO m, MonadBaseControl IO m) => (Connection -> m b) -> m b
withConnection func = withResource conns $ \conn -> do
  liftIO $ begin conn
  res <- func conn
  liftIO $ commit conn
  return res

