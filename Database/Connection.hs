module Database.Connection where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Concurrent
import Control.Exception (finally)
import Database.PostgreSQL.Simple
import Network.URI
import System.IO.Unsafe

{-# NOINLINE conns #-}
conns :: Chan Connection
conns = unsafePerformIO $ newChan

createConnection :: ConnectInfo -> IO ()
createConnection config = connect config >>= writeChan conns

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

withConnection :: MonadIO m => (Connection -> IO b) -> m b
withConnection func = liftIO $ do
  conn <- readChan conns
  finally (func conn) (writeChan conns conn)

