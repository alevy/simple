{-# LANGUAGE FlexibleInstances #-}
module Database.Connection where

import Control.Exception (throwIO, SomeException)
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad.Trans.Resource
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

withConnection :: MonadCatchIO m => (Connection -> m b) -> m b
withConnection func = do
  conn <- liftIO $ readChan conns
  liftIO $ begin conn
  ee <- try $ func conn
  liftIO $ case ee of
    Right a -> do
      commit conn
      writeChan conns conn
      return a
    Left e -> do
      rollback conn
      writeChan conns conn
      throwIO (e :: SomeException)
      
  where try a = catch (a >>= \v -> return (Right v)) (\e -> return (Left e))

instance MonadCatchIO (ResourceT IO) where
  m `catch` f = liftIO $ (runResourceT m) `catch` \e -> runResourceT (f e)
  block = liftIO . block . runResourceT
  unblock = liftIO . block . runResourceT

