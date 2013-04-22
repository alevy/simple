{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Web.Simple.Cache 
  ( Cache(..), fetchOr
  , FileSystemCache(..)
  , InMemCache, newInMemCache
  ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.HashTable.IO as H
import System.FilePath
import System.Directory

class Cache c where
  put :: MonadIO m => c -> String -> L8.ByteString -> m L8.ByteString
  fetch :: MonadIO m => c -> String -> m (Maybe L8.ByteString)

  invalidate :: MonadIO m => c -> String -> m ()

fetchOr :: (Cache c, MonadIO m)
        => c
        -> String
        -> m L8.ByteString
        -> m L8.ByteString
fetchOr c path act = do
  mcached <- fetch c path
  maybe (act >>= put c path) return mcached

newtype FileSystemCache = FileSystemCache { fsCacheBase :: FilePath }

instance Cache FileSystemCache where
  put c path d = liftIO $ do
    let components =  splitDirectories $ dropDrive path
    let fullPath = (fsCacheBase c):components
    createDirectoryIfMissing True $
      joinPath $ reverse $ tail $ reverse fullPath
    L8.writeFile (joinPath fullPath) d
    return d

  fetch c path = liftIO $ do
    let components =  splitDirectories $ dropDrive path
    let fullPath = joinPath $ (fsCacheBase c):components
    exists <- doesFileExist fullPath
    if exists then
      fmap Just $ L8.readFile fullPath
      else return Nothing

  invalidate c path = liftIO $ do
    let components =  splitDirectories $ dropDrive path
    let fullPath = joinPath $ (fsCacheBase c):components
    exists <- doesFileExist fullPath
    when exists $
      removeFile fullPath

newtype InMemCache = InMemCache (H.BasicHashTable String L8.ByteString)

newInMemCache :: MonadIO m => m InMemCache
newInMemCache = liftIO $ fmap InMemCache H.new

instance Cache InMemCache where
  put (InMemCache h) path d = liftIO $ H.insert h path d >> return d
  fetch (InMemCache h) = liftIO . (H.lookup h)
  invalidate (InMemCache h) = liftIO . (H.delete h)

