{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Provides a general caching interface along with a simple in-memory
-- (process only) and file based cache implementations.
module Web.Simple.Cache 
  ( -- * Cache Interface
    Cache(..), fetchOr
    -- * Cache Implementations
  , FileSystemCache, newFileSystemCache
  , InMemCache, newInMemCache
  ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.HashTable.IO as H
import System.FilePath
import System.Directory

-- | A class that captures a simple key-value caching interface. The keys are
-- simply 'String'\'s and values are simply 'ByteString'\'s.
class Cache c where
  put        :: MonadIO m => c -> String -> L8.ByteString -> m L8.ByteString
  -- ^ Store a value in the cache. The returned value should be the value that
  -- was just stored.
  fetch      :: MonadIO m => c -> String -> m (Maybe L8.ByteString)
  -- ^ Retrieve a value from the cache.
  invalidate :: MonadIO m => c -> String -> m ()
  -- ^ Invalidate a potentially existing value in the cache. Depending on the
  -- implementation this may or may not free the space used by the key-value
  -- pair.

fetchOr :: (Cache c, MonadIO m)
        => c
        -> String
        -> m L8.ByteString
        -> m L8.ByteString
fetchOr c path act = do
  mcached <- fetch c path
  maybe (act >>= put c path) return mcached

-- | A file based cache implementation. Files are stored in subdirectories of
-- @fsCacheBase@.
newtype FileSystemCache = FileSystemCache { fsCacheBase :: FilePath }

-- | Create a new @FileSystemCache@.
newFileSystemCache :: FilePath -> FileSystemCache
newFileSystemCache = FileSystemCache

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

-- | An in-memory cache implementation. The current processes heap space is
-- simply used as the cache.
newtype InMemCache = InMemCache (H.BasicHashTable String L8.ByteString)

-- | Create a new @InMemCache@.
newInMemCache :: MonadIO m => m InMemCache
newInMemCache = liftIO $ fmap InMemCache H.new

instance Cache InMemCache where
  put (InMemCache h) path d = liftIO $ H.insert h path d >> return d
  fetch (InMemCache h) = liftIO . (H.lookup h)
  invalidate (InMemCache h) = liftIO . (H.delete h)

