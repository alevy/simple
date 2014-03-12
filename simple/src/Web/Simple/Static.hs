module Web.Simple.Static where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Network.Wai
import Network.HTTP.Types
import Network.Mime
import Web.Simple.Controller
import System.Directory
import System.FilePath

serveStatic :: FilePath -> Controller IO a ()
serveStatic baseDir = do
  req <- request
  let fp = foldl (</>) baseDir (map T.unpack $ pathInfo req)
  exists <- liftIO $ doesFileExist fp
  when exists $ do
    liftIO $ do
      modTime <- getModificationTime fp
      print modTime
    respond $ responseFile status200
      [(hContentType, defaultMimeLookup $ T.pack $ takeFileName fp)]
      fp Nothing

