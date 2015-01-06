{-# LANGUAGE OverloadedStrings #-}
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

serveStatic :: FilePath -> Controller a ()
serveStatic baseDir = do
  req <- request
  let fp = foldl (</>) baseDir (map T.unpack $ pathInfo req)
  exists <- liftIO $ doesFileExist fp
  when exists $ do
    respond $ responseFile status200
      [(hContentType, defaultMimeLookup $ T.pack $ takeFileName fp)]
      fp Nothing
  when (null $ takeExtension fp) $ do
    let fpIdx = fp </> "index.html"
    existsIdx <- liftIO $ doesFileExist fpIdx
    when existsIdx $ do
      respond $ responseFile status200
        [(hContentType, "text/html")]
        fpIdx Nothing

