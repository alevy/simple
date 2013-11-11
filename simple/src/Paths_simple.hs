{-# LANGUAGE CPP #-}
module Paths_simple where

import System.FilePath

getDataFileName :: FilePath -> IO FilePath
getDataFileName fp =  return $ (dropFileName __FILE__) </> ".." </> fp

