{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- | The `smpl` utility for helping a user setup a Simple web project.
module Main (main) where

import Prelude hiding (writeFile, FilePath)
import Control.Monad
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.Search as L8
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Environment
import System.Process

import Paths_simple

data Smpl =
    Server
      { port :: Int
      , moduleName :: String
      } |
    Create { appName :: String }
    deriving (Show, Data, Typeable)

main :: IO ()
main = do
    myenv <- getEnvironment
    let myport = maybe 3000 read $ lookup "PORT" myenv
    let develMode = cmdArgsMode $ modes
                  [ Server { port = myport &= typ "PORT"
                           , moduleName = "Main" &= typ "MODULE"
                                        &= explicit &= name "module"
                           } &= auto
                  , Create { appName = "" &= argPos 0 &= typ "APP_NAME" } ]
    smpl <- cmdArgsRun develMode
    case smpl of
      Server p m -> void $
        rawSystem "wai-handler-devel" [show p, m, "app"]
      Create myAppName -> createApplication myAppName

createApplication :: String -> IO ()
createApplication myAppName = do
  createDirectory myAppName
  let mappings = [("pkgname", myAppName)]
  copyTemplate ("template" </> "Main_hs.tmpl")
               (myAppName </> "Main.hs") mappings
  copyTemplate ("template" </> "package_cabal.tmpl")
               (myAppName </> myAppName ++ ".cabal") mappings

copyTemplate :: FilePath -> FilePath -> [(String, String)] -> IO ()
copyTemplate orig target mappings = do
  tmpl <- getDataFileName orig >>= L8.readFile
  L8.writeFile target $
    (flip . flip foldl) tmpl mappings $ \accm (key, val) ->
      L8.replace (S8.pack $ "{{" ++ key ++ "}}") (L8.pack val) accm

