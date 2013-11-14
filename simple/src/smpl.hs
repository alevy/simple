{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- | The `smpl` utility for helping a user setup a Simple web project.
module Main (main) where

import Prelude hiding (writeFile, FilePath)
import Data.Char
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.Search as L8
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.SetEnv
import System.Process

import Paths_simple

data Smpl =
    Server
      { port :: Int
      , moduleName :: String
      } |
    Create { appDir :: FilePath }
    deriving (Show, Data, Typeable)

main :: IO ()
main = do
    setEnv "ENV" "development"
    myenv <- getEnvironment
    let myport = maybe 3000 read $ lookup "PORT" myenv
    let develMode = cmdArgsMode $ modes
                  [ Server { port = myport &= typ "PORT"
                           , moduleName = "Application" &= typ "MODULE"
                                        &= explicit &= name "module"
                           } &= auto
                  , Create { appDir = "" &= argPos 0 &= typ "app_dir" } ]
    smpl <- cmdArgsRun develMode
    case smpl of
      Server p m -> do
        exitCode <- rawSystem "wai-handler-devel" [show p, m, "app"]
        case exitCode of
          ExitFailure 127 -> do
            putStrLn "You must install wai-handler devel first"
            exitWith $ ExitFailure 1
          _ -> exitWith exitCode
      Create dir -> createApplication dir

humanize :: String -> String
humanize = capitalize
  where go [] = []
        go ('_':xs) = ' ':(capitalize xs)
        go (x:xs) = x:(go xs)
        capitalize [] = []
        capitalize x@('_':_) = go x
        capitalize (x:xs) = (toUpper x):(go xs)

moduleCase :: String -> String
moduleCase = capitalize
  where go [] = []
        go ('_':xs) = capitalize xs
        go (x:xs) = x:(go xs)
        capitalize [] = []
        capitalize ('_':xs) = go xs
        capitalize (x:xs) = (toUpper x):(go xs)

createApplication :: FilePath -> IO ()
createApplication dir = do
  let myAppName = takeBaseName $ dropTrailingPathSeparator dir
      modName = moduleCase myAppName
      mappings = [ ("appname", myAppName)
                 , ("name", humanize myAppName)
                 , ("module", modName)]

  createDirectory dir
  createDirectory $ dir </> "db"
  createDirectory $ dir </> "db" </> "migrations"
  createDirectory $ dir </> "views"
  createDirectory $ dir </> "templates"
  createDirectory $ dir </> modName

  copyTemplate ("template" </> "Main_hs.tmpl")
               (dir </> "Main.hs") mappings
  copyTemplate ("template" </> "Application_hs.tmpl")
               (dir </> "Application.hs") mappings
  copyTemplate ("template" </> "package_cabal.tmpl")
               (dir </> myAppName ++ ".cabal") mappings
  copyTemplate ("template" </> "main_html.tmpl")
               (dir </> "templates" </> "main.html") mappings
  copyTemplate ("template" </> "index_html.tmpl")
               (dir </> "views" </> "index.html") mappings
  copyTemplate ("template" </> "Common_hs.tmpl")
               (dir </> modName </> "Common.hs") mappings

copyTemplate :: FilePath -> FilePath -> [(String, String)] -> IO ()
copyTemplate orig target mappings = do
  tmpl <- getDataFileName orig >>= L8.readFile
  L8.writeFile target $
    (flip . flip foldl) tmpl mappings $ \accm (key, val) ->
      L8.replace (S8.pack $ "{{" ++ key ++ "}}") (L8.pack val) accm

