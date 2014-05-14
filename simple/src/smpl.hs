{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- | The `smpl` utility for helping a user setup a Simple web project.
module Main (main) where

import Prelude hiding (writeFile, FilePath, all)
import Control.Applicative
import Control.Monad (when)
import Data.Aeson
import Data.Char
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text.Encoding as T
import Data.Monoid (mempty)
import Data.Version
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.Process
import Web.Simple.Templates.Language

import Paths_simple

data Smpl =
    Server
      { port :: Int
      , moduleName :: String
      } |
    Create { appDir :: FilePath
           , includeTemplates :: Bool
           , includePostgresql :: Bool
           , includeSessions :: Bool
           , includeAll :: Bool }
    deriving (Show, Data, Typeable)

main :: IO ()
main = do
    setEnv "ENV" "development"
    myenv <- getEnvironment
    let myport = maybe 3000 read $ lookup "PORT" myenv
    let develModes = modes
                  [ Server { port = myport &= typ "PORT"
                           , moduleName = "Application" &= typ "MODULE"
                                        &= explicit &= name "module"
                           } &= auto &= help "Run a development server"
                           &= details [
                            "You must have wai-handler-devel installed " ++
                            "to run this command"]
                  , Create { appDir = "" &= argPos 0 &= typ "app_dir"
                           , includeTemplates = False
                                     &= help "include templates"
                                     &= explicit &= name "templates"
                                     &= groupname "Plugins"
                           , includePostgresql = False
                                     &= help "include postgresql-orm"
                                     &= explicit &= name "postgresql"
                           , includeSessions = False
                                     &= help "include cookie-based sessions"
                                     &= explicit &= name "sessions"
                           , includeAll = False
                                     &= help
                                          ("include templates, cookie-based " ++
                                           "sessions and postgresql")
                                     &= explicit &= name "all"}
                          &= help "Create a new application in app_dir"]
    smpl <- cmdArgsRun $ cmdArgsMode $
      develModes &= (summary $
        "Simple web framework " ++ (showVersion version))
    case smpl of
      Server p m -> do
        exitCode <- rawSystem "wai-handler-devel" [show p, m, "app"]
        case exitCode of
          ExitFailure 127 -> do
            putStrLn "You must install wai-handler devel first"
            exitWith $ ExitFailure 1
          _ -> exitWith exitCode
      Create dir tmpls pg sess all ->
        createApplication dir (all || tmpls) (all || sess) (all || pg)

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

createApplication :: FilePath -> Bool -> Bool -> Bool -> IO ()
createApplication dir tmpls sessions postgresql = do
  let myAppName = takeBaseName $ dropTrailingPathSeparator dir
      modName = moduleCase myAppName
      mappings = object
                  [ "appname" .= myAppName
                  , "name" .= humanize myAppName
                  , "module" .= modName
                  , "include_templates" .= tmpls
                  , "include_sessions" .= sessions
                  , "include_postgresql" .= postgresql]

  createDirectory dir
  createDirectory $ dir </> modName
  copyTemplate ("template" </> "Main_hs.tmpl")
               (dir </> "Main.hs") mappings
  copyTemplate ("template" </> "Application_hs.tmpl")
               (dir </> "Application.hs") mappings
  copyTemplate ("template" </> "package_cabal.tmpl")
               (dir </> myAppName ++ ".cabal") mappings
  copyTemplate ("template" </> "Common_hs.tmpl")
               (dir </> modName </> "Common.hs") mappings

  when postgresql $ do
    createDirectory $ dir </> "db"
    createDirectory $ dir </> "db" </> "migrations"

  when tmpls $ do
    createDirectory $ dir </> "views"
    createDirectory $ dir </> "layouts"
    copyTemplate ("template" </> "main_html.tmpl")
                 (dir </> "layouts" </> "main.html") mappings
    copyTemplate ("template" </> "index_html.tmpl")
                 (dir </> "views" </> "index.html") mappings

copyTemplate :: FilePath -> FilePath -> Value -> IO ()
copyTemplate orig target mappings = do
  etmpl <- compileTemplate <$> T.decodeUtf8 <$>
    (S8.readFile =<< getDataFileName orig)
  case etmpl of
    Left err -> fail err
    Right tmpl -> S8.writeFile target $ T.encodeUtf8 $
      renderTemplate tmpl mempty mappings

