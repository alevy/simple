{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (writeFile, FilePath)
import qualified Data.ByteString.Char8 as S8
import Network.Wai.Handler.DevelServer (runQuit)
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Environment

data Smpl =
    Server
      { port :: Int
      , moduleName :: String
      } |
    Create { appName :: String }
    deriving (Show, Data, Typeable)

main :: IO ()
main = do
    env <- getEnvironment
    let myport = maybe 3000 read $ lookup "PORT" env
    let develMode = cmdArgsMode $ modes
                  [ Server { port = myport &= typ "PORT"
                           , moduleName = "Main" &= typ "MODULE"
                                        &= explicit &= name "module"
                           } &= auto
                  , Create { appName = "" &= argPos 0 &= typ "APP_NAME" } ]
    smpl <- cmdArgsRun develMode
    case smpl of
      Server p m -> do
        putStrLn $ "Starting server at 0.0.0.0:" ++ (show myport)
        runQuit p m "app" (const $ return [])
      Create myAppName -> createApplication myAppName

createApplication :: String -> IO ()
createApplication myAppName = do
  createDirectoryIfMissing True $ myAppName </> "db" </> "migrations"
  S8.writeFile (myAppName </> "Main.hs") mainTemplate

mainTemplate :: S8.ByteString
mainTemplate = S8.concat
  [ "{-# LANGUAGE OverloadedStrings #-}\n\n"
  , "module Main where\n\n"
  , "import Web.Simple\n\n"
  , "app runner = runner $ mkRouter $ okHtml \"Hello World\"\n\n"]

