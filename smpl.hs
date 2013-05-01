{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (writeFile, FilePath)
import Control.Monad
import qualified Data.ByteString.Char8 as S8
import Data.List (sort)
import Data.String
import Filesystem
import Filesystem.Path hiding (concat)
import Filesystem.Path.CurrentOS hiding (concat)
import Language.Haskell.Interpreter hiding (name)
import Network.Wai.Handler.DevelServer (runQuit)
import System.Console.CmdArgs
import System.Environment

import Web.Simple.Migrations
import Web.Simple.Router ()

data Smpl =
    Server
      { port :: Int
      , moduleName :: String
      } |
    New
      { thing :: String
      , newArgs :: [String]} |
    Create { appName :: String } |
    Migrate { migrationDir :: String } |
    Rollback
      { steps :: Int }
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
                  , New { thing = "migration" &= argPos 0 &= typ "migration|..."
                        , newArgs = [] &= args }
                  , Create { appName = "" &= argPos 0 &= typ "APP_NAME" }
                  , Migrate { migrationDir = "migrate" &= typ "MIGRATIONS_DIR" &= explicit &= name "m" }
                  , Rollback { steps = 1 &= typ "INTEGER" }]
    smpl <- cmdArgsRun develMode
    case smpl of
      Server p m -> do
        putStrLn $ "Starting server at 0.0.0.0:" ++ (show myport)
        runQuit p m "app" (const $ return [])
      Rollback steps -> do
        fls <- listDirectory "migrate"
        whileI steps (reverse . sort $ fls) $ \f -> do
          let fileName = encodeString $ filename f
          let version = takeWhile (/= '_') fileName
          let mname = drop 1 $ dropWhile (/= '_') $
                        takeWhile (/= '.') fileName
          runRollback (encodeString f) version mname
      Migrate dir -> do
        fls <- listDirectory $ fromString dir
        migrateWithFiles fls
      New "migration" (name:[]) -> do
        putStrLn $ "Creating migration " ++ name
        newMigration name
      Create appName -> createApplication appName
      otherwise -> return ()
  where whileI 0 _ _ = return ()
        whileI _ [] _ = return ()
        whileI i (f:fs) act = do
          b <- act f
          if b then whileI (i - 1) fs act
            else whileI i fs act

migrateWithFiles :: [FilePath] -> IO ()
migrateWithFiles fls =
  forM_ (sort fls) $ \f -> do
    let fileName = encodeString $ filename f
    let version = takeWhile (/= '_') fileName
    let mname = drop 1 $ dropWhile (/= '_') $
                  takeWhile (/= '.') fileName
    runMigration (encodeString f) version mname

runMigration :: String -> String -> String -> IO Bool
runMigration fileName version mname = do
  eup <- runInterpreter $ do
    loadModules [fileName]
    setImports ["Prelude"]
    setTopLevelModules ["Main"]
    interpret "up" (undefined :: Migration)
  case eup of
    Right up -> do
                  res <- up version mname
                  when res $ putStrLn $ "=== " ++
                      "Finished migration " ++ mname ++ " (" ++ version ++ ")"
                  return res
    Left err -> fail $ show err

runRollback :: String -> String -> String -> IO Bool
runRollback fileName version mname = do
  edown <- runInterpreter $ do
    loadModules [fileName]
    setImports ["Prelude"]
    setTopLevelModules ["Main"]
    interpret "down" (undefined :: Migration)
  case edown of
    Right down -> do
                  res <- down version mname
                  when res $ putStrLn $ "=== " ++
                      "Finished rollback " ++ mname ++ " (" ++ version ++ ")"
                  return res
    Left err -> fail $ show err

createApplication :: String -> IO ()
createApplication appName = do
  createTree $ fromString appName </> "migrate"
  writeFile (fromString appName </> "Main.hs") mainTemplate

mainTemplate :: S8.ByteString
mainTemplate = S8.concat
  [ "{-# LANGUAGE OverloadedStrings #-}\n\n"
  , "module Main where\n\n"
  , "import Web.Simple\n\n"
  , "app runner = runner $ mkRouter $ okHtml \"Hello World\"\n\n"]

