module Web.Simple.Migrations
  ( Migration
  , newMigration
  ) where

import System.Locale
import System.Time

type Migration =  String -- ^ Version
               -> String -- ^ Migration name
               -> IO Bool

newMigration :: String -> IO ()
newMigration name = do
  time <- getClockTime >>= return . toUTCTime
  let timestr = formatCalendarTime defaultTimeLocale
                  "%Y%m%d%H%M%S" time
  writeFile ("migrate/" ++ timestr ++ "_" ++ name ++ ".hs") migrationTemplate

migrationTemplate :: String
migrationTemplate = concat
  [ "import Web.Simple.Migrations\n\n"
  , "up :: Migration\n"
  , "up = const . const $ return False\n\n"
  , "down :: Migration\n"
  , "down = const . const $ return False\n\n"]

