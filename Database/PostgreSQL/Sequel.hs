{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.Sequel where

import Database.PostgreSQL.Simple
import Control.Monad.Identity
import Control.Monad.State
import Data.List
import Data.String

data Sequel a = Sequel { runSequel :: Connection -> IO a }

instance Monad Sequel where
  return a = Sequel $ const $ return a
  (>>=) (Sequel run) f = Sequel $ \conn ->
    run conn >>= \res -> runSequel (f res) conn

instance MonadIO Sequel where
  liftIO = Sequel . const

drop_table :: String -> Sequel ()
drop_table name = sqlExecute_ (fromString $ "DROP TABLE " ++ name ++ ";")

create_table :: String -> CreateTable a -> Sequel ()
create_table name block = do
  let blk = runCreateTable block
  let execStr = fromString $ concat $
            [ "CREATE TABLE "
            , name
            , " (\n"
            , blk
            , "\n);"]
  sqlExecute_ execStr

type ColumnType = String

serial :: ColumnType
serial = "serial"

integer :: ColumnType
integer = "integer"

time :: ColumnType
time = "time"

varchar :: Integer -> ColumnType
varchar size = "varchar(" ++ (show size) ++ ")"

string :: ColumnType
string = varchar 255

text :: ColumnType
text = "text"

boolean :: ColumnType
boolean = "boolean"

data ColumnConstraint = NOT_NULL
                      | UNIQUE
                      | PRIMARY_KEY
                      | DEFAULT String
                      | REFERENCES String String

stringifyConstraint :: ColumnConstraint -> String
stringifyConstraint NOT_NULL = "NOT NULL"
stringifyConstraint UNIQUE = "UNIQUE"
stringifyConstraint PRIMARY_KEY = "PRIMARY KEY"
stringifyConstraint (DEFAULT str) = "DEFAULT " ++ str
stringifyConstraint (REFERENCES table col) =
  "REFERENCES " ++ table ++ "(" ++ col ++ ")"

drop_column :: String -> String -> Sequel ()
drop_column tableName colName = sqlExecute_ $ fromString $ concat
    ["ALTER TABLE "
    , tableName
    , " DROP COLUMN "
    , colName
    , ";"]

add_column :: String -> String -> ColumnType -> [ColumnConstraint] -> Sequel ()
add_column tableName colName colType ctrs = sqlExecute_ $ fromString $ concat
  [ "ALTER TABLE "
  , tableName
  , " ADD COLUMN "
  , colName
  , " "
  , colType
  , " "
  , concat $ intersperse " " (map stringifyConstraint ctrs), ";"]

rename_column :: String -> String -> String -> Sequel ()
rename_column tableName fromName toName = sqlExecute_ $ fromString $ concat
  [ "ALTER TABLE "
  , tableName
  , " RENAME COLUMN "
  , fromName
  , " TO "
  , toName, ";"]

type CreateTable = StateT [(ColumnType, String, [ColumnConstraint])] Identity

runCreateTable :: CreateTable a -> String
runCreateTable ct = runIdentity $ do
  (_, cols) <- runStateT ct []
  let colStrs = map
        (\(t, name, crts) ->
            "  " ++ name ++ " " ++ t ++ " " ++
            (concat $ intersperse " " (map stringifyConstraint crts))
        ) cols
  return $ concat $ intersperse ",\n" (reverse colStrs)

column :: String -> ColumnType -> [ColumnConstraint] -> CreateTable ()
column colName colType constraints = do
  modify $ \cols -> (colType, colName, constraints):cols

sqlQuery :: (ToRow q, FromRow r) => Query -> q -> Sequel [r]
sqlQuery myq params = Sequel $ \conn -> query conn myq params

sqlQuery_ :: FromRow r => Query -> Sequel [r]
sqlQuery_ myq = Sequel $ \conn -> query_ conn myq

sqlExecute :: ToRow q => Query -> q -> Sequel ()
sqlExecute myq params = Sequel $ \conn -> execute conn myq params >> return ()

sqlExecute_ :: Query -> Sequel ()
sqlExecute_ myq = Sequel $ \conn -> execute_ conn myq >> return ()

