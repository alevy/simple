module Database.Sequel where

import Control.Monad.State
import Data.List
import Data.String

type Sequel = StateT String IO

runSequel :: IsString s => Sequel a -> IO s
runSequel sql = fmap (fromString . snd) $ runStateT sql ""

append :: String -> Sequel ()
append str1 = modify $ \str0 -> concat $
  [ str0
  , "\n\n"
  , str1]

drop_table :: String -> Sequel ()
drop_table name = append $ "DROP TABLE " ++ name ++ ";"

create_table :: String -> CreateTable a -> Sequel ()
create_table name block = do
  blk <- runCreateTable $ do
    block
  append $ concat $
    [ "CREATE TABLE " ++ name ++ " (\n"
    , blk
    , "\n);"]

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
drop_column tableName colName = append $ concat $
  [ "ALTER TABLE "
  , tableName
  , " DROP COLUMN "
  , colName, ";"]

add_column :: String -> String -> ColumnType -> [ColumnConstraint] -> Sequel ()
add_column tableName colName colType ctrs = append $ concat $
  [ "ALTER TABLE "
  , tableName
  , " ADD COLUMN "
  , colName
  , " "
  , colType
  , " "
  , concat $ intersperse " " (map stringifyConstraint ctrs), ";"]

rename_column :: String -> String -> String -> Sequel ()
rename_column tableName fromName toName = append $ concat $
  [ "ALTER TABLE "
  , tableName
  , " RENAME COLUMN "
  , fromName
  , " TO "
  , toName
  , ";"]

type CreateTable = StateT [(ColumnType, String, [ColumnConstraint])] Sequel

runCreateTable :: CreateTable a -> Sequel String
runCreateTable ct = do
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

