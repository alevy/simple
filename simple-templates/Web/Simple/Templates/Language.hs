{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, CPP #-}
module Web.Simple.Templates.Language where

import Control.Applicative
import Control.Monad
import Data.Char (isAlphaNum)
import qualified Data.HashMap.Strict as H
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Attoparsec.Text as A

newtype Function = Function { call :: [Value] -> Value }

#define TypesConds(macro) \
  macro(a1 -> a2, \
        (FromJSON a1, FromJSON a2)); \
  macro(a1 -> a2 -> a3, \
        (FromJSON a1, FromJSON a2, FromJSON a3)); \
  macro(a1 -> a2 -> a3 -> a4, \
        (FromJSON a1, FromJSON a2, FromJSON a3, FromJSON a4)); \
  macro(a1 -> a2 -> a3 -> a4 -> a5, \
        (FromJSON a1, FromJSON a2, FromJSON a3, FromJSON a4, FromJSON a5)); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6, \
        (FromJSON a1, FromJSON a2, FromJSON a3, FromJSON a4, FromJSON a5, FromJSON a6)); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7, \
        (FromJSON a1, FromJSON a2, FromJSON a3, FromJSON a4, FromJSON a5, FromJSON a6, FromJSON a7)); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8, \
    (FromJSON a1, FromJSON a2, FromJSON a3, FromJSON a4, FromJSON a5, FromJSON a6, FromJSON a7, FromJSON a8))

class ToFunction a where
  toFunction :: a -> Function

fromJSONStrict :: FromJSON a => Value -> a
fromJSONStrict val = case fromJSON val of
                  Error err -> error err
                  Success result -> result

#define TOFUNCTION(types, conds) \
instance (conds) => ToFunction (types -> Value) where { \
  toFunction f = Function $ \(a:as) -> call (toFunction (f $ fromJSONStrict a)) as; \
}


instance (FromJSON a) => ToFunction (a -> Value) where
  toFunction f = Function $ \(a:_) -> toJSON $ f $ fromJSONStrict a

TypesConds(TOFUNCTION)

type FunctionMap = H.HashMap Text Function

newtype Template = Template { unTemplate :: FunctionMap -> Value -> Text }
  deriving (Monoid)

reservedWords :: [Text]
reservedWords = ["for", "endfor", "if", "else", "endif"]

compileTemplate :: Text -> Either String Template
compileTemplate tmpl = A.parseOnly pTemplate tmpl

pTemplate :: A.Parser Template
pTemplate = mconcat <$> many (pFor <|>
                              pIf <|>
                              pVar <|>
                              pRaw <|>
                              pEscapedDollar)

raw :: Text -> Template
raw = Template . const . const

pEscapedDollar :: A.Parser Template
pEscapedDollar = raw "$" <$ A.string "$$"

pRaw :: A.Parser Template
pRaw = raw <$> A.takeWhile1 (/= '$')

pIf :: A.Parser Template
pIf = do
  -- \$if(x)
  A.string "$if("
  varName <- pIdent
  A.string ")$"
  trueBranch <- pTemplate
  falseBranch <- A.option mempty $ A.string "$else$" *> pTemplate
  A.string "$endif$"
  return $ Template $ \fm global ->
    let v = resolve global fm varName
    in case v of
          Null -> unTemplate falseBranch fm global
          Bool False -> unTemplate falseBranch fm global
          _ -> unTemplate trueBranch fm global

pFor :: A.Parser Template
pFor = do
  -- \$for(x in xs)
  A.string "$for("
  varName <- pIdentPart
  A.string " in "
  listName <- pIdent
  A.string ")$"
  A.option ' ' $ A.skipWhile (\x -> x == ' ' || x == '\t') *> A.char '\n'

  contents <- pTemplate

  -- \$endfor
  A.string "$endfor$"
  A.skipSpace
  return $ Template $ \fm global ->
      let v = resolve global fm listName
      in case v of
        Array vec -> mconcat $
                      map (withVar contents fm global varName) $
                        V.toList vec
        Null -> mempty
        Bool False -> mempty
        x -> withVar contents fm global varName x
  where withVar :: Template -> FunctionMap -> Value -> Text -> Value -> Text
        withVar (Template inner) fm global varName val =
          inner fm (replaceVar global varName val)

replaceVar :: Value -> Text -> Value -> Value
replaceVar (Object orig) varName newVal = Object $ H.insert varName newVal orig
replaceVar orig varName newVal = object ["@" .= orig, varName .= newVal]

pVar :: A.Parser Template
pVar =  evaluate <$> (A.char '$' *> pIdent <* A.char '$')

data Evaluable = EvaluableFunc Text [Evaluable]
               | EvaluableVar VariableName
               | EvaluableValue Value
               | EvaluableArray [Evaluable]

pIdent :: A.Parser Evaluable
pIdent = pLiteral <|> pFunc <|> pVarName <|> pSpecial
  where pVarName = do
          first <- pIdentPart
          rest <- many (A.char '.' *> pIdentPart)
          return $ EvaluableVar (first:rest)
        pSpecial = EvaluableVar <$> (:[]) <$> A.string "@"
        pFunc = do
          funcName <- pIdentPart
          A.char '('
          args <- pIdent `A.sepBy` (A.char ',' >> A.option ' ' (A.char ' '))
          A.char ')'
          return $ EvaluableFunc funcName args
        pLiteral = literal

literal :: A.Parser Evaluable
literal =
  EvaluableValue <$>
    (Number <$> A.number <|>
    String <$> (A.string "'" *> A.takeWhile (/= '\'') <* A.string "'") <|>
    Bool <$> (A.string "false" >> pure False) <|>
    Bool <$> (A.string "true" >> pure True) <|>
    (A.string "null" >> pure Null)) <|>
  EvaluableArray <$>
    (A.char '[' *> pIdent `A.sepBy` (A.char ',' *> A.skipSpace) <* A.char ']')

pIdentPart :: A.Parser Text
pIdentPart = do
  a <- T.singleton <$> A.letter
  rst <- A.takeWhile (\c -> isAlphaNum c || c == '_' || c == '-')
  let ident = a <> rst
  guard $ ident `notElem` reservedWords
  return $ ident 

type VariableName = [Text]

resolve :: Value -> FunctionMap -> Evaluable -> Value
resolve global _ (EvaluableVar varName) = resolveVar global varName
resolve global fm (EvaluableFunc funcName args) =
  resolveFunc global fm funcName args
resolve _ _ (EvaluableValue val) = val
resolve global fm (EvaluableArray arr) =
  Array . V.fromList $ map (resolve global fm) arr

resolveFunc :: Value -> FunctionMap -> Text -> [Evaluable] -> Value
resolveFunc global fm funcName args =
  case funcName `H.lookup` fm of
    Nothing -> Null
    Just (Function func) -> func $ map (resolve global fm) args

resolveVar :: Value -> VariableName -> Value
resolveVar global varName =
  case varName of
    ["@"] -> global
    _ -> multiLookup varName global

evaluate :: Evaluable -> Template
evaluate ev = Template $ \fm global -> evaluateValue $ resolve global fm ev

evaluateValue :: Value -> Text
evaluateValue val =
  case val of
    String str -> str
    Number n -> T.pack $ show n
    Bool b -> T.pack $ show b
    Array _ -> "[array]"
    Object _ -> "[object]"
    Null -> "NULL"

multiLookup :: VariableName -> Value -> Value
multiLookup [] val = val
multiLookup (x:xs) (Object obj) = maybe Null (multiLookup xs) $ H.lookup x obj
multiLookup _ _ = Null

