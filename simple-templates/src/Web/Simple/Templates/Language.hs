{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, CPP #-}
module Web.Simple.Templates.Language where

import Control.Applicative
import Control.Monad
import Data.Char (isAlphaNum)
import qualified Data.HashMap.Strict as H
import Data.Maybe
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

type FunctionMap = H.HashMap Identifier Function

newtype Template = Template
  { unTemplate :: FunctionMap -> Value -> (Text, Value) }

instance Monoid Template where
  mempty = Template $ const $ \global -> (mempty, global)
  tm1 `mappend` tm2 = Template $ \fm global ->
    let (txt1, newGlobal) = unTemplate tm1 fm global
        (txt2, newerGlobal) = unTemplate tm2 fm newGlobal
    in (txt1 `mappend` txt2, newerGlobal)

-- | A symbol identifier following the format [a-z][a-zA-Z0-9_-]*
type Identifier = Text

-- | 'AST's encode the various types of expressions in the language.
data AST = ASTRoot [AST]
         | ASTLiteral Value
         | ASTFunc Identifier [AST]
         | ASTVar Identifier
         | ASTIndex AST [Identifier]
         | ASTArray (V.Vector AST)
         | ASTIf AST AST (Maybe AST)
         | ASTFor Identifier AST AST (Maybe AST)
  deriving (Show, Eq)

fromLiteral :: ToJSON a => a -> AST
fromLiteral = ASTLiteral . toJSON

evaluateAST :: FunctionMap -> Value -> AST -> (Value, Value)
evaluateAST fm global ast =
  case ast of
    ASTRoot asts -> foldl (\(v, g) iast ->
                            let (val, ng) = evaluateAST fm g iast
                            in (String $ valueToText v <> valueToText val, ng))
                          (String "", global) asts

    ASTLiteral val -> (val, global)

    ASTFunc ident args ->
      case H.lookup ident fm of
        Nothing -> (Null, global)
        Just func ->
          let argVals = map (fst . evaluateAST fm global) args
          in (call func argVals, global)

    ASTVar ident ->
      let val = if ident == "@" then global else
                  case global of
                    Object obj -> fromMaybe Null $ H.lookup ident obj
                    _ -> Null
      in (val, global)

    ASTIndex objAst idents ->
      foldl (\(val, newGlobal) ident -> 
        case val of
          Object obj -> (fromMaybe Null $ H.lookup ident obj, newGlobal)
          _ -> (Null, newGlobal)) (evaluateAST fm global objAst) idents

    ASTArray asts -> (Array $ V.map (fst . evaluateAST fm global) asts, global)

    ASTIf cond trueBranch mfalseBranch ->
      let (condVal, newGlobal) = evaluateAST fm global cond
          falseBranch = fromMaybe (ASTLiteral $ String "") mfalseBranch
      in if condVal == Null || condVal == Bool False then
           evaluateAST fm newGlobal trueBranch
           else evaluateAST fm newGlobal falseBranch

    ASTFor varName lst body msep -> astForLoop fm global varName lst body msep

astForLoop :: FunctionMap -> Value
           -> Identifier -> AST -> AST -> Maybe AST -> (Value, Value)
astForLoop fm global varName lst body msep =
  case val of
    Null -> (String "", newGlobal)
    Bool False -> (String "", newGlobal)
    Array vec ->
      (String $ go (V.toList vec) mempty, newGlobal)
    v -> evaluateAST fm (replaceVar newGlobal varName v) body
  where sep = maybe (String "") (fst . evaluateAST fm global) msep
        (val, newGlobal) = evaluateAST fm global lst
        go [] accm = accm
        go (v:[]) accm =
          let scope = replaceVar global varName v
              nv = fst $ evaluateAST fm scope body
          in valueToText v <> valueToText nv <> accm
        go (v:x1:xs) accm =
          let scope = replaceVar global varName v
              nv = fst $ evaluateAST fm scope body
              accmN =
                valueToText v <> valueToText nv <> valueToText sep <> accm
          in go (x1:xs) accmN

replaceVar :: Value -> Identifier -> Value -> Value
replaceVar (Object orig) varName newVal = Object $ H.insert varName newVal orig
replaceVar _ varName newVal = object [varName .= newVal]

evaluate :: AST -> Template
evaluate ast = Template $ \fm global ->
  let (v, g) = evaluateAST fm global ast
  in (valueToText v, g)

valueToText :: Value -> Text
valueToText val =
  case val of
    String str -> str
    Number n -> T.pack $ show n
    Bool b -> T.pack $ show b
    Array _ -> "[array]"
    Object _ -> "[object]"
    Null -> "null"

reservedWords :: [Text]
reservedWords =
  [ "for", "endfor", "sep"
  , "if", "else", "endif"
  , "true", "false"]

compileTemplate :: Text -> Either String Template
compileTemplate tmpl = evaluate <$>
  A.parseOnly pAST tmpl

pAST :: A.Parser AST
pAST = ASTRoot <$> many (pRaw <|> pEscapedExpr)

pRaw :: A.Parser AST
pRaw = ASTLiteral . String . mconcat <$> (A.many1 $ do
  dollar <- A.option mempty pEscapedDollar
  txt <- A.takeWhile1 (/= '$')
  return $ dollar <> txt)

pEscapedDollar :: A.Parser Text
pEscapedDollar = A.char '$' *> A.string "$"

pEscapedExpr :: A.Parser AST
pEscapedExpr = do
  A.char '$' *> pExpr <* A.char '$'

-- | Anything that can be evaluated: for, if or value
pExpr :: A.Parser AST
pExpr = pIf <|> pValue

pIf :: A.Parser AST
pIf = do
  A.string "if"
  brace <- A.satisfy (\c -> c == ' ' || c == '(')
  cond <- pValue
  when (brace == '(') $ A.char ')' >> return ()
  A.char '$'
  trueBranch <- pAST
  falseBranch <- A.option Nothing $ do
    A.string "$else$"
    Just <$> pAST
  A.string "$endif"
  return $ ASTIf cond trueBranch falseBranch

pFor :: A.Parser AST
pFor = do
  A.string "for"
  brace <- A.satisfy (\c -> c == ' ' || c == '(')
  varName <- pIdentifier
  A.string " in "
  lst <- pValue
  when (brace == '(') $ A.char ')' >> return ()
  A.char '$'
  loop <- pAST
  sep <- A.option Nothing $ do
    A.string "$sep$"
    Just <$> pAST
  A.string "$endfor"
  return $ ASTFor varName lst loop sep

-- | A variable, function call, literal, etc
pValue :: A.Parser AST
pValue = pFunc <|> pIndex <|> pVar <|> pLiteral

pFunc :: A.Parser AST
pFunc = do
  funcName <- pIdentifier
  A.char '('
  args <- pValue `A.sepBy` (A.skipSpace *> A.char ',' *> A.skipSpace)
  A.char ')'
  return $ ASTFunc funcName args

pVar :: A.Parser AST
pVar = ASTVar <$> pIdentifier

pIndex :: A.Parser AST
pIndex = do
  first <- pIdentifier <* A.char '.'
  rst <- pIdentifier `A.sepBy` A.char '.'
  return $ ASTIndex (ASTVar first) $ rst

pIdentifier :: A.Parser Identifier
pIdentifier = do
  a <- T.singleton <$> A.letter
  rst <- A.takeWhile (\c -> isAlphaNum c || c == '_' || c == '-')
  let ident = a <> rst
  guard $ ident `notElem` reservedWords
  return ident 

-- Literals --

pLiteral :: A.Parser AST
pLiteral = pArray <|>
           pNumber <|>
           pString <|>
           pBoolean <|>
           pNull

pNull :: A.Parser AST
pNull = A.string "null" *> (return $ ASTLiteral Null)

pBoolean :: A.Parser AST
pBoolean = A.string "true" *> (return $ fromLiteral True) <|>
           A.string "false" *> (return $ fromLiteral False)

pString :: A.Parser AST
pString = ASTLiteral . String <$>
  (A.char '"' *> (T.pack <$> many escapedChar) <* A.char '"')
  where escapedChar = (A.char '\\' *> A.char '"') <|>
                      A.satisfy (/= '"')

pNumber :: A.Parser AST
pNumber = ASTLiteral . Number <$>
  A.number

pArray :: A.Parser AST
pArray = do
  A.char '['
  A.skipSpace
  vals <- pValue `A.sepBy` (A.skipSpace *> A.char ',' *> A.skipSpace)
  A.skipSpace
  A.char ']'
  return $ ASTArray $ V.fromList vals

