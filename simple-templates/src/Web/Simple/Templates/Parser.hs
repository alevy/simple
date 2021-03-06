{-# LANGUAGE OverloadedStrings, Trustworthy #-}

{-| Language parser -}
module Web.Simple.Templates.Parser
  ( reservedWords
  , pAST
  , pRaw
  , pEscapedDollar
  , pEscapedExpr, pExpr
  , pIf, pFor
  , pFunc, pValue, pVar
  , pIndex, pIdentifier, pLiteral, pNull, pBoolean, pString, pNumber, pArray
  , module Web.Simple.Templates.Types
  ) where

import Control.Applicative
import Control.Monad
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Attoparsec.Text as A
import Web.Simple.Templates.Types

-- | Reserved words: for, endfor, sep, if, else, endif, true, false
reservedWords :: [Text]
reservedWords =
  [ "for", "endfor", "sep"
  , "if", "else", "endif"
  , "true", "false"]

-- | Parse an AST
pAST :: A.Parser AST
pAST = ASTRoot <$> many (pRaw <|> pEscapedExpr)

pRaw :: A.Parser AST
pRaw = ASTLiteral . String . mconcat <$> (A.many1 $
  A.takeWhile1 (/= '$') <|> pEscapedDollar)

pEscapedDollar :: A.Parser Text
pEscapedDollar = A.string "$$" >> return "$"

pEscapedExpr :: A.Parser AST
pEscapedExpr = do
  A.char '$' *> pExpr <* A.char '$'

-- | Anything that can be evaluated: for, if or value
pExpr :: A.Parser AST
pExpr = pFor <|> pIf <|> pValue

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
  mkeyName <- optional $ pIdentifier <* A.char ','
  valName <- pIdentifier
  A.string " in "
  lst <- pValue
  when (brace == '(') $ A.char ')' >> return ()
  A.char '$'
  loop <- pAST
  sep <- A.option Nothing $ do
    A.string "$sep$"
    Just <$> pAST
  A.string "$endfor"
  return $ ASTFor mkeyName valName lst loop sep

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
pIdentifier = A.string "@" <|> do
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
pNumber = ASTLiteral . Number <$> A.rational

pArray :: A.Parser AST
pArray = do
  A.char '['
  A.skipSpace
  vals <- pValue `A.sepBy` (A.skipSpace *> A.char ',' *> A.skipSpace)
  A.skipSpace
  A.char ']'
  return $ astListToArray vals

