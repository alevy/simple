{-# LANGUAGE OverloadedStrings #-}
module Web.Simple.Templates.Language where

import Control.Applicative
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Attoparsec.Text as A
import Web.Simple.Templates.Parser
import Web.Simple.Templates.Types

evaluateAST :: FunctionMap -> Value -> AST -> Value
evaluateAST fm global ast =
  case ast of
    ASTRoot asts -> foldl (\v iast ->
                            let val = evaluateAST fm global iast
                            in String $ valueToText v <> valueToText val)
                          (String "") asts

    ASTLiteral val -> val

    ASTFunc ident args ->
      case H.lookup ident fm of
        Nothing -> Null
        Just func ->
          let argVals = map (evaluateAST fm global) args
          in call func argVals

    ASTVar ident ->
      if ident == "@" then global else
        case global of
          Object obj -> fromMaybe Null $ H.lookup ident obj
          _ -> Null

    ASTIndex objAst idents ->
      foldl (\val ident -> 
        case val of
          Object obj -> fromMaybe Null $ H.lookup ident obj
          _ -> Null) (evaluateAST fm global objAst) idents

    ASTArray asts -> Array $ V.map (evaluateAST fm global) asts

    ASTIf cond trueBranch mfalseBranch ->
      let condVal = evaluateAST fm global cond
          falseBranch = fromMaybe (ASTLiteral $ String "") mfalseBranch
      in if condVal == Null || condVal == Bool False then
           evaluateAST fm global falseBranch
           else evaluateAST fm global trueBranch

    ASTFor varName lst body msep -> astForLoop fm global varName lst body msep

astForLoop :: FunctionMap -> Value
           -> Identifier -> AST -> AST -> Maybe AST -> Value
astForLoop fm global varName lst body msep =
  case val of
    Null -> String ""
    Bool False -> String ""
    Array vec ->
      String $ go (V.toList vec) mempty
    v -> evaluateAST fm (replaceVar global varName v) body
  where sep = maybe (String "") (evaluateAST fm global) msep
        val = evaluateAST fm global lst
        go [] accm = accm
        go (v:[]) accm =
          let scope = replaceVar global varName v
              nv = evaluateAST fm scope body
          in accm <> valueToText nv
        go (v:x1:xs) accm =
          let scope = replaceVar global varName v
              nv = evaluateAST fm scope body
              accmN =
                accm <> valueToText nv <> valueToText sep
          in go (x1:xs) accmN

replaceVar :: Value -> Identifier -> Value -> Value
replaceVar (Object orig) varName newVal = Object $ H.insert varName newVal orig
replaceVar _ varName newVal = object [varName .= newVal]

evaluate :: AST -> Template
evaluate ast = Template $ \fm global ->
  valueToText $ evaluateAST fm global ast

valueToText :: Value -> Text
valueToText val =
  case val of
    String str -> str
    Number n -> T.pack $ show n
    Bool b -> T.pack $ show b
    Array _ -> "[array]"
    Object _ -> "[object]"
    Null -> "null"

compileTemplate :: Text -> Either String Template
compileTemplate tmpl = evaluate <$>
  A.parseOnly pAST tmpl

