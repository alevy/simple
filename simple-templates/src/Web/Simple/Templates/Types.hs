{-# LANGUAGE FlexibleInstances, CPP #-}
module Web.Simple.Templates.Types where

import qualified Data.HashMap.Strict as H
import Data.Monoid
import Data.Text (Text)
import Data.Aeson
import qualified Data.Vector as V

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
  toFunction f = Function $ \args -> \
    case args of { \
      [] -> call (toFunction (f $ fromJSONStrict Null)) [] ; \
      a:as -> call (toFunction (f $ fromJSONStrict a)) as} ; \
}


instance (FromJSON a) => ToFunction (a -> Value) where
  toFunction f = Function $ \args ->
    case args of
      [] -> toJSON $ f $ fromJSONStrict Null
      a:_ -> toJSON $ f $ fromJSONStrict a

TypesConds(TOFUNCTION)

type FunctionMap = H.HashMap Identifier Function

newtype Template = Template
  { unTemplate :: FunctionMap -> Value -> Text }

instance Monoid Template where
  mempty = Template $ const $ const mempty
  tm1 `mappend` tm2 = Template $ \fm global ->
    unTemplate tm1 fm global <> unTemplate tm2 fm global

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

