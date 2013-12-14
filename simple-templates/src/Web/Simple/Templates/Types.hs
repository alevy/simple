{-# LANGUAGE FlexibleInstances, CPP, Trustworthy #-}
{- | Types and helpers to encode the language AST -}
module Web.Simple.Templates.Types where

import qualified Data.HashMap.Strict as H
import Data.Monoid
import Data.Text (Text)
import Data.Aeson
import qualified Data.Vector as V

-- | A funcation that's callable from inside a template
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

-- | Like 'fromJSON' but throws an error if there is a parse failure.
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

-- | A compiled template is a function that takes a 'FunctionMap' and a global
-- aeson 'Value' and renders the template.
newtype Template = Template
  { renderTemplate :: FunctionMap -> Value -> Text }

instance Monoid Template where
  mempty = Template $ const $ const mempty
  tm1 `mappend` tm2 = Template $ \fm global ->
    renderTemplate tm1 fm global <> renderTemplate tm2 fm global

-- | A symbol identifier following the format [a-z][a-zA-Z0-9_-]*
type Identifier = Text

-- | 'AST's encode the various types of expressions in the language.
data AST = ASTRoot [AST] -- ^ A series of sub-ASTs
         | ASTLiteral Value -- ^ A literal that does not require evaluation
         | ASTFunc Identifier [AST] -- ^ A function call and list of arguments
         | ASTVar Identifier -- ^ Variable dereference
         | ASTIndex AST [Identifier] -- ^ Nested index into an object
         | ASTArray (V.Vector AST)
         -- ^ A literal array (may contain non-literals)
         | ASTIf AST AST (Maybe AST)
         -- ^ If - condition, true branch and optional false branch
         | ASTFor (Maybe Identifier) Identifier AST AST (Maybe AST)
         -- ^ for([k,]v in expr) body separator
  deriving (Show, Eq)

-- | Lift a 'ToJSON' to an 'ASTLiteral'
fromLiteral :: ToJSON a => a -> AST
fromLiteral = ASTLiteral . toJSON

astListToArray :: [AST] -> AST
astListToArray = ASTArray . V.fromList

