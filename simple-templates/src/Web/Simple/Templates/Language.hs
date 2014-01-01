{-# LANGUAGE OverloadedStrings, Trustworthy #-}
{-|
A simple templating system with variable substitution, function invokation, for
loops and conditionals. Most callers should use 'compileTemplate' and invoke
the template with 'renderTemplate'. E.g.:

> let myTemplate = compileTemplate "Hello, $@$!"
> print $ renderTemplate myTemplate mempty "World"

-}
module Web.Simple.Templates.Language
  (
  -- * Language Description
  -- $lang_def

  -- ** Literals
  -- $literals

  -- ** Variable substitution
  -- $variables

  -- ** Function Invokation
  -- $functions

  -- ** Conditionals
  -- $conditionals

  -- ** For Loops
  -- $loops

  -- * Compilation
  compileTemplate, evaluate, evaluateAST
  -- * Helpers
  , valueToText, replaceVar
  , module Web.Simple.Templates.Types
  ) where

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

evaluateAST :: FunctionMap -- ^ Mapping of functions accessible to the template
            -> Value -- ^ The global 'Object' or 'Value'
            -> AST -> Value
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

    ASTFor mkeyName valName lst body msep ->
      astForLoop fm global mkeyName valName lst body msep

astForLoop :: FunctionMap -> Value
           -> Maybe Identifier -> Identifier
           -> AST -> AST -> Maybe AST -> Value
astForLoop fm global mkeyName valName lst body msep =
  case val of
    Null -> String ""
    Bool False -> String ""
    Array vec ->
      String $ go (zip [0..(V.length vec)] $ V.toList vec) mempty
    Object obj -> String $ go (H.toList obj) mempty
    v -> evaluateAST fm (replaceVar global valName v) body
  where sep = maybe (String "") (evaluateAST fm global) msep
        val = evaluateAST fm global lst
        go [] accm = accm
        go ((k,v):[]) accm =
          let scope = replaceVar (mreplaceKey k) valName v
              nv = evaluateAST fm scope body
          in accm <> valueToText nv
        go ((k,v):x1:xs) accm =
          let scope = replaceVar (mreplaceKey k) valName v
              nv = evaluateAST fm scope body
              accmN =
                accm <> valueToText nv <> valueToText sep
          in go (x1:xs) accmN
        mreplaceKey :: ToJSON a => a -> Value
        mreplaceKey v =
          maybe global (\k -> replaceVar global k $ toJSON v) mkeyName

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
    Null -> ""

compileTemplate :: Text -> Either String Template
compileTemplate tmpl = evaluate <$>
  A.parseOnly pAST tmpl

-- $lang_def
-- A template may contain plain-text, which is reproduced as is, as well as
-- blocks of code, escaped by surrounding with dollar-signs ($), for variable
-- expansion, function invokation, conditionals and loops. For example, given
-- a global variable \"answer\" with the value /42/,
--
-- > The answer to the universe is $answer$.
--
-- would expand to
--
-- > The answer to the universe is 42.
--
-- Since the dollar-sign is used to denote code sections, it must be escaped
-- in plaintext sections by typing two dollar-signs. For example, to reproduce
-- the lyrics for /Bonzo Goes to Bitburg/, by The Ramones:
--
-- > Shouldn't wish you happiness,
-- > wish her the very best.
-- > $$50,000 dress
-- > Shaking hands with your highness

-- $literals
-- 'Data.Aeson.Bool's, 'Number's, 'String's, 'Array's and 'Null' can be typed
-- as literals.
--
-- * 'Data.Aeson.Bool's are the lower-case \"true\" and \"false\"
--
-- * 'Number's are simply typed as decimals
--
-- > Pi is approximately $3.14159$
--
-- * 'String's are surrounded by double-quotes (\"). Double-quotes inside a
-- string can be escaped by proceeding it with a backslash (\\\"), however
-- backslashes themselves do not need to be escaped:
--
-- > And then, Dr. Evil said: $"Mini Me, stop humping the \"laser\"."$
--
-- * 'Array's are surrounded by square-brackets (\[ \]) and elements are comma
-- separated. Elements can be literals, variables or function invokations, and
-- do not have to be the same type:
--
-- > $["Foo", 42, ["bar", "baz"], length([1, 2, 3, 6])]$
--
-- * 'Null' is type as the literal /null/ (in lower case):
--
-- > $null$
--

-- $variables
-- Templates are evaluated with a single global variable called /@/. For
-- example, you can refernce the global in your template like so:
--
-- > The value in my global is $@$.
--
-- If the global is an 'Object', it can be indexed using dot-notation:
--
-- > The Sex Pistols' bassist was $@.bassist.name.first$
--
-- In this case, you may also discard the /@/ global reference and simply name
-- the field in the global object, for example:
--
-- > Field 'foo' is $foo$.
-- > Field 'bar.baz' is $bar.baz$.
--
-- 'String's, 'Number's and 'Data.Aeson.Bool's are meaningful when evaluated to
-- text in a template, while 'Object's, 'Array's and 'Null's simply render as
-- strings representing their types (e.g. \"[object]\"). However, all types can
-- be used as arguments to functions, or in conditionals and loops.

-- $functions
-- Functions are invoked with similar syntax to imperative languages:
--
-- > $myfunc(arg1, arg2, arg3)$
--
-- where arguments can be literals, variables or other function calls --
-- basically anything that can be evaluated can be an argument to a function.
-- Function names are in a separate namespace than variables, so there can be
-- a function and variable both named /foo/ and they are differentiated by
-- their use. For example:
--
-- > $mysymbol$
--
-- is a variable expansion, whereas
--
-- > $mysymbol()$
--
-- is a function invokation.
--

-- $conditionals
-- Branching is supported through the common /if/ statement with an optional
-- /else/ branch. Conditions can be any expression. /false/ and /null/ are
-- evaluated as /false/, while everything else is evaluated as /true/.
--
-- /if/ blocks are surround by an /if/-statement and and /endif/, each
-- surrounded separately by dollar signs. Optionally, the /else/ branch is
-- declared by with \"$else$\". The blocks themselves are templates and may
-- contain regular text as well as evaluable expressions.
--
-- > Should I stay or should I go?
-- > $if(go)$
-- > Trouble will be $trouble$.
-- > $else$
-- > Trouble will be $double(trouble)$
-- > $endif$
--

-- $loops
-- For loops iterate over collections, setting a variable name to one element
-- in the collection for each iteration of the loop. Collections are usually
-- 'Array's, however non-false expressions (e.g., 'String's and 'Number's) are
-- treated as collections with one element. A loop starts with a
-- /for/-statement surrounded by dollar-signs and end with an \"$endfor$\":
--
-- > <h1>The Clash</h1>
-- > <ul>
-- > $for(member in band)$
-- > <li>$member.name$ played the $member.instrument$</li>
-- > $endfor$
-- > </ul>
--
-- There is also an optional \"$sep$\" (for /separator/) clause, which is
-- rendered /between/ iterations. So if I have a collection with three items,
-- the /sep/ clause will be rendered after the first and second, but not third
-- elements:
--
-- > <h1>Grocery list</h1>
-- > <p>
-- > $for(item in groceries)$
-- > $item.quantity$ $item.name$(s).
-- > $sep$
-- > <br/>
-- > $endfor$
-- > </p>
--
-- Will render something like:
--
-- > <h1>Grocery list</h1>
-- > <p>
-- > 2 MC(s).
-- > <br/>
-- > 1 DJ(s)
-- > <br/>
-- > </p>
--

