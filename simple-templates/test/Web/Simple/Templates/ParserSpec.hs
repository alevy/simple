{-# LANGUAGE OverloadedStrings #-}
module Web.Simple.Templates.ParserSpec where

import Data.Aeson
import qualified Data.Attoparsec.Text as A
import Data.Maybe
import Data.Scientific
import qualified Data.Vector as V
import Test.HUnit
import Test.Hspec
import Web.Simple.Templates.Parser
import Web.Simple.Templates.Types

spec :: Spec
spec = describe "Web.Simple.Templates.Parser" $ do
  describe "pEscapedDollar" $ do
    it "reads $$ as escaped dollar" $ do
      let parsedStr = A.parseOnly pEscapedDollar "$$"
      assertEqual "" (Right "$") parsedStr
    it "fails on single $" $ do
      let parsedStr = A.maybeResult $ A.parse pEscapedDollar "$ "
      assert $ isNothing parsedStr
  describe "pRaw" $ do
    it "fails on dollar-sign" $ do
      let parsedStr = A.maybeResult $ A.parse pRaw "$blor doop"
      assert $ isNothing parsedStr
    it "reads non-dollar-sign text" $ do
      let parsedStr = A.parseOnly pRaw "blor doop"
      assertEqual "" (Right $ ASTLiteral (String "blor doop")) parsedStr
    it "reads text until dollar sign" $ do
      let parsedStr = A.parseOnly pRaw "blor doop$"
      assertEqual "" (Right $ ASTLiteral (String "blor doop")) parsedStr
    it "reads text with escaped dollar sing" $ do
      let parsedStr = A.parseOnly pRaw "you owe me $$4"
      assertEqual "" (Right $ ASTLiteral (String "you owe me $4")) parsedStr

  describe "pLiteral" $ do
    it "reads a number" $ do
      let parsedStr = A.parseOnly pLiteral "12345.66"
      assertEqual "" (Right $ fromLiteral (12345.66 :: Scientific)) parsedStr
    it "reads a string" $ do
      let parsedStr = A.parseOnly pLiteral "\"hello\""
      assertEqual "" (Right $ fromLiteral ("hello" :: String)) parsedStr
    it "reads a string with escaped quote" $ do
      let parsedStr = A.parseOnly pLiteral "\"hello \\ \\\"world\""
      assertEqual "" (Right $ fromLiteral ("hello \\ \"world" :: String))
                     parsedStr
    it "reads array" $ do
      let parsedStr = A.parseOnly pLiteral "[1]"
      let expected = ASTArray $ V.fromList [fromLiteral (1 :: Int)]
      assertEqual "" (Right expected) parsedStr
    it "reads 'true' as boolean" $ do
      let parsedStr = A.parseOnly pLiteral "true"
      assertEqual "" (Right $ fromLiteral True) parsedStr
    it "reads 'false' as boolean" $ do
      let parsedStr = A.parseOnly pLiteral "false"
      assertEqual "" (Right $ fromLiteral False) parsedStr
    it "reads null as null value" $ do
      let parsedStr = A.parseOnly pLiteral "null"
      assertEqual "" (Right $ ASTLiteral Null) parsedStr

  describe "pArray" $ do
    it "matches array with literals" $ do
      let parsedStr = A.parseOnly pArray "[1  ,2 ,3 ,  4]"
      let expected = ASTArray $ V.fromList $ map fromLiteral
                      [ 1 :: Int
                      , 2
                      , 3
                      , 4 ]
      assertEqual "" (Right expected) parsedStr
    it "matches array with mixed literals" $ do
      let parsedStr = A.parseOnly pArray "[1,\"hello\"]"
      let expected = ASTArray $ V.fromList
                      [ fromLiteral (1 :: Int)
                      , fromLiteral ("hello" :: String)]
      assertEqual "" (Right expected) parsedStr

  describe "pIdentifier" $ do
    it "matches alphunumeric string starting with a letter" $ do
      let parsedStr = A.parseOnly pIdentifier "l33Th8x0r"
      assertEqual "" (Right "l33Th8x0r") parsedStr
    it "must start with a letter" $ do
      let parsedStr = A.maybeResult $ A.parse pIdentifier "3rd"
      assert $ isNothing parsedStr
    it "allows underscores and dashes" $ do
      let parsedStr = A.parseOnly pIdentifier "l33Th-8x_r"
      assertEqual "" (Right "l33Th-8x_r") parsedStr

  describe "pIndex" $ do
    it "matches an variable name on the left and an identifier on the right" $
      do
        let parsedStr = A.parseOnly pIndex "foo.bar.baz"
        assertEqual "" (Right $ ASTIndex (ASTVar "foo") ["bar", "baz"])
                       parsedStr

  describe "pVar" $ do
    it "matches an identifier" $ do
      let parsedStr = A.parseOnly pVar "foo"
      assertEqual "" (Right $ ASTVar "foo") parsedStr

  describe "pFunc" $ do
    it "matches no argument function call" $ do
      let parsedStr = A.parseOnly pFunc "foo()"
      assertEqual "" (Right $ ASTFunc "foo" []) parsedStr
    it "matches function call with one argument" $ do
      let parsedStr = A.parseOnly pFunc "foo(1234)"
      assertEqual "" (Right $ ASTFunc "foo" [ASTLiteral $ Number 1234])
                     parsedStr
    it "matches function call with many arguments" $ do
      let parsedStr = A.parseOnly pFunc "foo(1234, \"hello\", 5432)"
      assertEqual "" (Right $ ASTFunc "foo" [ ASTLiteral $ Number 1234
                                            , ASTLiteral $ String "hello"
                                            , ASTLiteral $ Number 5432])
                     parsedStr
    it "matches nested function calls" $ do
      let parsedStr = A.parseOnly pFunc "foo(bar())"
      assertEqual "" (Right $ ASTFunc "foo" [ASTFunc "bar" []]) parsedStr
    it "matches function calls with index" $ do
      let parsedStr = A.parseOnly pFunc "foo(bar.baz)"
      assertEqual ""
        (Right $ ASTFunc "foo" [ASTIndex (ASTVar "bar") ["baz"]])
        parsedStr

  describe "pIf" $ do
    it "matches if with no false branch" $ do
      let parsedStr = A.parseOnly pIf "if(1234)$hello world$endif"
      assertEqual "" (Right $
                        ASTIf (ASTLiteral $ Number 1234)
                          (ASTRoot [ASTLiteral $ String "hello world"])
                          Nothing)
                     parsedStr
    it "matches if with nested statement in true branch" $ do
      let parsedStr = A.parseOnly pIf "if(1234)$hello $123$ world$endif"
      assertEqual "" (Right $
                        ASTIf (ASTLiteral $ Number 1234)
                          (ASTRoot [ ASTLiteral $ String "hello "
                                  , ASTLiteral $ Number 123
                                  , ASTLiteral $ String " world"])
                          Nothing)
                     parsedStr
    it "matches with a space after" $ do
      let parsedStr = A.parseOnly pIf "if 1234$hello world$endif"
      assertEqual "" (Right $
                        ASTIf (ASTLiteral $ Number 1234)
                          (ASTRoot [ASTLiteral $ String "hello world"])
                          Nothing)
                     parsedStr
    it "matches with if/else" $ do
      let parsedStr = A.parseOnly pIf "if(true)$$else$$endif"
      assertEqual "" (Right $
                        ASTIf (ASTLiteral $ Bool True)
                          (ASTRoot [])
                          (Just $ ASTRoot []))
                      parsedStr

  describe "pFor" $ do
    it "matches basic for loop" $ do
      let parsedStr = A.parseOnly pFor "for(i in [])$$i$$endfor"
      assertEqual "" (Right $
                        ASTFor Nothing "i" (ASTArray (V.fromList []))
                          (ASTRoot [ASTVar "i"]) Nothing) parsedStr
    it "matches with space after for" $ do
      let parsedStr = A.parseOnly pFor "for i in []$$i$$endfor"
      assertEqual "" (Right $
                        ASTFor Nothing "i" (ASTArray (V.fromList []))
                          (ASTRoot [ASTVar "i"]) Nothing) parsedStr
    it "matches with separator" $ do
      let parsedStr = A.parseOnly pFor "for(i in [])$$i$$sep$hello$endfor"
      assertEqual "" (Right $
                        ASTFor Nothing "i" (ASTArray (V.fromList []))
                          (ASTRoot [ASTVar "i"])
                          (Just $ ASTRoot [ASTLiteral $ String "hello"]))
                     parsedStr
    it "matches for loop with index" $ do
      let parsedStr = A.parseOnly pFor "for(i,v in [])$$i$$endfor"
      assertEqual "" (Right $
                        ASTFor (Just "i") "v" (ASTArray (V.fromList []))
                          (ASTRoot [ASTVar "i"]) Nothing) parsedStr

