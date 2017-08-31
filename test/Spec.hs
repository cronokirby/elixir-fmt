{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.Attoparsec

import Data.Text (Text)
import Lib


main :: IO ()
main = hspec $ do
    parsingTests



shouldParseType :: (Eq a, Show a) => Parser a -> Text -> a -> Expectation
shouldParseType p t e = t ~> p `shouldParse` e


parsingTests = do
    parseExprTest
    parseInModTest
    parseModTest


parseExprTest = describe "parseExpr" $ do
    it "parses simple bindings" $ do
        "x = y" `shouldParseText` Binding "x" "y"
        "foo = bar" `shouldParseText` Binding "foo" "bar"
    it "parses a simple number binding" $ do
        "x = 3" `shouldParseText` Binding "x" "3"
        "x = 1000" `shouldParseText` Binding "x" "1000"
    it "parses decimal number bindings" $ do
        "x = 3.0" `shouldParseText` Binding "x" "3.0"
        "foo = 5.343" `shouldParseText` Binding "foo" "5.343"
    it "parses base litterals" $ do
        "x = 0xFF" `shouldParseText` Binding "x" "0xFF"
        "x = 0b101" `shouldParseText` Binding "x" "0b101"
        "x = 0o101" `shouldParseText` Binding "x" "0o101"
  where
    shouldParseText :: Text -> Expr -> Expectation
    shouldParseText = shouldParseType parseExpr 


parseInModTest = describe "parseInModule" $ do
    it "parses expressions" $ do
        "x = y" `shouldParseInMod` Expression (Binding "x" "y")
        "x = 0xFF" `shouldParseInMod` Expression (Binding "x" "0xFF")
    it "parses basic functions" $ do
        "def foo, do: x = 3"  `shouldParseInMod` Func "foo" [Binding "x" "3"]
        "def foo do x = 3 end" `shouldParseInMod` Func "foo" [Binding "x" "3"]
    it "parses larger functions" $ do
        "def foo do\nx=3\ny=bar\nend" 
          `shouldParseInMod` Func "foo" [Binding "x" "3", Binding "y" "bar"]
  where
    shouldParseInMod :: Text -> InModule -> Expectation
    shouldParseInMod = shouldParseType parseInModule


parseModTest = describe "parseModule" $ do
    it "parses empty modules" $ do
        "defmodule M do end" `shouldParseMod` Module "M" []
        "defmodule M12, do: " `shouldParseMod` Module "M12" []
    it "parses simple expressions in modules" $ do
        "defmodule M, do: x = 3" 
            `shouldParseMod` Module "M" [Expression (Binding "x" "3")]
        "defmodule Foo do\nx=3\nfoo=bar\nend"
            `shouldParseMod` Module "Foo" 
            (Expression <$> [Binding "x" "3", Binding "foo" "bar"])
    it "parses functions + expressions" $ do
        "defmodule Foo do\ndef foo, do: x = 3\ny=3 end"
            `shouldParseMod` Module "Foo" [Func "foo" [Binding "x" "3"],
                Expression (Binding "y" "3")]
  where
    shouldParseMod :: Text -> Module -> Expectation
    shouldParseMod = shouldParseType parseModule