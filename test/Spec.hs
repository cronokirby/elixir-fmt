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
        "x = y" `shouldParseText` Binding "x" (Identifier "y")
        "foo = bar" `shouldParseText` Binding "foo" (Identifier "bar")
    it "parses a simple number binding" $ do
        "x = 3" `shouldParseText` Binding "x" (Number "3")
        "x = 1000" `shouldParseText` Binding "x" (Number "1000")
    it "parses decimal number bindings" $ do
        "x = 3.0" `shouldParseText` Binding "x" (Number "3.0")
        "foo = 5.343" `shouldParseText` Binding "foo" (Number "5.343")
    it "parses base litterals" $ do
        "x = 0xFF" `shouldParseText` Binding "x" (Number "0xFF")
        "x = 0b101" `shouldParseText` Binding "x" (Number "0b101")
        "x = 0o101" `shouldParseText` Binding "x" (Number "0o101")
  where
    shouldParseText :: Text -> Expr -> Expectation
    shouldParseText = shouldParseType parseExpr 


parseInModTest = describe "parseInModule" $ do
    it "parses expressions" $ do
        "x = y" `shouldParseInMod` Expression (Binding "x" (Identifier "y"))
        "x = 0xFF" `shouldParseInMod` Expression (Binding "x" (Number "0xFF"))
    it "parses basic functions" $ do
        "def foo, do: x = 3"  `shouldParseInMod` Func "foo" [] [Binding "x" (Number "3")]
        "def foo do x = 3 end" `shouldParseInMod` Func "foo" [] [Binding "x" (Number "3")]
    it "parses larger functions" $ do
        "def foo do\nx=3\ny=bar\nend" 
          `shouldParseInMod` Func "foo" [] [Binding "x" (Number "3"), Binding "y" (Identifier "bar")]
    it "parses alias statements" $ do
        "alias Foo\n.\nBar" `shouldParseInMod` Alias ["Foo"] ["Bar"]
        "alias Foo.Bar.Baz" `shouldParseInMod` Alias ["Foo", "Bar"] ["Baz"]
        "alias Foo.{Bar, Baz}" `shouldParseInMod` Alias ["Foo"] ["Bar", "Baz"]
        "alias Foo.Bar.Baz.{Scooby, Doo}"
            `shouldParseInMod` Alias ["Foo", "Bar", "Baz"] ["Scooby", "Doo"]
        "alias Foo\n.{\nScooby,\nDoo}" 
            `shouldParseInMod` Alias ["Foo"] ["Scooby", "Doo"]
    it "parses functions with arguments" $ do
        "def foo(a, b, c), do: x = 3" `shouldParseInMod` Func "foo" ["a", "b", "c"] [Binding "x" (Number "3")]
  where
    shouldParseInMod :: Text -> InModule -> Expectation
    shouldParseInMod = shouldParseType parseInModule


parseModTest = describe "parseModule" $ do
    it "parses empty modules" $ do
        "defmodule M do end" `shouldParseMod` Module "M" []
        "defmodule M12, do: " `shouldParseMod` Module "M12" []
    it "parses simple expressions in modules" $ do
        "defmodule M, do: x = 3" 
            `shouldParseMod` Module "M" [Expression (Binding "x" (Number "3"))]
        "defmodule Foo do\nx=3\nfoo=bar\nend"
            `shouldParseMod` Module "Foo" 
            (Expression <$> [Binding "x" (Number "3"), Binding "foo" (Identifier "bar")])
    it "parses functions + expressions" $ do
        "defmodule Foo do\ndef foo, do: x = 3\ny=3 end"
            `shouldParseMod` Module "Foo" [Func "foo" [] [Binding "x" (Number "3")],
                Expression (Binding "y" (Number "3"))]
    it "parses functions, expressions, aliases" $ do
        "defmodule Foo do\nalias Foo.{Scooby, Doo}\nx=3\ndef foo, do: y = 3 end"
            `shouldParseMod` Module "Foo" [Alias ["Foo"] ["Scooby", "Doo"],
              Expression (Binding "x" (Number "3")), Func "foo" [] [Binding "y" (Number "3")]]
  where
    shouldParseMod :: Text -> Module -> Expectation
    shouldParseMod = shouldParseType parseModule


parseValueTest = describe "parseValue" $ do
    it "parses identifiers" $ do
        "a3" `shouldParseValue` Identifier "a3"
        "b55_bomber" `shouldParseValue` Identifier "b55_bomber"
    it "parses numbers" $ do
        "0x34" `shouldParseValue` Number "0x34"
    it "parses basic functions" $ do
        "f(1, 2, 3)" `shouldParseValue` FuncAp "f" [Number "1", Number "2", Number "3"]
        "f(1, x)" `shouldParseValue` FuncAp "f" [Number "1", Identifier "x"]
        "f()" `shouldParseValue` FuncAp "f" []
    it "parses nested functions" $ do
        "f(g(\n1, 2), 3)" 
            `shouldParseValue` FuncAp "f" [FuncAp "g" [Number "1", Number "2"], Number "3"]
        "f(g(\nh()))" `shouldParseValue` FuncAp "f" [FuncAp "g" [FuncAp "h" []]]
        
  where
    shouldParseValue :: Text -> Value -> Expectation 
    shouldParseValue = shouldParseType parseValue