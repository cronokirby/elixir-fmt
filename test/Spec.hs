{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.Attoparsec

import Data.Text (Text)
import Lib


main :: IO ()
main = hspec $ do
    parsingTests



parsingTests = do
    parseExprTest


parseExprTest = describe "ParseExpr" $ do
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
    shouldParseText t e = t ~> parseExpr `shouldParse` e