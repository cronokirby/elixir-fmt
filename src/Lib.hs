{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    , Parser
    , Module(..)
    , InModule(..)
    , Expr(..)
    , parseExpr
    , parseInModule
    , parseModule
    ) where

import Data.Attoparsec.Text as A
import Control.Applicative
import Data.Char
import Data.Text
import Data.Functor (($>))


someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Module 
    = Module Text [InModule]
    deriving (Eq, Show)

data InModule
    = Func Text [Expr]
    | Expression Expr
    deriving (Eq, Show)

data Expr
    = Binding Text Text
    deriving (Eq, Show)


identifier :: Parser Text
identifier = do
    c <- satisfy isAlpha
    cs <- A.takeWhile isAlphaNum
    pure (c `cons` cs)

skipHoriSpace :: Parser ()
skipHoriSpace = skipWhile isHorizontalSpace

untilEnd :: Parser a -> Parser [a]
untilEnd p = manyTill p (string "end")

parseExpr :: Parser Expr
parseExpr = do
    x <- identifier
    skipHoriSpace
    char '='
    skipHoriSpace
    y <- identifier <|> number
    pure (Binding x y)
  where
    number = fst <$> (match $
      hexLit <|> binLit <|> octLit <|> double $> "")
    hexLit = string "0x" *> takeWhile1 isHexDigit
    binLit = string "0b" *> takeWhile1 (\c -> c == '0' || c == '1')
    octLit = string "0o" *> takeWhile1 isOctDigit


doBlock :: Parser a -> Parser [a]
doBlock parser = do
    skipHoriSpace
    line <|> (string "do" *> block [])
  where
    line = do
        char ','
        skipSpace -- new lines are allowed here
        string "do:"
        skipHoriSpace
        r <- eitherP parser (pure ())
        case r of
            Left e -> pure [e]
            Right _ -> pure []
    block exprs = do
        skipSpace
        x <- eitherP (string "end") parser
        case x of
            Left _ -> pure $ Prelude.reverse exprs
            Right e -> block (e : exprs)

        
        

parseModule :: Parser Module
parseModule = do
    skipSpace
    string "defmodule"
    skipHoriSpace
    name <- identifier
    inMod <- doBlock parseInModule
    skipWhile (const True)
    pure (Module name inMod)


parseInModule :: Parser InModule
parseInModule = func <|> Expression <$> parseExpr
  where
    func = do
      string "def"
      skipHoriSpace
      name <- identifier
      body <- doBlock parseExpr
      pure (Func name body)

