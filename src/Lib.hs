{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    , Parser
    , Module(..)
    , InModule(..)
    , Expr(..)
    , Value(..)
    , parseExpr
    , parseInModule
    , parseModule
    ) where

import Data.Attoparsec.Text as A
import Control.Applicative
import Data.Char
import Data.Text hiding (reverse)
import Data.Functor (($>))


someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Module 
    = Module Text [InModule]
    deriving (Eq, Show)

data InModule
    = Func Text [Expr]
    | Alias [Text] [Text]
    | Expression Expr
    deriving (Eq, Show)

data Expr
    = Binding Text Value
    deriving (Eq, Show)

data Value
    = Number Text
    | Identifier Text
    deriving (Eq, Show)


maybeParse :: Parser a -> Parser (Maybe a)
maybeParse parser = 
    either pure (\_ -> Nothing) <$> eitherP parser (pure ())

identifier :: Parser Text
identifier = do
    c <- satisfy isAlpha
    cs <- A.takeWhile isAlphaNum
    pure (c `cons` cs)

skipHoriSpace :: Parser ()
skipHoriSpace = skipWhile isHorizontalSpace

parseExpr :: Parser Expr
parseExpr = do
    x <- identifier
    skipSpace
    char '='
    skipSpace
    y <- Identifier <$> identifier <|> num
    pure (Binding x y)
  where
    num = Number . fst <$> (match $
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
        r <- maybeParse parser
        case r of
            Just e -> pure [e]
            Nothing -> pure []
    block exprs = do
        skipSpace
        x <- eitherP (string "end") parser
        case x of
            Left _ -> pure $ reverse exprs
            Right e -> block (e : exprs)

        
parseModule :: Parser Module
parseModule = do
    skipSpace
    string "defmodule"
    skipHoriSpace
    name <- identifier
    inMod <- doBlock parseInModule
    pure (Module name inMod)


parseInModule :: Parser InModule
parseInModule = func <|> alias <|> Expression <$> parseExpr
  where
    func = do
      string "def"
      skipHoriSpace
      name <- identifier
      body <- doBlock parseExpr
      pure (Func name body)
    alias = do
        string "alias"
        skipHoriSpace
        first <- identifier
        loop first []
      where
        loop first ns = do
            skipSpace
            m <- maybeParse (char '.')
            case m of
                Nothing ->
                    let (x:xs) = ns
                    in pure (Alias (first : reverse xs) [x])
                Just _ -> tupleOrId first ns
        tupleOrId first ns = do
            skipSpace
            r <- eitherP tuple identifier
            case r of
                Left ts -> pure (Alias (first : reverse ns) ts)
                Right n -> loop first (n : ns)
    tuple = do
        char '{'
        skipSpace
        n <- identifier
        loop [n]
      where
        loop ns = do
            skipSpace
            r <- peekChar'
            A.take 1
            case r of
                '}' -> pure (reverse ns)
                ',' -> do
                    skipSpace
                    n <- identifier
                    loop (n : ns)







