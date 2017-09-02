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
    , parseValue
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
    = Func Text [Text] [Expr]
    | Alias [Text] [Text]
    | Expression Expr
    deriving (Eq, Show)

data Expr
    = Binding Text Value
    deriving (Eq, Show)

data Value
    = Number Text -- We don't care about the number
    | Identifier Text -- The name of the identifer
    | FuncAp Text [Value] -- List of arguments
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


parseValue :: Parser Value
parseValue =  
    Identifier <$> identifier <|> num <|> funcAp
  where
    num = Number . fst <$> match (
        numLit "0x" isHexDigit <|>
        numLit "0b" (\c -> c == '0' || c == '1') <|>
        numLit "0o" isOctDigit <|>
        double $> ""
        )
    numLit s predicate = string s *> takeWhile1 predicate
    funcAp = do
        name <- identifier
        args <- container (char ',') (char '(') (char ')') parseValue
        pure (FuncAp name args)
                    

parseExpr :: Parser Expr
parseExpr = do
    x <- identifier
    skipSpace
    char '='
    skipSpace
    y <- Identifier <$> identifier <|> num
    pure (Binding x y)
  where
    num = Number . fst <$> match (
      numLit "0x" isHexDigit <|> 
      numLit "0b" (\c -> c == '0' || c == '1') <|> 
      numLit "0o" isOctDigit <|> 
      double $> "")
    numLit s predicate = string s *> takeWhile1 predicate


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
        r <- peekChar'
        let a = case r of
                '(' -> container (char ',') (char '(') (char ')') identifier
                _ -> pure []
        args <- a
        body <- doBlock parseExpr
        pure (Func name args body)
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


container :: Parser sep -> Parser start -> Parser end -> Parser a -> Parser [a]
container sep start end p = start *> loop []
  where
    loop ns = do
        skipSpace
        r <- eitherP end (eitherP sep p)
        case r of
            Left _ -> pure (reverse ns)
            Right (Left _) -> loop ns
            Right (Right n) -> loop (n : ns)
                    





