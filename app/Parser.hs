{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Control.Monad.Except
import Control.Monad.State
import Data.Functor (($>))
import Data.Sequence (Seq, fromList, (!?))
import qualified Data.Text as T
import Data.Time (UTCTime)
import IdiomaticScanner (Token (..))
import Scanner (TokenType (..))

-- See notes, I decided to keep the book-like parser just for educational purposes
-- and as I go along I'll be only adding the new features to the functional parser.
-- So this file purpose from now on will be two fold:
-- 1) Show the initial parser from the book(with just the expressions, to show that recursive descent is implementable in Haskell)
-- 2) Host the AST types

data Stmt
  = Expression Expr
  | Print Expr
  | VarDecl Token Expr
  | InvalidStmt LoxParseError
  | Block [Stmt]
  | If Expr Stmt (Maybe Stmt)
  | While Expr Stmt
  deriving (Show, Eq)

data Expr
  = Binary Expr Token Expr
  | Logical Expr Token Expr
  | Grouping Expr
  | Literal LiteralValue
  | Unary Token Expr
  | Assign Token Expr
  | Variable Token
  | Call Expr Token [Expr]
  deriving (Show, Eq)

data LiteralValue
  = StringValue String
  | NumberValue Double
  | BoolValue Bool
  | NilValue
  | Fun [Token] [Stmt]
  | Time UTCTime
  deriving (Show, Eq)

data RuntimeError = RuntimeError
  { message :: String,
    token :: Token
  }
  deriving (Show, Eq)

-- As with Scanner, we'll be making stateful
-- computations to match the book as close as possible,
-- so we'll use the State monad.
data ParserState = ParserState
  { tokens :: Seq Token,
    current :: Int
  }
  deriving (Show, Eq)

data LoxParseError = LoxParseError
  { message :: String,
    token :: Maybe Token
  }
  deriving (Show, Eq)

type Parser a = StateT ParserState (Either LoxParseError) a

expression :: Parser Expr
expression = equality

equality :: Parser Expr
equality = comparison >>= go
  where
    go expr = do
      matches <- match [BangEqual, EqualEqual]
      if matches
        then Binary expr <$> previous <*> comparison >>= go
        else pure expr

comparison :: Parser Expr
comparison = term >>= go
  where
    go expr = do
      matches <- match [Greater, GreaterEqual, Less, LessEqual]
      if matches
        then Binary expr <$> previous <*> term >>= go
        else pure expr

term :: Parser Expr
term = factor >>= go
  where
    go expr = do
      matches <- match [Minus, Plus]
      if matches
        then Binary expr <$> previous <*> factor >>= go
        else pure expr

factor :: Parser Expr
factor = unary >>= go
  where
    go expr = do
      matches <- match [Slash, Star]
      if matches
        then Binary expr <$> previous <*> unary >>= go
        else pure expr

unary :: Parser Expr
unary = do
  matches <- match [Bang, Minus]
  if matches
    then Unary <$> previous <*> unary
    else primary

primary :: Parser Expr
primary = do
  isFalse <- match [FalseTok]
  if isFalse
    then pure (Literal $ BoolValue False)
    else do
      isTrue <- match [TrueTok]
      if isTrue
        then pure (Literal $ BoolValue True)
        else do
          isNil <- match [Nil]
          if isNil
            then pure (Literal NilValue)
            else do
              tok <- tokenType <$> peek
              case tok of
                Number n -> advance $> Literal (NumberValue n)
                String s -> advance $> Literal (StringValue $ T.unpack s)
                _ -> do
                  isLeftParen <- match [LeftParen]
                  if isLeftParen
                    then do
                      expr <- expression
                      consume RightParen "Expect ')' after expression." $> Grouping expr
                    else do
                      t <- peek
                      lift $ Left $ LoxParseError "Expect expression." (Just t)

consume :: TokenType -> String -> Parser Token
consume tt errormsg = do
  checks <- check tt
  if checks
    then advance
    else peek >>= lift . Left . LoxParseError errormsg . Just

match :: [TokenType] -> Parser Bool
match [] = pure False
match (t : tt) =
  check t >>= \case
    True -> advance $> True
    False -> match tt

check :: TokenType -> Parser Bool
check tok = (\end p -> not end && p == tok) <$> isAtEnd <*> (tokenType <$> peek)

advance :: Parser Token
advance = do
  atEnd <- isAtEnd
  unless atEnd (modify $ \ParserState {..} -> ParserState {current = current + 1, ..})
  previous

previous :: Parser Token
previous = do
  ParserState {tokens, current} <- get
  case tokens !? (current - 1) of
    Just t -> pure t
    Nothing -> peek >>= lift . Left . LoxParseError "Unexpected end of file." . Just

isAtEnd :: Parser Bool
isAtEnd = (== EOF) . tokenType <$> peek

peek :: Parser Token
peek = do
  ParserState {tokens, current} <- get
  case tokens !? current of
    Just t -> pure t
    Nothing -> lift $ Left $ LoxParseError ("peek failed, tokens: " ++ show tokens ++ " current: " ++ show current) Nothing

parse :: [Token] -> Either LoxParseError Expr
parse tokens = evalStateT expression ParserState {tokens = fromList tokens, current = 0}
