{-# LANGUAGE OverloadedStrings #-}

module AST where

import qualified Data.Text as T
import IdiomaticScanner (Token (..))
import Scanner (TokenType (..))

data Expr
  = Binary Expr Token Expr
  | Grouping Expr
  | Literal LiteralValue
  | Unary Token Expr
  deriving (Show)

data LiteralValue
  = StringValue String
  | NumberValue Double
  | BoolValue Bool
  | NilValue
  deriving (Show, Eq)

data RuntimeError = RuntimeError
  { message :: String,
    token :: Token
  }
  deriving (Show, Eq)

evaluate :: Expr -> Either [RuntimeError] LiteralValue
evaluate (Literal v) = pure v
evaluate (Grouping expr) = evaluate expr
evaluate (Unary tok@(Token {tokenType = operator}) expr) = case operator of
  Minus -> do
    expr' <- evaluate expr
    case expr' of
      NumberValue value -> pure $ NumberValue (-value)
      _ -> Left [RuntimeError "Operand must be a number" tok]
  Bang -> BoolValue . not . isTruthy <$> evaluate expr
  _ -> Left [RuntimeError "Unknown unary operator, this is likely a parser error" tok]
evaluate (Binary leftExpr tok@(Token {tokenType = operator}) rightExpr) = do
  left <- evaluate leftExpr
  right <- evaluate rightExpr
  case operator of
    Minus -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ NumberValue (n - n')
      _ -> Left [RuntimeError "Operands must be numbers" tok]
    Slash -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ NumberValue (n / n')
      _ -> Left [RuntimeError "Operands must be numbers" tok]
    Star -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ NumberValue (n * n')
      _ -> Left [RuntimeError "Operands must be numbers" tok]
    Plus -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ NumberValue (n + n')
      (StringValue s, StringValue s') -> pure $ StringValue (s <> s')
      _ -> Left [RuntimeError "Operands must be two numbers or two strings" tok]
    Greater -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ BoolValue (n > n')
      _ -> Left [RuntimeError "Operands must be numbers" tok]
    GreaterEqual -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ BoolValue (n >= n')
      _ -> Left [RuntimeError "Operands must be numbers" tok]
    Less -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ BoolValue (n < n')
      _ -> Left [RuntimeError "Operands must be numbers" tok]
    LessEqual -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ BoolValue (n <= n')
      _ -> Left [RuntimeError "Operands must be numbers" tok]
    Equal -> pure $ BoolValue (left == right)
    BangEqual -> pure $ BoolValue (left /= right)
    _ -> Left [RuntimeError "Unknown binary operator, this is likely a parser error" tok]

isTruthy :: LiteralValue -> Bool
isTruthy (BoolValue value) = value
isTruthy NilValue = False
isTruthy _ = True

printLiteral :: LiteralValue -> T.Text
printLiteral (StringValue value) = T.pack $ show value
printLiteral (NumberValue value) = T.pack $ show value
printLiteral (BoolValue True) = "true"
printLiteral (BoolValue False) = "false"
printLiteral NilValue = "nil"

printExpr :: Expr -> T.Text
printExpr (Binary left (Token {tokenType = operator}) right) = "(" <> T.pack (show operator) <> " " <> printExpr left <> " " <> printExpr right <> ")"
printExpr (Grouping expr) = "(group " <> printExpr expr <> ")"
printExpr (Literal value) = printLiteral value
printExpr (Unary (Token {tokenType = operator}) expr) = "(" <> T.pack (show operator) <> " " <> printExpr expr <> ")"